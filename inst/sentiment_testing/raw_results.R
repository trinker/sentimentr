if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr", "trinker/coreNLPsetup", "trinker/stansent", 
    "trinker/textshape", "sfeuerriegel/SentimentAnalysis", "wrathematics/meanr")

pacman::p_load(syuzhet, dplyr, tidyr, textreadr, ggplot2, RSentiment)

loc <- "sentiment_data"
dir.create(loc)

'http://archive.ics.uci.edu/ml/machine-learning-databases/00331/sentiment%20labelled%20sentences.zip' %>%
    textreadr::download() %>%
    unzip(exdir = loc)
	
	
## function to see if the signs of two columns differ row by row
## NA means you couldn't figure it out and thus results in a FALSE
signer <- function(x, y) {
    m<- sign(x) == sign(y)
    m[sapply(m, is.na)] <- FALSE
    m
}

replace_emograte <- function(text.var, ...){
    replace_rating(replace_emoticon(text.var))
}

## Make syuzhet dictionaries into sentimentr keys
bing <- as_key(syuzhet:::bing)
afinn <- as_key(syuzhet:::afinn)
nrc <- lexicon::hash_sentiment_nrc

syuzhet_dict <- as_key(syuzhet:::syuzhet_dict)

results_list <- file.path(loc, "sentiment labelled sentences") %>%
    dir(pattern = "labelled\\.txt$", full.names = TRUE) %>%
    lapply(function(x){

    ## read in the data and split into sentences
    dat <- x %>%
        read.delim(sep = "\t", header=FALSE, stringsAsFactors = FALSE,
            strip.white = TRUE, quote = "") %>%
        setNames(c("text", "rating")) %>%
        na.omit() %>%
        mutate(rating = 2*(as.numeric(rating) - .5)) %>%
        mutate(text = replace_emograte(text)) %>%  
        textshape::split_sentence() %>%
        mutate(text2 = sentimentr::get_sentences(text)) #%>% slice(1:10)

    ## syuzhet sentiment
    syuzhet <- setNames(as.data.frame(lapply(c("syuzhet", "bing", "afinn", "nrc"),
        function(x) round(get_sentiment(dat$text, method=x), 2))), paste0("syuzhet_", c("syuzhet", "bing", "afinn", "nrc")))

    ## RSentiment and replace sarcasm with negative
    #RSentiment <- calculate_score(dat$text)
    #RSentiment[RSentiment == 99] <- -1    

    ## meanr scores
    meanr <- meanr::score(dat$text)$score

    ## calculate sentimentr sentiment and put all the pieces together
    data.frame(
        dat,

        stanford = round(sentiment_stanford_by(dat[["text"]])[["ave_sentiment"]], 2),

        sentimentr_hu_liu = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_huliu, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_sentiword = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_sentiword, question.weight = 0)[["ave_sentiment"]], 2),
        
        sentimentr_jockers = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_jockers, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_jockers_rinker = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_jockers_rinker, question.weight = 0)[["ave_sentiment"]], 2),    
        
        sentimentr_bing = round(sentiment_by(dat$text2, polarity_dt = bing, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_afinn = round(sentiment_by(dat$text2, polarity_dt = afinn, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_nrc = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_nrc, question.weight = 0)[["ave_sentiment"]], 2),

        #RSentiment = RSentiment,

        meanr = meanr,

        syuzhet,
        stringsAsFactors = FALSE
    )
}) %>%
    setNames(file.path(loc, "sentiment labelled sentences") %>%
        dir(pattern = "labelled\\.txt$") %>%
        gsub("_[^_]+$", "", .)
    )

# saveRDS(results_list, file = "results_list.rds")
# results_list <- readRDS("inst/sentiment_testing/results_list.rds")


results_list %>%
    tidy_list("Context") %>%
    select(-text2) %>%
    gather(Method, Score, -c(text, rating, Context, element_id, sentence_id)) %>%
    tbl_df() %>%
    group_by(Method, Context, element_id) %>%
    summarize(
        rating = mean(rating, na.rm	= TRUE),
        Score = mean(Score, na.rm	= TRUE),
        text = paste(text, collapse = " ")
    ) %>%
    ungroup()  %>%
    arrange(Context, Method, element_id) %>%
    mutate(
        rating = sign(rating),
        Score = sign(Score)
    ) %>%
    split(., .[c("Method", "Context")]) %>%
    lapply(function(x) {
        as.data.frame(table(x$rating, x$Score)) %>%
            setNames(c("Actual", "Predicted", "n")) %>%
            spread(Predicted, n)
    }) %>%
    tidy_list("Context") %>%
    separate(Context, c("Method", "Context"), "\\.") %>%
    gather(Predicted, n, - c(Method, Context, Actual)) %>% #select(Method) %>% unlist() %>% unique() %>% sort()
    mutate(Method = factor(Method, levels = c("stanford", "sentimentr_afinn", "sentimentr_bing", 
        "sentimentr_hu_liu", "sentimentr_nrc", "sentimentr_sentiword", "sentimentr_jockers_rinker", "sentimentr_jockers",
        "meanr", "syuzhet_syuzhet", "syuzhet_afinn", "syuzhet_bing", "syuzhet_nrc"))) %>%
    ggplot(aes(Predicted, Actual, fill =n)) +
        geom_tile() +
        facet_grid(Method~Context) +
        scale_fill_gradient(high="red", low="white") +
        geom_text(aes(label = n), color = "grey80", size=2) +
        theme_minimal() +
        theme(
            panel.border = element_rect(colour = "grey90", fill = NA),
            panel.grid = element_blank(),
            strip.text.y = element_text(angle=0, hjust = 0)
        )

ggsave("comparisons_between_sentiment_detectors2.pdf", width=7, height=5.5)
ggsave("comparisons_between_sentiment_detectors2.png", width=7, height=5.5)



