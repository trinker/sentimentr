if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh(file.path("trinker", c('lexicon', "sentimentr", "stansent", "textshape", "textreadr")))
pacman::p_load_gh("sfeuerriegel/SentimentAnalysis", "wrathematics/meanr")
pacman::p_load(syuzhet, dplyr, tidyr, ggplot2, RColorBrewer, gridExtra, RSentiment)

loc <- "sentiment_data"
dir.create(loc)

'http://archive.ics.uci.edu/ml/machine-learning-databases/00331/sentiment%20labelled%20sentences.zip' %>%
    download() %>%
    unzip(exdir = loc)
	
	
## function to see if the signs of two columns differ row by row
## NA means you couldn't figure it out and thus results in a FALSE
signer <- function(x, y) {
    m<- sign(x) == sign(y)
    m[sapply(m, is.na)] <- FALSE
    m
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
        split_sentence() %>%
        mutate(text2 = sentimentr::get_sentences(text))# %>% slice(1:10)

    ## syuzhet sentiment
    syuzhet <- setNames(as.data.frame(lapply(c("syuzhet", "bing", "afinn", "nrc"),
        function(x) round(get_sentiment(dat$text, method=x), 2))), paste0("syuzhet_", c("syuzhet", "bing", "afinn", "nrc")))

    ## throws error as or 2/24/2017
    ## RSentiment and replace sarcasm with negative
    #RSentiment <- calculate_score(dat$text)
    #RSentiment[RSentiment == 99] <- -1    

    ## meanr scores
    meanr <- meanr::score(dat$text)$score

    ## calculate sentimentr sentiment and put all the pieces together
    out <- data.frame(
        dat,

        stanford = round(sentiment_stanford_by(dat[["text"]])[["ave_sentiment"]], 2),

        sentimentr_hu_liu = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_huliu, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_sentiword = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_sentiword, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_jockers = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_jockers, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_jockers_rinker = round(sentiment_by(dat$text2, polarity_dt = lexicon::hash_sentiment_jockers_rinker, question.weight = 0)[["ave_sentiment"]], 2),        
        sentimentr_bing = round(sentiment_by(dat$text2, polarity_dt = bing, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_afinn = round(sentiment_by(dat$text2, polarity_dt = afinn, question.weight = 0)[["ave_sentiment"]], 2),
        sentimentr_nrc = round(sentiment_by(dat$text2, polarity_dt = nrc, question.weight = 0)[["ave_sentiment"]], 2),

        #RSentiment = RSentiment,

        meanr = meanr,

        syuzhet,
        stringsAsFactors = FALSE
    )

    data_frame(
            Method =  c("stanford", "sentimentr_hu_liu", "sentimentr_bing", "sentimentr_afinn",
                "sentimentr_nrc", "sentimentr_sentiword", "sentimentr_jockers", "sentimentr_jockers_rinker",
                "meanr", "syuzhet_bing", "syuzhet_afinn", "syuzhet_nrc",
                "syuzhet_syuzhet"),
            package = c('java', rep('sentimentr', 7), "meanr", rep('syuzhet', 4))
        ) %>%
        {suppressWarnings(left_join(.,
            out %>%
                tbl_df() %>%
                select(-c(sentence_id, text, text2)) %>%
                gather(Method, score, -c(rating, element_id)) %>%
                group_by(Method, element_id) %>%
                summarize(score = mean(score), rating=mean(rating)) %>%
                mutate(same = signer(score, rating)) %>%
                select(-score) %>%
                group_by(Method) %>%
                summarize(accurate = mean(same, rm.na=TRUE))
        ))} %>%
        arrange(desc(accurate))

})


plots <- results_list %>%
    setNames(file.path(loc, "sentiment labelled sentences") %>%
        dir(pattern = "labelled\\.txt$") %>%
        gsub("_[^_]+$", "", .)
    ) %>%
    tidy_list("context") %>%
    split(., .[["context"]]) %>%
    lapply(function(x){
        x %>%
            arrange(desc(accurate)) %>%
            mutate(Method = factor(Method, levels=rev(.[["Method"]]))) %>%
            ggplot(aes(weight = accurate, x = Method, fill=package)) +
                geom_bar(width = .85) +
                geom_hline(yintercept = .5, color="blue", alpha = .4, size=.8, linetype = 2) +
                coord_flip() +
                #facet_wrap(~context, ncol=1) +
                scale_y_continuous(expand = c(0, 0), limits = c(0, 1), labels = scales::percent) +
                ggtitle(x[["context"]][1]) +
                theme_minimal() +
                guides(fill = guide_legend(keywidth = .7, keyheight = .7)) +
                theme(
                    panel.grid.major.y = element_blank(),
                    strip.text = element_text(face="bold"),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(color = "gray65"),
                    axis.ticks.x = element_blank(),
                    axis.title = element_text(size=10, color = "gray65"),
                    legend.title = element_text(color = "grey65", size=10),
                    legend.text = element_text(color = "grey40"),
                    legend.position = if (x[["context"]][1] == "yelp") {c(.87, .28)} else {"none"},
                    plot.title = element_text(size=12, hjust = 0, color='gray40')
                ) +
                annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "gray40")+
                annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "gray40") +
                geom_text(aes(y = accurate + .01,
                    label = paste0(round(100*accurate, 1), "%")),
                    color = "grey70", size = 3, hjust = 0) +
                ylab(if (x[["context"]][1] == "yelp") {"Accuracy Rate"} else {""}) +
                xlab(if (x[["context"]][1] == "imdb") {"Method"} else {""}) +
                scale_fill_manual(values = brewer.pal(7, "BrBG")[c(1, 7, 2, 6)], "Package")
    })




set.seed(14)
ranked <- results_list %>%
    setNames(file.path(loc, "sentiment labelled sentences") %>%
        dir(pattern = "labelled\\.txt$") %>%
        gsub("_[^_]+$", "", .)
    ) %>%
    tidy_list("context") %>%
    mutate(Method = factor(Method, levels=rev(results_list[[1]][["Method"]]))) %>%
    group_by(context) %>%
    mutate(Rank = rank(accurate, ties.method="random"))




rank_dat <- ranked %>%
    filter(context != "imdb") %>%
    split(., .[["context"]])%>%
    lapply(function(x){
        x %>%
            arrange(desc(Rank)) %>%
            mutate(Method = factor(Method, levels=rev(results_list[[1]][["Method"]])))
    })



rank_plot <- ggplot(ranked, mapping = aes(context, y = Rank, group = Method, color = package)) +
    geom_line(size = 3) +
    geom_point(size = 4) +
    geom_point(size = 1.75, color = "white") +
    scale_y_continuous(breaks=1:12, labels = 12:1) +
    theme_bw() +
    theme(
        legend.position="none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'gray65'),
        axis.text.y = element_text(size=10, color = "gray65"),
        axis.title = element_text(size=10, color = "gray65"),
        axis.text.x = element_text(size=12, color = "gray40"),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(colour = "gray85")
    ) +
    scale_color_manual(values = brewer.pal(7, "BrBG")[c(1, 7, 2, 6)], "Package") +
    geom_text(data=rank_dat[[1]], aes(label = Method), x = .9, hjust = 1, size = 3.2, color="gray65") +
    geom_text(data=rank_dat[[2]], aes(label = Method), x = 3.1, hjust = 0, size = 3.2, color="gray65") +
    coord_cartesian(xlim = c(.38, 3.65)) +
    xlab(NULL)



grid.arrange(do.call(arrangeGrob, plots), rank_plot, ncol = 2)

# png("comparisons_between_sentiment_detectors.png", width=800, height=800)
# pdf("comparisons_between_sentiment_detectors.pdf", width=9.5, height=7.5)
grid.arrange(do.call(arrangeGrob, plots), rank_plot, ncol = 2)
# dev.off()



pdf("comparisons_between_sentiment_detectors.pdf", width=12, height=10)
grid.arrange(do.call(arrangeGrob, plots), rank_plot, ncol = 2)
dev.off()

png("comparisons_between_sentiment_detectors.png", width=900, height=900)
grid.arrange(do.call(arrangeGrob, plots), rank_plot, ncol = 2)
dev.off()