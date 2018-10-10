if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load_current_gh('trinker/sentimentr', 'trinker/numform', 'trinker/textcorpus')
p_load(sentimentr, tidyverse, lexicon, textshape, textreadr, janeaustenr, textclean)
p_load(rvest, xml2)

trump_speeches <- 'https://raw.githubusercontent.com/ryanmcdermott/trump-speeches/master/speeches.txt' %>%
    download() %>%
    readLines() %>%
    split_match('SPEECH \\d+', regex = TRUE) %>%
    lapply(paste, collapse =" ") %>%
    tidy_list() %>%
    split_sentence()

books <- p_data(janeaustenr)[["Data"]]
austen <- books %>%
    lapply(function(x) eval(parse(text=x))) %>%
    setNames(books) %>%
    tidy_list('book')

trump_tweets <- 'https://raw.githubusercontent.com/sashaperigo/Trump-Tweets/master/data.csv' %>%
    download() %>%
    read_csv()

dylan <- textcorpus::dylan_songs$corpus$text

attributes_rate <- list(
    sentiment_attributes(hu_liu_cannon_reviews$review_id),
    sentiment_attributes(presidential_debates_2012$dialogue),
    sentiment_attributes(trump_speeches$content),
    sentiment_attributes(trump_tweets$Text),
    sentiment_attributes(dylan),
    sentiment_attributes(rm_non_ascii(austen$content)),
    sentiment_attributes(hamlet$dialogue)    
) %>%
    lapply(function(y){
        x <- y[['Polarized_Cooccurrences']]
        data.frame(setNames(as.list(f_prop2percent(x[[2]], 0)), gsub('-', '', x[[1]])), 
        stringsAsFactors = FALSE, check.names = FALSE)
    }) %>%
    setNames(c('Cannon reviews', '2012 presidential debate', 'Trump speeches', 'Trump tweets', "Dylan songs", 'Austen books', 'Hamlet')) %>%
    tidy_list('text')


saveRDS(attributes_rate, 'attributes_rate.rds')
