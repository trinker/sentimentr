pacman::p_load(qdapRegex, tidyverse, dplyr, data.table, readr, textreadr, sentimentr, stansent)

loc <- file.path(tempdir(), "vadar")
dir.create(loc)

'https://github.com/cjhutto/vaderSentiment/raw/master/additional_resources/hutto_ICWSM_2014.tar.gz' %>%
    textreadr::download() %>%
    untar(exdir = loc)

nyt_articles <- read.csv(file.path(loc, 'hutto_ICWSM_2014/nytEditorialSnippets_GroundTruth.txt'), sep="\t",
    header = FALSE, stringsAsFactors = FALSE) %>%
    setNames(c('x', 'Sentiment', 'Text')) %>%
    tbl_df() %>%
    separate(x, c("element_id", "sentence_id"), "_") %>%
    mutate(
        Polarity = ifelse(Sentiment < 0, 'Negative', ifelse(Sentiment > 0, 'Positive', 'Neutral')),
        Text = gsub("(\\s+'\\s+s\\s+)", "'s ", textclean::replace_non_ascii(Text), perl = TRUE)
    ) %>%
    setNames(tolower(names(.))) %>%
    select(element_id, sentence_id, polarity, sentiment, text)

pax::new_data(nyt_articles)
