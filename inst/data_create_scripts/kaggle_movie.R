library(tidyverse)

#https://www.kaggle.com/c/si650winter11/data

kaggle_movie_reviews <- read.csv(
    "C:/Users/Tyler/Desktop/kaggle_training.txt" ,
    sep='\t', 
    header=FALSE, 
    quote = "",
    stringsAsFactor=F,
    col.names=c("Sentiment", "Text")) %>%
    tbl_df() %>%
    mutate(
        Sentiment = ifelse(Sentiment == 0, -1, Sentiment),
        Polarity = ifelse(Sentiment == -1, 'Negative', 'Positive'),
        Text = gsub("(\\s+'\\s+s\\s+)", "'s ", textclean::replace_non_ascii(Text), perl = TRUE)
    ) %>%
    select(Polarity, Sentiment, Text) %>%
    setNames(tolower(names(.)))

kaggle_movie_reviews <- sentimentr::kaggle_movie_reviews %>%
    setNames(tolower(names(.)))


pax::new_data(kaggle_movie_reviews)

pp7 <- with(kaggle_movie_reviews, check_text(Text))
