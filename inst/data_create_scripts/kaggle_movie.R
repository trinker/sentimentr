library(tidyverse)

https://www.kaggle.com/c/si650winter11/data

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
        Polarity = ifelse(Sentiment == -1, 'Negative', 'Positive')
    ) %>%
    select(Polarity, Sentiment, Text)

pax::new_data(kaggle_movie_reviews)
