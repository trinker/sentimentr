pacman::p_load(tidyverse, textshape, textclean)

safe_replace <- function(text) {
    
    text2 <- gsub('[^ -~]', ' ', stringi::stri_enc_toascii(text))
    out <- try(replace_non_ascii(text2)) 
    out
}

crowdflower_self_driving_cars <- 'https://www.crowdflower.com/wp-content/uploads/2016/03/Twitter-sentiment-self-drive-DFE.csv' %>%
    read_csv() %>%
    filter(sentiment %in% 1:5) %>%
    select(sentiment, text) %>%
    mutate(
        sentiment = as.integer(sentiment) - 3,
        text = safe_replace(text)
    ) %>% 
    data.table::data.table()

crowdflower_self_driving_cars$sentiment %>% table()
sum(grepl('[^ -~]', crowdflower_self_driving_cars$text))



pax::new_data(crowdflower_self_driving_cars)
# 
# 
# 
# crowdflower_progressives <- 'https://www.crowdflower.com/wp-content/uploads/2016/03/progressive-tweet-sentiment.csv' %>%
#     read_csv() %>%
#     filter(sentiment %in% 1:5) %>%
#     select(sentiment, text) %>%
#     mutate(
#         sentiment = as.integer(sentiment) - 3,
#         text = safe_replace(text)
#     ) %>% 
#     data.table::data.table()
# 
# pax::new_data(crowdflower_progressives)
# 


crowdflower_weather <- 'https://www.crowdflower.com/wp-content/uploads/2016/03/weather-evaluated-agg-DFE.csv' %>%
    read_csv() %>%
    #filter(sentiment %in% 1:5) %>%
    select(sentiment, tweet_text) %>%
    rename(text = tweet_text) %>%
    mutate(
        sentiment = case_when(
            grepl('^negative', sentiment, ignore.case = TRUE) ~ -1, 
            grepl('^positive', sentiment, ignore.case = TRUE) ~ 1, 
            grepl('neutral', sentiment, ignore.case = TRUE) ~ 0, 
            TRUE ~ as.numeric(NA)
        ),
        text = safe_replace(text)
    ) %>%
    filter(!is.na(sentiment)) %>% 
    data.table::data.table()


pax::new_data(crowdflower_weather)

to_byte <- function(x){
    Encoding(x) <- "latin1"
    iconv(x, "latin1", "ASCII", "byte")
}

crowdflower_deflategate <- 'https://www.crowdflower.com/wp-content/uploads/2016/03/Deflategate-DFE.csv' %>%
    read_csv() %>%
    #filter(sentiment %in% 1:5) %>%
    select(deflate_sentiment, text) %>%
    rename(sentiment = deflate_sentiment) %>%
    mutate(
        sentiment = case_when(
            sentiment == 'negative' ~ -1, 
            sentiment == 'slighly negative' ~ -.5, 
            sentiment == 'neutral' ~  0,
            sentiment == 'slightly positive' ~  .5,
            sentiment == 'positive' ~  1,
            TRUE ~ as.numeric(NA)
        ),
        text = safe_replace(to_byte(text))
    ) %>%
    filter(!is.na(sentiment)) %>% 
    data.table::data.table()


pax::new_data(crowdflower_deflategate)



crowdflower_products <- 'https://www.crowdflower.com/wp-content/uploads/2016/03/judge-1377884607_tweet_product_company.csv' %>%
    read_csv() %>%
    #filter(sentiment %in% 1:5) %>%
    select(is_there_an_emotion_directed_at_a_brand_or_product, tweet_text) %>%
    rename(sentiment = is_there_an_emotion_directed_at_a_brand_or_product, text = tweet_text) %>%
    mutate(
        sentiment = case_when(
            grepl('^negative', sentiment, ignore.case = TRUE) ~ -1, 
            grepl('^positive', sentiment, ignore.case = TRUE) ~ 1, 
            grepl('^no emotion', sentiment, ignore.case = TRUE) ~ 0,
            TRUE ~ as.numeric(NA)
        ),
        text =  safe_replace(to_byte(text))
    ) %>%
    filter(!is.na(sentiment)) %>% 
    data.table::data.table()


pax::new_data(crowdflower_products)
hist(crowdflower_products[[1]])
