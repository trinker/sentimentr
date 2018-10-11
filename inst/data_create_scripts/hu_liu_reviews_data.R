if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, textreadr, textshape, data.table)

dl_loc <- 'http://www.cs.uic.edu/~liub/FBS/CustomerReviewData.zip'  %>%
    download() %>%
    un_zip()


reviews <- dir(dl_loc, pattern = 'customer', full.names = TRUE)[1] %>%
    dir(pattern = '\\.txt$', full.names = TRUE) %>%
    `[`(-6)

##================
## From the README
##================
##   [t]: the title of the review: Each [t] tag starts a review. 
##        We did not use the title information in our papers.
##   xxxx[+|-n]: xxxx is a product feature. 
##       [+n]: Positive opinion, n is the opinion strength: 3 strongest, 
##             and 1 weakest. Note that the strength is quite subjective. 
##             You may want ignore it, but only considering + and -
##       [-n]: Negative opinion
##   ##  : start of each sentence. Each line is a sentence. 
##   [u] : feature not appeared in the sentence.
##   [p] : feature not appeared in the sentence. Pronoun resolution is needed.
##   [s] : suggestion or recommendation.
##   [cc]: comparison with a competing product from a different brand.
##   [cs]: comparison with a competing product from the same brand.



extract_categories <- function(x){

    stringi::stri_split_regex(x, ',') %>% 
        lapply(function(y) { 
            trimws(gsub('\\[[+0-9a-z-]{1,3}\\]', '', y))
        })
}

    # x <- reviews[5]

i <- 1:5
    
hu_liu <- lapply(reviews[i], function(x){
    
    txt <- x %>%
        readLines() 
# browser()    
    # txt %>%
    # {grep('\\[t\\]', ., value = TRUE)}
# browser()    
    txt %>%
        {gsub('\\[\\+\\]', '[+1]', ., perl = TRUE)} %>%
        {gsub('\\[\\-\\]', '[-1]', ., perl = TRUE)} %>%
        split_match_regex('\\[t\\]') %>% 
        `[`(-1) %>%
        tidy_list('review_id', 'text') %>%
        as_tibble() %>%
        mutate(
            category = gsub('##.+$', '', text),
            rating = lapply(stringi::stri_extract_all_regex(category, '(?<=\\[)([+0-9-]+)(?=\\])'), as.numeric),
            category = extract_categories(category),
            text = trimws(gsub('^[^#]*##', '', text)) %>%
                {gsub('\\bi\\b', 'I', ., perl = TRUE)} %>%
                {gsub('(^.)', '\\U\\1', ., perl = TRUE)} %>%
                {gsub('(\\s+)([.?!;,:]|n\'t|\'[a-z])', '\\2', ., perl = TRUE)} %>%
                {gsub('(\\s+)(\'s\\b)', '\\2', ., perl = TRUE)}
        ) %>%
        filter(!grepl('^\\s*$', text)) %>%
        mutate(
            review_id = as.integer(review_id) - 1 ,#%>% as.integer(),
            id = 1:n()
        ) %>%
        group_by(review_id) %>%
        mutate(
            sent_id = 1:n()
        ) %>%
        ungroup()  %>%
        group_by(id) %>%
        mutate(
            sentiment = mean(unlist(rating)),
            sentiment = ifelse(is.na(sentiment), 0, sentiment)/3
        ) %>%
        ungroup() %>%
        select(sentiment, text, review_id) %>%
        data.table::as.data.table()
        
}) %>%
    setNames(paste0('hu_liu_', c('apex', 'cannon', 'jukebox', 'nikon', 'nokia'), '_reviews')[i] )


# hu_liu
# 
# cannon_reviews
# 
# dat$rating
# dat$category


list2env(hu_liu, .GlobalEnv)


# Map(function(x, y){
# 
#     readr::write_rds(x, sprintf('%s.rds', y))
# 
# }, hu_liu, names(hu_liu))


lapply(names(hu_liu), function(x){
    
    eval(parse(text = sprintf('pax::new_data(%s)', x)))
    
})





