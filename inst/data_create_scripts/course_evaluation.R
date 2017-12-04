p_load(SentimentAnalysis, sentimentr, tidyverse, textshape)

splits <- readLines('EECS_annotated_samples_anonymized.txt') %>%
    split_match()

course_evaluation <- lapply(splits, function(x) {
    string <- paste(x[-c(1:2)], collapse = ', ')
    senti <- unique(unlist(stringi::stri_extract_all_regex(string, '(?<=sentiment=)[a-z]+')))
    if (length(senti) > 1) return(NULL)
    if (is.na(senti)) return(NULL)
    data_frame(
        polatity = senti, 
        sentiment = ifelse(tolower(senti) == 'negative', -1, ifelse(tolower(senti) == 'positive', 1, 0)), 
        text = x[1]
    )
}) %>%
    bind_rows() 


readr::write_rds(course_evaluation, 'course_evaluation.rds')