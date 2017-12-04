if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, jsonlite, textclean)

fls <- dir('json', full.names = TRUE)


hotel_reviews <- lapply(fls, function(x){

    try(
        fromJSON(x)$Reviews %>%
            select(ReviewID, Author, Date, Content) %>%
            mutate(rating = as.numeric(fromJSON(x)$Reviews$Ratings$Overall)) %>%
            select(ReviewID:Date, rating, everything()) %>%
            rename(review_id = ReviewID, review = Content) %>%
            setNames(tolower(colnames(.))) %>%
            tbl_df() %>%
            mutate(
                date = as.Date(date, format = '%B %d, %Y'),
                #title = trimws(gsub('^"|"$', '', replace_non_ascii(title))),
                review = trimws(replace_non_ascii(review))
            )
    )

})

hotel_reviews <- hotel_reviews[!sapply(hotel_reviews, inherits, 'try-error')] %>%
    bind_rows() %>%
    mutate_if(is.character, funs(replace_non_ascii)

    )
    

readr::write_rds(hotel_reviews, 'hotel_reviews.rds')
