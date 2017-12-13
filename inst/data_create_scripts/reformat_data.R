library(dplyr); library(data.table)

#rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)

data.names <- list(
    'cannon_reviews',
    'course_evaluations',
    'hotel_reviews',
    'kaggle_movie_reviews',
    'kotzias_reviews',
    'nyt_articles'
)



## get the data from sentimentr
data <- lapply(data.names, function(x) eval(parse(text = paste0('sentimentr::', x))))

## unlist individual data sets
data <- unlist(setNames(lapply(data, function(x) if (is.data.frame(x)) {list(x)} else {x}), data.names), recursive = FALSE)

## lapply(data, colnames)

conformer <- function(x){

    ## rename ratings to sentiment
    colnames(x)[colnames(x) %in% c("opinion.score", "rating")] <- 'sentiment'

    ## rename review to text
    colnames(x)[colnames(x) %in% c("review")] <- 'text'

    ## grab just sentiment and text
    x <- x[, c('sentiment', 'text')]

    ## reformat ratings to -/+
    if (!any(x[['sentiment']] < 0)) {
        x$sentiment <- x$sentiment - mean(range(x$sentiment))
    }

    x
}

reformatted <- textshape::tidy_list(lapply(data, conformer), 'source') %>%
    dplyr::mutate(source = gsub('\\.', '_', source)) %>%
    #data.table::data.table() %>%
    {split(.[c('sentiment', 'text')], .$source)} %>%
    lapply(data.table::data.table)

ls()
list2env(reformatted, envir = .GlobalEnv)
ls()


ls()


cat(sprintf('pax::new_data(%s)', names(reformatted)), sep = '\n', file = 'clipboard')

pax::new_data(cannon_reviews)
pax::new_data(course_evaluations)
pax::new_data(hotel_reviews)
pax::new_data(kaggle_movie_reviews)
pax::new_data(kotzias_reviews_amazon_cells)
pax::new_data(kotzias_reviews_imdb)
pax::new_data(kotzias_reviews_yelp)
pax::new_data(nyt_articles)





