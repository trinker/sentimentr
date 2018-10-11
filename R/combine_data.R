#' Combine \pkg{sentimentr}'s Sentiment Data Sets
#' 
#' Combine trusted sentiment data sets from \pkg{sentimentr}.
#' 
#' @param data A character vector of \pkg{sentimentr} data sets.
#' @param \ldots ignored.
#' @return Returns an rbinded \pkg{data.table} of sentiment data with the source
#' added as column.
#' @export
#' @examples
#' combine_data()
#' combine_data(c("kotzias_reviews_amazon_cells", "kotzias_reviews_imdb", 
#'     "kotzias_reviews_yelp"))
combine_data <- function(data = c("course_evaluations", 
    "hotel_reviews", "kaggle_movie_reviews", "kotzias_reviews_amazon_cells", 
    "kotzias_reviews_imdb", "kotzias_reviews_yelp", "nyt_articles"), ...){

    dats <- lapply(data, function(x) eval(parse(text = paste0('sentimentr::', x))))

    dats <- Map(function(x, y){
        x <- data.table::copy(x)
        
        x <- x[,'source' := y]

        if ('review_id' %in% colnames(x)) {
            data.table::setcolorder(x, c("source", "sentiment", "text", "review_id"))
        } else {
            data.table::setcolorder(x, c("source", "sentiment", "text")) 
        }
        
    }, dats, data)

    data.table::rbindlist(dats, fill = TRUE)
}



#' Get Available Data
#'
#' See available \pkg{sentimentr} data a data.frame.  Note that 
#' \code{sentimentr_data} is the main function to be used but 
#' \code{available_data} is exposed to allow other packages to use the
#' functionality in a generic way.
#'
#' @param regex A regex to search for within the data columns.
#' @param package The name of the package to extract data from.
#' @param \ldots Other arguments passed to \code{grep}.
#' @return Returns a data.frame
#' @export
#' @rdname sentimentr_data
#' @examples
#' sentimentr_data()
#' available_data() ## generic version for export
#' available_data(package = 'datasets')
#' sentimentr_data('^hu')
#' sentimentr_data('^(hu|kot)')
#' combine_data(sentimentr_data('^(hu|kot)')[[1]])
#' 
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(sentimentr, tidyverse, magrittr)
#' 
#' sentiment_data <- sentimentr_data('^hu') %>%
#'     pull(Data) %>%
#'     combine_data() %>%
#'     mutate(id = seq_len(n())) %>%
#'     as_tibble()
#'     
#' sentiment_test <- sentiment_data %>%
#'     select(-sentiment) %>%
#'     get_sentences() %$%
#'     sentiment(., by = c('id'))
#' 
#' testing <- sentiment_data %>%
#'     left_join(sentiment_test, by = 'id') %>%
#'     as_tibble() %>%
#'     mutate(
#'         actual = sign(sentiment),
#'         predicted = sign(ave_sentiment)
#'     )
#' 
#' testing %$%
#'     ftable(predicted, actual)
#' }
available_data <- function(regex = NULL, package = 'sentimentr', ...){


    results <- utils::data(package = package)[["results"]]
    dat <- stats::setNames(data.frame(results[, 3:4, drop = FALSE],
        stringsAsFactors = FALSE), c("Data", "Description"))

    dat <- dat[order(dat[['Data']]),]
    row.names(dat) <- NULL

    if (!is.null(regex)){
        locs <- sort(unique(unlist( lapply(dat, function(x){ grep(regex, x, ...) }) )))

        if (length(locs) > 0) {
            dat <- dat[locs,]
        } else {
            warning('`regex` not found, returning all available data')
        }
    }

    dat 

}


#' @export
#' @rdname sentimentr_data
sentimentr_data <- function(regex = NULL, package = 'sentimentr', ...){
    
    available_data(regex = regex, package = package, ...)
    
}


