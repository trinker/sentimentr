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
    "kotzias_reviews_imdb", "kotzias_reviews_yelp", "nyt_articles",
    "crowdflower_self_driving_cars", "crowdflower_weather", 
    "crowdflower_deflategate", "crowdflower_products"), ...){

    dats <- lapply(data, function(x) eval(parse(text = paste0('sentimentr::', x))))

    dats <- Map(function(x, y){
        x <- data.table::copy(x)
        x <- x[,'source' := y]
        data.table::setcolorder(x, c("source", "sentiment", "text"))
    }, dats, data)

    data.table::rbindlist(dats)
}

