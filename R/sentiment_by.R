#' Polarity Score (Sentiment Analysis) By Groups
#'
#' Approximate the sentiment (polarity) of text by grouping variable(s).
#'
#' @param text.var The text variable.
#' @param by The grouping variable(s).  Default \code{NULL} uses the original 
#' row/element indices; if you used a column of 12 rows for \code{text.var}
#' these 12 rows will be used as the grouping variable.  Also takes a single 
#' grouping variable or a list of 1 or more grouping variables.                                       
#' @param \ldots Other arguments passed to \code{\link[sentimentr]{sentiment}}.
#' @return Returns a \pkg{data.table} with grouping variables plus:
#' \itemize{
#'   \item  element_id - The id number of the original vector passed to \code{sentiment}
#'   \item  word_count - Word count \code{\link[base]{sum}}med by grouping variable
#'   \item  sd - Standard deviation (\code{\link[stats]{sd}}) of the sentiment/polarity score by grouping variable
#'   \item  ave_sentiment - Sentiment/polarity score \code{\link[base]{mean}} average by grouping variable
#' }
#' @keywords sentiment, polarity, group
#' @export
#' @family sentiment functions
#' @examples 
#' mytext <- c(
#'    'do you like it?  But I hate really bad dogs',
#'    'I am the best friend.',
#'    'Do you really like it?  I\'m not happy'
#' )
#' sentiment(mytext)
#' 
#' sentiment_by(mytext)
#' get_sentences(sentiment_by(mytext))
#' 
#' (mysentiment <- sentiment_by(mytext, question.weight = 0))
#' stats::setNames(get_sentences(sentiment_by(mytext, question.weight = 0)),
#'     round(mysentiment[["ave_sentiment"]], 3))
sentiment_by <- function(text.var, by = NULL,  ...){

	  word_count <- NULL
    out <- sentiment(text.var = text.var, ...)

    if (is.null(by)){
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
        	  'sd' = stats::sd(sentiment, na.rm = TRUE),
        	  'ave_sentiment' = mean(sentiment, na.rm = TRUE)), by = "element_id"]
        group.vars <- "element_id"
    } else {
    	
    	
    }
    
    class(out2) <- unique(c("sentiment_by", class(out)))
    sentiment <- new.env(FALSE)
    sentiment[["sentiment"]] <- out
    attributes(out2)[["sentiment"]] <- sentiment 
    attributes(out2)[["groups"]] <- group.vars
    out2
	
}


