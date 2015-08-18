#' Polarity Score (Sentiment Analysis) By Groups
#'
#' Approximate the sentiment (polarity) of text by grouping variable(s).
#'
#' @param text.var The text variable.
#' @param by The grouping variable(s).  Default \code{NULL} uses the original
#' row/element indices; if you used a column of 12 rows for \code{text.var}
#' these 12 rows will be used as the grouping variable.  Also takes a single
#' grouping variable or a list of 1 or more grouping variables.
#' @param group.names A vector of names that corresponds to group.  Generally
#' for internal use.
#' @param \ldots Other arguments passed to \code{\link[sentimentr]{sentiment}}.
#' @return Returns a \pkg{data.table} with grouping variables plus:
#' \itemize{
#'   \item  element_id - The id number of the original vector passed to \code{sentiment}
#'   \item  sentence_id - The id number of the sentences within each \code{element_id}
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
#'
#' with(presidential_debates_2012, sentiment_by(dialogue, person))
#' with(presidential_debates_2012, sentiment_by(dialogue, list(person, time)))
sentiment_by <- function(text.var, by = NULL, group.names, ...){

	  word_count <- NULL
    out <- sentiment(text.var = text.var, ...)

    if (is.null(by)){
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
        	  'sd' = stats::sd(sentiment, na.rm = TRUE),
        	  'ave_sentiment' = mean(sentiment, na.rm = TRUE)), by = "element_id"]
        G <- "element_id"
        uncombined <- out
    } else {
        if (is.list(by) & length(by) > 1) {
            m <- unlist(as.character(substitute(by))[-1])
            G <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            grouping <- by
        } else {
            G <- as.character(substitute(by))
            G <- G[length(G)]
            grouping <- unlist(by)
        }

        if(!missing(group.names)) {
            G <- group.names
        }


        group_dat <- stats::setNames(as.data.frame(grouping,
            stringsAsFactors = FALSE), G)

        data.table::setDT(group_dat)
        group_dat <- group_dat[out[["element_id"]], ]

        uncombined <- out2 <- cbind(group_dat, out)

        out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
            'sd' = stats::sd(sentiment, na.rm = TRUE),
            'ave_sentiment' = mean(sentiment, na.rm = TRUE)), keyby = G]

    }

    class(out2) <- unique(c("sentiment_by", class(out)))
    sentiment <- new.env(FALSE)
    sentiment[["sentiment"]] <- out
    attributes(out2)[["sentiment"]] <- sentiment
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    out2

}

