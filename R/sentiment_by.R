#' Polarity Score (Sentiment Analysis) By Groups
#'
#' Approximate the sentiment (polarity) of text by grouping variable(s).  For a
#' full description of the sentiment detection algorithm see 
#' \code{\link[sentimentr]{sentiment}}.  See \code{\link[sentimentr]{sentiment}}
#' for more details about the algorithm, the sentiment/valence shifter keys
#' that can be passed into the function, and other arguments that can be passed.
#'
#' @param text.var The text variable.  Also takes a \code{sentimentr} or
#' \code{sentiment_by} object.
#' @param by The grouping variable(s).  Default \code{NULL} uses the original
#' row/element indices; if you used a column of 12 rows for \code{text.var}
#' these 12 rows will be used as the grouping variable.  Also takes a single
#' grouping variable or a list of 1 or more grouping variables.
#' @param averaging.function A function for performing the group by averaging.  
#' The default, \code{\link[sentimentr]{average_downweighted_zero}}, downweights 
#' zero values in the averaging.  Note that the function must handle 
#' \code{NA}s.  The \pkg{sentimentr} functions 
#' \code{average_weighted_mixed_sentiment} and \code{average_mean} are also 
#' available.  The former upweights negative when the analysts suspects the 
#' speaker is likely to surround negatives with positives (mixed) as a polite 
#' social convention but still the affective state is negative.  The later is a 
#' standard mean average.
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
#' @export
#' @family sentiment functions
#' @section Chaining:
#' \pkg{sentimentr} uses non-standard evaluation when you use \code{with()} OR 
#' \code{\%$\%} (\pkg{magrittr}) and looks for the vectors within the data set 
#' passed to it. There is one exception to this...when you pass a 
#' \code{get_sentences()} object to \code{sentiment_by()} to the first argument 
#' which is \code{text.var} it calls the \code{sentiment_by.get_sentences_data_frame} 
#' method which requires \code{text.var} to be a \code{get_sentences_data_frame} 
#' object. Because this object is a \code{data.frame} its method knows this and 
#' knows it can access the columns of the \code{get_sentences_data_frame} object 
#' directly (usually \code{text.var} is an atomic vector), it just needs the 
#' names of the columns to grab.
#' 
#' To illustrate this point understand that all three of these approaches 
#' result in exactly the same output:
#' 
#' \preformatted{
#' ## method 1
#' presidential_debates_2012 \%>\%
#'     get_sentences() \%>\%
#'     sentiment_by(by = c('person', 'time'))
#' 
#' ## method 2
#' presidential_debates_2012 \%>\%
#'     get_sentences() \%$\%
#'     sentiment_by(., by = c('person', 'time'))
#' 
#' ## method 3
#' presidential_debates_2012 \%>\%
#'     get_sentences() \%$\%
#'     sentiment_by(dialogue, by = list(person, time))
#' }
#' 
#' Also realize that a \code{get_sentences_data_frame} object also has a column
#' with a \code{get_sentences_character} class column which also has a method in
#' \pkg{sentimentr}.
#' 
#' When you use \code{with()} OR \code{\%$\%} then you're not actually passing
#' the \code{get_sentences_data_frame} object to \pkg{sentimentr} and hence the
#' \code{sentiment_by.get_sentences_data_frame} method isn't called rather
#' \code{sentiment_by} is evaluated in the environment/data of the
#' \code{get_sentences_data_frame object}. You can force the object passed this
#' way to be evaluated as a \code{get_sentences_data_frame} object and thus
#' calling the \code{sentiment_by.get_sentences_data_frame} method by using the
#' \code{.} operator as I've done in method 2 above. Otherwise you pass the name
#' of the text column which is actually a \code{get_sentences_character class}
#' and it calls its own method. In this case the by argument expects vectors or
#' a list of vectors and since it's being evaluated within the data set you can
#' use \code{list()}.
#' @examples
#' mytext <- c(
#'    'do you like it?  It is red. But I hate really bad dogs',
#'    'I am the best friend.',
#'    "Do you really like it?  I'm not happy"
#' )
#' 
#' ## works on a character vector but not the preferred method avoiding the 
#' ## repeated cost of doing sentence boundary disambiguation every time 
#' ## `sentiment` is run
#' \dontrun{
#' sentiment(mytext)
#' sentiment_by(mytext)
#' }
#' 
#' ## preferred method avoiding paying the cost 
#' mytext <- get_sentences(mytext)
#' 
#' sentiment_by(mytext)
#' sentiment_by(mytext, averaging.function = average_mean)
#' sentiment_by(mytext, averaging.function = average_weighted_mixed_sentiment)
#' get_sentences(sentiment_by(mytext))
#'
#' (mysentiment <- sentiment_by(mytext, question.weight = 0))
#' stats::setNames(get_sentences(sentiment_by(mytext, question.weight = 0)),
#'     round(mysentiment[["ave_sentiment"]], 3))
#' 
#' pres_dat <- get_sentences(presidential_debates_2012)
#' 
#' \dontrun{
#' ## less optimized way
#' with(presidential_debates_2012, sentiment_by(dialogue, person))
#' }
#' 
#' \dontrun{
#' sentiment_by(pres_dat, 'person')
#' 
#' (out <- sentiment_by(pres_dat, c('person', 'time')))
#' plot(out)
#' plot(uncombine(out))
#' 
#' sentiment_by(out, presidential_debates_2012$person)
#' with(presidential_debates_2012, sentiment_by(out, time))
#' 
#' highlight(with(presidential_debates_2012, sentiment_by(out, list(person, time))))
#' }
#' 
#' \dontrun{
#' ## tidy approach
#' library(dplyr)
#' library(magrittr)
#' 
#' hu_liu_cannon_reviews %>%
#'    mutate(review_split = get_sentences(text)) %$%
#'    sentiment_by(review_split)
#' }
sentiment_by <- function(text.var, by = NULL, 
    averaging.function = sentimentr::average_downweighted_zero, group.names, ...){

    UseMethod("sentiment_by")
}


#' @export
#' @method sentiment_by get_sentences_character    
sentiment_by.get_sentences_character <- function(text.var, by = NULL, 
    averaging.function = sentimentr::average_downweighted_zero, group.names, ...){

	word_count <- ave_sentiment <- NULL
    out <- suppressWarnings(sentiment(text.var = text.var, ...))

    if (is.null(by)){
        
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
        	  'sd' = stats::sd(sentiment, na.rm = TRUE),
        	  'ave_sentiment' = averaging.function(sentiment)), by = "element_id"]
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
            'ave_sentiment' = averaging.function(sentiment)), keyby = G]#[order(-ave_sentiment)]

    }

    class(out2) <- unique(c("sentiment_by", class(out)))
    sentiment <- new.env(FALSE)
    sentiment[["sentiment"]] <- out
    attributes(out2)[["sentiment"]] <- sentiment
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    
    attributes(out2)[["averaging.function"]] <- averaging.function
    
    out2

}


#' @export
#' @method sentiment_by get_sentences_data_frame    
sentiment_by.get_sentences_data_frame <- function(text.var, by = NULL, 
    averaging.function = sentimentr::average_downweighted_zero, group.names, ...){

    x <- make_class(unname(split(text.var[[attributes(text.var)[['text.var']]]], 
        unlist(text.var[['element_id']]))), "get_sentences", "get_sentences_character")

	word_count <- ave_sentiment <- sentiment_sentimentr_package <- NULL
    out <- sentiment(text.var = x, ...)
    data.table::setnames(out, old = 'sentiment', new = 'sentiment_sentimentr_package')
    
    if (is.null(by)){
        by <- "element_id"
    }
    
    uncombined <- out2 <- cbind(text.var, out[, c('word_count', 'sentiment_sentimentr_package')])

    out2 <- out2[, list('word_count' = sum(word_count, na.rm = TRUE),
        'sd' = stats::sd(sentiment_sentimentr_package, na.rm = TRUE),
        'ave_sentiment' = averaging.function(sentiment_sentimentr_package)), keyby = by]
    
    class(out2) <- unique(c("sentiment_by", class(out)))
    sentiment <- new.env(FALSE)
    data.table::setnames(out, new = 'sentiment', old = 'sentiment_sentimentr_package')
    sentiment[["sentiment"]] <- out
    attributes(out2)[["sentiment"]] <- sentiment
    attributes(out2)[["groups"]] <- by
    
    attributes(attributes(out2)[["sentiment"]][["sentiment"]])[['sentences']][['sentences']] <- x

    uncombine <- new.env(FALSE)
    data.table::setnames(uncombined, new = 'sentiment', old = 'sentiment_sentimentr_package')     
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    
    attributes(out2)[["averaging.function"]] <- averaging.function
    
    out2

}



#' @export
#' @method sentiment_by character    
sentiment_by.character <- function(text.var, by = NULL, 
    averaging.function = sentimentr::average_downweighted_zero, group.names, ...){

    split_warn(text.var, 'sentiment_by', ...)
    
	word_count <- ave_sentiment <- NULL
    out <- suppressWarnings(sentiment(text.var = text.var, ...))

    if (is.null(by)){
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
        	  'sd' = stats::sd(sentiment, na.rm = TRUE),
        	  'ave_sentiment' = averaging.function(sentiment)), by = "element_id"]
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
            'ave_sentiment' = averaging.function(sentiment)), keyby = G]#[order(-ave_sentiment)]

    }

    class(out2) <- unique(c("sentiment_by", class(out)))
    sentiment <- new.env(FALSE)
    sentiment[["sentiment"]] <- out
    attributes(out2)[["sentiment"]] <- sentiment
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    
    attributes(out2)[["averaging.function"]] <- averaging.function
    
    out2

}


#' @export
#' @method sentiment_by sentiment_by    
sentiment_by.sentiment_by <- function(text.var, by = NULL, 
    averaging.function = sentimentr::average_downweighted_zero, group.names, ...){

	word_count <- ave_sentiment <- NULL
    out <- attributes(text.var)[['sentiment']][['sentiment']]

    if (is.null(by)){
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
        	  'sd' = stats::sd(sentiment, na.rm = TRUE),
        	  'ave_sentiment' = averaging.function(sentiment)), by = "element_id"]
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
            'ave_sentiment' = averaging.function(sentiment)), keyby = G]#[order(-ave_sentiment)]

    }

    class(out2) <- unique(c("sentiment_by", class(out)))
    sentiment <- new.env(FALSE)
    sentiment[["sentiment"]] <- out
    attributes(out2)[["sentiment"]] <- sentiment
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    
    attributes(out2)[["averaging.function"]] <- averaging.function
    
    out2

}


#' @export
#' @method sentiment_by sentiment    
sentiment_by.sentiment <- function(text.var, by = NULL, 
    averaging.function = sentimentr::average_downweighted_zero, group.names, ...){

	word_count <- ave_sentiment <- NULL
    out <- text.var

    if (is.null(by)){
        out2 <- out[, list('word_count' = sum(word_count, na.rm = TRUE),
        	  'sd' = stats::sd(sentiment, na.rm = TRUE),
        	  'ave_sentiment' = averaging.function(sentiment)), by = "element_id"]
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
            'ave_sentiment' = averaging.function(sentiment)), keyby = G]#[order(-ave_sentiment)]

    }

    class(out2) <- unique(c("sentiment_by", class(out)))
    sentiment <- new.env(FALSE)
    sentiment[["sentiment"]] <- out
    attributes(out2)[["sentiment"]] <- sentiment
    attributes(out2)[["groups"]] <- G

    uncombine <- new.env(FALSE)
    uncombine[["uncombine"]] <- uncombined
    attributes(out2)[["uncombine"]] <- uncombine
    
    attributes(out2)[["averaging.function"]] <- averaging.function
    
    out2

}



#' Plots a sentiment_by object
#'
#' Plots a sentiment_by object.  Red centers are average sentiment.  Alpha
#' jittered dots are raw sentence level sentiment data.  Boxes are boxplots.
#'
#' @param x The sentiment_by object.
#' @param ordered logical.  If \code{TRUE} order the output grouping by sentiment.
#' @param \ldots ignored
#' @method plot sentiment_by
#' @importFrom graphics plot
#' @return Returns a \pkg{ggplot2} object.
#' @export
plot.sentiment_by <- function(x, ordered = TRUE, ...){

    ave_sentiment <- sentiment <- grouping.vars <- NULL
    
    dat2 <- uncombine(x)

    grps <- attributes(x)[["groups"]]
    if (length(grps) == 1 && grps == "element_id") return(plot(dat2))

    x[, "grouping.vars"] <- paste2(x[, attributes(x)[["groups"]], with=FALSE])

    # 
    dat2[, "grouping.vars"] <- paste2(dat2[, attributes(x)[["groups"]], with=FALSE])
    
    x <- x[order(-ave_sentiment)]    
    x[, grouping.vars := factor(grouping.vars, levels = rev(grouping.vars))]
    
    dat2[, grouping.vars := factor(grouping.vars, levels = levels(x[["grouping.vars"]]))]

#     center_dat <- dat2[, list(upper = mean(sentiment, na.rm = TRUE) + 2*SE(sentiment),
#         lower = mean(sentiment, na.rm = TRUE) - 2*SE(sentiment),
#         means = mean(sentiment, na.rm = TRUE)), keyby = "grouping.vars"]
# 
#     center_dat[, grouping.vars := factor(grouping.vars, levels = levels(x[["grouping.vars"]]))]

    ggplot2::ggplot()+
        ggplot2::geom_boxplot(data=dat2, ggplot2::aes(y=sentiment, 
            x=grouping.vars),fill=NA, color='grey70',width=.55, size=.35, 
            outlier.color = NA)  +  
        ggplot2::geom_jitter(data=dat2, ggplot2::aes(y=sentiment, 
            x=grouping.vars), width=.35, height=0, alpha=.15, size=1.5) +        
        ggplot2::theme_bw() +
#         ggplot2::geom_crossbar(data=center_dat, aes(x=grouping.vars, ymax = upper, 
#             ymin = lower, y=means), color='grey70', width=0.55) +
        ggplot2::geom_point(data=x, ggplot2::aes(y=ave_sentiment, x=grouping.vars), 
            colour = "red", shape=18, size=4) +
        ggplot2::ylab("Sentiment") +
        ggplot2::xlab("Groups") +
        ggplot2::coord_flip()

}


# plot.sentiment_by <- function(x, ...){  #old plot version changed 5/3/2016
# 
#     ave_sentiment <- grouping.vars <- NULL
#     dat2 <- uncombine(x)
# 
#     grps <- attributes(x)[["groups"]]
#     if (length(grps) == 1 && grps == "element_id") return(plot(dat2))
# 
#     x[, "grouping.vars"] <- paste2(x[, attributes(x)[["groups"]], with=FALSE])
#     x[, grouping.vars := factor(grouping.vars, levels = rev(grouping.vars))]
# 
#     dat2[, "grouping.vars"] <- paste2(dat2[, attributes(x)[["groups"]], with=FALSE])
#     dat2[, grouping.vars := factor(grouping.vars, levels = levels(x[["grouping.vars"]]))]
# 
#     #center_dat <- dat2[, list(upper = mean(sentiment, na.rm = TRUE) + 2*SE(sentiment),
#     #    lower = mean(sentiment, na.rm = TRUE) - 2*SE(sentiment),
#     #    means = mean(sentiment, na.rm = TRUE)), keyby = "grouping.vars"]
# 
#     ggplot2::ggplot() +
#         ggplot2::geom_hline(ggplot2::aes(yintercept=0), size = 1, color="grey70") +
#         ggplot2::geom_point(data=dat2, ggplot2::aes_string('grouping.vars', 'sentiment'), alpha=.05, shape=1) +
#         ggplot2::geom_point(data=x, ggplot2::aes_string('grouping.vars','ave_sentiment'), color="red", shape=3, size=4)  +
#         ggplot2::geom_boxplot(data=dat2, ggplot2::aes_string('grouping.vars', 'sentiment', color = "grouping.vars"),
#             alpha=.5, fill =NA) +
#         ggplot2::coord_flip() +
#         #ggplot2::geom_errorbar(data = center_dat, width=.25, alpha=.4,
#         #    ggplot2::aes_string('grouping.vars', y = "means", ymin="upper", ymax="lower"), height = .3) +
#         ggplot2::theme_bw() +
#         ggplot2::guides(color=FALSE) +
#         ggplot2::ylab("Sentiment") +
#         ggplot2::xlab("Groups") +
#         ggplot2::theme(panel.grid = ggplot2::element_blank())
# 
# }
# 

