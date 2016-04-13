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
#' (out <- with(presidential_debates_2012, sentiment_by(dialogue, list(person, time))))
#' plot(out)
#' plot(uncombine(out))
#'
#' with(cannon_reviews, sentiment_by(review, number))[order(as.numeric(number))]
#' \dontrun{
#' highlight(with(cannon_reviews, sentiment_by(review, number)))
#' }
sentiment_by <- function(text.var, by = NULL, group.names, ...){

	word_count <- ave_sentiment <- NULL
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

        out2 <-
            out2[, list(word_count = sum(word_count, na.rm = TRUE),
                        sd = stats::sd(sentiment, na.rm = TRUE),
                        ave_sentiment = mean(sentiment, na.rm = TRUE)),
                 keyby = G]
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

#' Plots a sentiment_by object
#'
#' Plots a sentiment_by object.
#'
#' @param x The sentiment_by object.
#' @param \ldots ignored
#' @method plot sentiment_by
#' @importFrom graphics plot
#' @return Returns a \pkg{ggplot2} object.
#' @export
plot.sentiment_by <- function(x, ...){

    ave_sentiment <- grouping.vars <- NULL
    dat2 <- uncombine(x)

    grps <- attributes(x)[["groups"]]
    if (length(grps) == 1 && grps == "element_id") return(plot(dat2))

    x[, "grouping.vars"] <- paste2(x[, attributes(x)[["groups"]], with=FALSE])
    x[, grouping.vars := factor(grouping.vars, levels = rev(grouping.vars))]

    dat2[, "grouping.vars"] <- paste2(dat2[, attributes(x)[["groups"]], with=FALSE])
    dat2[, grouping.vars := factor(grouping.vars, levels = levels(x[["grouping.vars"]]))]

    #center_dat <- dat2[, list(upper = mean(sentiment, na.rm = TRUE) + 2*SE(sentiment),
    #    lower = mean(sentiment, na.rm = TRUE) - 2*SE(sentiment),
    #    means = mean(sentiment, na.rm = TRUE)), keyby = "grouping.vars"]

    ggplot2::ggplot() +
        ggplot2::geom_hline(ggplot2::aes(yintercept=0), size = 1, color="grey70") +
        ggplot2::geom_point(data=dat2, ggplot2::aes_string('grouping.vars', 'sentiment'), alpha=.05, shape=1) +
        ggplot2::geom_point(data=x, ggplot2::aes_string('grouping.vars','ave_sentiment'), color="red", shape=3, size=4)  +
        ggplot2::geom_boxplot(data=dat2, ggplot2::aes_string('grouping.vars', 'sentiment', color = "grouping.vars"),
            alpha=.5, fill =NA) +
        ggplot2::coord_flip() +
        #ggplot2::geom_errorbar(data = center_dat, width=.25, alpha=.4,
        #    ggplot2::aes_string('grouping.vars', y = "means", ymin="upper", ymax="lower"), height = .3) +
        ggplot2::theme_bw() +
        ggplot2::guides(color=FALSE) +
        ggplot2::ylab("Sentiment") +
        ggplot2::xlab("Groups") +
        ggplot2::theme(panel.grid = ggplot2::element_blank())

}

