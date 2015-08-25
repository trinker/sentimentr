#' Convert data.frame to a Hash Key
#'
#' Create your own hash keys from a data frame for use in key arguments such as
#' \code{polarity_dt} in the \code{sentiment} function.
#'
#' @param x A \code{\link[base]{data.frame}} with the first column containing
#' polarized words and the second containing polarity values.
#' @param \ldots ignored.
#' @return Returns a \pkg{data.table} object that can be used as a hash key.
#' @keywords key hash lookup
#' @export
#' @examples
#' key <- data.frame(
#'     words = sample(LETTERS),
#'     polarity = rnorm(26),
#'     stringsAsFactors = FALSE
#' )
#'
#' (mykey <- as.key(key))
#'
#' # Looking up values
#' mykey[c("A", "K")][[2]]
#'
#' # Using syuzhet's sentiment lexicons
#' \dontrun{
#' library(syuzhet)
#' as.key(syuzhet:::bing)
#' as.key(syuzhet:::afinn)
#' nrc <- data.frame(
#'     words = rownames(syuzhet:::nrc),
#'     polarity = syuzhet:::nrc[, "positive"] - syuzhet:::nrc[, "negative"],
#'     stringsAsFactors = FALSE
#' )
#'
#' as.key(nrc[nrc[["polarity"]] != 0, ])
#'
#' bing <- syuzhet:::bing[!as.character(syuzhet:::bing[[1]]) %in%
#'     sentimentr::valence_shifters_table[["x"]], ]
#' sentiment(gsub("Sam-I-am", "Sam I am", sam_i_am), as.key(bing))
#' }
as.key <- function(x, ...){

    stopifnot(is.data.frame(x))
    if (is.factor(x[[1]])) {
        warning("Column 1 was a factor...\nConverting to character.")
        x[[1]] <- as.character(x[[1]])
    }

    if (!is.character(x[[1]])) stop("Column 1 must be character")
    if (!is.numeric(x[[2]])) stop("Column 2 must be numeric")

    colnames(x) <- c("x", "y")

    data.table::setDT(x)
    x <- x[order(x),]
    data.table::setkey(x, "x")
    x
}
