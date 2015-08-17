#' Get Sentences
#'
#' Get sentences from a character vector, \code{sentiment}, or
#' \code{sentiment_by} object.
#'
#' @param x A character vector, \code{sentiment}, or \code{sentiment_by} object.
#' @param \ldots Ignored.
#' @export
#' @return \code{subs} - Returns a list of sub-expressions.
#' @examples
#' (x <- paste0(
#'     "Mr. Brown comes! He says hello. i give him coffee.  i will ",
#'     "go at 5 p. m. eastern time.  Or somewhere in between!go there"
#' ))
#' get_sentences(x)
get_sentences <- function(x, ...) {
    UseMethod("get_sentences")
}



#' @export
#' @method get_sentences character
get_sentences.character <- function(x, ...) {
    lapply(get_sents(x), function(x) gsub("^\\s+|\\s+$", "", x))
}
