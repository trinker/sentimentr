#' Get Sentences (Deprecated)
#'
#' Deprecated, use \code{\link[sentimentr]{get_sentences}} instead.
#'
#' @param x A character vector, \code{sentiment}, or \code{sentiment_by} object.
#' @param \ldots Ignored.
#' @export
get_sentences2 <- function(x, ...) {
    
    warning("Deprecated, use sentimentr::get_sentences instead.", call. = FALSE)
    
    lapply(lapply(get_sents2(x), function(x) gsub("<<<TEMP>>>", ".", x)),
        function(x) gsub("^\\s+|\\s+$", "", x))
}
