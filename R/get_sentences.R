#' Get Sentences
#'
#' \code{get_sentences} - Get sentences from a character vector, \code{sentiment}, or
#' \code{sentiment_by} object. This is optimized for internal use, converting the
#' text to lower case and removing non-sentence boundary periods. \code{get_sentences2}
#' retains case and non-sentence boundary periods and should be preferable in
#' such instances where these features are deemed important to the analysis at
#' hand.
#'
#' @param x A character vector, \code{sentiment}, or \code{sentiment_by} object.
#' @param \ldots Ignored.
#' @export
#' @return Returns a list of vectors of sentences.
#' @rdname get_sentences
#' @examples
#' (x <- paste0(
#'     "Mr. Brown comes! He says hello. i give him coffee.  i will ",
#'     "go at 5 p. m. eastern time.  Or somewhere in between!go there"
#' ))
#' get_sentences(x)
#' get_sentences2(x)
get_sentences <- function(x, ...) {
    UseMethod("get_sentences")
}



#' @export
#' @method get_sentences character
get_sentences.character <- function(x, ...) {
    out <- lapply(get_sents(trimws(x)), function(x) gsub("^\\s+|\\s+$", "", x))
    class(out) <- unique(c("get_sentences", class(out)))
    out
}

#' @export
#' @method get_sentences get_sentences
get_sentences.get_sentences <- function(x, ...) {
    return(x)
}

#' @export
#' @method get_sentences sentiment
get_sentences.sentiment <- function(x, ...) {
	  attributes(x)[["sentences"]][["sentences"]]
}

#' @export
#' @method get_sentences sentiment_by
get_sentences.sentiment_by <- function(x, ...) {
	  y <- attributes(x)[["sentiment"]][["sentiment"]]
	  attributes(y)[["sentences"]][["sentences"]]
}


#' Get Sentences
#'
#' \code{get_sentences2} - Get sentences from a character vector but does not
#' force to lower case.
#'
#' @rdname get_sentences
#' @export
get_sentences2 <- function(x, ...) {
    lapply(lapply(get_sents2(x), function(x) gsub("<<<TEMP>>>", ".", x)),
        function(x) gsub("^\\s+|\\s+$", "", x))
}


