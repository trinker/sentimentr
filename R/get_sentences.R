#' Get Sentences
#'
#' \code{get_sentences} - Get sentences from a character vector, \code{sentiment}, or
#' \code{sentiment_by} object. 
#'
#' @param x A character vector, \code{sentiment}, or \code{sentiment_by} object.
#' @param \ldots Other arguments passed to \code{\link[textshape]{split_sentence}}.
#' @export
#' @return Returns a list of vectors of sentences.
#' @rdname get_sentences
#' @examples
#' dat <- data.frame(
#'     w = c('Person 1', 'Person 2'),
#'     x = c(paste0(
#'         "Mr. Brown comes! He says hello. i give him coffee.  i will ",
#'         "go at 5 p. m. eastern time.  Or somewhere in between!go there"
#'     ), "One more thought for the road! I am going now.  Good day."),
#'     y = state.name[c(32, 38)], 
#'     z = c(.456, .124),
#'     stringsAsFactors = FALSE
#' )
#' get_sentences(dat$x)
#' get_sentences(dat)
get_sentences <- function(x, ...) {
    UseMethod("get_sentences")
}



#' @export
#' @method get_sentences character
get_sentences.character <- function(x, ...) {
    out <- textshape::split_sentence(x, ...)
    make_class(out, "get_sentences", "get_sentences_character")
}


#' @export
#' @method get_sentences data.frame
get_sentences.data.frame <- function(x, ...) {
    
    dots <- list(...)

    ## detect text variable
    if (is.null(dots[['text.var.name']])) {
        
        z <- data.table::data.table(data.frame(x, stringsAsFactors = FALSE)) 
        
        text.var.name <- names(which.max(sapply(as.data.frame(z), function(y) {
                if (!is.character(y) && !is.factor(y)) return(0)
                mean(nchar(as.character(y)), na.rm = TRUE)
        }))[1])

        if (length(text.var.name) == 0) {
            stop("Could not detect `text.var`.  Please supply `text.var` explicitly via\n    ellipsis (...) argument (e.g. `text.var.name = \'my_text_column\'` ).")
        }
    } else {
        text.var.name <- dots[['text.var.name']]
    }

    out <- textshape::split_sentence(x, text.var = text.var.name, ...)

    class(out[[text.var.name]]) <- unique(c("get_sentences", "get_sentences_character", class(out[[text.var.name]])))   
    out <- make_class(out, "get_sentences", "get_sentences_data_frame")
    attributes(out)[['text.var']] <- text.var.name
        
    out
}

# get_sentences.character <- function(x, ...) {
#     out <- lapply(get_sents(trimws(x)), function(x) gsub("^\\s+|\\s+$", "", x))
#     class(out) <- unique(c("get_sentences", class(out)))
#     out
# }

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



#' @export
#' @method get_sentences profanity
get_sentences.profanity <- function(x, ...) {
	  attributes(x)[["sentences"]][["sentences"]]
}

#' @export
#' @method get_sentences profanity_by
get_sentences.profanity_by <- function(x, ...) {
	  y <- attributes(x)[["profanity"]][["profanity"]]
	  attributes(y)[["sentences"]][["sentences"]]
}




#' @export
#' @method get_sentences emotion
get_sentences.emotion <- function(x, ...) {
	  attributes(x)[["sentences"]][["sentences"]]
}

#' @export
#' @method get_sentences emotion_by
get_sentences.emotion_by <- function(x, ...) {
	  y <- attributes(x)[["emotion"]][["emotion"]]
	  attributes(y)[["sentences"]][["sentences"]]
}


