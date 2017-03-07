#' Downweighted Zeros Averaging
#' 
#' \code{average_downweighted_zero}- Downweight the zeros in a vector for 
#' averaging.  This is useful in the context of language where we don't want the 
#' neutral sentences to have such a strong influence on the general sentiment of 
#' the discourse with multiple sentences.  Essentially, this means neutral 
#' sentences are seen as having less emotional impact than a polarized sentence.
#' 
#' @param x A numeric vector.
#' @param na.rm logical.  Should \code{NA} values should be stripped before the 
#' computation proceeds.
#' @param \ldots ignored.
#' @return Returns a scalar summary of the re-weighted average
#' @export
#' @rdname average_downweighted_zero
#' @examples 
#' x <- c(1, 2, 0, 0, 0, -1)
#' mean(x)
#' average_downweighted_zero(x)
#' average_downweighted_zero(c(NA, x))
#' mean(c(0, 0, 0, x))
#' average_downweighted_zero(c(0, 0, 0, x))
average_downweighted_zero <- function (x, na.rm = TRUE, ...) {
    sum(x, na.rm = na.rm)/{
        sum(x != 0, na.rm = na.rm) + sqrt(log(1 + sum(x == 0, 
            na.rm = na.rm)))
    }
} 


#' Downweighted Zeros Averaging
#' 
#' \code{average_weighted_mixed_sentiment}- Upweight the negative values in a 
#' vector while also downweighting the zeros in a vector.  Useful for small text
#' chunks with several sentences in which some one states a negative sentence
#' but then uses the social convention of several positive sentences in an 
#' attempt to negate the impact of the negative.  The affective state isn't
#' a neutral but a slightly lessened negative state.  
#' 
#' @param mixed.less.than.zero.weight The weighting factor to multiply the 
#' negative elements of the vector by (this increases the intensity of the 
#' negatives in the numerator of the mean formula).
#' @export
#' @rdname average_downweighted_zero
average_weighted_mixed_sentiment <- function (x, mixed.less.than.zero.weight = 4, 
    na.rm = TRUE, ...) {

    if (any(x > 0) && any(x < 0)) {

        numerator <- sum(x[x < 0 & !is.na(x)]) * mixed.less.than.zero.weight + sum(x[x > 0 & !is.na(x)])

    } else {

        numerator <- sum(x, na.rm = na.rm)
 
    }

    numerator/{sum(x != 0, na.rm = na.rm) + sqrt(log(1 + sum(x == 0, na.rm = na.rm)))}
} 


#' Downweighted Zeros Averaging
#' 
#' \code{average_mean}- Standard mean averaging with \code{na.rm} set to \code{TRUE}.  
#' 
#' @export
#' @rdname average_downweighted_zero
average_mean <- function(x, na.rm = TRUE, ...) mean(x, na.rm = na.rm)



