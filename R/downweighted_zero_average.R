#' Downweighted Zeros Averaging
#' 
#' Downweight the zeros in a vector for averaging.  This is sueful in the 
#' context of language where we don't want the neutral sentences to have such a 
#' strong influence on the general sentiment of the discourse with multiple 
#' sentences.  Essentially, this means neutral sentences are seen as having less 
#' emotional impact than a polarized sentence.
#' 
#' @param x A numeric vector.
#' @param na.rm logical.  Should \code{NA} values should be stripped before the 
#' computation proceeds.
#' @param \ldots ignored.
#' @return Returns a scalar summary of the zero downweighted average
#' @export
#' @examples 
#' x <- c(1, 2, 0, 0, 0, -1)
#' mean(x)
#' downweighted_zero_average(x)
#' downweighted_zero_average(c(NA, x))
#' mean(c(0, 0, 0, x))
#' downweighted_zero_average(c(0, 0, 0, x))
downweighted_zero_average <- function (x, na.rm = TRUE, ...) {
    sum(x, na.rm = na.rm)/{
        sum(x != 0, na.rm = na.rm) + sqrt(log(1 + sum(x == 0, 
            na.rm = na.rm)))
    }
} 

