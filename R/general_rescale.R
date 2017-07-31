#' Rescale a Numeric Vector
#' 
#' Rescale a numeric vector with the option to make signed (-1, 1, or 0) and 
#' retain zero as neutral.
#' 
#' @param x A numeric vector.
#' @param lower An upper limit to rescale to.
#' @param upper A lower limit to rescale to.
#' @param mute A positive value greater than 1 to lower the extremes and pull 
#' the fractions up.  This becomes the denominator in a power to raise each 
#' element by (sign is retained) where the numerator is 1.  This is useful for 
#' mellowing out the extremes.
#' @param keep.zero logical.  If \code{TRUE} the zeros are kept as neutral.
#' @param sign logical.  If \code{TRUE} the vector will be scaled as (-1, 1, or 0)
#' @param \ldots ignored.
#' @return Returns a rescaled vector of the same length as \code{x}.
#' @export
#' @examples
#' 
#' general_rescale(c(1, 0, -1))
#' general_rescale(c(1, 0, -1, 1.4, -2))
#' general_rescale(c(1, 0, -1, 1.4, -2), lower = 0, upper = 1)
#' general_rescale(c(NA, -4:3))
#' general_rescale(c(NA, -4:3), keep.zero = FALSE)
#' general_rescale(c(NA, -4:3), keep.zero = FALSE, lower = 0, upper = 100)
#' 
#' ## mute extreme values
#' set.seed(10)
#' x <- sort(c(NA, -100, -10, 0, rnorm(10, 0, .1), 10, 100), na.last = FALSE)
#' general_rescale(x)
#' general_rescale(x, mute = 5)
#' general_rescale(x, mute = 10)
#' general_rescale(x, mute = 100)
general_rescale <- function(x, lower = -1, upper = 1, mute = NULL, 
    keep.zero = lower < 0, sign = FALSE, ...){
    
    if (!is.null(mute)) {
        stopifnot(is.numeric(mute) & mute >= 1)
        x <- sign(x) * (abs(x) ^ (1/mute))
    }
    
    if (isTRUE(sign)) return(sign(x))
    if (!isTRUE(keep.zero)) return(general_rescale_h(x, lower=lower, upper=upper))

    stopifnot(lower < -.001 | upper > .001)
    if (isTRUE(keep.zero & lower >= 0)) stop('If `lower >= 0` then `keep.zero` must be set to `FALSE`')

    y <- sign(x)  
    na <- is.na(x)
    if (lower < 0) {
        x[y==-1 & !na] <- -general_rescale_h(c(0, abs(x[y==-1 & !na])), lower=.001, upper = abs(lower))[-1]
    } else {
        x[y==-1 & !na] <- general_rescale_h(c(0, x[y==-1 & !na]), lower=.001, upper = abs(lower))[-1]
    }
    x[y==1 & !na] <- general_rescale_h(c(0, x[y==1 & !na]), lower=.001, upper = upper)[-1]
    x
}



general_rescale_h <- function(x, lower, upper){

    rng <-  range(x, na.rm = TRUE, finite = TRUE)
    if (diff(rng) == 0) return(stats::setNames(rep(upper, length(x)), names(x)))
    (x - rng[1])/diff(rng) * diff(range(c(lower, upper))) + lower

}

