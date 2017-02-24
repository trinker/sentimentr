#' Rescale a Numeric Vector
#' 
#' Rescale a numeric vector with the option to make signed (-1, 1, or 0) and 
#' retain zero as neutral.
#' 
#' @param x A numeric vector.
#' @param lower An upper limit to rescale to.
#' @param upper A lower limit to rescale to.
#' @param keep.zero logical.  If \code{TRUE} the zeros are kept as neutral.
#' @param sign logical.  If \code{TRUE} the vector will be scaled as (-1, 1, or 0)
#' @param \ldots ignored.
#' @return Returns a rescaled vector of the same length as \code{x}.
#' @export
#' @examples
#' general_rescale(c(NA, -3:3))
#' general_rescale(c(NA, -3:3), binary = TRUE)
#' general_rescale(c(NA, -3:3), keep.zero = FALSE)
#' general_rescale(c(NA, -3:3), keep.zero = FALSE, lower = 0, upper = 100)
general_rescale <- function(x, lower = -1, upper = 1, keep.zero = TRUE, sign = FALSE, ...){
    if (isTRUE(sign)) return(sign(x))
    if (!isTRUE(keep.zero)) return(general_rescale_h(x, lower=lower, upper=upper))

    stopifnot(lower < -.001 | upper > .001)

    y <- sign(x)  
    na <- is.na(x)
    x[y==-1 & !na] <- general_rescale_h(x[y==-1 & !na], lower=lower, upper = -.001)
    x[y==1 & !na] <- general_rescale_h(x[y==1 & !na], lower=.001, upper = upper)
    x
}


general_rescale_h <- function(x, lower, upper){

    rng <-  range(x, na.rm = TRUE, finite = TRUE)
    if (diff(rng) == 0) return(stats::setNames(rep(upper, length(x)), names(x)))
    (x - rng[1])/diff(rng) * diff(range(c(lower, upper))) + lower

}

