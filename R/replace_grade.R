#' Replace Grades With Words
#'
#' Replaces gradess with word equivalents.
#'
#' @param text.var The text variable.
#' @param grade_dt A \pkg{data.table} of gradess and corresponding word meanings.
#' @param \ldots ignored.
#' @return Returns a vector of strings with grades replaced with word
#' equivalents.
#' @keywords grade
#' @export
#' @examples
#' (text <- replace_grade(c(
#'     "I give an A+",
#'     "He deserves an F",
#'     "It's C+ work",
#'     "A poor example deserves a C!"
#' )))
#' sentiment(text)
replace_grade <- function (text.var, grade_dt = sentimentr::grades, ...) {
    .mgsub(grade_dt[["x"]], grade_dt[["y"]], text.var, fixed = FALSE)
}

