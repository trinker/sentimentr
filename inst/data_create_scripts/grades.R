
grades <- structure(list(x = c("A+", "A", "A-", "B+", "B", "B-", "C+",
    "C", "C-", "D+", "D", "D-", "F+", "F", "F-"), y = c("very excellent excellent",
    "very excellent", "excellent", "almost excellent", "slightly excellent",
    "slightly good", "slightly above average", "average", "slightly below average",
    "kinda bad", "bad", "bad", "very bad", "very very bad", "very bad bad"
    )), .Names = c("x", "y"), class = c("tbl_df", "data.frame"), row.names = c(NA,
    -15L))

data.table::setDT(grades)

replace_grade <- function (text.var, grade_dt = sentimentr::grades, ...) {
   .mgsub(grade_dt[["x"]], grade_dt[["y"]], text.var, fixed = FALSE)
}

text <- replace_grade(c("I give an A+", "He deserves an F", "It's C+ work", "A poor example deserves a C!"))
sentiment(text)
