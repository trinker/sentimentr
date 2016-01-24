context("Checking replace_rating")

test_that("replace_rating replaces ratings",{

    x <- c(
        "I give an A+",
        "He deserves an F",
        "It's C+ work",
        "A poor example deserves a C!"
    )

    x2 <- c("I give an very excellent excellent+", "He deserves an very bad",
        "It's slightly above average+ work",
        "very excellent excellent poor example deserves a slightly above average!"
    )

    expect_equal(replace_grade(x), x2)


})

