context("Checking as.key")

test_that("as.key makes a data.table key",{

    key <- data.frame(
        words = sample(LETTERS),
        polarity = rnorm(26),
        stringsAsFactors = FALSE
    )

    mykey <- as.key(key)

    expect_true(is(mykey, "data.table"))
    expect_true(all(dim(mykey) == c(26, 2)))
})

