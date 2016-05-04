context("Checking as_key")

test_that("as_key makes a data.table key",{

    key <- data.frame(
        words = sample(letters),
        polarity = rnorm(26),
        stringsAsFactors = FALSE
    )

    mykey <- as_key(key)

    key2 <- data.frame(
        words = sample(letters),
        words2 = sample(letters),
        stringsAsFactors = FALSE
    )

    expect_true(is_key(as_key(key2, sentiment=FALSE), sentiment=FALSE))
    expect_error(as_key(key2))

    expect_true(is(mykey, "data.table"))
    expect_true(all(dim(mykey) == c(26, 2)))
})

