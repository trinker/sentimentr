context("Checking is_key")

test_that("is_key detects keys",{

    key <- data.frame(
        words = sample(LETTERS),
        polarity = rnorm(26),
        stringsAsFactors = FALSE
    )

    mykey <- as_key(key)

    expect_true(is_key(mykey))
    expect_false(is_key(key))


})

