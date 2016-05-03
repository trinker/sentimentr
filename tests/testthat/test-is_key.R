context("Checking is_key")

test_that("is_key detects keys",{

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

    mykey2 <- as_key(key2, sentiment=FALSE)

    expect_true(is_key(mykey))
    expect_false(is_key(key))
    expect_false(is_key(mykey2))
    expect_true(is_key(mykey2, sentiment=FALSE))

})

