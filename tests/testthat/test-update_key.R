context("Checking update_key")

test_that("update_key ...",{


    key <- data.frame(
        words = sample(letters),
        polarity = rnorm(26),
        stringsAsFactors = FALSE
    )

    mykey <- as_key(key)

    out <- update_key(
        mykey, 
        drop = c("f", "h"),
        x = data.frame(x = c("dog", "cat", "rock"), 
        y = c(1, -1, 1), stringsAsFactors = FALSE)
    )

    expect_true(is_key(out))
    expect_equal(nrow(out), 27)

})

