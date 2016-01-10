context("Checking update_key")

test_that("update_key ...",{


    key <- data.frame(
        words = sample(LETTERS),
        polarity = rnorm(26),
        stringsAsFactors = FALSE
    )

    mykey <- as_key(key)

    out <- update_key(mykey, drop = c("F", "H"),
        x = data.frame(x = c("Dog", "Cat", "Rock"), y = c(1, -1, 1), stringsAsFactors = FALSE))

    expect_true(is_key(out))
    expect_equal(nrow(out), 27)

})

