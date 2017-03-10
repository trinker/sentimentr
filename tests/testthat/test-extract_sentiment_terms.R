context("Checking extract_sentiment_terms")

test_that("extract_sentiment_terms extracts the terms",{

    set.seed(10)
    x <- sample(cannon_reviews[[3]], 3000, TRUE)
    pol_words <- extract_sentiment_terms(x)
    expect_equal(pol_words$negative[1:3], list(character(0), c("wait", "restless", "fire"), character(0)))
})

