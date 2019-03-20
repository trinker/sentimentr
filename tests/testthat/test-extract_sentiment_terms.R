context("Checking extract_sentiment_terms")

test_that("extract_sentiment_terms extracts the terms",{

    suppressWarnings(RNGversion("3.5.0"))
    set.seed(10)
    x <- get_sentences(sample(hu_liu_cannon_reviews[[2]], 3000, TRUE))
    pol_words <- extract_sentiment_terms(x)
    expect_equal(pol_words$negative[1:3], list(character(0), c("wait", "restless", "fire"), character(0)))
})

