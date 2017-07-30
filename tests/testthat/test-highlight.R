context("Checking highlight")

test_that("highlight produces an HTML file",{

    sam <- sentiment_by(get_sentences(gsub("Sam-I-am", "Sam I am", sam_i_am)))
    temp <- tempdir()
    highlight(sam, open=FALSE, file = file.path(temp, "polarity.html"))
    expect_true(file.exists(file.path(temp, "polarity.html")))

})

