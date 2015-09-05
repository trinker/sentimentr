context("Checking syntax_highlight")

test_that("syntax_highlight ...",{

    sam <- sentiment_by(gsub("Sam-I-am", "Sam I am", sam_i_am))
    temp <- tempdir()
    syntax_highlight(sam, open=FALSE, file = file.path(temp, "polarity.html"))
    expect_true(file.exists(file.path(temp, "polarity.html")))

})

