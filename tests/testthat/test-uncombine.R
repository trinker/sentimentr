context("Checking uncombine")

test_that("uncombine ...",{

    mytext <- get_sentences(c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not happy'
    ))

    x <- sentiment_by(mytext)
    y <- uncombine(x)
    expect_true(nrow(x) < nrow(y))
    expect_true(is(y, "data.table"))

})

