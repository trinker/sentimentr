context("Checking sentiment_by")

test_that("sentiment_by gives combined elements when `by = NULL`",{

    mytext <- get_sentences(c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not happy'
    ))

    expect_true(is(sentiment_by(mytext), "data.table"))
    expect_true(nrow(sentiment_by(mytext)) == 3)

})

test_that("sentiment_by gives combined group when `by` is given",{

    x <- with(presidential_debates_2012, sentiment_by(get_sentences(dialogue), list(person, time)))
    expect_true(all(c('person', 'time') %in%colnames(x)))
})

test_that("sentiment_by plots a ggplot object",{

    mytext <- get_sentences(c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not happy'
    ))

    expect_true(is(plot(sentiment_by(mytext)), "ggplot"))

})
