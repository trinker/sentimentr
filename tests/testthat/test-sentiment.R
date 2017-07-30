context("Checking sentiment")

test_that("sentiment produces a data.table with numeric sentiment column",{

    mytext <- get_sentences(c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    ))
    expect_true(is(sentiment(mytext), "data.table"))
    expect_equal(nrow(sentiment(mytext)), 5)

    x <- sentiment(mytext)[["sentiment"]]
    expect_true(is.numeric(x))

})

test_that("sentiment question.weight = 0 alters the output",{

    mytext <- get_sentences(c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    ))
    x <- sentiment(mytext)[["sentiment"]]
    y <- sentiment(mytext, question.weight = 0)[["sentiment"]]

    expect_false(all(x == y))
})



test_that("sentiment n.before/n.after alters the output",{

    mytext <- get_sentences(c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    ))
    x <- sentiment(mytext)[["sentiment"]]
    y <- sentiment(mytext, n.before = Inf, n.after = 0)[["sentiment"]]

    expect_false(all(x == y))
})

test_that("sentiment adversative.weight = 0 alters the output",{

    mytext <- get_sentences(c(
       'do you like it, but I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    ))
    x <- sentiment(mytext)[["sentiment"]]
    y <- sentiment(mytext, adversative.weight=3)[["sentiment"]]

    expect_false(all(x == y))
})


test_that("sentiment_by plots a ggplot object",{

    mytext <- get_sentences(c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    ))

    expect_true(is(plot(sentiment(mytext)), "ggplot"))

})
