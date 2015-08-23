context("Checking sentiment")

test_that("sentiment produces a data.table with numeric sentiment column",{

    mytext <- c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    )
    expect_true(is(sentiment(mytext), "data.table"))
    expect_equal(nrow(sentiment(mytext)), 5)

    x <- sentiment(mytext)[["sentiment"]]
    expect_true(is.numeric(x))

})

test_that("sentiment question.weight = 0 alters the output",{

    mytext <- c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    )
    x <- sentiment(mytext)[["sentiment"]]
    y <- sentiment(mytext, question.weight = 0)[["sentiment"]]

    expect_false(all(x == y))
})



test_that("sentiment n.before/n.after alters the output",{

    mytext <- c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not a fan'
    )
    x <- sentiment(mytext)[["sentiment"]]
    y <- sentiment(mytext, n.before = Inf, n.after = 0)[["sentiment"]]

    expect_false(all(x == y))
})

test_that("sentiment but.weight = 0 alters the output",{

    mytext <- c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'I like it but I really like eggs'
    )
    x <- sentiment(mytext)[["sentiment"]]
    y <- sentiment(mytext, but.weight=3)[["sentiment"]]

    expect_false(all(x == y))
})
