context("Checking get_sentences")

test_that("get_sentences works on character vectors",{

    x <- paste0(
        "Mr. Brown comes! He says hello. i give him coffee.  i will ",
        "go at 5 p. m. eastern time.  Or somewhere in between!go there"
    )

    g1 <- structure(list(c("Mr. Brown comes!", "He says hello.", "i give him coffee.", 
        "i will go at 5 p.m. eastern time.", "Or somewhere in between!", 
        "go there")), class = c("get_sentences", "get_sentences_character", 
        "list"))

    expect_equal(get_sentences(x), g1)

})

test_that("get_sentences works on sentment/sentiment_by",{

    mytext <- c(
       'do you like it?  But I hate really bad dogs',
       'I am the best friend.',
       'Do you really like it?  I\'m not happy'
    )

    mytext <- get_sentences(mytext)
    y <- structure(list(c("do you like it?", "But I hate really bad dogs"
        ), "I am the best friend.", c("Do you really like it?", "I'm not happy"
        )), class = c("get_sentences", "get_sentences_character", "list"
        ))

    x <- sentiment_by(mytext, question.weight = 0)

    expect_equal(get_sentences(x), y)
    expect_equal(get_sentences(uncombine(x)), y)

})

test_that("get_sentences works on get_sentences",{

    x <- paste0(
        "Mr. Brown comes! He says hello. i give him coffee.  i will ",
        "go at 5 p. m. eastern time.  Or somewhere in between!go there"
    )

    expect_equal(get_sentences(x), get_sentences(get_sentences(x)))

})
