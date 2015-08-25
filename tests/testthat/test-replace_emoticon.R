context("Checking replace_emoticon")

test_that("replace_emoticon converts emoticons to words",{

    x <- sentimentr::emoticons[[1]][16]

    expect_equal(replace_emoticon(x), " sad ")

})

