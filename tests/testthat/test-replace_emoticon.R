context("Checking replace_emoticon")
library(lexicon)

test_that("replace_emoticon converts emoticons to words",{

    x <- lexicon::hash_emoticons[[1]][16]

    expect_equal(replace_emoticon(x), " sad ")

})

