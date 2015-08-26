context("Checking utils")

test_that("paste2 pastes multiple columns",{

    expect_equal(paste2(list(1:5, 1:5, 1:5)), c("1.1.1", "2.2.2", "3.3.3", "4.4.4", "5.5.5"))
    expect_equal(paste2(list(1:5, 1:5, c(1:4, " 5")), trim=FALSE), c("1.1.1", "2.2.2", "3.3.3", "4.4.4", "5.5. 5"))
    expect_equal(paste2(list(1:5, 1:5, c(1:4, NA)), handle.na = FALSE), c("1.1.1", "2.2.2", "3.3.3", "4.4.4", "5.5.NA"))
})

test_that("SE gives a number",{

    expect_true(is.numeric(SE(1:10)))

})

