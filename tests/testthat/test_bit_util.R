context("COnverting bit indices to numeric")

test_that("we obtain the numeric values corresponding to bit indices", {
    expect_equal(bitToInt(c(0, 3)), 9)
    expect_equal(bitToInt(c(7, 4, 0, 1, 8)), 403)
})