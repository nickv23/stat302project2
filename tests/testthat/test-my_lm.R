test_that("my_lm function runs regression and returns data frame of results", {
  expect_is(my_lm(mpg ~ hp + wt, data = mtcars), "data.frame")
})
