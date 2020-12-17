test_that("my_t.test throws error over invalid alternative", {
  expect_error(my_t.test(sample(1:5, 10), "alternative", 3))
})

test_that("my_t.test runs test and returns list", {
  expect_is(my_t.test(sample(1:5, 10, replace = TRUE), "less", 2), "list")
  expect_is(my_t.test(sample(1:5, 10, replace = TRUE), "greater", 4), "list")
  expect_is(my_t.test(sample(1:5, 10, replace = TRUE), "two.sided", 3), "list")
})
