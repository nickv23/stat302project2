test_that("my_rf_cv_test runs and returns the cross validation error", {
  expect_is(my_rf_cv(5), "numeric")
})
