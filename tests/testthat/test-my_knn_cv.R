test_that("my_knn_cv_test runs and returns a list", {
  data <- na.omit(my_penguins)
  cl <- data$species
  data <- data[3:6]
  expect_is(my_knn_cv(data, cl, 5, 5), "list")
})
