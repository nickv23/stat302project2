#' Random Forest Cross-Validation Function
#'
#' This function uses performs random forests to predict the body mass of penguins using the bill length, bill depth,
#'   and flipper length and calculates the misclassification error.
#'
#' @param k - A numeric indicating the number of folds.
#' @keywords prediction
#'
#' @return A numeric indicating the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
my_rf_cv <- function(k) {

  fold <- sample(rep(1:k, length = nrow(my_penguins)))
  split_penguins <- data.frame(my_penguins, "split" = fold)
  split_penguins <- na.omit(split_penguins) %>% dplyr::select(-island, -year, -sex, -species)
  mse <- c()

  #For loop that determines test and training folds and runs random forest model
  for (i in 1:k) {
    forest_train <- split_penguins %>% dplyr::filter(split_penguins$split != i) %>% dplyr::select(-split)
    forest_test <- split_penguins %>% dplyr::filter(split_penguins$split == i) %>% dplyr::select(-split)
    forest_model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data =
                                   forest_train, ntree = 100)
    predictions <- predict(forest_model, forest_test[, -4])

    mse[i] <- mean((predictions - forest_test[[4]])^2)
  }

  cv_err <- mean(mse)
  return(cv_err)
}
