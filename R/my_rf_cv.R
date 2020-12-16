my_rf_cv <- function(k) {

  fold <- sample(rep(1:k, length = nrow(penguins_data)))
  split_penguins <- data.frame(penguins_data, "split" = fold)
  mse <- c()

  #For loop that determines test and training folds and runs random forest model
  for (i in 1:k) {
    forest_train <- split_penguins %>% filter(split_penguins$split != i)
    forest_test <- split_penguins %>% filter(split_penguins$split == i)
    forest_model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data =
                                   forest_train, ntree = 100)
    predictions <- predict(forest_model, forest_test[, -1])

    mse[i] <- ((mean(split_penguins$body_mass_g) - mean(predictions))^2)
  }

  cv_err <- mean(mse)
  return(cv_err)
}
