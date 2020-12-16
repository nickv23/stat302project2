my_knn_cv <- function(train, cl, k_nn, k_cv) {

  fold <- sample(rep(1:k_cv, length = nrow(train)))
  data <- data.frame(train, "split" = fold)
  class <- c()
  mse <- c()

  # For loop that determines the test and training folds and runs knn model
  for (i in 1:k_cv) {
    data_train <- data %>% filter(data$split != i)
    data_train <- data_train[3:6]
    train_cl <- data %>% filter(data$split != i) %>% pull(species)
    data_test <- data %>% filter(split == i)
    data_test <- data_test[3:6]
    test_cl <- data %>% filter(data$split == i) %>% pull(species)
    test_pred <- knn(train = data_train, test = data_test, cl = train_cl, k = k_nn)
    class[fold == i] <- test_pred
    pred_mat <- data.frame(test_cl, test_pred)
    count <- 0
    # For loop that determines misclassification rate for one fold
    for (j in 1:nrow(pred_mat)) {
      if (pred_mat[j, 1] == pred_mat[j, 2]) {
        count <- count + 1
      }
    }
    mse[i] <- 1 - (count / nrow(pred_mat))
  }

  cv_err <- mean(mse)
  outputs <- list("Predictions" = class, "CV_err" = cv_err)
  return(outputs)
}
