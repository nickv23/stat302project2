#' Nearest Neighbor Cross-Validation Function
#'
#' This function uses performs k nearest neighbors to predict an output class and cross validation to
#'   calculate the misclassification rate.
#'
#' @param train - An input data frame.
#' @param cl - Vector of the true class values of the training data.
#' @param k_nn - Numeric representing the number of neighbors.
#' @param k_cv - Numeric representing the number of folds.
#' @keywords prediction
#'
#' @return A list with two objects:
#' \itemize {
#' \item class - Vector with the predicted class values of the testing data.
#' \item cv_err - Numeric representing the rate of misclassification.
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "select"
#' @import stats dplyr magrittr randomForest class tidyr
#'
#' @examples
#' penguins_data <- na.omit(my_penguins)
#' true_class <- penguins_data$species
#' penguins_data <- penguins_data %>% dplyr::select(-island, -year, -sex, -species)
#' my_knn_cv(penguins_data, true_class, 3, 5)
#' my_knn_cv(penguins_data, true_class, 5, 5)
#' my_knn_cv(penguins_data, true_class, 10, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  fold <- sample(rep(1:k_cv, length = nrow(train)))
  data <- data.frame(train, "split" = fold)
  class <- knn(train, train, cl, k_nn)
  mse <- c()


  # For loop that determines the test and training folds and runs knn model
  for (i in 1:k_cv) {
    data_train <- data %>% dplyr::filter(data$split != i) %>% dplyr::select(-split)
    train_cl <- cl[fold != i]

    data_test <- data %>% dplyr::filter(split == i) %>% dplyr::select(-split)
    test_cl <- cl[fold == i]

    test_pred <- knn(data_train, data_test, train_cl, k_nn)
    mse[i] <- mean(test_pred != test_cl)
  }

  cv_err <- mean(mse)
  outputs <- list("Predictions" = class, "CV_err" = cv_err)
  return(outputs)
}
