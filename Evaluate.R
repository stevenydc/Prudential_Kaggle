setwd('~/Desktop/Prudential')
source("0-Load Libraries.R")

## quadratic weighted kappa --------------------------------------------------
weightedKappa <- function(actual, predicted, min_rating = 1, max_rating = 8) {
  # ensure pairs are valid
  if (length(actual) != length(predicted))
    stop("Lengths of actual labels and predicted labels are not equal")
  
  # build pairs of (actual rating, predicted rating)
  pairs <- cbind(actual, predicted)
  
  # get number of ratings
  n <- (max_rating - min_rating) + 1
  
  # build a observed histogram matrix
  histogram_matrix <- matrix(0, nrow = n, ncol = n)
  for (row in 1:nrow(pairs)) {
    pair <- pairs[row, ]
    histogram_matrix[pair[1], pair[2]] <- histogram_matrix[pair[1], pair[2]] + 1
  }
  
  histogram_matrix <- histogram_matrix / sum(histogram_matrix)
  
  # build a weight matrix
  weight_matrix <- matrix(0, nrow = n, ncol = n)
  for (row in 1:n) {
    for (col in 1:n) {
      weight_matrix[row, col] <- (row - col) ^ 2 / (n - 1) ^ 2
    }
  }
  
  # build an expected frequency matrix
  histogram_actual <- apply(histogram_matrix, 1, sum)
  histogram_predicted <- apply(histogram_matrix, 2, sum)
  expected_ratings_matrix <- histogram_actual %o% histogram_predicted
  normalized_ratio <- sum(histogram_matrix) / sum(expected_ratings_matrix)
  expected_ratings_matrix <- expected_ratings_matrix * normalized_ratio
  
  # calculate kappa
  kappa <- 1 - (sum(weight_matrix * histogram_matrix) / sum(weight_matrix * expected_ratings_matrix))
  
  return (kappa)
}

## k fold cross evaluation ---------------------------------------------------

crossEvaluate <- function(data, numFolds, modelFunction, formula, ...) {
  folds <- createFolds(1:nrow(data), k = numFolds)
  scores <- numeric(length = numFolds)
  for (fold in seq_along(folds)) {
    train_rows <- folds[[fold]]
    test_rows <- unlist(folds[seq_along(folds) != fold]) %>% sort() %>% as.vector()
    train <- data[train_rows, ]
    test <- data[test_rows, ]
    model <- modelFunction(formula = formula, data = train, ...)
    pred <- predict(model, test, type = "response")
    actual <- test[["Response"]]
    scores[fold] <- weightedKappa(actual = actual, predicted = pred, min_rating = 1, max_rating = 8)
    print("Complete once!"); print(scores[fold])
  }
  return (scores)
}
