setwd('~/Desktop/Prudential')
library(readr)
library(e1071)
library(dplyr)

train <- read_csv('train.csv')
#* test <- read_csv('test.csv')
hist(train$Response, probability = T)

getNAColumns <- function(data) {
  na_summary <- apply(data, 2, function(x) { return (sum(! complete.cases(x))) } )
  na_columns <- na_summary[na_summary != 0]
  return (attributes(na_columns)[['names']])
}

train_na_columns <- getNAColumns(train)
train_no_na <- train %>% dplyr::select(-one_of(train_na_columns))

set.seed(100)
sample_size <- 0.05 * nrow(train_no_na) %>% round()
sampled <- sample(1:nrow(train_no_na), size = sample_size)
svm_model <- svm(as.factor(Response) ~ ., data = train_no_na, subset = sampled,
                 type = 'C-classification')

test_prediction <- predict(svm_model, test)


createSubmission <- function(predictions) {
  submission <- data.frame(Id = test$Id, Response = predictions)
  write_csv(submission, 'submission.csv')
}


createSubmission(test_prediction)
