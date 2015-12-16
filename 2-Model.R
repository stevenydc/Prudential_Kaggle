setwd('~/Desktop/Prudential')
source("1-Clean.R")
source("Evaluate.R")

load("important_vars.RData")

train_1 <- select(train_less_na, one_of(as.character(important_vars$Variable)), Response)
sampled <- sample(1:nrow(train_1), size = 40000)
train_sampled <- train_1[sampled, ]
train_sampled$Response <- as.factor(train_sampled$Response)

scores <- crossEvaluate(train_sampled, numFolds = 3, modelFunction = randomForest,
                        formula = as.formula("Response ~ ."), ntree = 200)

scores3 <- crossEvaluate(train_sampled, numFolds = 3, modelFunction = svm,
                        formula = as.formula("Response ~ ."))
