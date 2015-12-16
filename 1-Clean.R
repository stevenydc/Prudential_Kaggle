setwd('~/Desktop/Prudential')
source("0-Load Data.R")

# remove these variables that have lots of missing data
countMissingData <- function(column) {
  return (sum(is.na(column)))
}
missing_data <- apply(train[, -1], 2, countMissingData)
remove_these <- names(missing_data[missing_data > 15000])
train_less_na <- select(train, -one_of(remove_these)) # create a train set with much less missing data

# fill in missing data for these variables with median
fill_in_these <- names( missing_data[missing_data > 0 & missing_data < 15000] )
apply(train_less_na[, fill_in_these], 2, summary) # look at these variables
for (column in fill_in_these) {
  missing <- is.na(train_less_na[, column])
  train_less_na[missing, column] <- median(train_less_na[, column], na.rm = T)
}

train_less_na$Response <- as.factor(train_less_na$Response)

