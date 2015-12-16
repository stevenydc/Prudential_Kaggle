setwd('~/Desktop/Prudential')
source("Libraries.R")
source("Load.R")
source("Clean.R")

train_less_na$Product_Info_2 <- as.factor(train_less_na$Product_Info_2)

set.seed(100)
sampled <- sample(1:nrow(train_less_na), nrow(train_less_na) * 0.2)

rf <- randomForest(as.factor(Response) ~ ., data = train_less_na[, -1], importance = T)
varImpPlot(rf)
var_importance <- data.frame(Variable = colnames(train_less_na[, -c(1, 119)]),
                             Importance = as.vector(importance(rf)))
important_vars <- filter(var_importance, Importance > 100) %>% arrange(desc(Importance))

save(important_vars, file = "important_vars.RData")
save(rf, file = "rf.RData")

