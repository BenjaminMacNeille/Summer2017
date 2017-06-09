#install.packages("caret")
install.packages("mlbench")
library(randomForest)
library(mlbench)
library(caret)

#dataframe created from other R script
df = read.csv("Crohns_DF.csv")

#
x = df[ , !(names(df) %in% c("DUPERSID", "totalXP", "officeXP", "opXP"))]
y = df$totalXP


#mtry: Number of variables randomly sampled as candidates at each split.
#ntree: Number of trees to grow.



# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(totalXP~., data=df, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
