# Install and load required packages
# install.packages("ggplot2")
library(ggplot2)
# install.packages("lattice")
library(lattice)
# install.packages("caret")
library(caret)

# Get the current working directory of the script
getwd()

# Load the dataset
startups = read.csv("D:/Downloads/Stats Modelling Projects/Datasets/50_Startups.csv")

# View the structure of the dataset
str(startups)

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(startups$Profit, p = 0.8,
                                  list = FALSE,
                                  times = 1)

trainData <- startups[trainIndex, ]
testData <- startups[-trainIndex, ]

# Simple Linear Regression
lm_model <- lm(Profit ~ RnDSpend, data = trainData)

# Multiple Linear Regression
mlm_model <- lm(Profit ~ RnDSpend+Administration+MKTSpend, data = trainData)

# Predictions
lm_pred <- predict(lm_model, testData)
mlm_pred <- predict(mlm_model, testData)

# Evaluate models
lm_rmse <- RMSE(lm_pred, testData$Profit)
mlm_rmse <- RMSE(mlm_pred, testData$Profit)

# Compare performances
cat("Simple Linear Regression RMSE:", lm_rmse, "\n")
cat("Multiple Linear Regression RMSE:", mlm_rmse, "\n")

summary(mlm_model)
