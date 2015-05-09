######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - KNN Study
#######################################################################

#--------------------------- GETTING THE DATA ----------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train.data <- read.csv(paste(working_directory, "/csv/train_preprocessed.csv", sep=""), header=TRUE)

labels <- train.data[,1]
train.data <- train.data[,-1]

#----------------------- BUILD THE KNN PREDICTION -----------------------#

library(FNN)
set.seed(28)

n <- 10
train.sector <- sample(1:n, nrow(train.data), replace = TRUE)
list <- 1:n

trainingset <- subset(train.data, train.sector %in% list[-1])
trainlabels <- subset(labels, train.sector %in% list[-1])
testset <- subset(train.data, train.sector %in% c(1))
testlabels <- subset(labels, train.sector %in% c(1))

# KNN
#k_min <- 1
#k_max <- 100

results <- (0:9)[knn(trainingset, testset, trainlabels, k = 10, algorithm="cover_tree")]    

# Results
message("Success Percent:")
print(sum(results == testlabels) / length(results))
message("Success Percent per Number:")
a <- table(testlabels)
b <- table(testlabels[results == testlabels])
print(b / a * 100)
message("Confusion Matrix:")
print(table(Predictions = results, TrueLabels = testlabels))
