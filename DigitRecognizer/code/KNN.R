######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - KNN
#######################################################################

#--------------------------- GETTING THE DATA ----------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/train_row.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test_row.csv", sep=""),  header=TRUE)

labels <- train[,1]
train <- train[,-1]

#----------------------- BUILD THE KNN PREDICTION -----------------------#

library(FNN)

# KNN with k = 10. (0.96557 in Kaggle)
results <- (0:9)[knn(train, test, labels, k = 10, algorithm="cover_tree")]


# KNN with k = 1. (0.97114 in Kaggle, 0.9755 with data preprocesing)
results <- (0:9)[knn(train, test, labels, k = 1, algorithm="cover_tree")]

#------------------------- WRITE THE PREDICTION -------------------------#

write.csv(data.frame(ImageId = 1:length(results), Label = results), file = paste(working_directory, "/Results/knn1_row.csv", sep=""), row.names = FALSE)