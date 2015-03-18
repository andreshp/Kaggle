######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - KNN with averages images
#######################################################################

#----------------------------- GET THE DATA ------------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/train_preprocesed.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test_preprocesed.csv", sep=""),  header=TRUE)

labels <- as.numeric(train[,1])
train <- train[,-1]

#---------------------------- GET THE IMAGES -----------------------------#

# Find the mean of each class of numbers:
images <- data.frame()
for(x in 0:9){
    average <- colMeans(train[labels == x,])
    images <- rbind(images, average)
}

#----------------------- TRY ON TRAINING DATA -----------------------#

library(FNN)

# AverageKNN
results <- (0:9)[knn(images, train, 0:9, k = 1, algorithm="cover_tree")]

success_rate <- sum(results == labels) / nrow(train)

# We see that success_rate is 0.8095. So there is a 20% of the data where this idea fails :(
# So... let's eliminate that data becasue it is not good enough!
new_train <- train[results == labels,]
write.csv(data.frame(Labels = labels[results==labels], new_train), file = paste(working_directory, "/csv/new_train6.csv", sep=""), row.names = FALSE)

#----------------------- BUILD THE KNN PREDICTION -----------------------#

# AverageKNN
results <- (0:9)[knn(images, train, 0:9, k = 1, algorithm="cover_tree")]

#------------------------- WRITE THE PREDICTION -------------------------#

write.csv(data.frame(ImageId = 1:length(results), Label = results), file = paste(working_directory, "/Results/AverageKNN1.csv", sep=""), row.names = FALSE)