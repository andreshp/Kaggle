######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Titanic - Random Forest
#######################################################################

#--------------------------- GETTING THE DATA ----------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/Titanic"

train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'integer',   # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'factor',    # Cabin
                        'factor',    # Embarked
                        'character', # Title
                        'integer',   # FamilySize
                        'factor',    # Surname
                        'factor'     # FamilyID
)

test.column.types <- train.column.types[-2]

train.data <- read.csv(paste(working_directory, "/csv/train_preprocessed.csv", sep=""), colClasses=train.column.types, stringsAsFactors=FALSE)
test.data  <- read.csv(paste(working_directory, "/csv/test_preprocessed.csv", sep=""), colClasses=test.column.types, stringsAsFactors=FALSE)

#------------------------------ GBM -------------------------------#

library(plyr)
library(gbm)


set.seed(128)

k <- 20 #Folds

# sample from 1 to k, nrow times (the number of observations in the data)
train.sector <- sample(1:k, nrow(train.data), replace = TRUE)
list <- 1:k

# Prepare the data
ntrees <- 1000000
train.data$Survived <- as.numeric(train.data$Survived)-1

# prediction and testset data frames that we add to with each iteration over the folds

prediction <- data.frame()
testsetCopy <- data.frame()
test_prediction <- data.frame(index=1:nrow(test.data))

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(train.data, train.sector %in% list[-i])
    testset <- subset(train.data, train.sector %in% c(i))
    
    # run a random forest model
    mymodel <- gbm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked + FamilySize + FamilyID,
                     data=train.data, distribution = "gaussian", n.trees = ntrees, shrinkage = 0.000001, interaction.depth = 1, n.minobsinnode = 10,
                     train.fraction = 0.8, verbose = FALSE)
    
    
    # remove response column 2, Survived
    temp <- as.data.frame(predict(mymodel, testset[,-2]))
    # append this iteration's predictions to the end of the prediction data frame
    prediction <- rbind(prediction, temp)
    
    # Prediction in the test set
    temp <- as.data.frame(predict(mymodel, test.data))
    test_prediction <- cbind(test_prediction, temp)

    # append this iteration's test set to the test set copy data frame
    # keep only the Survived column
    testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,2]))

    progress.bar$step()
}

# add predictions and actual Sepal Length values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Predicted = ifelse(result$Predicted > 0.5, 1, 0)
result$Difference <- abs(result$Actual - result$Predicted)

# As an example use Mean Absolute Error as Evalution 
summary(result$Difference)

# Compute essamble for the test data
test_prediction <- test_prediction[-1]
names(test_prediction) <- c(1:k)
enssamble <- c()

for (i in 1:k){
    test_prediction[i] <- ifelse(test_prediction[[i]] > 0.5, 1, 0)
}

for (j in 1:nrow(test_prediction)){
    enssamble <- rbind(enssamble, ifelse(mean(as.numeric(test_prediction[j,])) > 0.5, 1, 0))
}

submit <- data.frame(PassengerId = test.data$PassengerId, Survived = enssamble)
write.csv(submit, file = paste(working_directory, "/R/Results/GBMgaussianEnsambledMultiple2.csv", sep=""), row.names = FALSE)
