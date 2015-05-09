######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Data Preprocesing (Preprocessed data with
#     the sum of rows and cols as variables)
#######################################################################

#-------------------------- GET THE DATA ---------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/train_preprocessed.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test_preprocessed.csv", sep=""),  header=TRUE)

train_row <- read.csv(paste(working_directory, "/csv/train_row.csv", sep=""), header=TRUE)
test_row <-  read.csv(paste(working_directory, "/csv/test_row.csv", sep=""),  header=TRUE)

train_col <- read.csv(paste(working_directory, "/csv/train_col.csv", sep=""), header=TRUE)
test_col <-  read.csv(paste(working_directory, "/csv/test_col.csv", sep=""),  header=TRUE)

train_row <- train_row[,-1]
train_col <- train_col[,-1]

#-------------------------- NEW DATAFRAME ---------------------------#

new_train <- cbind(cbind(train, train_row), train_col)
new_test  <- cbind(cbind(test, test_row), test_col)

write.csv(new_train, file = paste(working_directory, "/csv/train_total.csv", sep=""), row.names = FALSE)
write.csv(new_test, file = paste(working_directory, "/csv/test_total.csv", sep=""), row.names = FALSE)
