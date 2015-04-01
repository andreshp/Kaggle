######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Data Preprocesing
#######################################################################

#-------------------------- GET THE DATA ---------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/train_preprocessed.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test_preprocessed.csv", sep=""),  header=TRUE)

labels <- as.numeric(train[,1])
train <- train[,-1]
data <- rbind(train, test)
nrow_image <- 20 # Images' pixels per row
ncol_image <- 20 # Images' pixels per col

#--------------------------- FUNCTIONS  ----------------------------#



#------------------- APPLY IT TO THE DATASET  -----------------------#

# Remove the white rows and columns from each image
message("Doing the mean to each column ...")
row_sum <- vector(mode="list", length=nrow(data))
for (i in 1:nrow(data)) {
    row_sum[[i]] <- colMeans(matrix(as.numeric(unlist(data[i,])), nrow = nrow_image))
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}

# Build the new data frames
message("Building the new data frames...")
new_data <- data.frame( matrix(rep(NA, nrow(data)*ncol_image), nrow=nrow(data), ncol=ncol_image))
for (i in 1:nrow(data)){
    new_data[i,] <- row_sum[[i]]   
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}

new_train <- cbind(labels, new_data[1:nrow(train),])
new_test  <- new_data[(nrow(train)+1):nrow(data),]

# Write the data frames
write.csv(new_train, file = paste(working_directory, "/csv/train_row.csv", sep=""), row.names = FALSE)
write.csv(new_test, file = paste(working_directory, "/csv/test_row.csv", sep=""), row.names = FALSE)
