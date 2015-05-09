######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Data Preprocesing (Sum of rows and cols)
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

#------------------- APPLY IT TO THE DATASET  -----------------------#

# Remove the white rows and columns from each image
message("Doing the mean to each column and row ...")
row_sum <- vector(mode="list", length=nrow(data))
col_sum <- vector(mode="list", length=nrow(data))
for (i in 1:nrow(data)) {
    row_sum[[i]] <- colMeans(matrix(as.numeric(unlist(data[i,])), nrow = nrow_image))
    col_sum[[i]] <- rowMeans(matrix(as.numeric(unlist(data[i,])), nrow = nrow_image))
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}

# Build the new data frames
message("Building the new data frames...")
new_data_row <- data.frame( matrix(rep(NA, nrow(data)*ncol_image), nrow=nrow(data), ncol=ncol_image))
new_data_col <- data.frame( matrix(rep(NA, nrow(data)*nrow_image), nrow=nrow(data), ncol=nrow_image))
for (i in 1:nrow(data)){
    new_data_row[i,] <- row_sum[[i]]   
    new_data_col[i,] <- col_sum[[i]]   
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}

new_train_row <- cbind(labels, new_data_row[1:nrow(train),])
new_test_row  <- new_data_row[(nrow(train)+1):nrow(data),]

new_train_col <- cbind(labels, new_data_col[1:nrow(train),])
new_test_col  <- new_data_col[(nrow(train)+1):nrow(data),]

new_train_both <- cbind(new_train_col, new_data_row[1:nrow(train),])
new_test_both  <- cbind(new_test_col, new_test_row)

# Write the data frames
write.csv(new_train_row, file = paste(working_directory, "/csv/train_row.csv", sep=""), row.names = FALSE)
write.csv(new_test_row, file = paste(working_directory, "/csv/test_row.csv", sep=""), row.names = FALSE)

write.csv(new_train_col, file = paste(working_directory, "/csv/train_col.csv", sep=""), row.names = FALSE)
write.csv(new_test_col, file = paste(working_directory, "/csv/test_col.csv", sep=""), row.names = FALSE)

write.csv(new_train_both, file = paste(working_directory, "/csv/train_row_col.csv", sep=""), row.names = FALSE)
write.csv(new_test_both, file = paste(working_directory, "/csv/test_row_col.csv", sep=""), row.names = FALSE)
