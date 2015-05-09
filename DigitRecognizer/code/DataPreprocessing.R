######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Data Preprocesing
#######################################################################

#-------------------------- GET THE DATA ---------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/train.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test.csv", sep=""),  header=TRUE)

labels <- as.numeric(train[,1])
train <- train[,-1]
data <- rbind(train, test)
nrow_image <- 28 # Images' pixels per row

#--------------------------- FUNCTIONS  ----------------------------#

# zoomRows: A function to increase the size of the image rows per the zoom factor r. 
# Parameters:
#    - im : Image to resize.
#    - r  : Factor to resize each row.
# Return:
#    The image where each pixel has become r equal pixels in the row.
zoomRows <- function(im, r){
    # sapply will return a matrix where for each row indexed by i it is asigned 
    # unlist(lapply(im[i,], function(x) rep(x, r) ), that is, the vector im[i,]
    # with every element repeted r times.
    t(sapply(1:nrow(im), function(i) unlist(lapply(im[i,], function(x) rep(x, r) )) ))
}

# zoomCols: A function to increase the size of the image cols per the zoom factor r. 
# Parameters:
#    - im : Image to resize
#    - r  : Factor to resize each colum.
# Return:
#    The image where each pixel has become r equal pixels in the column.
zoomCols <- function(im, r){
    # sapply will return a matrix where for each column indexed by j it is asigned 
    # unlist(lapply(im[,j], function(x) rep(x, r) ), that is, the vector im[,j]
    # with every element repeted r times.
    sapply(1:ncol(im), function(j) unlist(lapply(im[,j], function(x) rep(x, r) )) )
}

# reduceRows: A function to reduce the size of the image rows per the reduce factor r. 
# Parameters:
#    - im : Image to resize.
#    - r  : Factor to reduce each row.
# Return:
#    The image where each r consecutive pixel has become the mean of those pixels in the row.
reduceRows <- function(im, r){
    # t(sapply) will return a matrix where for each row indexed by i it is asigned 
    # colMeans(matrix(im[i,], nrow=r)), that is, a vector where each element is
    # the mean of r consecutives elements in im[i,]
    t(sapply(1:nrow(im), function(i) colMeans(matrix(im[i,], nrow=r)) ))
}

# reduceCols: A function to reduce the size of the image cols per the reduce factor r. 
# Parameters:
#    - im : Image to resize.
#    - r  : Factor to reduce each col.
# Return:
#    The image where each r consecutive pixel has become the mean of those pixels in the col.
reduceCols <- function(im, r){
    # sapply will return a matrix where for each col indexed by j it is asigned 
    # colMeans(matrix(im[,j], nrow=r)), that is, a vector where each element is
    # the mean of r consecutives elements in im[,j]
    sapply(1:ncol(im), function(j) colMeans(matrix(im[,j], nrow=r)) )
}

# deleteWhiteRows: Delete the rows with mean less than the given ratio. 
# Parameters:
#    - im     : Image to which apply the function.
#    - ratio  : value that must exceed a row in order to be kept.
# Return:
#    The image whitout rows with mean less than ratio.
deleteWhiteRows <- function(im, ratio = 0){
    blanc_rows <- sapply(1:nrow(im), function(i) mean(unlist(im[i,])) > ratio)    
    im[blanc_rows,]    
}

# deleteWhiteCols: Delete the columns with mean less than the given ratio. 
# Parameters:
#    - im     : Image to which apply the function.
#    - ratio  : value that must exceed a column in order to be kept.
# Return:
#    The image whitout colums with mean less than ratio.
deleteWhiteCols <- function(im, ratio = 0){
    blanc_cols <- sapply(1:ncol(image), function(j) mean(unlist(image[,j])) > ratio)    
    im[,blanc_cols]
}

# regularize: Given an image, puts its maximum pixel to 255.
# Parameters:
#    - im     : Image to which apply the function.
# Return:
#    The image regularized.
regularize <- function(im){
    max_value <- max(unlist(im))
    im <- t(sapply(1:nrow(im), function(i) unlist(im[i,]) / max_value))
    im    
}


#------------------- APPLY IT TO THE DATASET  -----------------------#

# Remove the white rows and columns from each image
message("Removing white rows and columns ...")
images <- vector(mode="list", length=nrow(data))
for (i in 1:nrow(data)) {
    image <- matrix(data[i,], nrow = nrow_image)
    image <- deleteWhiteCols(image, 0)
    image <- deleteWhiteRows(image, 0)
    images[[i]] <- image
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}

# Regularize the pixels' value
message("Regularizing pixels' value...")
for (i in 1:nrow(data)) {
    images[[i]] <- regularize(images[[i]])
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}

# Get the maximum number of rows and columns:
max_row = max(sapply(images, nrow))
max_col = max(sapply(images, ncol))

# Resize each image to max_row x max_col
message("Resizing each image...")
for (i in 1:nrow(data)) {
    c_nrow <- nrow(images[[i]])
    c_ncol <- ncol(images[[i]])
    images[[i]] <- reduceCols(zoomCols(images[[i]],max_row),c_nrow)
    images[[i]] <- reduceRows(zoomRows(images[[i]],max_col),c_ncol)    
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}

# Build the new data frames
message("Building the new data frames...")
new_data_1 <- lapply(1:length(images), function(i) as.vector(images[[i]]))
new_data <- data.frame( matrix(rep(NA, nrow(data)*max_row*max_col), nrow=nrow(data), ncol=max_row*max_col) )
for (i in 1:nrow(data)){
    new_data[i,] <- new_data_1[[i]]   
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(data), " %")
    }
}


new_train <- cbind(labels, new_data[1:nrow(train),])
new_test  <- new_data[(nrow(train)+1):nrow(data),]

# Write the data frames
write.csv(new_train, file = paste(working_directory, "/csv/train_preprocessed.csv", sep=""), row.names = FALSE)
write.csv(new_test, file = paste(working_directory, "/csv/test_preprocessed.csv", sep=""), row.names = FALSE)

