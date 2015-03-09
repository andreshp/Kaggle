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

# deleteBlancRows: Delete the rows with mean less than the given ratio. 
# Parameters:
#    - im     : Image to which apply the function.
#    - ratio  : value that must exceed a row in order to be kept.
# Return:
#    The image whitout rows with mean less than ratio.
deleteBlancRows <- function(im, ratio){
    blanc_rows <- sapply(1:nrow(im), function(i) mean(unlist(im[i,])) > 0)    
    im[blanc_rows,]    
}

# deleteBlancCols: Delete the columns with mean less than the given ratio. 
# Parameters:
#    - im     : Image to which apply the function.
#    - ratio  : value that must exceed a column in order to be kept.
# Return:
#    The image whitout colums with mean less than ratio.
deleteBlancCols <- function(im, ratio){
    blanc_cols <- sapply(1:ncol(image), function(j) mean(unlist(image[,j])) > 0)    
    im[,blanc_cols]    
}

#------------------- APPLICATION TO THE DATASET  -----------------------#

# Remove the blanc rows and columns from each train image
images_train <- vector(mode="list", length=nrow(train))
for (i in 1:nrow(train)) {
    image <- matrix(train[i,], nrow = nrow_image)
    image <- deleteBlancCols(image, 0)
    image <- deleteBlancRows(image, 0)
    images_train[[i]] <- image
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(train))
    }
}

# Remove the blanc rows and columns from each test image
images_test <- vector(mode="list", length=nrow(test))
for (i in 1:nrow(test)) {
    image <- matrix(test[i,], nrow = nrow_image)
    image <- deleteBlancCols(image, 0)
    image <- deleteBlancRows(image, 0)
    images_test[[i]] <- image
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(test))
    }
}

# Get the maximum number of rows and columns:
max_row = max(c(max(sapply(images, nrow)), max(sapply(images_test, nrow))))
max_col = max(c(max(sapply(images, ncol)), max(sapply(images_test, ncol))))

# Resize each train image to max_row x max_col
for (i in 1:nrow(train)) {
    c_nrow <- nrow(images_train[[i]])
    c_ncol <- ncol(images_train[[i]])
    images_train[[i]] <- reduceCols(zoomCols(images_train[[i]],max_row),c_nrow)
    images_train[[i]] <- reduceRows(zoomRows(images_train[[i]],max_col),c_ncol)    
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(train))
    }
}

# Resize each test image to max_row x max_col
for (i in 1:nrow(test)) {
    c_nrow <- nrow(images_test[[i]])
    c_ncol <- ncol(images_test[[i]])
    images_test[[i]] <- reduceCols(zoomCols(images_test[[i]],max_row),c_nrow)
    images_test[[i]] <- reduceRows(zoomRows(images_test[[i]],max_col),c_ncol)    
    if ( i %% 1000 == 0) {
        message("Completed: ", 100*i/nrow(test))
    }
}

# Build the new data frames
new_train <- data.frame(matrix(unlist(images_train), ncol=max_col*max_row))
new_train <- cbind(labels, new_train)
new_test  <- data.frame(matrix(unlist(images_test), ncol=max_col*max_row))

# Write the data frames
write.csv(new_train_1, file = paste(working_directory, "/csv/train_preprocesed.csv", sep=""), row.names = FALSE)
write.csv(new_test, file = paste(working_directory, "/csv/test_preprocesed.csv", sep=""), row.names = FALSE)

