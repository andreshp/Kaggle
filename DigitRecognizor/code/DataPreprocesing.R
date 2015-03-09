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
train <- data.frame(lapply(1:ncol(train), function(j) as.numeric(train[,j])))
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
    blanc_rows <- sapply(1:nrow(im), function(i) mean(im[i,]) > ratio)    
    im[blanc_rows,]    
}

# deleteBlancCols: Delete the columns with mean less than the given ratio. 
# Parameters:
#    - im     : Image to which apply the function.
#    - ratio  : value that must exceed a column in order to be kept.
# Return:
#    The image whitout colums with mean less than ratio.
deleteBlancCols <- function(im, ratio){
    blanc_cols <- sapply(1:ncol(image), function(j) mean(image[,j]) > 0)    
    im[,blanc_cols]    
}

#------------------- APPLICATION TO THE DATASET  -----------------------#

images <- c()
for (i in 1:nrow(train)) {
    image <- matrix(train[1,], nrow = nrow_image)
    message(image)
    image <- deleteBlancCols(image, 0)
    image <- deleteBlancRows(image, 0)
    images <- rbind(images, c(image))
    if ( i %% 10 == 0) {
        message("Completed:")
        break
    }
}