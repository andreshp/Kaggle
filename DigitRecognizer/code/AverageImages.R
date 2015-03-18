######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Average the images
#######################################################################


#----------------------------- GET THE DATA ------------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/train_preprocesed.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test_preprocesed.csv", sep=""),  header=TRUE)

labels <- as.numeric(train[,1])
train <- train[,-1]
nrow_image <- 20 # Images' pixels per row

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


#----------------------------- GET THE IMAGE ------------------------------#

library(png)

# Find the mean of each class of numbers:
images <- list()
zoom_ratio <- 4
for(x in 0:9){
    average <- colMeans(train[labels == x,])
    im <- t(matrix(1 - average/256, nrow = nrow_image))
    im <- zoomCols(im, zoom_ratio)
    im <- zoomRows(im, zoom_ratio)
    images[[as.character(x)]] <- im
}

# Do a matrix with the 10 numbers
row <- matrix(nrow=nrow_image*zoom_ratio)
for(i in 0:9){
    row <- cbind(row, images[[as.character(i)]])
}
row <- row[,-1] # kills the NA row

#--------------------------- WRITE THE IMAGE ---------------------------#

writePNG(row, paste(working_directory, "/averages.png", sep=""))
