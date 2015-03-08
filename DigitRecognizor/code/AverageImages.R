######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Average the images
#######################################################################


#----------------------------- GET THE DATA ------------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/new_train.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test.csv", sep=""),  header=TRUE)

labels <- as.numeric(train[,1])
train <- train[,-1]
nrow_image <- 28 # Images' pixels per row

#----------------------------- GET THE IMAGE ------------------------------#

library(png)

# A function to increase the size of the image per the zoom factor r
bigify <- function(im, r){
    result <- matrix(nrow=nrow(im)*r, ncol=ncol(im)*r)
    for(i in 1:nrow(im)){
        for(j in 1:ncol(im)){
            for(k in 1:r){
                for(l in 1:r){
                    result[(i - 1) * r + (k - 1) + 1, (j - 1) * r + (l - 1) + 1] <- im[i, j]
                }
            }
        }
    }
    return(result)
}

# Find the mean of each class of numbers:
images <- list()
zoom_ratio <- 4
for(x in 0:9){
    average <- colMeans(train[labels == x,])
    im <- t(matrix(1 - average/256, nrow = nrow_image))
    im <- bigify(im, zoom_ratio)
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
