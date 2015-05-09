######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Deep Learning Study
#######################################################################

#--------------------------- GETTING THE DATA ----------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train.data <- read.csv(paste(working_directory, "/csv/train_total.csv", sep=""), header=TRUE)

labels <- train.data[,1]

#----------------------- STARTING H2O LIBRARY -----------------------#

# Library h2o for deep learning
library(h2o)

# Start a local cluster with 4GB RAM
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '4g')

#----------------------- BUILD THE DEEP LEARNING PREDICTION -----------------------#

set.seed(28)

# Get a partition of the data
n <- 10
train.sector <- sample(1:n, nrow(train.data), replace = TRUE)
list <- 1:n

trainingset <- subset(train.data, train.sector %in% list[-1])
trainlabels <- subset(labels, train.sector %in% list[-1])
testset <- subset(train.data, train.sector %in% c(1))[,-1]
testlabels <- subset(labels, train.sector %in% c(1))

# Convert the data to the h2o format
train_h2o <- as.h2o(localH2O, trainingset)
test_h2o  <- as.h2o(localH2O, testset)


# Model
model <- h2o.deeplearning( x = 2:ncol(trainingset),                # column numbers for predictors
                           y = 1,                                  # column number for label
                           data = train_h2o,                       # data in H2O format
                           activation = "RectifierWithDropout",    # Rectifier With Dropout
                           balance_classes = TRUE,
                           hidden=c(1024,1024,2048),
                           epochs = 200,                           # max. no. of epochs
                           adaptive_rate = FALSE,
                           rate=0.01,                              # rate
                           rate_annealing = 1.0e-6,                # annealing rate
                           rate_decay = 1.0,
                           momentum_start = 0.5, 
                           momentum_ramp = 42000*12, 
                           momentum_stable = 0.99, 
                           input_dropout_ratio = 0.2,
                           l1 = 1.0e-5, l2 = 0.0,
                           max_w2 = 15.0, 
                           initial_weight_distribution = "Normal",
                           initial_weight_scale = 0.01,
                           nesterov_accelerated_gradient = T, 
                           loss = "CrossEntropy", 
                           fast_mode = T, 
                           diagnostics = T, 
                           ignore_const_cols = T,
                           force_load_balance = T)

# Use the DNN model for predictions
h2o_results <- h2o.predict(model, test_h2o)
# Convert H2O format into data frame
results <- as.data.frame(h2o_results)

#----------------------- STUDY RESULTS -----------------------#

# Results
message("Success Percent:")
print(sum(results$predict == testlabels) / length(results$predict))
message("Success Percent per Number:")
a <- table(testlabels)
b <- table(testlabels[results$predict == testlabels])
print(b / a * 100)

