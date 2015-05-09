######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Digit Recognizer - Deep Learning
#######################################################################

#-------------------------- GETTING DATA ---------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/DigitRecognizer/R"
train <- read.csv(paste(working_directory, "/csv/train_total.csv", sep=""), header=TRUE)
test <-  read.csv(paste(working_directory, "/csv/test_total.csv", sep=""),  header=TRUE)

#----------------------- STARTING H2O LIBRARY -----------------------#

# Library h2o for deep learning
library(h2o)

# Start a local cluster with 4GB RAM
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '4g')

# Convert the data to the h2o format:
train_h2o <- as.h2o(localH2O, train)
test_h2o  <- as.h2o(localH2O, test)


#----------------------- TRAINING THE MODEL -----------------------#

# Model 1: (About 79.5% in Kaggle :( )
model <- h2o.deeplearning( x = 2:ncol(train),                              # column numbers for predictors
                           y = 1,                                  # column number for label
                           data = train_h2o,                       # data in H2O format
                           activation = "TanhWithDropout",         # Tanh With Dropout
                           input_dropout_ratio = 0.2,              # % of inputs dropout
                           hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                           balance_classes = TRUE, 
                           hidden = c(50,50,50),                   # three layers of 50 nodes
                           epochs = 100)                           # max. no. of epochs

# Model 2: 
model <- h2o.deeplearning( x = 2:ncol(train),                              # column numbers for predictors
                           y = 1,                                  # column number for label
                           data = train_h2o,                       # data in H2O format
                           activation = "Tanh",                    # Tanh
                           balance_classes = TRUE, 
                           hidden=c(500,500,1000),
                           epochs = 20,                            # max. no. of epochs
                           rate=0.01,                              # rate
                           rate_annealing = 0.001)                 # annealing rate

# Model 3 : (0.98229 in Kaggle), (+ Data Preprocesing = 0.98714 in Kaggle)
set.seed(28)
model <- h2o.deeplearning( x = 2:ncol(train),                      # column numbers for predictors
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

# Model 4 : (World record)
model <- h2o.deeplearning(x = 2:ncol(train), y = 1, data = train_h2o,
                          activation = "RectifierWithDropout", hidden = c(1024,1024,2048),
                          epochs = 200, l1 = 1e-5, input_dropout_ratio = 0.2,
                          train_samples_per_iteration = -1, classification_stop = 0)

# Use the DNN model for predictions
h2o_test_results <- h2o.predict(model, test_h2o)

# Convert H2O format into data frame
df_test_results <- as.data.frame(h2o_test_results)

# Build the results
write.csv(data.frame(ImageId=1:nrow(df_test_results), Label=df_test_results$predict), file = paste(working_directory, "/Results/DeepLearning_total_4_200.csv", sep=""), row.names = FALSE)
h2o.saveModel(model, dir=paste(working_directory, "/Models/", sep=""), name="deep_learning_4_total_200", save_cv = TRUE, force=FALSE)