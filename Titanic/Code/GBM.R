######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Titanic - Random Forest
#######################################################################

#--------------------------- GETTING THE DATA ----------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/Titanic"

train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'integer',   # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'factor',    # Cabin
                        'factor',    # Embarked
                        'character', # Title
                        'integer',   # FamilySize
                        'factor',    # Surname
                        'factor'     # FamilyID
)

test.column.types <- train.column.types[-2]

train.data <- read.csv(paste(working_directory, "/csv/train_preprocessed.csv", sep=""), colClasses=train.column.types, stringsAsFactors=FALSE)
test.data  <- read.csv(paste(working_directory, "/csv/test_preprocessed.csv", sep=""), colClasses=test.column.types, stringsAsFactors=FALSE)

#------------------------------ GBM -------------------------------#

library(gbm)
library(dplyr)

set.seed(400)

ntrees <- 100000
train.data$Survived <- as.numeric(train.data$Survived)-1

boost.fit <- gbm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked + FamilySize,
                    data=train.data, distribution = "adaboost", n.trees = ntrees, shrinkage = 0.00001, interaction.depth = 3, n.minobsinnode = 10,
                    train.fraction = 0.8, verbose = TRUE)

summary(boost.fit)
gbm.perf(boost.fit)

for (i in 1:length(boost.fit$var.names)){
    plot(boost.fit, i.var = i, ntrees = gbm.perf(boost.fit, plot.it = FALSE), type = "response")
}

boost.pred = predict(boost.fit, newdata = test.data, n.trees = gbm.perf(boost.fit, plot.it = FALSE), type="response")
survival.boost = ifelse(boost.pred > 0.5, 1, 0)
train.pred = predict(boost.fit, newdata = train.data, n.trees = gbm.perf(boost.fit, plot.it = FALSE), type="response")
train.survival = ifelse(train.pred > 0.5, 1, 0)

# Training Error
1 - sum(abs(train.data$Survived - train.survival)) / nrow(train.data)

submit <- data.frame(PassengerId = test.data$PassengerId, Survived = survival.boost)
write.csv(submit, file = paste(working_directory, "/R/Results/GBMAdaBoost100000.csv", sep=""), row.names = FALSE)
