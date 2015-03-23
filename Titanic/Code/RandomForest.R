######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Titanic - Random Forest
#######################################################################

#--------------------------- GETTING THE DATA ----------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/Titanic"

train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'character',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'factor',    # Cabin
                        'factor',    # Embarked
                        'factor',    # Title
                        'numeric',   # FamilySize
                        'factor',    # Surname
                        'factor'     # FamilyID
)
test.column.types <- train.column.types[-2]

train.data <- read.csv(paste(working_directory, "/csv/train_preprocessed.csv", sep=""), colClasses=train.column.types, stringsAsFactors=FALSE)
test.data  <- read.csv(paste(working_directory, "/csv/test_preprocessed.csv", sep=""), colClasses=test.column.types, stringsAsFactors=FALSE)

#--------------------------- PARTY RANDOM FOREST ----------------------------#

# Party Random Forest:
library(party)
set.seed(400)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + FamilyID, data = train.data, controls=cforest_unbiased(ntree=10000, mtry=3))

Prediction <- predict(fit, test.data, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = paste(working_directory, "/R/RandomForestParty_new8.csv", sep=""), row.names = FALSE)
