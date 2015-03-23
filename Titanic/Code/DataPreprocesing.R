######################################################################
# Author: Andrés Herrera Poyatos
# Date:   March, 2015
# Kaggle Titanic - Data Preprocessing
#######################################################################

#--------------------------- GETTING THE DATA ----------------------------#

working_directory <- "/home/andreshp/ComputerScience/MachineLearning/Kaggle/Titanic"

train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]

train.data <- read.csv(paste(working_directory, "/csv/train.csv", sep=""), colClasses=train.column.types, stringsAsFactors=FALSE)
test.data  <- read.csv(paste(working_directory, "/csv/test.csv", sep=""), colClasses=test.column.types, stringsAsFactors=FALSE)

#--------------------------- NEW VARIABLES ----------------------------#

test.data$Survived <- NA

# New data frame to which we will apply the changes
data <- rbind(train.data, test.data)

# New variable Title. It contains the passangers' title.
# For example, Braund, Mr. Owen Harris' tittle is Mr
data$Title <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
data$Title <- sub(' ', '', data$Title)
data$Title[data$Title %in% c('Mlle', 'Dona', 'the Countess', 'Ms')] <- 'Mrs' # They are the same
data$Title[data$Title %in% c('Mme')] <- 'Miss' # They are the same
data$Title[data$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer', 'Lady')] <- 'Noble'
data$Title <- factor(data$Title)

# New variable Family Size. It indicates the size of the
# passanger's family.
data$FamilySize <- data$SibSp + data$Parch + 1

# New variable Surname. The surname of the passangers.
# For example, Braund, Mr. Owen Harris' surname is Braund.
data$Surname <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# New variable FamilyID. If the family has more than 1 member
# their ID is theirs surname. Otherwise it is Single.
data$FamilyID <- data$Surname
data$FamilyID[data$FamilySize <= 1] <- 'Single'
data$FamilyID <- factor(data$FamilyID)

#--------------------------- MISSING VALUES ----------------------------#

# Embarked.Put to S those with "". (Both are in cabin B28 and lived)
no_embarked = which(data$Embarked == '')
data$Embarked[no_embarked] = "S"
data$Embarked <- factor(data$Embarked)

# Fare. Put the mean of the fare paid by males in Pclass 3
# with FamilySize 1 and Embarked.
no_fare = which(is.na(data$Fare))
data$Fare[no_fare] <- mean(data$Fare[data$Embarked == 'S' & data$Sex == 'male' & data$Pclass == 3], na.rm=TRUE)

# Predict the age for those with NA:
library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + FamilyID + FamilySize, data=data[!is.na(data$Age),], method="anova")
data$Age[is.na(data$Age)] <- predict(Agefit, data[is.na(data$Age),])

# Convert variables
data$Cabin <- as.factor(data$Cabin)
data$Pclass <- as.integer(data$Pclass)
data$FamilySize <- as.integer(data$FamilySize)
data$FamilySize <- as.integer(data$FamilySize)


# Get again train and test data:
train.data <- data[1:nrow(train.data),]
test.data <- data[892:1309,]
test.data <- test.data[-2]

# Write the data frames
write.csv(train.data, file = paste(working_directory, "/csv/train_preprocessed.csv", sep=""), row.names = FALSE)
write.csv(test.data, file = paste(working_directory, "/csv/test_preprocessed.csv", sep=""), row.names = FALSE)
