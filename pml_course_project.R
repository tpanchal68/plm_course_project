# raw file name assign
raw_training_file <- "pml-training.csv"
raw_testing_file <- "pml-testing.csv"

# Download training file if it doesn't exist
if (!file.exists(raw_training_file)){
        # Download training file
        download.file(url="d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                      destfile=raw_training_file, method="curl")
}

# Download testing file if it doesn't exist
if (!file.exists(raw_testing_file)){
        # Download testing file
        download.file(url="d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                      destfile=raw_testing_file, method="curl")
}

training <- read.csv(raw_training_file, sep = ",", na.strings = c('NA','#DIV/0!',''))
print(dim(training))
train_colnames <- colnames(training)

testing <- read.csv(raw_testing_file, sep = ",", na.strings = c('NA','#DIV/0!',''))
print(dim(testing))
test_colnames <- colnames(testing)

# Validate if training and test data are identical.
if ((length(train_colnames) - length(test_colnames)) == 0){
        print("Same number of columns.")
        # Now let's find if train_colnames are same as test_colnames
        print(setdiff(train_colnames, test_colnames))
        # Now let's find the otherway if test_colnames are same as train_colnames
        print(setdiff(test_colnames, train_colnames))
}

# Use only complete columns thus remove any columns with NA in training and testing data set
training <- training[,complete.cases(t(training))]
testing <- testing[,complete.cases(t(testing))]

# Also, first 7 columns are not the feature data set so remove them
training <- training[,8:length(colnames(training))]
testing <- testing[,8:length(colnames(testing))]

print(colnames(training))
print(colnames(testing))

library(caret)
# set seed
set.seed(32343)

# Create data partition
inTrain <- createDataPartition(y=training$classe, time=1, p=0.75, list=FALSE)
trainData <- training[inTrain,]
validationData <- training[-inTrain,]

print(rbind("original dataset" = dim(training),"training set" = dim(trainData), "validation set" = dim(validationData)))

library(randomForest)

# process in parallel
library(doParallel)
registerDoSEQ()
# Detect the CPU cores
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
set.seed(1)

modelFit_RF <- randomForest(classe ~ ., data=trainData, method="rf")
# modelFit_RF <- train(classe ~ ., data = trainData, method = 'rf',
#                 trControl = trainControl(method = "cv", 
#                                          number = 4, 
#                                          allowParallel = TRUE, 
#                                          verboseIter = TRUE))
print(modelFit_RF)
# Plot Random Forest Model
plot(modelFit_RF)
varImpPlot(modelFit_RF)

pred_RF <- predict(modelFit_RF,newdata=validationData)
# logic value for whether or not the rf algorithm predicted correctly
validationData$predRight <- pred_RF==validationData$classe
# tabulate results
print(table(pred_RF, validationData$classe))

cm_RF <- confusionMatrix(pred_RF,validationData$classe)
print(cm_RF)
# Plot Confusion Matrix
plot(cm_RF$table, col = cm_RF$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cm_RF$overall['Accuracy'], 4)))

# Now apply the RF model to testing data and evaluate
test_result_RF <- predict(modelFit_RF, testing)
print(test_result_RF)
summary(test_result_RF)
plot(test_result_RF)

# library(gbm)
# # Model with GBM Algorithm
# # modelFit_GBM <- train(classe ~., data=trainData, method="gbm",verbose=FALSE)
# modelFit_GBM <- gbm(classe ~., data=trainData, verbose=FALSE)
# print(modelFit_GBM)

# pred_GBM <- predict(modelFit_GBM,newdata=validationData)
# # logic value for whether or not the rf algorithm predicted correctly
# validationData$predRight_GBM <- pred_GBM==validationData$classe
# # tabulate results
# table(pred_GBM, validationData$classe)
# 
# cm_GBM <- confusionMatrix(pred_GBM,validationData$classe)
# print(cm_GBM)
# 
# # Now apply the GBM model to testing data
# test_result_GBM <- predict(modelFit_GBM, testing)
# print(test_result_GBM)

