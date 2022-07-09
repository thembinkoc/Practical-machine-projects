# Practical-machine-projects
#Loading Data
pml.testing <- read.csv("C:/Users/Thembinkosi/Videos/pml-testing.csv", header=FALSE)
View(pml.testing)
pml.training <- read.csv("C:/Users/Thembinkosi/Videos/pml-training.csv", header=FALSE)
View(pml.training)
#Estimating the out-of-sample error by splitting the full training data (ptrain) into a smaller training set (ptrain1) and a validation set (ptrain2):
set.seed(10)
inTrain <- createDataPartition(y=ptrain$classe, p=0.7, list=F)
ptrain1 <- ptrain[inTrain, ]
ptrain2 <- ptrain[-inTrain, ]
# bring variables with nearly zero variance
nzv <- nearZeroVar(ptrain1)
ptrain1 <- ptrain1[, -nzv]
ptrain2 <- ptrain2[, -nzv]
# elemenate variables that are almost always NA
mostlyNA <- sapply(ptrain1, function(x) mean(is.na(x))) > 0.95
ptrain1 <- ptrain1[, mostlyNA==F]
ptrain2 <- ptrain2[, mostlyNA==F]
# Cut variables that don't make untaught sense for prediction (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp), which happen to be the first five variables
ptrain1 <- ptrain1[, -(1:5)]
ptrain2 <- ptrain2[, -(1:5)]
# instruct train to use 3-fold CV to select optimal tuning parameters
fitControl <- trainControl(method="cv", number=3, verboseIter=F)

# fit model on ptrain1
fit <- train(classe ~ ., data=ptrain1, method="rf", trControl=fitControl)
# print final model to see tuning parameters it chose
fit$finalModel
# utilize model to predict classe in validation set 
preds <- predict(fit, newdata=ptrain2)

# show dubiety matrix to get estimate of out-of-sample error
confusionMatrix(ptrain2$classe, preds)
# remove variables with nearly zero variance
nzv <- nearZeroVar(ptrain)
ptrain <- ptrain[, -nzv]
ptest <- ptest[, -nzv]

# remove variables that are almost always NA
mostlyNA <- sapply(ptrain, function(x) mean(is.na(x))) > 0.95
ptrain <- ptrain[, mostlyNA==F]
ptest <- ptest[, mostlyNA==F]

# remove variables that don't make innate sense for prediction (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp), which happen to be the first five variables
ptrain <- ptrain[, -(1:5)]
ptest <- ptest[, -(1:5)]

# re-equip model using full training set (ptrain)
fitControl <- trainControl(method="cv", number=3, verboseIter=F)
fit <- train(classe ~ ., data=ptrain, method="rf", trControl=fitControl)
# predict on test set
preds <- predict(fit, newdata=ptest)

# recast predictions to nature vector
preds <- as.character(preds)

# generate function to write predictions to files
pml_write_files <- function(x) {
    n <- length(x)
    for(i in 1:n) {
        filename <- paste0("problem_id_", i, ".txt")
        write.table(x[i], file=filename, quote=F, row.names=F, col.names=F)
    }
}

# create prediction files to submit
pml_write_files(preds)
