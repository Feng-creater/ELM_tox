# Install/Load required packages
library(dplyr)
library(caret)
library(randomForest)
library(stringr)

load(file = 'E:/reproductive/models/feature_slelect-tc0.95.RData')

# Fit models with multiple feature sets, accept a list of dataset names, and return a list of models trained using each dataset
fitMixModel <- function(train.index = 'all', train.Dataset.name.vector, fold.name = '', ...)
{
    model.list <- list()
    model.list$rf <- list()
    model.list$svm <- list()
    model.list$xgb <- list()

    i <- 1
    for (train.Dataset.name in train.Dataset.name.vector$rf){
        print(paste("Fit RF:", fold.name, train.Dataset.name))
        train.Dataset <- eval(parse(text=train.Dataset.name))
        train.Dataset <- train.Dataset$Train
        if (class(train.index) == 'integer'){
            train <- train.Dataset[train.index,]
        }else{
            train <- train.Dataset
        }
        rf.fit <- randomForest(Toxicity ~ ., data = train, ntree = 300) #Model fitting
        model.list$rf[[i]] <- rf.fit
        i <- i + 1
    }

    i <- 1
    for (train.Dataset.name in train.Dataset.name.vector$svm){
        print(paste("Fit SVM:", fold.name, train.Dataset.name))
        train.Dataset <- eval(parse(text=train.Dataset.name))
        train.Dataset <- train.Dataset$Train
        if (class(train.index) == 'integer'){
            train <- train.Dataset[train.index,]
        }else{
            train <- train.Dataset
        }
        svm.parm <- svm.parms[train.Dataset.name,]
        fitControl <- trainControl(method = "none", summaryFunction = myTwoClassSummary, classProbs = TRUE, verboseIter = TRUE)
        svm.fit <- train(Toxicity ~ ., data = train, method = "svmRadial", tuneGrid = svm.parm, trControl = fitControl, metric = "AUC", maximize = "TRUE", verbose = TRUE)
        model.list$svm[[i]] <- svm.fit
        i <- i + 1
    }
    i <- 1
    for (train.Dataset.name in train.Dataset.name.vector$xgb){
        print(paste("Fit XGB:", fold.name, train.Dataset.name))
        train.Dataset <- eval(parse(text=train.Dataset.name))
        train.Dataset <- train.Dataset$Train
        if (class(train.index) == 'integer'){
            train <- train.Dataset[train.index,]
        }else{
            train <- train.Dataset
        }
        xgb.parm <- xgb.parms[train.Dataset.name,]
        fitControl <- trainControl(method = "none", summaryFunction = myTwoClassSummary, classProbs = TRUE, verboseIter = TRUE)
        xgb.fit <- train(Toxicity ~ ., data = train, method = "xgbTree", tuneGrid = xgb.parm, trControl = fitControl, metric = "AUC", maximize = "TRUE", verbose = TRUE, nthread = 1)
        model.list$xgb[[i]] <- xgb.fit
        i <- i + 1
    }
    names(model.list$rf)  <- train.Dataset.name.vector$rf
    names(model.list$svm) <- train.Dataset.name.vector$svm
    names(model.list$xgb) <- train.Dataset.name.vector$xgb
    return(model.list)
}

#Evaluate all models in the model list and calculate their average
evaluateMixModel <- function(model.list, train.index, test.Dataset.name.vector, fold.name = '', dataset.type = 'Train')
{
    rf.fit.pred.prob.all  <- data.frame()
    svm.fit.pred.prob.all <- data.frame()
    xgb.fit.pred.prob.all <- data.frame()
    len_rf <- length(test.Dataset.name.vector$rf)
    len_svm <- length(test.Dataset.name.vector$svm)
    len_xgb <- length(test.Dataset.name.vector$xgb)
    if (len_rf != 0) {
        for (test.Dataset.name in test.Dataset.name.vector$rf){
            print(paste("Evaluate RF:", fold.name, test.Dataset.name))
            train.Dataset <- eval(parse(text=test.Dataset.name))
            if (dataset.type == 'Train'){
                test  <- train.Dataset$Train[-train.index,]
            }
            if (dataset.type == 'IndTest'){
                test <- train.Dataset$IndTest
            }
            if (dataset.type == 'IndTest2'){
                test <- train.Dataset$IndTest2
            }
            rf.fit <- eval(parse(text=paste0('model.list$rf$', test.Dataset.name)))
            rf.fit.pred.prob <- predict(rf.fit, newdata=test, type = "prob") #Probability prediction
            rf.fit.pred.prob.all <- rbind(rf.fit.pred.prob.all, rf.fit.pred.prob[,1]) #Combine the probability of positive examples into the probability data frame
            rownames(rf.fit.pred.prob.all)[nrow(rf.fit.pred.prob.all)] <- test.Dataset.name # Modify the newly added row name
        }
    }
    
    if (len_svm != 0) {
        for (test.Dataset.name in test.Dataset.name.vector$svm){
            print(paste("Evaluate SVM:", fold.name, test.Dataset.name))
            train.Dataset <- eval(parse(text=test.Dataset.name))
            if (dataset.type == 'Train'){
                test  <- train.Dataset$Train[-train.index,]
            }
            if (dataset.type == 'IndTest'){
                test <- train.Dataset$IndTest
            }
            if (dataset.type == 'IndTest2'){
                test <- train.Dataset$IndTest2
            }
            svm.fit <- eval(parse(text=paste0('model.list$svm$', test.Dataset.name)))
            svm.fit.pred.prob <- predict(svm.fit, newdata=test, type='prob') #Probability prediction
            svm.fit.pred.prob.all <- rbind(svm.fit.pred.prob.all, svm.fit.pred.prob[,1]) #Combine the probability of positive examples into the probability data frame
            rownames(svm.fit.pred.prob.all)[nrow(svm.fit.pred.prob.all)] <- test.Dataset.name # Modify the newly added row name
        }
    }
    
    if (len_xgb != 0) {
        for (test.Dataset.name in test.Dataset.name.vector$xgb){
            print(paste("Evaluate XGB:", fold.name, test.Dataset.name))
            train.Dataset <- eval(parse(text=test.Dataset.name))
            if (dataset.type == 'Train'){
                test  <- train.Dataset$Train[-train.index,]
            }
            if (dataset.type == 'IndTest'){
                test <- train.Dataset$IndTest
            }
            if (dataset.type == 'IndTest2'){
                test <- train.Dataset$IndTest2
            }
            xgb.fit <- eval(parse(text=paste0('model.list$xgb$', test.Dataset.name)))
            xgb.fit.pred.prob <- predict(xgb.fit, newdata=test, type='prob') #Probability prediction
            print(xgb.fit.pred.prob)
            xgb.fit.pred.prob.all <- rbind(xgb.fit.pred.prob.all, xgb.fit.pred.prob[,1]) #Combine the probability of positive examples into the probability data frame
            rownames(xgb.fit.pred.prob.all)[nrow(xgb.fit.pred.prob.all)] <- test.Dataset.name # Modify the newly added row name
        }
    }
    
    if (len_rf != 0) {
        print(length(test.Dataset.name.vector$rf))
        colnames(rf.fit.pred.prob.all) <- paste0('V', seq_len(ncol(rf.fit.pred.prob.all)))
    }
    if (len_svm != 0) {
        print(length(test.Dataset.name.vector$svm))
        colnames(svm.fit.pred.prob.all) <- paste0('V', seq_len(ncol(svm.fit.pred.prob.all)))
    }
    if (len_xgb != 0) {
        print(length(test.Dataset.name.vector$xgb))
        colnames(xgb.fit.pred.prob.all) <- paste0('V', seq_len(ncol(xgb.fit.pred.prob.all)))
    }
    fit.pred.prob.all <- rbind(rf.fit.pred.prob.all, svm.fit.pred.prob.all, xgb.fit.pred.prob.all)
    fit.pred.all.mean <- colMeans(fit.pred.prob.all) # Calculate the average value of the prediction probability of each model
    fit.pred.all.class <- ifelse(fit.pred.all.mean > 0.5, levels(test$Toxicity)[1], levels(test$Toxicity)[2])#Convert the probability of the model to classification
    fit.pred.all.class <- factor(fit.pred.all.class, levels = c(levels(test$Toxicity)[1], levels(test$Toxicity)[2])) # Forcibly modify the order of factors in fit.pred.all.class to make it consistent with the order of input data
    fit.pred.metric <- myEvaluate(test$Toxicity, fit.pred.all.class, fit.pred.all.mean) # Evaluate the predicted performance after calculating the average of all models

    return(fit.pred.metric)
}

#重复repeats次的number折交叉验证

svm.parms <- read.csv('E:/reproductive/models/svm.cv.csv', row.names=1) # Read the best parameters of a single model
svm.parms <- svm.parms[,1:2]
rownames(svm.parms) <- str_replace(str_replace(rownames(svm.parms), 'Fit', 'Train_fp_all'), 'svm', 'sets')

xgb.parms <- read.csv('E:/reproductive/models/xgb.cv.csv', row.names=1)
xgb.parms <- xgb.parms[,1:6]
rownames(xgb.parms) <- str_replace(str_replace(rownames(xgb.parms), 'Fit', 'Train_fp_all'), 'xgb', 'sets')
xgb.parms <- cbind(xgb.parms, subsample = 1)

repeats = 100
k = 5
folds <- createMultiFolds(y=Train_fp_all.KRFPC.sets$Train[,1], k = k, times = repeats)
fold.names <- names(folds)
train.Dataset.name.vector <- list()
train.Dataset.name.vector$rf <- c('Train_fp_all.PubchemFP.sets')
train.Dataset.name.vector$svm <- c('Train_fp_all.PubchemFP.sets')
train.Dataset.name.vector$xgb <- c('Train_fp_all.PubchemFP.sets', 'Train_fp_all.MACCSFP.sets')
library(doMC)
registerDoMC(cores = 16) #  Setting the number of CPU cores running in parallel
mix.model.results <- foreach(i = 1:length(folds), .combine=rbind) %dopar% {
    fold = folds[[i]]
    print(fold.names[i])
    MixModel <- fitMixModel(train.index = fold, train.Dataset.name.vector = train.Dataset.name.vector, fold.name = fold.names[i])
    print(MixModel)
    MixModel.metric <- evaluateMixModel(MixModel, train.index = fold, test.Dataset.name.vector = train.Dataset.name.vector, fold.name = fold.names[i])
    print(MixModel.metric)
}
# Summarize the results of cross-validation
mix.model.cv.metric <- as.data.frame(mix.model.results) %>% summarise_all(funs(mean,sd))

#Fit all the data in the trainset, and then use two independent test sets to detect
finalMixModel <- fitMixModel(train.Dataset.name.vector = train.Dataset.name.vector)
finalMixModel.metric.Ind  <- evaluateMixModel(finalMixModel, test.Dataset.name.vector = train.Dataset.name.vector, dataset.type = 'IndTest')
finalMixModel.metric.Ind2  <- evaluateMixModel(finalMixModel, test.Dataset.name.vector = train.Dataset.name.vector, dataset.type = 'IndTest2')


# Output performance indexes for all models
write.csv(mix.model.cv.metric, file='E:/reproductive/models/rf.svm.xgb.mix.top4.cv.csv')
write.csv(t(finalMixModel.metric.Ind), file='E:/reproductive/models/rf.svm.xgb.mix.top4.Ind.csv')
write.csv(t(finalMixModel.metric.Ind2), file='E:/reproductive/models/rf.svm.xgb.mix.top4.Ind2.csv')
# Save all the variables
save.image(file='E:/reproductive/models/rf.svm.xgb.mix.top4.RData')

print("All finished")

