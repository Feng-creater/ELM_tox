library(dplyr)
library(caret)
library(randomForest)
library(stringr)
library(tidyverse)
library(plotROC)

# Loading the final selected integration model file
load(file = '/home/zl/zl/reproductive/models/feature_slelect-tc0.95.RData')

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
        rf.fit <- randomForest(Toxicity ~ ., data = train, ntree = 300) #拟合模型
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

# Evaluate all models in the model list and calculate their average
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
            rf.fit.pred.prob.all <- rbind(rf.fit.pred.prob.all, rf.fit.pred.prob[,1]) # Combine the probability of positive examples into the probability data frame
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
            svm.fit.pred.prob.all <- rbind(svm.fit.pred.prob.all, svm.fit.pred.prob[,1]) # Combine the probability of positive examples into the probability data frame
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
            xgb.fit.pred.prob.all <- rbind(xgb.fit.pred.prob.all, xgb.fit.pred.prob[,1]) # Combine the probability of positive examples into the probability data frame
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
    fit.pred.all.class <- ifelse(fit.pred.all.mean > 0.5, levels(test$Toxicity)[1], levels(test$Toxicity)[2])# Convert the probability of the model to classification
    fit.pred.all.class <- factor(fit.pred.all.class, levels = c(levels(test$Toxicity)[1], levels(test$Toxicity)[2])) # Forcibly modify the order of factors in fit.pred.all.class to make it consistent with the order of input data
    fit.pred.metric <- myEvaluate(test$Toxicity, fit.pred.all.class, fit.pred.all.mean) # Evaluate the predicted performance after calculating the average of all models

    return(fit.pred.metric)
}


#使用模型列表中的所有模型预测数据集
predictMixModel <- function(model.list, train.index, test.Dataset.name.vector, fold.name = '', dataset.type = 'Train')
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
            rf.fit.pred.prob.all <- rbind(rf.fit.pred.prob.all, rf.fit.pred.prob[,1]) # Combine the probability of positive examples into the probability data frame
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
            svm.fit.pred.prob.all <- rbind(svm.fit.pred.prob.all, svm.fit.pred.prob[,1]) # Combine the probability of positive examples into the probability data frame
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
            xgb.fit.pred.prob.all <- rbind(xgb.fit.pred.prob.all, xgb.fit.pred.prob[,1]) # Combine the probability of positive examples into the probability data frame
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
    fit.pred.all.class <- ifelse(fit.pred.all.mean > 0.5, levels(test$Toxicity)[1], levels(test$Toxicity)[2])# Convert the probability of the model to classification
    fit.pred.all.class <- factor(fit.pred.all.class, levels = c(levels(test$Toxicity)[1], levels(test$Toxicity)[2])) # Forcibly modify the order of factors in fit.pred.all.class to make it consistent with the order of input data
    pred.dataframe <- data.frame(Toxicity = test$Toxicity, Pred = fit.pred.all.class, Prob = fit.pred.all.mean) # Evaluate the predicted performance after calculating the average of all models

    return(pred.dataframe)
}


#Repeat the 5-fold cross-validation 100 times

svm.parms <- read.csv('/home/zl/zl/reproductive/models/svm.cv.csv', row.names=1) # Read the best parameters of a single model
svm.parms <- svm.parms[,1:2]
rownames(svm.parms) <- str_replace(str_replace(rownames(svm.parms), 'Fit', 'Train_fp_all'), 'svm', 'sets')

xgb.parms <- read.csv('/home/zl/zl/reproductive/models/xgb.cv.csv', row.names=1)
xgb.parms <- xgb.parms[,1:6]
rownames(xgb.parms) <- str_replace(str_replace(rownames(xgb.parms), 'Fit', 'Train_fp_all'), 'xgb', 'sets')
xgb.parms <- cbind(xgb.parms, subsample = 1)

repeats = 100
k = 5
folds <- createMultiFolds(y=Train_fp_all.KRFPC.sets$Train[,1], k = k, times = repeats)
fold.names <- names(folds)
train.Dataset.name.vector <- list()
train.Dataset.name.vector$rf <- c('Train_fp_all.PubchemFP.sets', 'Train_fp_all.MACCSFP.sets', 'Train_fp_all.SubFPC.sets', 'Train_fp_all.KRFPC.sets')
train.Dataset.name.vector$svm <- c('Train_fp_all.PubchemFP.sets', 'Train_fp_all.MACCSFP.sets')
train.Dataset.name.vector$xgb <- c('Train_fp_all.PubchemFP.sets', 'Train_fp_all.MACCSFP.sets', 'Train_fp_all.SubFPC.sets', 'Train_fp_all.APC2D.sets', 'Train_fp_all.KRFPC.sets', 'Train_fp_all.KRFP.sets')
# train.Dataset.name.vector$rf <- c('Train_fp_all.PubchemFP.sets')
# train.Dataset.name.vector$svm <- c()
# train.Dataset.name.vector$xgb <- c()
library(doMC)
registerDoMC(cores = 16) # Set the number of CPU cores running in parallel
CV.Pred_all <- foreach(i = 1:length(folds), .combine=rbind) %dopar% {
    fold = folds[[i]]
    print(fold.names[i])
    MixModel <- fitMixModel(train.index = fold, train.Dataset.name.vector = train.Dataset.name.vector, fold.name = fold.names[i])
    print(MixModel)
    MixModel.metric <- evaluateMixModel(MixModel, train.index = fold, test.Dataset.name.vector = train.Dataset.name.vector, fold.name = fold.names[i])
    print(MixModel.metric)
    MixModel.pred <- predictMixModel(MixModel, train.index = fold, test.Dataset.name.vector = train.Dataset.name.vector, fold.name = fold.names[i])
    print(MixModel.pred)
    MixModel.pred$ModelName <- fold.names[i]
    MixModel.pred
}





#roc plot

CV.Pred_all$ModelName <- factor(CV.Pred_all$ModelName)

rocplot_CV <- ggplot(data = CV.Pred_all, aes()) +
                    geom_roc(aes(d = Toxicity, m = Prob, color = ModelName), labels = FALSE, n.cuts = 0, size = 0.3, linealpha = 0.5) +
                    geom_roc(aes(d = Toxicity, m = Prob), color = "blue", labels = FALSE, n.cuts = 0) +
                    annotate("text", x = 0.3, y = 0.7, label = "AUC=0.937") +
                    labs(title = "Cross-validation (Ensemble-Top12)") +
                    style_roc() +
                    scale_color_manual(values = rep("deepskyblue", length(unique(CV.Pred_all$ModelName)))) +
                    scale_x_continuous("1 - Specificity", expand = c(0.03, 0), breaks = seq(0, 1, by = .1)) +
                    scale_y_continuous("Sensitivity", expand = c(0.03, 0), breaks = seq(0, 1, by = .1)) +
                    theme(legend.position = "none") +
                    theme(panel.border = element_rect(size=0.5, color = 'black')) +
                    theme(panel.grid.minor = element_blank()) +
                    theme(plot.title = element_text(hjust = 0.5))


load(file = '/home/zl/zl/reproductive/models/rf.svm.xgb.mix.top12.RData')

Ind.Pred  <- predictMixModel(finalMixModel, test.Dataset.name.vector = train.Dataset.name.vector, dataset.type = 'IndTest')
Ind.Pred$ModelName <- "Ensemble-Top12"

rocplot_Ind <- ggplot(Ind.Pred, aes(d = Toxicity, m = Prob)) +
                    geom_roc(color = "blue", labels = FALSE, n.cuts = 0) +
                    annotate("text", x = 0.3, y = 0.7, label = "AUC=0.920") +
                    labs(title = "External validation (Ensemble-Top12)") +
                    style_roc() +
                    scale_x_continuous("1 - Specificity", expand = c(0.03, 0), breaks = seq(0, 1, by = .1)) +
                    scale_y_continuous("Sensitivity", expand = c(0.03, 0), breaks = seq(0, 1, by = .1)) +
                    theme(legend.title = element_blank(), legend.key = element_rect(colour = 'white')) +
                    theme(legend.position = c(1.01,0), legend.justification = c(1,0)) +
                    theme(legend.text = element_text(size = 9.5), legend.background = element_blank(), legend.key=element_blank()) +
                    theme(panel.border = element_rect(size=0.5, color = 'black')) +
                    theme(panel.grid.minor = element_blank()) +
                    theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = 'rocplot_CV.pdf', plot = rocplot_CV, width = 4.5, height = 4.5)
ggsave(filename = 'rocplot_Ind.pdf', plot = rocplot_Ind, width = 4.5, height = 4.5)



library(cowplot)

combine_plot <- plot_grid(rocplot_CV, rocplot_Ind, labels = c('(A)', '(B)'), label_size = 14)

ggsave(filename = 'combine_rocplot.pdf', plot = combine_plot, width = 9, height = 4.5)

# Save all the variables
save.image(file='/home/zl/zl/reproductive/models/rf.svm.xgb.mix.top12.rocplot.RData')

print("All finished")

