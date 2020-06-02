# Install/Load required packages
library(dplyr)
library(caret)
library(randomForest)
library(stringr)


load(file = 'E:/reproductive/models/feature_slelect-tc0.95.RData')



# Multi-core parallel computing
library(doMC)
registerDoMC(cores = 16)

# Support vector machine function
train.svm <- function (train.set, test.sets, fitControl){
    fitControl.tune <- trainControl(method = "repeatedcv", number = 5, repeats = 100, search='random', summaryFunction = myTwoClassSummary, classProbs = TRUE, verboseIter = TRUE)
    svm.tune <- train(Toxicity ~ ., data = train.set, method = "svmRadial", tuneLength = 300, preProc = c("center", "scale"), trControl = fitControl.tune, metric = "AUC", maximize = "TRUE", verbose = TRUE)
    # tune.plot <- ggplot(svm.tune)
    # ggsave(filename = 'tune.svm.pdf', plot = tune.plot)
    svm.Grid <- svm.tune$bestTune
    svm.Fit  <- train(Toxicity ~ ., data = train.set, method = "svmRadial", tuneGrid = svm.Grid, preProc = c("center", "scale"), trControl = fitControl, metric = "AUC", maximize = "TRUE", verbose = TRUE)

    cv.metric <- svm.Fit$results %>%
	             top_n(1, AUC)
    svm.pred.metrics <- data.frame()
    for (test.set in test.sets){
        svm.pred.raw <- predict(svm.Fit, newdata=test.set, type = "raw")
        svm.pred.prob <- predict(svm.Fit, newdata=test.set, type = "prob")
        svm.pred.metric <- myEvaluate(test.set$Toxicity, svm.pred.raw, svm.pred.prob[,1])
        svm.pred.metrics <- rbind(svm.pred.metrics, svm.pred.metric)
    }
    colnames(svm.pred.metrics) <- colnames(cv.metric)[3:((length(colnames(cv.metric))-2)/2+2)]
    #tune.plot <- ggplot(rf.Fit)
    #return(list(fit = rf.Fit, bestTune = rf.Fit$bestTune, cv = cv.metric, Ind = rf.pred.metrics, plot = tune.plot))
    return(list(fit = svm.Fit, bestTune = svm.Fit$bestTune, cv = cv.metric, Ind = svm.pred.metrics))
}

# Training control
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 100, search='grid', summaryFunction = myTwoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Train support vector machine models with different feature sets
Fit.EStateFP.svm <- train.svm(Train_fp_all.EStateFP.sets$Train, list(Train_fp_all.EStateFP.sets$IndTest, Train_fp_all.EStateFP.sets$IndTest2), fitControl)
Fit.MACCSFP.svm <- train.svm(Train_fp_all.MACCSFP.sets$Train, list(Train_fp_all.MACCSFP.sets$IndTest, Train_fp_all.MACCSFP.sets$IndTest2), fitControl)
Fit.PubchemFP.svm <- train.svm(Train_fp_all.PubchemFP.sets$Train, list(Train_fp_all.PubchemFP.sets$IndTest, Train_fp_all.PubchemFP.sets$IndTest2), fitControl)
Fit.SubFP.svm <- train.svm(Train_fp_all.SubFP.sets$Train, list(Train_fp_all.SubFP.sets$IndTest, Train_fp_all.SubFP.sets$IndTest2), fitControl)
Fit.KRFP.svm <- train.svm(Train_fp_all.KRFP.sets$Train, list(Train_fp_all.KRFP.sets$IndTest, Train_fp_all.KRFP.sets$IndTest2), fitControl)
Fit.AD2D.svm <- train.svm(Train_fp_all.AD2D.sets$Train, list(Train_fp_all.AD2D.sets$IndTest, Train_fp_all.AD2D.sets$IndTest2), fitControl)
Fit.SubFPC.svm <- train.svm(Train_fp_all.SubFPC.sets$Train, list(Train_fp_all.SubFPC.sets$IndTest, Train_fp_all.SubFPC.sets$IndTest2), fitControl)
Fit.KRFPC.svm <- train.svm(Train_fp_all.KRFPC.sets$Train, list(Train_fp_all.KRFPC.sets$IndTest, Train_fp_all.KRFPC.sets$IndTest2), fitControl)
Fit.APC2D.svm <- train.svm(Train_fp_all.APC2D.sets$Train, list(Train_fp_all.APC2D.sets$IndTest, Train_fp_all.APC2D.sets$IndTest2), fitControl)


# Summarize the results
svm.cv.metric <- data.frame()
svm.ind1.metric <- data.frame()
svm.ind2.metric <- data.frame()
svm.model.names <- ls(pattern='Fit.*.svm') # List the names of all support vector machine models
for (model.name in svm.model.names){
    model <- eval(parse(text=model.name))
    svm.cv.metric <- rbind(svm.cv.metric, model$cv)
    svm.ind1.metric <- rbind(svm.ind1.metric, model$Ind[1,])
    svm.ind2.metric <- rbind(svm.ind2.metric, model$Ind[2,])
}
rownames(svm.cv.metric) <- svm.model.names
rownames(svm.ind1.metric) <- svm.model.names
rownames(svm.ind2.metric) <- svm.model.names
# Output performance indexes for all models
write.csv(svm.cv.metric, file='E:/reproductive/models/svm.cv.csv')
write.csv(svm.ind1.metric, file='E:/reproductive/models/svm.ind1.csv')
write.csv(svm.ind2.metric, file='E:/reproductive/models/svm.ind2.csv')
# Save all variables
save.image(file='E:/reproductive/models/svm.models.RData')

print("All finished")


