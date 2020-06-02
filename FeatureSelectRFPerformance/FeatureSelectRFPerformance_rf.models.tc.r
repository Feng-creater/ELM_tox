# Install/Load required packages
library(dplyr)
library(caret)
library(randomForest)
library(stringr)

# Importing the feature selection data with Tanimoto coefficient value of 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99 and 1.0, respectively
load(file = 'E:/reproductive/models/feature_slelect-tc0.5.RData')# The feature selection data with Tanimoto coefficient value of 0.5

# Multi-core parallel computing
library(doMC)
registerDoMC(cores = 4)

# Random forest function
train.rf <- function (train.set, test.sets, fitControl){
    rf.Grid <-  expand.grid(mtry = c(floor(sqrt(ncol(train.set)-1))))
    print(rf.Grid)
    rf.Fit <- train(Toxicity ~ ., data = train.set, method = "rf", tuneGrid = rf.Grid, ntree = 300, importance = TRUE, trControl = fitControl, metric = "AUC", maximize = "TRUE", verbose = TRUE)
    cv.metric <- rf.Fit$results %>%
	             top_n(1, AUC)
    rf.pred.metrics <- data.frame()
    for (test.set in test.sets){
        rf.pred.raw <- predict(rf.Fit, newdata=test.set, type = "raw")
        rf.pred.prob <- predict(rf.Fit, newdata=test.set, type = "prob")
        rf.pred.metric <- myEvaluate(test.set$Toxicity, rf.pred.raw, rf.pred.prob[,1])
        rf.pred.metrics <- rbind(rf.pred.metrics, rf.pred.metric)
    }
    colnames(rf.pred.metrics) <- colnames(cv.metric)[2:((length(colnames(cv.metric))-1)/2+1)]
    #tune.plot <- ggplot(rf.Fit)
    #return(list(fit = rf.Fit, bestTune = rf.Fit$bestTune, cv = cv.metric, Ind = rf.pred.metrics, plot = tune.plot))
    return(list(fit = rf.Fit, bestTune = rf.Fit$bestTune, cv = cv.metric, Ind = rf.pred.metrics))
}

# Training control
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 100, summaryFunction = myTwoClassSummary, classProbs = TRUE, verboseIter = TRUE)



# Train random forest models with different feature sets
Fit.EStateFP.rf <- train.rf(Train_fp_all.EStateFP.sets$Train, list(Train_fp_all.EStateFP.sets$IndTest, Train_fp_all.EStateFP.sets$IndTest2), fitControl)
Fit.MACCSFP.rf <- train.rf(Train_fp_all.MACCSFP.sets$Train, list(Train_fp_all.MACCSFP.sets$IndTest, Train_fp_all.MACCSFP.sets$IndTest2), fitControl)
Fit.PubchemFP.rf <- train.rf(Train_fp_all.PubchemFP.sets$Train, list(Train_fp_all.PubchemFP.sets$IndTest, Train_fp_all.PubchemFP.sets$IndTest2), fitControl)
Fit.SubFP.rf <- train.rf(Train_fp_all.SubFP.sets$Train, list(Train_fp_all.SubFP.sets$IndTest, Train_fp_all.SubFP.sets$IndTest2), fitControl)
Fit.KRFP.rf <- train.rf(Train_fp_all.KRFP.sets$Train, list(Train_fp_all.KRFP.sets$IndTest, Train_fp_all.KRFP.sets$IndTest2), fitControl)
Fit.AD2D.rf <- train.rf(Train_fp_all.AD2D.sets$Train, list(Train_fp_all.AD2D.sets$IndTest, Train_fp_all.AD2D.sets$IndTest2), fitControl)
Fit.SubFPC.rf <- train.rf(Train_fp_all.SubFPC.sets$Train, list(Train_fp_all.SubFPC.sets$IndTest, Train_fp_all.SubFPC.sets$IndTest2), fitControl)
Fit.KRFPC.rf <- train.rf(Train_fp_all.KRFPC.sets$Train, list(Train_fp_all.KRFPC.sets$IndTest, Train_fp_all.KRFPC.sets$IndTest2), fitControl)
Fit.APC2D.rf <- train.rf(Train_fp_all.APC2D.sets$Train, list(Train_fp_all.APC2D.sets$IndTest, Train_fp_all.APC2D.sets$IndTest2), fitControl)

# Summarize the results
rf.cv.metric <- data.frame()
rf.ind1.metric <- data.frame()
rf.ind2.metric <- data.frame()
rf.model.names <- ls(pattern='Fit.*.rf') # List the names of all random forest models
for (model.name in rf.model.names){
    model <- eval(parse(text=model.name))
    rf.cv.metric <- rbind(rf.cv.metric, model$cv)
    rf.ind1.metric <- rbind(rf.ind1.metric, model$Ind[1,])
    rf.ind2.metric <- rbind(rf.ind2.metric, model$Ind[2,])
}
rownames(rf.cv.metric) <- rf.model.names
rownames(rf.ind1.metric) <- rf.model.names
rownames(rf.ind2.metric) <- rf.model.names
# Output performance indexes for all models
write.csv(rf.cv.metric, file='E:/reproductive/models/rf.cv-tc0.5.csv')
write.csv(rf.ind1.metric, file='E:/reproductive/models/rf.ind1-tc0.5.csv')
write.csv(rf.ind2.metric, file='E:/reproductive/models/rf.ind2-tc0.5.csv')
# Save all variables
save.image(file='E:/reproductive/models/rf.models-tc0.5.RData')

