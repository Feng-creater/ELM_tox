# Install/Load required packages
library(dplyr)
library(caret)
library(randomForest)
library(stringr)

load(file = 'E:/reproductive/models/feature_slelect-tc0.95.RData')


################################################################################################
########################################Start the XGB parameter optimization process###################################

# Parameter tuning function, adjust 1 parameter
tune.parm <- function(fitControl, tune.parms, eta, gamma_p, min_child_weight, max_depth, nrounds, colsample_bytree, test.Set, train.set) {
    xgbTree.Grid <- expand.grid(eta = eta,
                                 gamma = gamma_p,
                                 min_child_weight = min_child_weight,
                                 max_depth = max_depth,
                                 nrounds = nrounds,
								 subsample = 1,
                                 colsample_bytree = colsample_bytree)

    xgbTree.Fit <- train(Toxicity ~ ., data = train.set,
	                              method = "xgbTree",
								  tuneGrid = xgbTree.Grid,
								  trControl = fitControl,
								  metric = "AUC",
								  maximize = "TRUE",
								  verbose = TRUE,
								  nthread = 1)
    print(xgbTree.Fit)
    cv.metric <- xgbTree.Fit$results %>%
	             top_n(1, desc(AUC))

    xgbTree.pred.test <- predict(xgbTree.Fit, newdata=test.Set)
    xgbTree.pred.test.metric <- myEvaluate(xgbTree.pred.test, test.Set$Toxicity)
    
	tune.parms.number <- length(tune.parms)
	# if (tune.parms.number == 1){
	# tune.plot <- ggplot(xgbTree.Fit$results, aes_string(x = tune.parms, y = 'AUC')) + 
                 # geom_line() +
	             # geom_point() +
	             # geom_linerange(aes(ymax = AUC + AUCSD, ymin = AUC - AUCSD)) +
	             # labs(x = tune.parms, title = paste('Tune', tune.parms))
	# }
	# if (tune.parms.number == 2){
	# tune.plot <- ggplot(xgbTree.Fit)
	# }
	tune.plot <- ggplot(xgbTree.Fit)
    return(list(bestTune = xgbTree.Fit$bestTune, cv = cv.metric,
				test = data.frame(as.list(xgbTree.pred.test.metric)),
				plot = tune.plot))
}


# Parameter adjustment process
tune.xgb <- function(fitControl, test.Set, train.set){
#Step 1: Fix learning rate, tune nrounds
tune.step.1 <- tune.parm(fitControl = fitControl,
                         tune.parms = 'nrounds',
						 eta = 0.2 ,
						 gamma_p = 0,
						 min_child_weight = 1,
						 max_depth = 4,
						 nrounds = seq(1, 1000, by = 1),
						 colsample_bytree = 0.8,
						 test.Set = test.Set,
                         train = train.set)
ggsave(filename = 'tune2.1.pdf', plot = tune.step.1$plot)
best_nrounds = tune.step.1$bestTune[,'nrounds']
print(tune.step.1)


#Step 2: Tune max_depth and min_child_weight
tune.step.2 <- tune.parm(fitControl = fitControl,
                         tune.parms = c('max_depth', 'min_child_weight'),
						 eta = 0.2 ,
						 gamma_p = 0,
						 min_child_weight = seq(from = 1, to = 6, by = 1),
						 max_depth = seq(from = 2, to = 10, by = 1),
						 nrounds = best_nrounds,
						 colsample_bytree = 0.8,
						 test.Set = test.Set,
                         train = train.set)

ggsave(filename = 'tune2.2.pdf', plot = tune.step.2$plot)
best_max_depth = tune.step.2$bestTune[,'max_depth']
best_min_child_weight = tune.step.2$bestTune[,'min_child_weight']

#Tune nrounds
# tune.step.2.1 <- tune.parm(fitControl = fitControl,
                         # tune.parms = 'nrounds',
						 # eta = 0.1 ,
						 # gamma_p = 0,
						 # min_child_weight = best_min_child_weight,
						 # max_depth = best_max_depth,
						 # nrounds = seq(1, 300, by = 1),
						 # colsample_bytree = 1,
						 # test.Set = test.Set,
                         # train = train.set)
# best_nrounds = tune.step.2.1$bestTune[,'nrounds']

#Step 3: Tune gamma
# tune.step.3 <- tune.parm(fitControl = fitControl,
                         # tune.parms = 'gamma',
						 # eta = 0.1 ,
						 # gamma_p = 0.1^seq(from = -10, to = 10, by = 1),
						 # min_child_weight = best_min_child_weight,
						 # max_depth = best_max_depth,
						 # nrounds = best_nrounds,
						 # colsample_bytree = 1,
						 # test.Set = test.Set,
                         # train = train.set)
# ggsave(filename = 'tune.3.pdf', plot = tune.step.3$plot + coord_trans(x = 'log'))
# best_gamma = tune.step.3$bestTune[,'gamma']
best_gamma = 0

#Tune nrounds
# tune.step.3.1 <- tune.parm(fitControl = fitControl,
                         # tune.parms = 'nrounds',
						 # eta = 0.1 ,
						 # gamma_p = best_gamma,
						 # min_child_weight = best_min_child_weight,
						 # max_depth = best_max_depth,
						 # nrounds = seq(1, 200, by = 1),
						 # colsample_bytree = 1,
						 # test.Set = test.Set,
                         # train = train.set)
# best_nrounds = tune.step.3.1$bestTune[,'nrounds']


#Step 4: Tune colsample_bytree
# tune.step.4 <- tune.parm(fitControl = fitControl,
                         # tune.parms = 'colsample_bytree',
						 # eta = 0.1 ,
						 # gamma_p = best_gamma,
						 # min_child_weight = best_min_child_weight,
						 # max_depth = best_max_depth,
						 # nrounds = best_nrounds,
						 # colsample_bytree = seq(from = 0.3, to = 1, by = 0.02),
						 # test.Set = test.Set,
                         # train = train.set)
# ggsave(filename = 'tune.4.pdf', plot = tune.step.4$plot)
# best_colsample_bytree = tune.step.4$bestTune[,'colsample_bytree']
best_colsample_bytree = 0.8

# Step 5: Reducing Learning Rate, tune nrounds
tune.step.5 <- tune.parm(fitControl = fitControl,
                         tune.parms = 'nrounds',
						 eta = 0.01 ,
						 gamma_p = best_gamma,
						 min_child_weight = best_min_child_weight,
						 max_depth = best_max_depth,
						 nrounds = seq(1, 4000, by = 1),
						 colsample_bytree = best_colsample_bytree,
						 test.Set = test.Set,
                         train = train.set)

ggsave(filename = 'tune2.5.pdf', plot = tune.step.5$plot)
best_nrounds = tune.step.5$bestTune[,'nrounds']
best.parms <- as.data.frame(list(nrounds = best_nrounds, max_depth = best_max_depth, eta = 0.01, gamma = best_gamma, colsample_bytree = best_colsample_bytree, min_child_weight = best_min_child_weight, subsample = 1))
return(best.parms)
}

#######################End the XGB parameter optimization process#####################################################







# XGB function
library(doMC)
registerDoMC(cores = 16) # Set the number of CPU cores running in parallel

train.xgb <- function (train.set, test.sets, fitControl){
    fitControl.tune <- trainControl(method = "repeatedcv", number = 5, repeats = 100, summaryFunction = myTwoClassSummary, classProbs = TRUE, verboseIter = TRUE)
    xgb.Grid <- tune.xgb(fitControl.tune, test.sets[[1]], train.set) # Adjustment parameters
    #xgb.Grid <- expand.grid(nrounds=405, max_depth=6,  eta=0.02, gamma=0, colsample_bytree=0.8, min_child_weight=1)
    xgb.Fit <- train(Toxicity ~ ., data = train.set, method = "xgbTree", tuneGrid = xgb.Grid, trControl = fitControl, metric = "AUC", maximize = "TRUE", verbose = TRUE)
    cv.metric <- xgb.Fit$results %>%
	             top_n(1, AUC)
    xgb.pred.metrics <- data.frame()
    for (test.set in test.sets){
        xgb.pred.raw <- predict(xgb.Fit, newdata=test.set, type = "raw")
        xgb.pred.prob <- predict(xgb.Fit, newdata=test.set, type = "prob")
        xgb.pred.metric <- myEvaluate(test.set$Toxicity, xgb.pred.raw, xgb.pred.prob[,1])
        xgb.pred.metrics <- rbind(xgb.pred.metrics, xgb.pred.metric)
    }
    colnames(xgb.pred.metrics) <- colnames(cv.metric)[7:((length(colnames(cv.metric))-6)/2+6)]
    return(list(fit = xgb.Fit, bestTune = xgb.Fit$bestTune, cv = cv.metric, Ind = xgb.pred.metrics))
}

# Training control
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 100, summaryFunction = myTwoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Train xgb models with different feature sets
Fit.EStateFP.xgb <- train.xgb(Train_fp_all.EStateFP.sets$Train, list(Train_fp_all.EStateFP.sets$IndTest, Train_fp_all.EStateFP.sets$IndTest2), fitControl)
Fit.MACCSFP.xgb <- train.xgb(Train_fp_all.MACCSFP.sets$Train, list(Train_fp_all.MACCSFP.sets$IndTest, Train_fp_all.MACCSFP.sets$IndTest2), fitControl)
Fit.PubchemFP.xgb <- train.xgb(Train_fp_all.PubchemFP.sets$Train, list(Train_fp_all.PubchemFP.sets$IndTest, Train_fp_all.PubchemFP.sets$IndTest2), fitControl)
Fit.SubFP.xgb <- train.xgb(Train_fp_all.SubFP.sets$Train, list(Train_fp_all.SubFP.sets$IndTest, Train_fp_all.SubFP.sets$IndTest2), fitControl)
Fit.KRFP.xgb <- train.xgb(Train_fp_all.KRFP.sets$Train, list(Train_fp_all.KRFP.sets$IndTest, Train_fp_all.KRFP.sets$IndTest2), fitControl)
Fit.AD2D.xgb <- train.xgb(Train_fp_all.AD2D.sets$Train, list(Train_fp_all.AD2D.sets$IndTest, Train_fp_all.AD2D.sets$IndTest2), fitControl)
Fit.SubFPC.xgb <- train.xgb(Train_fp_all.SubFPC.sets$Train, list(Train_fp_all.SubFPC.sets$IndTest, Train_fp_all.SubFPC.sets$IndTest2), fitControl)
Fit.KRFPC.xgb <- train.xgb(Train_fp_all.KRFPC.sets$Train, list(Train_fp_all.KRFPC.sets$IndTest, Train_fp_all.KRFPC.sets$IndTest2), fitControl)
Fit.APC2D.xgb <- train.xgb(Train_fp_all.APC2D.sets$Train, list(Train_fp_all.APC2D.sets$IndTest, Train_fp_all.APC2D.sets$IndTest2), fitControl)


# Summarize the results
xgb.cv.metric <- data.frame()
xgb.ind1.metric <- data.frame()
xgb.ind2.metric <- data.frame()
xgb.model.names <- ls(pattern='Fit.*.xgb') # List the names of all xgb models
for (model.name in xgb.model.names){
    model <- eval(parse(text=model.name))
    xgb.cv.metric <- rbind(xgb.cv.metric, model$cv)
    xgb.ind1.metric <- rbind(xgb.ind1.metric, model$Ind[1,])
    xgb.ind2.metric <- rbind(xgb.ind2.metric, model$Ind[2,])
}
rownames(xgb.cv.metric) <- xgb.model.names
rownames(xgb.ind1.metric) <- xgb.model.names
rownames(xgb.ind2.metric) <- xgb.model.names
# Output performance indexes for all models
write.csv(xgb.cv.metric, file='E:/reproductive/models/xgb.cv.csv')
write.csv(xgb.ind1.metric, file='E:/reproductive/models/xgb.ind1.csv')
write.csv(xgb.ind2.metric, file='E:/reproductive/models/xgb.ind2.csv')
# Save all variables
save.image(file='E:/reproductive/models/xgb.models.RData')

print("All finished")

