# Install/Load required packages
library(dplyr)
library(caret)
library(randomForest)
library(stringr)

# Importing the input data
load(file = 'E:/reproductive/models/information_intergeate.RData')

# Sort by molecular name
Train_fp_all <- arrange(Train_fp_all, Name)
External_fp_all <- arrange(External_fp_all, Name)
External_fp_all_2 <- arrange(External_fp_all_2, Name)

# Set the molecular name as the row name (rownames), and delete the molecular name column
rownames(Train_fp_all) <- Train_fp_all$Name
rownames(External_fp_all) <- External_fp_all$Name
rownames(External_fp_all_2) <- External_fp_all_2$Name
Train_fp_all <- select(Train_fp_all, -Name)
External_fp_all <- select(External_fp_all, -Name)
External_fp_all_2 <- select(External_fp_all_2, -Name)


# Different types of fingerprints are assembled to form a training set
#Train_fp_all.FP        <- Train_fp_all[,c(1,(2):(1+1024))]
#Train_fp_all.ExtFP     <- Train_fp_all[,c(1,(2+1024):(1+1024+1024))]
Train_fp_all.EStateFP  <- Train_fp_all[,c(1,(2+1024+1024):(1+1024+1024+79))]
#Train_fp_all.GraphFP   <- Train_fp_all[,c(1,(2+1024+1024+79):(1+1024+1024+79+1024))]
Train_fp_all.MACCSFP   <- Train_fp_all[,c(1,(2+1024+1024+79+1024):(1+1024+1024+79+1024+166))]
Train_fp_all.PubchemFP <- Train_fp_all[,c(1,(2+1024+1024+79+1024+166):(1+1024+1024+79+1024+166+881))]
Train_fp_all.SubFP     <- Train_fp_all[,c(1,(2+1024+1024+79+1024+166+881):(1+1024+1024+79+1024+166+881+307))]
Train_fp_all.KRFP      <- Train_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307):(1+1024+1024+79+1024+166+881+307+4860))]
Train_fp_all.AD2D      <- Train_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860):(1+1024+1024+79+1024+166+881+307+4860+780))]
Train_fp_all.SubFPC    <- Train_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780):(1+1024+1024+79+1024+166+881+307+4860+780+307))]
Train_fp_all.KRFPC     <- Train_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780+307):(1+1024+1024+79+1024+166+881+307+4860+780+307+4860))]
Train_fp_all.APC2D     <- Train_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780+307+4860):(1+1024+1024+79+1024+166+881+307+4860+780+307+4860+780))]

# Output the number of features of each fingerprint
print("Output the number of features of each fingerprint")
print(paste("EStateFP", dim(Train_fp_all.EStateFP)[2]-1))
print(paste("MACCSFP", dim(Train_fp_all.MACCSFP)[2]-1))
print(paste("PubchemFP", dim(Train_fp_all.PubchemFP)[2]-1))
print(paste("SubFP", dim(Train_fp_all.SubFP)[2]-1))
print(paste("KRFP", dim(Train_fp_all.KRFP)[2]-1))
print(paste("AD2D", dim(Train_fp_all.AD2D)[2]-1))
print(paste("SubFPC", dim(Train_fp_all.SubFPC)[2]-1))
print(paste("KRFPC", dim(Train_fp_all.KRFPC)[2]-1))
print(paste("APC2D", dim(Train_fp_all.APC2D)[2]-1))

# Different types of fingerprints are assembled to form a test set
#External_fp_all.FP        <- External_fp_all[,c(1,(2):(1+1024))]
#External_fp_all.ExtFP     <- External_fp_all[,c(1,(2+1024):(1+1024+1024))]
External_fp_all.EStateFP  <- External_fp_all[,c(1,(2+1024+1024):(1+1024+1024+79))]
#External_fp_all.GraphFP   <- External_fp_all[,c(1,(2+1024+1024+79):(1+1024+1024+79+1024))]
External_fp_all.MACCSFP   <- External_fp_all[,c(1,(2+1024+1024+79+1024):(1+1024+1024+79+1024+166))]
External_fp_all.PubchemFP <- External_fp_all[,c(1,(2+1024+1024+79+1024+166):(1+1024+1024+79+1024+166+881))]
External_fp_all.SubFP     <- External_fp_all[,c(1,(2+1024+1024+79+1024+166+881):(1+1024+1024+79+1024+166+881+307))]
External_fp_all.KRFP      <- External_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307):(1+1024+1024+79+1024+166+881+307+4860))]
External_fp_all.AD2D      <- External_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860):(1+1024+1024+79+1024+166+881+307+4860+780))]
External_fp_all.SubFPC    <- External_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780):(1+1024+1024+79+1024+166+881+307+4860+780+307))]
External_fp_all.KRFPC     <- External_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780+307):(1+1024+1024+79+1024+166+881+307+4860+780+307+4860))]
External_fp_all.APC2D     <- External_fp_all[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780+307+4860):(1+1024+1024+79+1024+166+881+307+4860+780+307+4860+780))]


# Different types of fingerprints are assembled to form a test set 2
#External_fp_all_2.FP        <- External_fp_all_2[,c(1,(2):(1+1024))]
#External_fp_all_2.ExtFP     <- External_fp_all_2[,c(1,(2+1024):(1+1024+1024))]
External_fp_all_2.EStateFP  <- External_fp_all_2[,c(1,(2+1024+1024):(1+1024+1024+79))]
#External_fp_all_2.GraphFP   <- External_fp_all_2[,c(1,(2+1024+1024+79):(1+1024+1024+79+1024))]
External_fp_all_2.MACCSFP   <- External_fp_all_2[,c(1,(2+1024+1024+79+1024):(1+1024+1024+79+1024+166))]
External_fp_all_2.PubchemFP <- External_fp_all_2[,c(1,(2+1024+1024+79+1024+166):(1+1024+1024+79+1024+166+881))]
External_fp_all_2.SubFP     <- External_fp_all_2[,c(1,(2+1024+1024+79+1024+166+881):(1+1024+1024+79+1024+166+881+307))]
External_fp_all_2.KRFP      <- External_fp_all_2[,c(1,(2+1024+1024+79+1024+166+881+307):(1+1024+1024+79+1024+166+881+307+4860))]
External_fp_all_2.AD2D      <- External_fp_all_2[,c(1,(2+1024+1024+79+1024+166+881+307+4860):(1+1024+1024+79+1024+166+881+307+4860+780))]
External_fp_all_2.SubFPC    <- External_fp_all_2[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780):(1+1024+1024+79+1024+166+881+307+4860+780+307))]
External_fp_all_2.KRFPC     <- External_fp_all_2[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780+307):(1+1024+1024+79+1024+166+881+307+4860+780+307+4860))]
External_fp_all_2.APC2D     <- External_fp_all_2[,c(1,(2+1024+1024+79+1024+166+881+307+4860+780+307+4860):(1+1024+1024+79+1024+166+881+307+4860+780+307+4860+780))]



# Delete undifferentiated features from all samples
# Train_fp_all.FP <- Train_fp_all.FP[,-(nearZeroVar(Train_fp_all.FP))]
# Train_fp_all.ExtFP <- Train_fp_all.ExtFP[,-(nearZeroVar(Train_fp_all.ExtFP))]
Train_fp_all.EStateFP <- Train_fp_all.EStateFP[,-(nearZeroVar(Train_fp_all.EStateFP))]
# Train_fp_all.GraphFP <- Train_fp_all.GraphFP[,-(nearZeroVar(Train_fp_all.GraphFP))]
Train_fp_all.MACCSFP <- Train_fp_all.MACCSFP[,-(nearZeroVar(Train_fp_all.MACCSFP))]
Train_fp_all.PubchemFP <- Train_fp_all.PubchemFP[,-(nearZeroVar(Train_fp_all.PubchemFP))]
Train_fp_all.SubFP <- Train_fp_all.SubFP[,-(nearZeroVar(Train_fp_all.SubFP))]
Train_fp_all.KRFP <- Train_fp_all.KRFP[,-(nearZeroVar(Train_fp_all.KRFP))]
Train_fp_all.AD2D <- Train_fp_all.AD2D[,-(nearZeroVar(Train_fp_all.AD2D))]
Train_fp_all.SubFPC <- Train_fp_all.SubFPC[,-(nearZeroVar(Train_fp_all.SubFPC))]
Train_fp_all.KRFPC <- Train_fp_all.KRFPC[,-(nearZeroVar(Train_fp_all.KRFPC))]
Train_fp_all.APC2D <- Train_fp_all.APC2D[,-(nearZeroVar(Train_fp_all.APC2D))]

# Output the number of features of each fingerprint
print("Output the number of features of each fingerprint")
print(paste("EStateFP", dim(Train_fp_all.EStateFP)[2]-1))
print(paste("MACCSFP", dim(Train_fp_all.MACCSFP)[2]-1))
print(paste("PubchemFP", dim(Train_fp_all.PubchemFP)[2]-1))
print(paste("SubFP", dim(Train_fp_all.SubFP)[2]-1))
print(paste("KRFP", dim(Train_fp_all.KRFP)[2]-1))
print(paste("AD2D", dim(Train_fp_all.AD2D)[2]-1))
print(paste("SubFPC", dim(Train_fp_all.SubFPC)[2]-1))
print(paste("KRFPC", dim(Train_fp_all.KRFPC)[2]-1))
print(paste("APC2D", dim(Train_fp_all.APC2D)[2]-1))

# Calculation function of the Tanimoto coefficient (URL:https://docs.tibco.com/pub/spotfire/6.5.3/doc/html/hc/hc_tanimoto_coefficient.htm)
tanimoto <- function(x, similarity=F) {
	res<-sapply(x, function(x1){
		sapply(x, function(x2) {i=length(which(x1 & x2)) / length(which(x1 | x2)); ifelse(is.na(i), 0, i)})
	})
	if(similarity==T) return(res)
	else return(1-res)
}

# Remove highly relevant features
deleteHighlyCor <- function(data.set) {
    cor.data.set <- tanimoto(data.set[,-1], similarity=TRUE) # The first column of data is only for classification and does not participate in the deletion of highly relevant features
    highlyCor.data.set <- findCorrelation(cor.data.set, cutoff = 0.5) # Set the threshold of the Tanimoto coefficient, the range is 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99 and 1.0
    if (length(highlyCor.data.set) > 0) {
    data.set <- data.set[,-(highlyCor.data.set+1)]
    }
    return(data.set)
 }

# Train_fp_all.FP <- deleteHighlyCor(Train_fp_all.FP)
# Train_fp_all.ExtFP <- deleteHighlyCor(Train_fp_all.ExtFP)
Train_fp_all.EStateFP <- deleteHighlyCor(Train_fp_all.EStateFP)
# Train_fp_all.GraphFP <- deleteHighlyCor(Train_fp_all.GraphFP)
Train_fp_all.MACCSFP <- deleteHighlyCor(Train_fp_all.MACCSFP)
Train_fp_all.PubchemFP <- deleteHighlyCor(Train_fp_all.PubchemFP)
Train_fp_all.SubFP <- deleteHighlyCor(Train_fp_all.SubFP)
Train_fp_all.KRFP <- deleteHighlyCor(Train_fp_all.KRFP)
Train_fp_all.AD2D <- deleteHighlyCor(Train_fp_all.AD2D)
Train_fp_all.SubFPC <- deleteHighlyCor(Train_fp_all.SubFPC)
Train_fp_all.KRFPC <- deleteHighlyCor(Train_fp_all.KRFPC)
Train_fp_all.APC2D <- deleteHighlyCor(Train_fp_all.APC2D)

# Output the number of features of each fingerprint
print("Output the number of features of each fingerprint")
print(paste("EStateFP", dim(Train_fp_all.EStateFP)[2]-1))
print(paste("MACCSFP", dim(Train_fp_all.MACCSFP)[2]-1))
print(paste("PubchemFP", dim(Train_fp_all.PubchemFP)[2]-1))
print(paste("SubFP", dim(Train_fp_all.SubFP)[2]-1))
print(paste("KRFP", dim(Train_fp_all.KRFP)[2]-1))
print(paste("AD2D", dim(Train_fp_all.AD2D)[2]-1))
print(paste("SubFPC", dim(Train_fp_all.SubFPC)[2]-1))
print(paste("KRFPC", dim(Train_fp_all.KRFPC)[2]-1))
print(paste("APC2D", dim(Train_fp_all.APC2D)[2]-1))




# Split the training set, independent test set 1 and independent test set 2
# If the training set and independent test set have been divided, there is no need to split. However, in order not to split the three data sets, you must run the following code and modify the contents of the splitSet function.
set.seed(1) # When dividing the data set, please fix the value of the random number to ensure that the data set used by different models is the same
trainIndex <- createDataPartition(Train_fp_all.EStateFP$Toxicity, p = 0.8, list = FALSE) # The stratified sampling function in caret uses 80% of the data as the training set and 20% as the external test set

# Unsplit dataset
splitSet <- function(data.set, trainIndex, IndTest1='', IndTest2=''){
    data.set.Train <- data.set
    data.set.IndTest <- IndTest1
    data.set.IndTest2 <- IndTest2
    return(list(Train = data.set.Train, IndTest = data.set.IndTest, IndTest2 = data.set.IndTest2))
}

# Split dataset
splitSet_2 <- function(data.set, trainIndex, IndTest1='', IndTest2=''){
    data.set.Train <- data.set[trainIndex,] # 80% training set
    data.set.IndTest <- data.set[-trainIndex,] # 20% test set
    data.set.IndTest2 <- IndTest2 # You can also set a second test set here
    return(list(Train = data.set.Train, IndTest = data.set.IndTest, IndTest2 = data.set.IndTest2))
}

Train_fp_all.EStateFP.sets <- splitSet(Train_fp_all.EStateFP, trainIndex, IndTest1 = External_fp_all.EStateFP, IndTest2 = External_fp_all_2.EStateFP)
Train_fp_all.MACCSFP.sets <- splitSet(Train_fp_all.MACCSFP, trainIndex, IndTest1 = External_fp_all.MACCSFP, IndTest2 = External_fp_all_2.MACCSFP)
Train_fp_all.PubchemFP.sets <- splitSet(Train_fp_all.PubchemFP, trainIndex, IndTest1 = External_fp_all.PubchemFP, IndTest2 = External_fp_all_2.PubchemFP)
Train_fp_all.SubFP.sets <- splitSet(Train_fp_all.SubFP, trainIndex, IndTest1 = External_fp_all.SubFP, IndTest2 = External_fp_all_2.SubFP)
Train_fp_all.KRFP.sets <- splitSet(Train_fp_all.KRFP, trainIndex, IndTest1 = External_fp_all.KRFP, IndTest2 = External_fp_all_2.KRFP)
Train_fp_all.AD2D.sets <- splitSet(Train_fp_all.AD2D, trainIndex, IndTest1 = External_fp_all.AD2D, IndTest2 = External_fp_all_2.AD2D)
Train_fp_all.SubFPC.sets <- splitSet(Train_fp_all.SubFPC, trainIndex, IndTest1 = External_fp_all.SubFPC, IndTest2 = External_fp_all_2.SubFPC)
Train_fp_all.KRFPC.sets <- splitSet(Train_fp_all.KRFPC, trainIndex, IndTest1 = External_fp_all.KRFPC, IndTest2 = External_fp_all_2.KRFPC)
Train_fp_all.APC2D.sets <- splitSet(Train_fp_all.APC2D, trainIndex, IndTest1 = External_fp_all.APC2D, IndTest2 = External_fp_all_2.APC2D)


# Evaluation function
myTwoClassSummary <- function(data, lev = NULL, model = NULL)
{
    if (length(levels(data$obs)) > 2)
        stop(paste("Your outcome has", length(levels(data$obs)),
            "levels. The twoClassSummary() function isn't appropriate."))
    requireNamespace("pROC")
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
        stop("levels of observed and predicted data do not match")
    out <- myEvaluate(data$obs, data$pred, eval(parse(text=paste0('data$',levels(data[, "obs"])[1]))))
    # print(out)
    out
}

myEvaluate <- function(obs, pred, Lev1)
{
    rocObject <- try(pROC::roc(obs, Lev1, direction = ">"),
        silent = TRUE)
    if (class(rocObject)[1] == "try-error"){
        rocAUC <- NA
    }else{
        rocAUC <- rocObject$auc
    }
    data <- data.frame(pred=pred, obs=obs)
    cfm <- table(data[1:2])
    tp <- as.numeric(cfm[1]) #True positive
    fn <- as.numeric(cfm[2]) #False negtive
    fp <- as.numeric(cfm[3]) #False positive
    tn <- as.numeric(cfm[4]) #True negtive
    n.pos <- tp + fn #Positive sample number
    n.neg <- fp + tn #Negtive sample number
    n.tot <- n.pos + n.neg #Total sample number
    acc <- (tp + tn) / (n.tot) #Accuracy
    sens <- tp / n.pos #Sensitivity, Recall, Hit rate, True positive rate (TPR)
    spec <- tn / n.neg #Specificity (SPC), True negative rate (TNR)
    mcc <- (tp * tn - fp * fn) / sqrt((tn + fp) * (tp + fn) * (fp + tp) * (tn + fn)) #Matthews correlation coefficient
    prec <- tp / (tp + fp)  #Precision
    F1 <- 2 * tp / (2 * tp + fp + fn) #F1 Score
    out <- c(rocAUC, sens, spec, acc, mcc, prec, F1, tp, fn, fp, tn)
    names(out) <- c('AUC', 'sen', 'spe', 'acc', 'mcc', 'prec', 'F1', 'tp', 'fn', 'fp', 'tn')
    out
}

save.image(file = 'E:/reproductive/models/feature_slelect-tc0.5.RData')
