library(dplyr)
library(caret)
library(randomForest)
library(stringr)
library(tidyverse)

load(file = 'E:/reproductive/models/imp.plot.RData')

rf.imp.results.filter <- as.data.frame(rf.imp.results.filter)

imp_fp_summary <- list()
for (i in seq(dim(rf.imp.results.filter)[1])) {
    FP.NAME <- rf.imp.results.filter[i, 'FP.NAME']
    FP.CAT <- rf.imp.results.filter[i, 'FP.CAT']
    FP.NAME <- as.character(FP.NAME)
    FP.CAT <- as.character(FP.CAT)
    FP.NAME.vec <- Train_fp_all[,c(FP.NAME, 'Toxicity')]
    P1 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "P" & eval(parse(text=FP.NAME))==1) %>% pull(1) #Positive molecules with this feature
    N0 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "N" & eval(parse(text=FP.NAME))==0) %>% pull(1) #Negative molecule without this feature
    N1 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "N" & eval(parse(text=FP.NAME))==1) %>% pull(1) #Positive molecule without this feature
    P0 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "P" & eval(parse(text=FP.NAME))==0) %>% pull(1) #Negative molecules with this feature
    imp_fp_summary_i <- list(FP.CAT = FP.CAT, FP.NAME = FP.NAME, P1 = P1, N0 = N0, N1 = N1, P0 = P0)
    imp_fp_summary[[i]] <- imp_fp_summary_i
}

capture.output(print(imp_fp_summary), file = "models/imp.fp.mol.txt")
