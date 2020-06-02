library(dplyr)
library(caret)
library(randomForest)
library(stringr)
library(tidyverse)

load(file = 'E:/reproductive/models/feature_slelect-tc0.95.RData')




# Repeat the 5-fold cross-validation 100 times
library(doMC)
registerDoMC(cores = 5) #  Set the number of CPU cores running in parallel

repeats = 1
k = 5
folds <- createMultiFolds(y=Train_fp_all.PubchemFP.sets$Train[,1], k = k, times = repeats)
fold.names <- names(folds)
train.Dataset.name.vector <- ls(pattern='Train_fp_all.*.sets')

rf.imp.results <- foreach(i = 1:length(folds), .combine=rbind) %dopar% {
    fold = folds[[i]]
    rf.imp.fold <- data.frame()
    for (train.Dataset.name in train.Dataset.name.vector) {
        train.Dataset <- eval(parse(text=train.Dataset.name))
        train.Dataset <- train.Dataset$Train
        train <- train.Dataset[fold,]
        rf.fit <- randomForest(Toxicity ~ ., data = train, importance = TRUE) #Model fitting
        rf.imp <- importance(rf.fit)
        rf.imp <- as.data.frame(rf.imp)
        rf.imp <- cbind(rf.imp, data.frame(FP.NAME = rownames(rf.imp)))
        rf.imp <- cbind(rf.imp, data.frame(FP.CAT = train.Dataset.name))
        rf.imp.fold <- rbind(rf.imp.fold, rf.imp)
    }
    rf.imp.fold
}

# Find the top 10 features in each feature set
rf.imp.results2 <- group_by(rf.imp.results, FP.CAT, FP.NAME) %>%
                    summarise_all(funs(mean)) %>%
                    top_n(10, MeanDecreaseGini)

rf.imp.results2 <- as.data.frame(rf.imp.results2)
rf.imp.results.filter <- data.frame()
for (i in seq(dim(rf.imp.results2)[1])){
    filtered <- filter(rf.imp.results, FP.CAT == rf.imp.results2[i, 'FP.CAT'] & FP.NAME == rf.imp.results2[i, 'FP.NAME'])
    rf.imp.results.filter <- rbind(rf.imp.results.filter, filtered)
}

rf.imp.results.filter <- group_by(rf.imp.results.filter, FP.CAT, FP.NAME) %>%
                                summarise_all(funs(mean, sd))

rf.imp.results.filter <- filter(rf.imp.results.filter,
                                FP.CAT != 'Train_fp_all.FP.sets' &
                                FP.CAT != 'Train_fp_all.ExtFP.sets' &
                                FP.CAT != 'Train_fp_all.GraphFP.sets' &
                                FP.CAT != 'Train_fp_all.APC2D.sets' &
                                FP.CAT != 'Train_fp_all.KRFPC.sets' &
                                FP.CAT != 'Train_fp_all.SubFPC.sets')

rf.imp.results.filter2 <- mutate(rf.imp.results.filter, FP.NAME = str_replace(FP.NAME, 'FP', '')) %>%
                            mutate(FP.NAME = str_replace(FP.NAME, 'AD2D', 'AP2D-')) %>%
                            mutate(FP.NAME = str_replace(FP.NAME, 'EState', 'EState-')) %>%
                            mutate(FP.NAME = str_replace(FP.NAME, 'KR', 'KR-')) %>%
                            mutate(FP.NAME = str_replace(FP.NAME, 'MACCS', 'MACCS-')) %>%
                            mutate(FP.NAME = str_replace(FP.NAME, 'Pubchem', 'Pubchem-')) %>%
                            mutate(FP.NAME = str_replace(FP.NAME, 'Sub', 'FP4-'))

FP.CAT.NAMES <-   c(`Train_fp_all.AD2D.sets` = 'AP2D',
                    `Train_fp_all.EStateFP.sets` = 'EState',
                    `Train_fp_all.KRFP.sets` = 'KR',
                    `Train_fp_all.MACCSFP.sets` = 'MACCS',
                    `Train_fp_all.PubchemFP.sets` = 'Pubchem',
                    `Train_fp_all.SubFP.sets` = 'FP4')

imp.plot <- ggplot(rf.imp.results.filter2, aes(x = MeanDecreaseGini_mean, y = reorder(FP.NAME, MeanDecreaseGini_mean, FUN=median))) +
                geom_point() +
                geom_errorbarh(aes(xmin =  MeanDecreaseGini_mean - MeanDecreaseGini_sd, xmax =  MeanDecreaseGini_mean + MeanDecreaseGini_sd), height = 0.2) +
                facet_wrap(~FP.CAT, scales = 'free', ncol = 3, labeller = as_labeller(FP.CAT.NAMES)) +
                xlab('MeanDecreaseGini') +
                theme_bw() +
                theme(panel.border = element_rect(size=1)) +
                theme(panel.grid.minor = element_blank()) +
                theme(panel.grid.major.x = element_blank()) +
                theme(panel.grid.major.y = element_line(linetype = 3)) +
                theme(axis.title.y = element_blank())

ggsave(filename = 'imp.plot.pdf', plot = imp.plot, width = 8, height = 5)



# Statisticsing frequency of occurrence of important variables in the reproductive and non-reproductive toxicity compounds
rf.imp.results.filter <- as.data.frame(rf.imp.results.filter)
freqs <- data.frame()
for (i in seq(dim(rf.imp.results.filter)[1])) {
    FP.NAME <- rf.imp.results.filter[i, 'FP.NAME']
    FP.CAT <- rf.imp.results.filter[i, 'FP.CAT']
    MeanDecreaseGini_mean <- rf.imp.results.filter[i, 'MeanDecreaseGini_mean']
    MeanDecreaseGini_sd <- rf.imp.results.filter[i, 'MeanDecreaseGini_sd']
    FP.NAME <- as.character(FP.NAME)
    FP.NAME.vec <- Train_fp_all[,c(FP.NAME, 'Toxicity')]
    FP.NAME.count <- table(FP.NAME.vec)
    print(FP.NAME.count)
    FP.NAME.count.can <- FP.NAME.count[2,1]
    FP.NAME.count.noncan <- FP.NAME.count[2,2]
    freqs <- rbind(freqs, data.frame(FP.CAT = FP.CAT, FP.NAME = FP.NAME, count.can = FP.NAME.count.can, count.noncan = FP.NAME.count.noncan, MeanDecreaseGini_mean, MeanDecreaseGini_sd))
}

write.csv(freqs, file='models/imp.freq.csv' )


imp_fp_summary <- list()
for (i in seq(dim(rf.imp.results.filter)[1])) {
    FP.NAME <- rf.imp.results.filter[i, 'FP.NAME']
    FP.CAT <- rf.imp.results.filter[i, 'FP.CAT']
    FP.NAME <- as.character(FP.NAME)
    FP.CAT <- as.character(FP.CAT)
    FP.NAME.vec <- Train_fp_all[,c(FP.NAME, 'Toxicity')]
    P1 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "P" & eval(parse(text=FP.NAME))==1) %>% pull(1) #存在此分子特征的阳性分子
    N0 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "N" & eval(parse(text=FP.NAME))==0) %>% pull(1) #不存在此分子特征的阴性分子
    N1 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "N" & eval(parse(text=FP.NAME))==1) %>% pull(1) #不存在此分子特征的阳性分子
    P0 <- rownames_to_column(FP.NAME.vec) %>% filter(Toxicity == "P" & eval(parse(text=FP.NAME))==0) %>% pull(1) #存在此分子特征的阴性分子
    imp_fp_summary_i <- list(FP.CAT = FP.CAT, FP.NAME = FP.NAME, P1 = P1, N0 = N0, N1 = N1, P0 = P0)
    imp_fp_summary[[i]] <- imp_fp_summary_i
}

capture.output(print(imp_fp_summary), file = "models/imp.fp.mol.txt")


#Save all variables
save.image(file='models/imp.plot.RData')

