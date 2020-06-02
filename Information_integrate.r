# Install/Load required packages
library(dplyr)
library(caret)
library(randomForest)
library(stringr)


# Processing training set
# Reading compound toxicity information from training set
Train_info <- read.csv('E:/reproductive/data_train/Jiang1823Train.csv', header=T, stringsAsFactors=FALSE)
colnames(Train_info) <- c("Name", "Toxicity")
Train_info$Toxicity <- factor(Train_info$Toxicity, levels = c("P", "N"))
# Reading the molecular fingerprint information from the training set
Train_fp <- read.csv('E:/reproductive/Train_fp.csv', header=T, stringsAsFactors=FALSE)

# Integrate compound toxicity information and sub-fingerprint information
Train_fp_all <- left_join(Train_info, Train_fp, by=c('Name' = 'Name'))



# Processing validation set
# Reading compound toxicity information from validation set
External_info <- read.csv('E:/reproductive/data_validate/Jiang1823Validate.csv', header=T, stringsAsFactors=FALSE)
External_info$Toxicity <- factor(External_info$Toxicity, levels = c("P", "N"))
# Reading the molecular fingerprint information from the training set
External_fp <- read.csv('E:/reproductive/Validate_fp.csv', header=T, stringsAsFactors=FALSE)

# Integrate inhibitor information and descriptor information
External_fp_all <- left_join(External_info, External_fp, by=c('Name' = 'Name'))




# If there is a second verification set, you can copy the above code and modify it
# External_fp_all_2
External_fp_all_2 <- External_fp_all

# Save all information
save.image(file = 'E:/reproductive/models/information_intergeate.RData')
