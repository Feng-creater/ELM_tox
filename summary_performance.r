library(tidyr)
library(dplyr)

args = commandArgs(trailingOnly=TRUE)
filename <- args[1]

cat(filename)

performance <- read.csv(filename, stringsAsFactors = FALSE)

performance <- performance %>% select(-mcc, -prec, -F1, -tp, -tn, -fp, -fn, -mccSD, -precSD, -F1SD, -tpSD, -tnSD, -fpSD, -fnSD) %>%
                                separate(1, c("a", "Fingerprint", "b")) %>% select(-a, -b) %>%
                                mutate(AUC = paste0(format(AUC, nsmall=3, digits=0))) %>%
                                mutate(sen = paste0(format(sen * 100, nsmall=1, digits=0))) %>%
                                mutate(spe = paste0(format(spe * 100, nsmall=1, digits=0))) %>%
                                mutate(acc = paste0(format(acc * 100, nsmall=1, digits=0))) %>%
                                mutate(AUCSD = paste0(format(AUCSD, nsmall=3, digits=0))) %>%
                                mutate(senSD = paste0(format(senSD * 100, nsmall=1, digits=0))) %>%
                                mutate(speSD = paste0(format(speSD * 100, nsmall=1, digits=0))) %>%
                                mutate(accSD = paste0(format(accSD * 100, nsmall=1, digits=0))) %>%
                                mutate(AUC = paste0(AUC, "±", AUCSD)) %>%
                                mutate(sen = paste0(sen, "±", senSD)) %>%
                                mutate(spe = paste0(spe, "±", speSD)) %>%
                                mutate(acc = paste0(acc, "±", accSD)) %>%
                                select(-AUCSD, -senSD, -speSD, -accSD)

write.csv(performance, file = paste0(filename, ".summary.csv"), row.names=FALSE)

cat("Summary saved in", paste0(filename, ".summary.csv\n"))
