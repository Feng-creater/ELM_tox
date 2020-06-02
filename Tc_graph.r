library(ggplot2)
library(tidyr)
library(dplyr)


tc1_0 <- read.csv("E:/reproductive/models/rf.cv-tc1.0.csv", stringsAsFactors = FALSE)
tc0_99 <- read.csv("E:/reproductive/models/rf.cv-tc0.99.csv", stringsAsFactors = FALSE)
tc0_95 <- read.csv("E:/reproductive/models/rf.cv-tc0.95.csv", stringsAsFactors = FALSE)
tc0_9 <- read.csv("E:/reproductive/models/rf.cv-tc0.9.csv", stringsAsFactors = FALSE)
tc0_8 <- read.csv("E:/reproductive/models/rf.cv-tc0.8.csv", stringsAsFactors = FALSE)
tc0_7 <- read.csv("E:/reproductive/models/rf.cv-tc0.7.csv", stringsAsFactors = FALSE)
tc0_6 <- read.csv("E:/reproductive/models/rf.cv-tc0.6.csv", stringsAsFactors = FALSE)
tc0_5 <- read.csv("E:/reproductive/models/rf.cv-tc0.5.csv", stringsAsFactors = FALSE)

tc1_0 <- select(tc1_0, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=1.0)
tc0_99 <- select(tc0_99, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=0.99)
tc0_95 <- select(tc0_95, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=0.95)
tc0_9 <- select(tc0_9, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=0.9)
tc0_8 <- select(tc0_8, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=0.8)
tc0_7 <- select(tc0_7, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=0.7)
tc0_6 <- select(tc0_6, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=0.6)
tc0_5 <- select(tc0_5, 1, AUC, sen, spe, acc, AUCSD, senSD, speSD, accSD) %>% separate(1, c("a", "Fingerprint", "c")) %>% select(-a, -c) %>% mutate(tc=0.5)


tc_all <- rbind(tc1_0, tc0_99, tc0_95, tc0_9, tc0_8, tc0_7, tc0_6, tc0_5)

colnames(tc_all) <- c("Fingerprint", "AUC", "SEN", "SPE", "ACC", "AUCSD", "SENSD", "SPESD", "ACCSD", "tc")


AD2D <- filter(tc_all, Fingerprint=="AD2D") %>% select(tc, AUC, SEN, SPE, ACC)
AD2D_SD <- filter(tc_all, Fingerprint=="AD2D") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

APC2D <- filter(tc_all, Fingerprint=="APC2D") %>% select(tc, AUC, SEN, SPE, ACC)
APC2D_SD <- filter(tc_all, Fingerprint=="APC2D") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

EStateFP <- filter(tc_all, Fingerprint=="EStateFP") %>% select(tc, AUC, SEN, SPE, ACC)
EStateFP_SD <- filter(tc_all, Fingerprint=="EStateFP") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

KRFP <- filter(tc_all, Fingerprint=="KRFP") %>% select(tc, AUC, SEN, SPE, ACC)
KRFP_SD <- filter(tc_all, Fingerprint=="KRFP") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

KRFPC <- filter(tc_all, Fingerprint=="KRFPC") %>% select(tc, AUC, SEN, SPE, ACC)
KRFPC_SD <- filter(tc_all, Fingerprint=="KRFPC") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

MACCSFP <- filter(tc_all, Fingerprint=="MACCSFP") %>% select(tc, AUC, SEN, SPE, ACC)
MACCSFP_SD <- filter(tc_all, Fingerprint=="MACCSFP") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

PubchemFP <- filter(tc_all, Fingerprint=="PubchemFP") %>% select(tc, AUC, SEN, SPE, ACC)
PubchemFP_SD <- filter(tc_all, Fingerprint=="PubchemFP") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

SubFP <- filter(tc_all, Fingerprint=="SubFP") %>% select(tc, AUC, SEN, SPE, ACC)
SubFP_SD <- filter(tc_all, Fingerprint=="SubFP") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

SubFPC <- filter(tc_all, Fingerprint=="SubFPC") %>% select(tc, AUC, SEN, SPE, ACC)
SubFPC_SD <- filter(tc_all, Fingerprint=="SubFPC") %>% select(tc, AUCSD, SENSD, SPESD, ACCSD) %>% mutate(AUC=AUCSD, SEN=SENSD, SPE=SPESD, ACC=ACCSD) %>% select(-AUCSD, -SENSD, -SPESD, -ACCSD)

#AD2D
# AD2D <- read.csv('AD2D.csv')

AD2D_long <- gather(AD2D,key = performance,value = num,-tc)

# AD2D_SD <- read.csv('AD2D_SD.csv')

AD2D_SD_long <- gather(AD2D_SD,key = performance,value = sd,-tc)

AD2D_all <- right_join(AD2D_long, AD2D_SD_long, by = c('tc','performance'))
AD2D_all_se <- mutate(AD2D_all, se = sd / 10 * 100,num = num * 100)

AD2D_graph <- ggplot(AD2D_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/AD2D_line.pdf", width = 4, height = 3.5) 

#APC2D
# APC2D <- read.csv('APC2D.csv')

APC2D_long <- gather(APC2D,key = performance,value = num,-tc)

# APC2D_SD <- read.csv('APC2D_SD.csv')

APC2D_SD_long <- gather(APC2D_SD,key = performance,value = sd,-tc)

APC2D_all <- right_join(APC2D_long, APC2D_SD_long, by = c('tc','performance'))
APC2D_all_se <- mutate(APC2D_all, se = sd / 10 * 100,num = num * 100)

APC2D_graph <- ggplot(APC2D_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/APC2D_line.pdf", width = 4, height = 3.5) 

#EStateFP
# EStateFP <- read.csv('EStateFP.csv')

EStateFP_long <- gather(EStateFP,key = performance,value = num,-tc)

# EStateFP_SD <- read.csv('EStateFP_SD.csv')

EStateFP_SD_long <- gather(EStateFP_SD,key = performance,value = sd,-tc)

EStateFP_all <- right_join(EStateFP_long, EStateFP_SD_long, by = c('tc','performance'))
EStateFP_all_se <- mutate(EStateFP_all, se = sd / 10 * 100,num = num * 100)

EStateFP_graph <- ggplot(EStateFP_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/EStateFP_line.pdf", width = 4, height = 3.5) 

#KRFPC
# KRFPC <- read.csv('KRFPC.csv')

KRFPC_long <- gather(KRFPC,key = performance,value = num,-tc)

# KRFPC_SD <- read.csv('KRFPC_SD.csv')

KRFPC_SD_long <- gather(KRFPC_SD,key = performance,value = sd,-tc)

KRFPC_all <- right_join(KRFPC_long, KRFPC_SD_long, by = c('tc','performance'))
KRFPC_all_se <- mutate(KRFPC_all, se = sd / 10 * 100,num = num * 100)

KRFPC_graph <- ggplot(KRFPC_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/KRFPC_line.pdf", width = 4, height = 3.5) 

#KRFP
# KRFP <- read.csv('KRFP.csv')

KRFP_long <- gather(KRFP,key = performance,value = num,-tc)

# KRFP_SD <- read.csv('KRFP_SD.csv')

KRFP_SD_long <- gather(KRFP_SD,key = performance,value = sd,-tc)

KRFP_all <- right_join(KRFP_long, KRFP_SD_long, by = c('tc','performance'))
KRFP_all_se <- mutate(KRFP_all, se = sd / 10 * 100,num = num * 100)

KRFP_graph <- ggplot(KRFP_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/KRFP_line.pdf", width = 4, height = 3.5) 

#MACCSFP
# MACCSFP <- read.csv('MACCSFP.csv')

MACCSFP_long <- gather(MACCSFP,key = performance,value = num,-tc)

# MACCSFP_SD <- read.csv('MACCSFP_SD.csv')

MACCSFP_SD_long <- gather(MACCSFP_SD,key = performance,value = sd,-tc)

MACCSFP_all <- right_join(MACCSFP_long, MACCSFP_SD_long, by = c('tc','performance'))
MACCSFP_all_se <- mutate(MACCSFP_all, se = sd / 10 * 100,num = num * 100)

MACCSFP_graph <- ggplot(MACCSFP_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/MACCSFP_line.pdf", width = 4, height = 3.5) 

#PubchemFP
# PubchemFP <- read.csv('PubchemFP.csv')

PubchemFP_long <- gather(PubchemFP,key = performance,value = num,-tc)

# PubchemFP_SD <- read.csv('PubchemFP_SD.csv')

PubchemFP_SD_long <- gather(PubchemFP_SD,key = performance,value = sd,-tc)

PubchemFP_all <- right_join(PubchemFP_long, PubchemFP_SD_long, by = c('tc','performance'))
PubchemFP_all_se <- mutate(PubchemFP_all, se = sd / 10 * 100,num = num * 100)

PubchemFP_graph <- ggplot(PubchemFP_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/PubchemFP_line.pdf", width = 4, height = 3.5) 

#SubFPC
# SubFPC <- read.csv('SubFPC.csv')

SubFPC_long <- gather(SubFPC,key = performance,value = num,-tc)

# SubFPC_SD <- read.csv('SubFPC_SD.csv')

SubFPC_SD_long <- gather(SubFPC_SD,key = performance,value = sd,-tc)

SubFPC_all <- right_join(SubFPC_long, SubFPC_SD_long, by = c('tc','performance'))
SubFPC_all_se <- mutate(SubFPC_all, se = sd / 10 * 100,num = num * 100)

SubFPC_graph <- ggplot(SubFPC_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/SubFPC_line.pdf", width = 4, height = 3.5) 

#SubFP
# SubFP <- read.csv('SubFP.csv')

SubFP_long <- gather(SubFP,key = performance,value = num,-tc)

# SubFP_SD <- read.csv('SubFP_SD.csv')

SubFP_SD_long <- gather(SubFP_SD,key = performance,value = sd,-tc)

SubFP_all <- right_join(SubFP_long, SubFP_SD_long, by = c('tc','performance'))
SubFP_all_se <- mutate(SubFP_all, se = sd / 10 * 100,num = num * 100)

SubFP_graph <- ggplot(SubFP_all_se,aes(tc,num,colour = performance)) +
    geom_point()+
    geom_line(size=1)+
    geom_errorbar(aes(ymin = num-se, ymax = num+se), width = 0.02)+
    labs(y = 'Performance(%)', x = 'Tanimoto coefficient') +
    ylim(60,100) +
    theme_bw() +
    theme(legend.position = c(0.1,0.98),legend.justification = c(0.1,0.98))+
    theme(legend.direction = "horizontal")+
    theme(legend.title = element_blank())+
    theme(panel.border = element_rect(size=1)) +
    theme(panel.grid = element_blank())

ggsave( file = "Tc_graph/SubFP_line.pdf", width = 4, height = 3.5) 
