library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)


csvlist = list.files(path="models", pattern="rf.svm.xgb.mix.top.*.cv.csv") 

performance_all <- data.frame()
for (csv_file in csvlist){
    performance <- read.csv(paste0("models/", csv_file), stringsAsFactors = FALSE)
    performance[1] <- csv_file
    performance_all <- rbind(performance_all, performance)
}



performance_all_summary <- performance_all %>% select(-mcc_mean, -prec_mean, -F1_mean, -tp_mean, -tn_mean, -fp_mean, -fn_mean, -mcc_sd, -prec_sd, -F1_sd, -tp_sd, -tn_sd, -fp_sd, -fn_sd) %>%
                                mutate(AUC = AUC_mean, sen = sen_mean, spe = spe_mean, acc = acc_mean, AUCSD = AUC_sd, senSD = sen_sd, speSD= spe_sd, accSD = acc_sd) %>%
                                select(-AUC_mean, -sen_mean, -spe_mean, -acc_mean, -AUC_sd, -sen_sd, -spe_sd, -acc_sd) %>%
                                mutate(AUC = paste0(format(AUC, nsmall=4, digits=0))) %>%
                                mutate(sen = paste0(format(sen * 100, nsmall=2, digits=0))) %>%
                                mutate(spe = paste0(format(spe * 100, nsmall=2, digits=0))) %>%
                                mutate(acc = paste0(format(acc * 100, nsmall=2, digits=0))) %>%
                                mutate(AUCSD = paste0(format(AUCSD, nsmall=4, digits=0))) %>%
                                mutate(senSD = paste0(format(senSD * 100, nsmall=2, digits=0))) %>%
                                mutate(speSD = paste0(format(speSD * 100, nsmall=2, digits=0))) %>%
                                mutate(accSD = paste0(format(accSD * 100, nsmall=2, digits=0))) %>%
                                mutate(AUC = paste0(AUC, "±", AUCSD)) %>%
                                mutate(sen = paste0(sen, "±", senSD)) %>%
                                mutate(spe = paste0(spe, "±", speSD)) %>%
                                mutate(acc = paste0(acc, "±", accSD)) %>%
                                select(-AUCSD, -senSD, -speSD, -accSD)

write.csv(performance_all_summary, file = paste0("models/ensemble.summary.csv"), row.names=FALSE)


csvlist_ind = list.files(path="models", pattern="rf.svm.xgb.mix.top.*.Ind.csv") 

performance_all_ind <- data.frame()
for (csv_file in csvlist_ind){
    performance <- read.csv(paste0("models/", csv_file), stringsAsFactors = FALSE)
    performance[1] <- csv_file
    performance_all_ind <- rbind(performance_all_ind, performance)
}

write.csv(performance_all_ind, file = paste0("models/ensemble.summary.Ind.csv"), row.names=FALSE)






performance_plot_CV <- performance_all %>% transmute(ensemble_Number = X, AUC = AUC_mean, ACC = acc_mean, SEN = sen_mean, SPE = spe_mean) %>%
                                            mutate(ensemble_Number = str_replace(ensemble_Number, "rf.svm.xgb.mix.top", "")) %>%
                                            mutate(ensemble_Number = as.numeric(str_replace(ensemble_Number, ".cv.csv", ""))) %>%
                                            arrange(ensemble_Number) %>%
                                            gather("metric", "value", -ensemble_Number) %>%
                                            mutate(Validate = "Cross-validation")
                                            

performance_plot_CV$metric <- factor(performance_plot_CV$metric, levels = c("AUC", "ACC", "SEN", "SPE"))



performance_plot_ind <- performance_all_ind %>% transmute(ensemble_Number = X, AUC = AUC, ACC = acc, SEN = sen, SPE = spe) %>%
                                            mutate(ensemble_Number = str_replace(ensemble_Number, "rf.svm.xgb.mix.top", "")) %>%
                                            mutate(ensemble_Number = as.numeric(str_replace(ensemble_Number, ".Ind.csv", ""))) %>%
                                            arrange(ensemble_Number) %>%
                                            gather("metric", "value", -ensemble_Number) %>%
                                            mutate(Validate = "External validation")
                                            

performance_plot_ind$metric <- factor(performance_plot_ind$metric, levels = c("AUC", "ACC", "SEN", "SPE"))

performance_plot <- rbind(performance_plot_CV, performance_plot_ind)

performance_plot$Validate <- factor(performance_plot$Validate, levels = c("Cross-validation", "External validation"))


plot_CV <- ggplot(performance_plot, aes(x = ensemble_Number, y = value * 100, color = Validate))  +
                    geom_point()+
                    geom_line(size=1)+
                    # ylim(80,100) +
                    facet_wrap(~metric, ncol = 1, scales = "free") +
                    scale_x_continuous("Number of Models", expand = c(0.03, 0), breaks = seq(0, 27, by = 1)) +
                    scale_y_continuous("Performance (%)", expand = expand_scale(mult = 0.1)) +
                    theme_bw() +
                    theme(legend.position = "top", legend.justification = "left", legend.direction = "horizontal")+
                    theme(legend.title = element_blank())+
                    theme(panel.border = element_rect(size=1)) +
                    theme(panel.grid.minor = element_blank())



ggsave(filename = 'performance_ensemble_CV.pdf', plot = plot_CV, width = 6, height = 9)

print("All finished")








