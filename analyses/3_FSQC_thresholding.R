#thresholding analysis - FSQC cut offs
library(effectsize)
library(parameters)
library(plyr)
library(ggplot2)
library(ggseg)
library(ggsegGlasser)
library(dplyr)

#lmer including partial R

######CORTICAL THICKNESS
#cut offs at 3, 2.5, 2, 1.5

# CT with HCP atlas 
#cut off below 3
output_CT_HCP_FSQC3<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 3))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])

  output=cbind(label,coefs,r_FSQC)  
  output_CT_HCP_FSQC3 <- rbind.fill(output_CT_HCP_FSQC3, output)
}
output_CT_HCP_FSQC3$pFDR_FSQC <- p.adjust(output_CT_HCP_FSQC3$p_FSQC, "fdr")


#cut off at 2.5
output_CT_HCP_FSQC2.5<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 2.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])

  output=cbind(label,coefs,r_FSQC)  
  output_CT_HCP_FSQC2.5 <- rbind.fill(output_CT_HCP_FSQC2.5, output)
}
output_CT_HCP_FSQC2.5$pFDR_FSQC <- p.adjust(output_CT_HCP_FSQC2.5$p_FSQC, "fdr")


#cut off at 2
output_CT_HCP_FSQC2<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 2))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])

  output=cbind(label,coefs,r_FSQC)  
  output_CT_HCP_FSQC2 <- rbind.fill(output_CT_HCP_FSQC2, output)
}
output_CT_HCP_FSQC2$pFDR_FSQC <- p.adjust(output_CT_HCP_FSQC2$p_FSQC, "fdr")


#cut off at 1.5
output_CT_HCP_FSQC1.5<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 1.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])

  output=cbind(label,coefs,r_FSQC)  
  output_CT_HCP_FSQC1.5 <- rbind.fill(output_CT_HCP_FSQC1.5, output)
}
output_CT_HCP_FSQC1.5$pFDR_FSQC <- p.adjust(output_CT_HCP_FSQC1.5$p_FSQC, "fdr")




########
######ggseg combined plot for all FSQC cut offs 

output_CT_HCP_FSQC1.5$cutoff = "1.5" 
output_CT_HCP_FSQC2$cutoff = "2" 
output_CT_HCP_FSQC2.5$cutoff = "2.5" 
output_CT_HCP_FSQC3$cutoff = "3"
output_CT_HCP$cutoff = "None"
output_CT_HCP$phenotype <- NULL

output_CT_HCP_fsqccut_all = rbind(output_CT_HCP_FSQC1.5, output_CT_HCP_FSQC2, output_CT_HCP_FSQC2.5, output_CT_HCP_FSQC3, output_CT_HCP)

output_CT_HCP_fsqccut_all$label <- gsub(x = output_CT_HCP_fsqccut_all$label, pattern = "_ROI", replacement = "")  
output_CT_HCP_fsqccut_all$label <- gsub(x = output_CT_HCP_fsqccut_all$label, pattern = "9.", replacement = "9-", fixed=T)  #some labels were not merging and plotting properly bc had . instead of - (label between my file and glasser ggseg file did not match; replace)
output_CT_HCP_fsqccut_all$label <- gsub(x = output_CT_HCP_fsqccut_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output_CT_HCP_fsqccut_all$label <- gsub(x = output_CT_HCP_fsqccut_all$label, pattern = "2.", replacement = "2-", fixed=T)  

write.csv(output_CT_HCP_fsqccut_all, 'output_CT_HCP_fsqccut_all.csv', row.names=F)


subset(output_CT_HCP_fsqccut_all, pFDR_FSQC < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("cutoff",nrow = 5)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('CT - with FSQC cut offs at 3, 2.5, 2, 1.5')


subset(output_CT_HCP_fsqccut_all, pFDR_FSQC < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser, hemisphere = 'left') +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("cutoff",nrow = 5)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + # this line gets rid of "left" (and 'right' if both hemis) underneath
  ggtitle('CT - with FSQC cut offs at 3, 2.5, 2, 1.5')


############################################################################
####SURFACE AREA
# SA with HCP atlas 
#cut off below 3
output_SA_HCP_FSQC3<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, FSQC < 3))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_SA_HCP_FSQC3 <- rbind.fill(output_SA_HCP_FSQC3, output)
}
output_SA_HCP_FSQC3$pFDR_FSQC <- p.adjust(output_SA_HCP_FSQC3$p_FSQC, "fdr")


#cut off at 2.5
output_SA_HCP_FSQC2.5<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, FSQC < 2.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_SA_HCP_FSQC2.5 <- rbind.fill(output_SA_HCP_FSQC2.5, output)
}
output_SA_HCP_FSQC2.5$pFDR_FSQC <- p.adjust(output_SA_HCP_FSQC2.5$p_FSQC, "fdr")


#cut off at 2
output_SA_HCP_FSQC2<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, FSQC < 2))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_SA_HCP_FSQC2 <- rbind.fill(output_SA_HCP_FSQC2, output)
}
output_SA_HCP_FSQC2$pFDR_FSQC <- p.adjust(output_SA_HCP_FSQC2$p_FSQC, "fdr")


#cut off at 1.5
output_SA_HCP_FSQC1.5<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, FSQC < 1.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_SA_HCP_FSQC1.5 <- rbind.fill(output_SA_HCP_FSQC1.5, output)
}
output_SA_HCP_FSQC1.5$pFDR_FSQC <- p.adjust(output_SA_HCP_FSQC1.5$p_FSQC, "fdr")



########
######ggseg combined plot for all FSQC cut offs 

output_SA_HCP_FSQC1.5$cutoff = "1.5" 
output_SA_HCP_FSQC2$cutoff = "2" 
output_SA_HCP_FSQC2.5$cutoff = "2.5" 
output_SA_HCP_FSQC3$cutoff = "3"
output_SA_HCP$cutoff = "None"
output_SA_HCP$phenotype <- NULL

output_SA_HCP_fsqccut_all = rbind(output_SA_HCP_FSQC1.5, output_SA_HCP_FSQC2, output_SA_HCP_FSQC2.5, output_SA_HCP_FSQC3, output_SA_HCP)

output_SA_HCP_fsqccut_all$label <- gsub(x = output_SA_HCP_fsqccut_all$label, pattern = "_ROI", replacement = "")  
output_SA_HCP_fsqccut_all$label <- gsub(x = output_SA_HCP_fsqccut_all$label, pattern = "9.", replacement = "9-", fixed=T)  #some labels were not merging and plotting properly bc had . instead of - (label between my file and glasser ggseg file did not match; replace)
output_SA_HCP_fsqccut_all$label <- gsub(x = output_SA_HCP_fsqccut_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output_SA_HCP_fsqccut_all$label <- gsub(x = output_SA_HCP_fsqccut_all$label, pattern = "2.", replacement = "2-", fixed=T)  


subset(output_SA_HCP_fsqccut_all, pFDR_FSQC < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("cutoff",nrow = 5)+
  labs(fill="Partial r") +
theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('SA - with FSQC cut offs at 3, 2.5, 2, 1.5')


subset(output_SA_HCP_fsqccut_all, pFDR_FSQC < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser, hemisphere = 'left') +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("cutoff",nrow = 5)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + # this line gets rid of "left" (and 'right' if both hemis) underneath
  ggtitle('SA - with FSQC cut offs at 3, 2.5, 2, 1.5')





############################################################################
####CORTICAL VOLUME
# CV with HCP atlas 
#cut off below 3
output_CV_HCP_FSQC3<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, FSQC < 3))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_CV_HCP_FSQC3 <- rbind.fill(output_CV_HCP_FSQC3, output)
}
output_CV_HCP_FSQC3$pFDR_FSQC <- p.adjust(output_CV_HCP_FSQC3$p_FSQC, "fdr")


#cut off at 2.5
output_CV_HCP_FSQC2.5<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, FSQC < 2.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_CV_HCP_FSQC2.5 <- rbind.fill(output_CV_HCP_FSQC2.5, output)
}
output_CV_HCP_FSQC2.5$pFDR_FSQC <- p.adjust(output_CV_HCP_FSQC2.5$p_FSQC, "fdr")


#cut off at 2
output_CV_HCP_FSQC2<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, FSQC < 2))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_CV_HCP_FSQC2 <- rbind.fill(output_CV_HCP_FSQC2, output)
}
output_CV_HCP_FSQC2$pFDR_FSQC <- p.adjust(output_CV_HCP_FSQC2$p_FSQC, "fdr")


#cut off at 1.5
output_CV_HCP_FSQC1.5<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, FSQC < 1.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  ds <- t_to_d(param_tab$t[2:5], param_tab$df_error[2:5])
  
  output=cbind(label,coefs,r_FSQC)  
  output_CV_HCP_FSQC1.5 <- rbind.fill(output_CV_HCP_FSQC1.5, output)
}
output_CV_HCP_FSQC1.5$pFDR_FSQC <- p.adjust(output_CV_HCP_FSQC1.5$p_FSQC, "fdr")



########
######ggseg combined plot for all FSQC cut offs 

output_CV_HCP_FSQC1.5$cutoff = "1.5" 
output_CV_HCP_FSQC2$cutoff = "2" 
output_CV_HCP_FSQC2.5$cutoff = "2.5" 
output_CV_HCP_FSQC3$cutoff = "3"
output_CV_HCP$cutoff = "None"
output_CV_HCP$phenotype <- NULL

output_CV_HCP_fsqccut_all = rbind(output_CV_HCP_FSQC1.5, output_CV_HCP_FSQC2, output_CV_HCP_FSQC2.5, output_CV_HCP_FSQC3, output_CV_HCP)

output_CV_HCP_fsqccut_all$label <- gsub(x = output_CV_HCP_fsqccut_all$label, pattern = "_ROI", replacement = "")  
output_CV_HCP_fsqccut_all$label <- gsub(x = output_CV_HCP_fsqccut_all$label, pattern = "9.", replacement = "9-", fixed=T)  
output_CV_HCP_fsqccut_all$label <- gsub(x = output_CV_HCP_fsqccut_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output_CV_HCP_fsqccut_all$label <- gsub(x = output_CV_HCP_fsqccut_all$label, pattern = "2.", replacement = "2-", fixed=T)  


subset(output_CV_HCP_fsqccut_all, pFDR_FSQC < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("cutoff",nrow = 5)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('CV - with FSQC cut offs at 3, 2.5, 2, 1.5')


subset(output_CV_HCP_fsqccut_all, pFDR_FSQC < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser, hemisphere = 'left') +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("cutoff",nrow = 5)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + # this line gets rid of "left" (and 'right' if both hemis) underneath
  ggtitle('CV - with FSQC cut offs at 3, 2.5, 2, 1.5')




