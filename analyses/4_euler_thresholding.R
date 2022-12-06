#thresholding analysis - EULER cut offs

library(lmerTest)
library(ggpubr)
library(effectsize)
library(parameters)
library(plyr)
library(ggplot2)
library(ggseg)
library(ggsegGlasser)
library(dplyr)
library(colorspace)


#lmer including partial R

#cut offs by median absolute deviation 
#find and define cut off points using mad
x <- subset(df_CT_parcs, surfholes!='NA')
#x <- as.vector(x$surfholes)
mad = mad(x$surfholes)
median = median(x$surfholes)

mad1 = mad + median
mad2 = 2*mad + median
mad3 = 3*mad + median

#also 1/2 MAD 
mad.5 = mad/2 + median
mad1.5 = mad*1.5 + median
mad2.5 = mad*2.5 + median

############################################################################
######CORTICAL THICKNESS
# CT with HCP atlas 

#mad1
output2_CT_HCP_mad1 <- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, surfholes < mad1))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CT_HCP_mad1 <- rbind.fill(output2_CT_HCP_mad1, output)
}
output2_CT_HCP_mad1$pFDR_surfholes <- p.adjust(output2_CT_HCP_mad1$p_surfholes, "fdr")


#mad2
output2_CT_HCP_mad2 <- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, surfholes < mad2))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CT_HCP_mad2 <- rbind.fill(output2_CT_HCP_mad2, output)
}
output2_CT_HCP_mad2$pFDR_surfholes <- p.adjust(output2_CT_HCP_mad2$p_surfholes, "fdr")


#mad3
output2_CT_HCP_mad3 <- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, surfholes < mad3))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CT_HCP_mad3 <- rbind.fill(output2_CT_HCP_mad3, output)
}
output2_CT_HCP_mad3$pFDR_surfholes <- p.adjust(output2_CT_HCP_mad3$p_surfholes, "fdr")


#mad.5 
output2_CT_HCP_mad.5 <- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, surfholes < mad.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CT_HCP_mad.5 <- rbind.fill(output2_CT_HCP_mad.5, output)
}
output2_CT_HCP_mad.5$pFDR_surfholes <- p.adjust(output2_CT_HCP_mad.5$p_surfholes, "fdr")


#mad1.5
output2_CT_HCP_mad1.5 <- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, surfholes < mad1.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CT_HCP_mad1.5 <- rbind.fill(output2_CT_HCP_mad1.5, output)
}
output2_CT_HCP_mad1.5$pFDR_surfholes <- p.adjust(output2_CT_HCP_mad1.5$p_surfholes, "fdr")


#mad2.5
output2_CT_HCP_mad2.5 <- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, surfholes < mad2.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CT_HCP_mad2.5 <- rbind.fill(output2_CT_HCP_mad2.5, output)
}
output2_CT_HCP_mad2.5$pFDR_surfholes <- p.adjust(output2_CT_HCP_mad2.5$p_surfholes, "fdr")



#####################################################################################

###ggseg combined plot 
output2_CT_HCP_mad.5$MAD = "0.5 MAD"
output2_CT_HCP_mad1$MAD = "1 MAD"
output2_CT_HCP_mad1.5$MAD = "1.5 MAD"
output2_CT_HCP_mad2$MAD = "2 MAD" 
output2_CT_HCP_mad2.5$MAD = "2.5 MAD"
output2_CT_HCP_mad3$MAD = "3 MAD"
output2_CT_HCP$MAD = "None" 

output2_CT_HCP_all_mad = rbind(output2_CT_HCP_mad.5, output2_CT_HCP_mad1, output2_CT_HCP_mad1.5, output2_CT_HCP_mad2, output2_CT_HCP_mad2.5, output2_CT_HCP_mad3, output2_CT_HCP)

output2_CT_HCP_all_mad$label <- gsub(x = output2_CT_HCP_all_mad$label, pattern = "_ROI", replacement = "")  
output2_CT_HCP_all_mad$label <- gsub(x = output2_CT_HCP_all_mad$label, pattern = "9.", replacement = "9-", fixed=T)  
output2_CT_HCP_all_mad$label <- gsub(x = output2_CT_HCP_all_mad$label, pattern = "6.", replacement = "6-", fixed=T)  
output2_CT_HCP_all_mad$label <- gsub(x = output2_CT_HCP_all_mad$label, pattern = "2.", replacement = "2-", fixed=T)  



###
subset(output2_CT_HCP_all_mad, pFDR_surfholes < 0.05) %>% 
  group_by(MAD) %>%
  ggseg(mapping=aes(fill=r_surfholes),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("MAD",nrow = 7)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('CT - Euler MAD cut offs')



############################################################################
######SURFACE AREA 

# SA with HCP atlas 
#mad1
output2_SA_HCP_mad1 <- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, surfholes < mad1))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_HCP_mad1 <- rbind.fill(output2_SA_HCP_mad1, output)
}
output2_SA_HCP_mad1$pFDR_surfholes <- p.adjust(output2_SA_HCP_mad1$p_surfholes, "fdr")


#mad2
output2_SA_HCP_mad2 <- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, surfholes < mad2))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_HCP_mad2 <- rbind.fill(output2_SA_HCP_mad2, output)
}
output2_SA_HCP_mad2$pFDR_surfholes <- p.adjust(output2_SA_HCP_mad2$p_surfholes, "fdr")


#mad3
output2_SA_HCP_mad3 <- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, surfholes < mad3))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_HCP_mad3 <- rbind.fill(output2_SA_HCP_mad3, output)
}
output2_SA_HCP_mad3$pFDR_surfholes <- p.adjust(output2_SA_HCP_mad3$p_surfholes, "fdr")



## half MADs
#mad.5 
output2_SA_HCP_mad.5 <- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, surfholes < mad.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_HCP_mad.5 <- rbind.fill(output2_SA_HCP_mad.5, output)
}
output2_SA_HCP_mad.5$pFDR_surfholes <- p.adjust(output2_SA_HCP_mad.5$p_surfholes, "fdr")


#mad1.5 
output2_SA_HCP_mad1.5 <- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, surfholes < mad1.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_HCP_mad1.5 <- rbind.fill(output2_SA_HCP_mad1.5, output)
}
output2_SA_HCP_mad1.5$pFDR_surfholes <- p.adjust(output2_SA_HCP_mad1.5$p_surfholes, "fdr")


#mad2.5
output2_SA_HCP_mad2.5 <- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_SA_parcs, surfholes < mad2.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_HCP_mad2.5 <- rbind.fill(output2_SA_HCP_mad2.5, output)
}
output2_SA_HCP_mad2.5$pFDR_surfholes <- p.adjust(output2_SA_HCP_mad2.5$p_surfholes, "fdr")




###ggseg combined plot 

output2_SA_HCP_mad.5$MAD = "0.5 MAD"
output2_SA_HCP_mad1$MAD = "1 MAD" 
output2_SA_HCP_mad1.5$MAD = "1.5 MAD"
output2_SA_HCP_mad2$MAD = "2 MAD" 
output2_SA_HCP_mad2.5$MAD = "2.5 MAD"
output2_SA_HCP_mad3$MAD = "3 MAD"
output2_SA_HCP$MAD = "None" 
output2_SA_HCP$phenotype <- NULL

output2_SA_HCP_all_mad = rbind(output2_SA_HCP_mad.5, output2_SA_HCP_mad1, output2_SA_HCP_mad1.5, output2_SA_HCP_mad2, output2_SA_HCP_mad2.5, output2_SA_HCP_mad3, output2_SA_HCP)

output2_SA_HCP_all_mad$label <- gsub(x = output2_SA_HCP_all_mad$label, pattern = "_ROI", replacement = "")  
output2_SA_HCP_all_mad$label <- gsub(x = output2_SA_HCP_all_mad$label, pattern = "9.", replacement = "9-", fixed=T)  
output2_SA_HCP_all_mad$label <- gsub(x = output2_SA_HCP_all_mad$label, pattern = "6.", replacement = "6-", fixed=T)  
output2_SA_HCP_all_mad$label <- gsub(x = output2_SA_HCP_all_mad$label, pattern = "2.", replacement = "2-", fixed=T)  

###
subset(output2_SA_HCP_all_mad, pFDR_surfholes < 0.05) %>% 
  group_by(MAD) %>%
  ggseg(mapping=aes(fill=r_surfholes),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("MAD",nrow = 7)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('SA - Euler cut offs at MAD 1, 2, 3')




############################################################################
######CORTICAL VOLUME
# CV with HCP atlas 

#mad1
output2_CV_HCP_mad1 <- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, surfholes < mad1))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_HCP_mad1 <- rbind.fill(output2_CV_HCP_mad1, output)
}
output2_CV_HCP_mad1$pFDR_surfholes <- p.adjust(output2_CV_HCP_mad1$p_surfholes, "fdr")


#mad2
output2_CV_HCP_mad2 <- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, surfholes < mad2))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_HCP_mad2 <- rbind.fill(output2_CV_HCP_mad2, output)
}
output2_CV_HCP_mad2$pFDR_surfholes <- p.adjust(output2_CV_HCP_mad2$p_surfholes, "fdr")


#mad3
output2_CV_HCP_mad3 <- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, surfholes < mad3))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_HCP_mad3 <- rbind.fill(output2_CV_HCP_mad3, output)
}
output2_CV_HCP_mad3$pFDR_surfholes <- p.adjust(output2_CV_HCP_mad3$p_surfholes, "fdr")



## half MADs:
#mad.5 
output2_CV_HCP_mad.5 <- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, surfholes < mad.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_HCP_mad.5 <- rbind.fill(output2_CV_HCP_mad.5, output)
}
output2_CV_HCP_mad.5$pFDR_surfholes <- p.adjust(output2_CV_HCP_mad.5$p_surfholes, "fdr")


#mad1.5 
output2_CV_HCP_mad1.5 <- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, surfholes < mad1.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_HCP_mad1.5 <- rbind.fill(output2_CV_HCP_mad1.5, output)
}
output2_CV_HCP_mad1.5$pFDR_surfholes <- p.adjust(output2_CV_HCP_mad1.5$p_surfholes, "fdr")


#mad2.5
output2_CV_HCP_mad2.5 <- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=subset(df_CV_parcs, surfholes < mad2.5))
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_HCP_mad2.5 <- rbind.fill(output2_CV_HCP_mad2.5, output)
}
output2_CV_HCP_mad2.5$pFDR_surfholes <- p.adjust(output2_CV_HCP_mad2.5$p_surfholes, "fdr")




###ggseg combined plot 
####
output2_CV_HCP_mad.5$MAD = "0.5 MAD"
output2_CV_HCP_mad1$MAD = "1 MAD"
output2_CV_HCP_mad1.5$MAD = "1.5 MAD"
output2_CV_HCP_mad2$MAD = "2 MAD"
output2_CV_HCP_mad2.5$MAD = "2.5 MAD"
output2_CV_HCP_mad3$MAD = "3 MAD"
output2_CV_HCP$MAD = "None"
output2_CV_HCP$phenotype <- NULL

output2_CV_HCP_all_mad = rbind(output2_CV_HCP_mad.5, output2_CV_HCP_mad1, output2_CV_HCP_mad1.5, output2_CV_HCP_mad2, output2_CV_HCP_mad2.5, output2_CV_HCP_mad3, output2_CV_HCP)

output2_CV_HCP_all_mad$label <- gsub(x = output2_CV_HCP_all_mad$label, pattern = "_ROI", replacement = "")  
output2_CV_HCP_all_mad$label <- gsub(x = output2_CV_HCP_all_mad$label, pattern = "9.", replacement = "9-", fixed=T)  
output2_CV_HCP_all_mad$label <- gsub(x = output2_CV_HCP_all_mad$label, pattern = "6.", replacement = "6-", fixed=T)  
output2_CV_HCP_all_mad$label <- gsub(x = output2_CV_HCP_all_mad$label, pattern = "2.", replacement = "2-", fixed=T)  

###
subset(output2_CV_HCP_all_mad, pFDR_surfholes < 0.05) %>% 
  group_by(MAD) %>%
  ggseg(mapping=aes(fill=r_surfholes),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("MAD",nrow = 7)+
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('CV - Euler cut offs at MAD 1, 2, 3')


save.images()