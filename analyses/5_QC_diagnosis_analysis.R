#run lmer on diagnosis with and without controlling for quality, and with thresholding

library(plyr)
library(ggplot2)
library(ggseg)
library(ggsegGlasser)
library(dplyr)
library(colorspace)
library(effectsize)
library(lmerTest)
library(ggpubr)


#Analyses in this script:
# 1. Dx effect only
# 2. Dx effect controlling for FSQC and for Euler
# 3. Dx thresholding analysis - FSQC 
# 4. Dx thresholding analysis - Euler 

#relevel to make control reference 
df_CT_parcs$dx<-as.factor(df_CT_parcs$dx)
df_CT_parcs$dx<-relevel(df_CT_parcs$dx, ref="CN")


#1. Dx only  

##CT (HCP atlas)
##dx only
output_dx_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(label,coefs,d_dxASD)
  output_dx_CT_HCP <- rbind.fill(output_dx_CT_HCP, output)
}
output_dx_CT_HCP$pFDR_dx <- p.adjust(output_dx_CT_HCP$p_dxASD, "fdr")


#2. Dx controlling for FSQC; Dx controlling for Euler

## dx + FSQC 
output_dx_FSQC_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + FSQC + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract only dx
  d_dxASD <- d[1,1]
  
  r <- t_to_r(param_tab$t[3], param_tab$df_error[3]) #extract only FSQC 
  r_FSQC <- r[1,1] 
  
  output=cbind(label,coefs,d_dxASD,r_FSQC)
  output_dx_FSQC_CT_HCP <- rbind.fill(output_dx_FSQC_CT_HCP, output)
}
output_dx_FSQC_CT_HCP$pFDR_dx <- p.adjust(output_dx_FSQC_CT_HCP$p_dxASD, "fdr")
output_dx_FSQC_CT_HCP$pFDR_FSQC <- p.adjust(output_dx_FSQC_CT_HCP$p_FSQC, "fdr")


## dx + Euler
output_dx_euler_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + surfholes + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract only dx
  d_dxASD <- d[1,1]
  
  r <- t_to_r(param_tab$t[3], param_tab$df_error[3]) #extract only euler 
  r_euler <- r[1,1] 
  
  output=cbind(label,coefs,d_dxASD,r_euler)
  output_dx_euler_CT_HCP <- rbind.fill(output_dx_euler_CT_HCP, output)
}
output_dx_euler_CT_HCP$pFDR_dx <- p.adjust(output_dx_euler_CT_HCP$p_dxASD, "fdr")
output_dx_euler_CT_HCP$pFDR_euler <- p.adjust(output_dx_euler_CT_HCP$p_surfholes, "fdr")



####
##ggseg combined (dx, dx+FSQC, dx+euler - corrected and uncorrected)

###ggseg combined plot ASD + CN
output_dx_CT_HCP$model <- "Diagnosis only"
output_dx_CT_HCP_temp <- output_dx_CT_HCP %>% select(label, t_dxASD, p_dxASD, d_dxASD, pFDR_dx, model)
output_dx_FSQC_CT_HCP$model <- "Diagnosis + FSQC"
output_dx_FSQC_CT_HCP_temp <- output_dx_FSQC_CT_HCP %>% select(label, t_dxASD, p_dxASD, d_dxASD, pFDR_dx, model)
output_dx_euler_CT_HCP$model <- "Diagnosis + Euler"
output_dx_euler_CT_HCP_temp <- output_dx_euler_CT_HCP %>% select(label, t_dxASD, p_dxASD, d_dxASD, pFDR_dx, model)

output_dx_CT_HCP_all = rbind(output_dx_CT_HCP_temp, output_dx_FSQC_CT_HCP_temp, output_dx_euler_CT_HCP_temp)
rm(output_dx_CT_HCP_temp, output_dx_FSQC_CT_HCP_temp, output_dx_euler_CT_HCP_temp)

output_dx_CT_HCP_all$label <- gsub(x = output_dx_CT_HCP_all$label, pattern = "_ROI", replacement = "")  
output_dx_CT_HCP_all$label <- gsub(x = output_dx_CT_HCP_all$label, pattern = "9.", replacement = "9-", fixed=T)  
output_dx_CT_HCP_all$label <- gsub(x = output_dx_CT_HCP_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output_dx_CT_HCP_all$label <- gsub(x = output_dx_CT_HCP_all$label, pattern = "2.", replacement = "2-", fixed=T)  

output_dx_CT_HCP_all$model <- as.factor(output_dx_CT_HCP_all$model)
output_dx_CT_HCP_all$model <- factor(output_dx_CT_HCP_all$model,
                                              levels = c("Diagnosis only", "Diagnosis + FSQC", "Diagnosis + Euler"))


plot1 <- subset(output_dx_CT_HCP_all, pFDR_dx < 0.05) %>% 
  group_by(model) %>%
  ggseg(mapping=aes(fill=d_dxASD),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("model",nrow = 3) + theme_linedraw() +
  labs(fill="Cohen's d dx") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle("Corrected")


plot2 <- subset(output_dx_CT_HCP_all, p_dxASD < 0.05) %>% 
  group_by(model) %>%
  ggseg(mapping=aes(fill=d_dxASD),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("model",nrow = 3) + theme_linedraw() +
  labs(fill="Cohen's d dx") +
  #theme(legend.position = "none", axis.title.x = element_blank())+ ##
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle("Uncorrected")


#combine with ggarrange 
library(ggpubr)
plot <- ggarrange(plot1, plot2, ncol=2, nrow=1, common.legend = TRUE, legend="right")

annotate_figure(plot, top = text_grob("CT diagnosis Cohen's d - with and without control for quality", size=14, hjust = 0.55))
ggsave('CT_dx_all_ggarrange.png')




#3. DIAGNOSIS thresholding analysis - FSQC 

#DX thresholding analysis 
#CT: FSQC cut offs at 3, 2.5, 2, 1.5, 1 

##dx only with cut off at 3 (N=1801)
output_dx3_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 3))
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(label,coefs,d_dxASD)
  output_dx3_CT_HCP <- rbind.fill(output_dx3_CT_HCP, output)
}
output_dx3_CT_HCP$pFDR_dx <- p.adjust(output_dx3_CT_HCP$p_dxASD, "fdr")


##dx only with cut off at 2.5 (N=1727)
output_dx2.5_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 2.5))
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(label,coefs,d_dxASD)
  output_dx2.5_CT_HCP <- rbind.fill(output_dx2.5_CT_HCP, output)
}
output_dx2.5_CT_HCP$pFDR_dx <- p.adjust(output_dx2.5_CT_HCP$p_dxASD, "fdr")


##dx only with cut off at 2 (N=1579)
output_dx2_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 2))
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(label,coefs,d_dxASD)
  output_dx2_CT_HCP <- rbind.fill(output_dx2_CT_HCP, output)
}
output_dx2_CT_HCP$pFDR_dx <- p.adjust(output_dx2_CT_HCP$p_dxASD, "fdr")


##dx only with cut off at 1.5 (N=1377)
output_dx1.5_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC < 1.5))
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(label,coefs,d_dxASD)
  output_dx1.5_CT_HCP <- rbind.fill(output_dx1.5_CT_HCP, output)
}
output_dx1.5_CT_HCP$pFDR_dx <- p.adjust(output_dx1.5_CT_HCP$p_dxASD, "fdr")


##dx only with cut off at 1 (N = 741)
output_dx1_CT_HCP<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ dx + age + age2 + sex + (1|site)")), data=subset(df_CT_parcs, FSQC == 1))
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(label,coefs,d_dxASD)
  output_dx1_CT_HCP <- rbind.fill(output_dx1_CT_HCP, output)
}
output_dx1_CT_HCP$pFDR_dx <- p.adjust(output_dx1_CT_HCP$p_dxASD, "fdr")


#ggseg combined CT FSQC cut off ablation:

output_dx1_CT_HCP$cutoff = "1"
output_dx1.5_CT_HCP$cutoff = "1.5" 
output_dx2_CT_HCP$cutoff = "2" 
output_dx2.5_CT_HCP$cutoff = "2.5" 
output_dx3_CT_HCP$cutoff = "3"
output_dx_CT_HCP$cutoff = "None"
output_dx_CT_HCP$model <- NULL

output_dx_CT_HCP_all_FSQCcut = rbind(output_dx1_CT_HCP, output_dx1.5_CT_HCP, output_dx2_CT_HCP, output_dx2.5_CT_HCP, output_dx3_CT_HCP, output_dx_CT_HCP)

output_dx_CT_HCP_all_FSQCcut$label <- gsub(x = output_dx_CT_HCP_all_FSQCcut$label, pattern = "_ROI", replacement = "")  
output_dx_CT_HCP_all_FSQCcut$label <- gsub(x = output_dx_CT_HCP_all_FSQCcut$label, pattern = "9.", replacement = "9-", fixed=T)  
output_dx_CT_HCP_all_FSQCcut$label <- gsub(x = output_dx_CT_HCP_all_FSQCcut$label, pattern = "6.", replacement = "6-", fixed=T)  
output_dx_CT_HCP_all_FSQCcut$label <- gsub(x = output_dx_CT_HCP_all_FSQCcut$label, pattern = "2.", replacement = "2-", fixed=T)  


write.csv(output_dx_CT_HCP_all_FSQCcut, 'output_dx_CT_HCP_all_FSQCcut.csv', row.names=F)


subset(output_dx_CT_HCP_all_FSQCcut, pFDR_dx < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=d_dxASD),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0, rev = TRUE) +
  facet_wrap("cutoff",nrow = 6) + theme_linedraw() +
  labs(fill="Cohen's d dx") +
  #theme(legend.position = "none", axis.title.x = element_blank(), 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle("Cortical thickness diagnosis Cohen's d with FSQC cut offs")

subset(output_dx_CT_HCP_all_FSQCcut, p_dxASD < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=d_dxASD),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0, rev = TRUE) +
  facet_wrap("cutoff",nrow = 6) + theme_linedraw() +
  labs(fill="Cohen's d dx") +
  #theme(legend.position = "none", axis.title.x = element_blank(), 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle("Cortical thickness diagnosis Cohen's d (uncorrected) with FSQC cut offs")




# 4. DIAGNOSIS thresholding analysis - Euler for CT 
#cut offs at MAD 1, 2, 3 (174, 245, 316) 

##dx only with cut off at MAD 1 (174) (N=1428)
output_dx174_CT_HCP<- data.frame()
for (j in names(df_CT_parcs_174[98:457])) {
  model <- lmer(as.formula(paste(j, "~ dx + age + age2 + sex + (1|site)")), data=df_CT_parcs_174)
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(j,coefs,d_dxASD)
  output_dx174_CT_HCP <- rbind.fill(output_dx174_CT_HCP, output)
}
output_dx174_CT_HCP$pFDR_dx <- p.adjust(output_dx174_CT_HCP$p_dxASD, "fdr")


##dx only with cut off at MAD 2 (245) (N=1689)
output_dx245_CT_HCP<- data.frame()
for (j in names(df_CT_parcs_245[98:457])) {
  model <- lmer(as.formula(paste(j, "~ dx + age + age2 + sex + (1|site)")), data=df_CT_parcs_245)
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(j,coefs,d_dxASD)
  output_dx245_CT_HCP <- rbind.fill(output_dx245_CT_HCP, output)
}
output_dx245_CT_HCP$pFDR_dx <- p.adjust(output_dx245_CT_HCP$p_dxASD, "fdr")


##dx only with cut off at MAD 3 (316) (N=1761)
output_dx316_CT_HCP<- data.frame()
for (j in names(df_CT_parcs_316[98:457])) {
  model <- lmer(as.formula(paste(j, "~ dx + age + age2 + sex + (1|site)")), data=df_CT_parcs_316)
  #Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  d <- t_to_d(param_tab$t[2], param_tab$df_error[2]) #extract dx only
  d_dxASD <- d[1,1]
  
  output=cbind(j,coefs,d_dxASD)
  output_dx316_CT_HCP <- rbind.fill(output_dx316_CT_HCP, output)
}
output_dx316_CT_HCP$pFDR_dx <- p.adjust(output_dx316_CT_HCP$p_dxASD, "fdr")



#ggseg combined CT surfholes cut off ablation:
output_dx174_CT_HCP$cutoff = "174 (1 MAD)"
output_dx245_CT_HCP$cutoff = "245 (2 MAD)" 
output_dx316_CT_HCP$cutoff = "316 (3 MAD)" 
outputdx_CT_HPC_lmer$cutoff = "None"

output_dx_CT_HCP_all_eulercut = rbind(output_dx174_CT_HCP, output_dx245_CT_HCP, output_dx316_CT_HCP, outputdx_CT_HPC_lmer)

names(output_dx_CT_HCP_all_eulercut)[1] <- "label"

output_dx_CT_HCP_all_eulercut$label <- gsub(x = output_dx_CT_HCP_all_eulercut$label, pattern = "_ROI", replacement = "")
output_dx_CT_HCP_all_eulercut$label <- gsub(x = output_dx_CT_HCP_all_eulercut$label, pattern = "9.", replacement = "9-", fixed=T)
output_dx_CT_HCP_all_eulercut$label <- gsub(x = output_dx_CT_HCP_all_eulercut$label, pattern = "6.", replacement = "6-", fixed=T)
output_dx_CT_HCP_all_eulercut$label <- gsub(x = output_dx_CT_HCP_all_eulercut$label, pattern = "2.", replacement = "2-", fixed=T)


output_dx_CT_HCP_all_eulercut$cutoff <- factor(output_dx_CT_HCP_all_eulercut$cutoff, 
                                     levels = c("None", "316 (3 MAD)", "245 (2 MAD)", "174 (1 MAD)"))

subset(output_dx_CT_HCP_all_eulercut, pFDR_dx < 0.05) %>% 
  group_by(cutoff) %>%
  ggseg(mapping=aes(fill=d_dxASD),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,  rev = TRUE) +
  facet_wrap("cutoff",nrow = 4) + theme_minimal() +
  labs(fill="Cohen's d") + 
  theme(legend.position = "none", axis.title.x = element_blank())+ ##
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  
  


