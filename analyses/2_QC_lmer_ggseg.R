#linear mixed effects models: relationship between each QC metric and each cortical phenotype (cortical thickness, volume, surface area)
#with Desikan Killianey and Glasser (HCP) parcellations

library(effectsize)
library(parameters)
library(plyr)
library(lmerTest)
library(ggpubr)

library(ggplot2)
library(ggseg)
library(ggsegGlasser)
library(dplyr)
library(colorspace)


#lmer including partial R 

#OUTPUTS = FSQC; output2 = surfholes; 3 = motion; 4 = pondrAI QC
#analyses run below:
#1. FSQC: CT (DK and HCP); SA (DK and HCP); CV (DK and HCP)
#2. Euler  """
#3. Motion """
#4. pondrAI """


###################################################################################################
####FSQC 

#CT
#CT with DK atlas
output_CT_DK<- data.frame()
for (label in names(df_CT_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  ###Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  output=cbind(label,coefs,r_FSQC)
  output_CT_DK <- rbind.fill(output_CT_DK, output)
}
output_CT_DK$pFDR_FSQC <- p.adjust(output_CT_DK$p_FSQC, "fdr")

# CT with HPC atlas 
output_CT_HPC<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]

  output=cbind(label,coefs,r_FSQC)  
  output_CT_HPC <- rbind.fill(output_CT_HPC, output)
}
output_CT_HPC$pFDR_FSQC <- p.adjust(output_CT_HPC$p_FSQC, "fdr")

#SA
#SA with DK atlas
output_SA_DK<- data.frame()
for (label in names(df_SA_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  ###Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  output=cbind(label,coefs,r_FSQC)
  output_SA_DK <- rbind.fill(output_SA_DK, output)
}
output_SA_DK$pFDR_FSQC <- p.adjust(output_SA_DK$p_FSQC, "fdr")

# SA with HPC atlas 
output_SA_HPC<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  output=cbind(label,coefs,r_FSQC)  
  output_SA_HPC <- rbind.fill(output_SA_HPC, output)
}
output_SA_HPC$pFDR_FSQC <- p.adjust(output_SA_HPC$p_FSQC, "fdr")


#CV
#CV with DK atlas
output_CV_DK<- data.frame()
for (label in names(df_CV_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  ###Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  output=cbind(label,coefs,r_FSQC)
  output_CV_DK <- rbind.fill(output_CV_DK, output)
}
output_CV_DK$pFDR_FSQC <- p.adjust(output_CV_DK$p_FSQC, "fdr")

# CV with HPC atlas 
output_CV_HPC<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ FSQC + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_FSQC <- rs[1,1]
  
  output=cbind(label,coefs,r_FSQC)  
  output_CV_HPC <- rbind.fill(output_CV_HPC, output)
}
output_CV_HPC$pFDR_FSQC <- p.adjust(output_CV_HPC$p_FSQC, "fdr")




###################################################################################################
###EULER #
#CT
# CT with DK atlas 
output2_CT_DK<- data.frame()
for (label in names(df_CT_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  ###Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]

  output=cbind(label,coefs,r_surfholes)
  output2_CT_DK <- rbind.fill(output2_CT_DK, output)
}
output2_CT_DK$pFDR_surfholes <- p.adjust(output2_CT_DK$p_surfholes, "fdr")

# CT with HPC atlas 
output2_CT_HPC<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]

  output=cbind(label,coefs,r_surfholes)  
  output2_CT_HPC <- rbind.fill(output2_CT_HPC, output)
}
output2_CT_HPC$pFDR_surfholes <- p.adjust(output2_CT_HPC$p_surfholes, "fdr")


#SA
# SA with DK atlas 
output2_SA_DK<- data.frame()
for (label in names(df_SA_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_DK <- rbind.fill(output2_SA_DK, output)
}
output2_SA_DK$pFDR_surfholes <- p.adjust(output2_SA_DK$p_surfholes, "fdr")

# SA with HPC atlas 
output2_SA_HPC<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_SA_HPC <- rbind.fill(output2_SA_HPC, output)
}
output2_SA_HPC$pFDR_surfholes <- p.adjust(output2_SA_HPC$p_surfholes, "fdr")


#CV
# CV with DK atlas 
output2_CV_DK<- data.frame()
for (label in names(df_CV_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_DK <- rbind.fill(output2_CV_DK, output)
}
output2_CV_DK$pFDR_surfholes <- p.adjust(output2_CV_DK$p_surfholes, "fdr")

# CV with HPC atlas 
output2_CV_HPC<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ surfholes + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_surfholes <- rs[1,1]
  
  output=cbind(label,coefs,r_surfholes)  
  output2_CV_HPC <- rbind.fill(output2_CV_HPC, output)
}
output2_CV_HPC$pFDR_surfholes <- p.adjust(output2_CV_HPC$p_surfholes, "fdr")






###################################################################################################
###Motion QC

#CT
# CT DK
output3_CT_DK<- data.frame()
for (label in names(df_CT_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ MOTION_QC + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  ###Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_MOTION_QC <- rs[1,1]

  output=cbind(label,coefs,r_MOTION_QC)
  output3_CT_DK <- rbind.fill(output3_CT_DK, output)
}
output3_CT_DK$pFDR_MOTION_QC <- p.adjust(output3_CT_DK$p_MOTION_QC, "fdr")

# CT with HPC atlas 
output3_CT_HPC<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ MOTION_QC + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_MOTION_QC <- rs[1,1]

  output=cbind(label,coefs,r_MOTION_QC)  
  output3_CT_HPC <- rbind.fill(output3_CT_HPC, output)
}
output3_CT_HPC$pFDR_MOTION_QC <- p.adjust(output3_CT_HPC$p_MOTION_QC, "fdr")


#SA
# SA DK
output3_SA_DK<- data.frame()
for (label in names(df_SA_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ MOTION_QC + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  ###Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_MOTION_QC <- rs[1,1]
  
  output=cbind(label,coefs,r_MOTION_QC)
  output3_SA_DK <- rbind.fill(output3_SA_DK, output)
}
output3_SA_DK$pFDR_MOTION_QC <- p.adjust(output3_SA_DK$p_MOTION_QC, "fdr")

# SA with HPC atlas 
output3_SA_HPC<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ MOTION_QC + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_MOTION_QC <- rs[1,1]

  output=cbind(label,coefs,r_MOTION_QC)  
  output3_SA_HPC <- rbind.fill(output3_SA_HPC, output)
}
output3_SA_HPC$pFDR_MOTION_QC <- p.adjust(output3_SA_HPC$p_MOTION_QC, "fdr")


#CV
# CV DK
output3_CV_DK<- data.frame()
for (label in names(df_CV_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ MOTION_QC + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  ###Store coefficients
  coefs<- parseLmer(model)
  
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_MOTION_QC <- rs[1,1]
  
  output=cbind(label,coefs,r_MOTION_QC)
  output3_CV_DK <- rbind.fill(output3_CV_DK, output)
}
output3_CV_DK$pFDR_MOTION_QC <- p.adjust(output3_CV_DK$p_MOTION_QC, "fdr")

# CV with HPC atlas 
output3_CV_HPC<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ MOTION_QC + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_MOTION_QC <- rs[1,1]

  output=cbind(label,coefs,r_MOTION_QC)  
  output3_CV_HPC <- rbind.fill(output3_CV_HPC, output)
}
output3_CV_HPC$pFDR_MOTION_QC <- p.adjust(output3_CV_HPC$p_MOTION_QC, "fdr")


###################################################################################################
#pondrAI QC

#CT
# CT with DK atlas 
output4_CT_DK<- data.frame()
for (label in names(df_CT_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ pondrAIQC + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_pondrAIQC <- rs[1,1]
  
  output=cbind(label,coefs,r_pondrAIQC)  
  output4_CT_DK <- rbind.fill(output4_CT_DK, output)
}
output4_CT_DK$pFDR_pondrAIQC <- p.adjust(output4_CT_DK$p_pondrAIQC, "fdr")

# CT with HPC atlas 
output4_CT_HPC<- data.frame()
for (label in names(df_CT_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ pondrAIQC + age + age2 + sex + (1|site)")), data=df_CT_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_pondrAIQC <- rs[1,1]

  output=cbind(label,coefs,r_pondrAIQC)  
  output4_CT_HPC <- rbind.fill(output4_CT_HPC, output)
}
output4_CT_HPC$pFDR_pondrAIQC <- p.adjust(output4_CT_HPC$p_pondrAIQC, "fdr")


#SA
# SA with DK atlas 
output4_SA_DK<- data.frame()
for (label in names(df_SA_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ pondrAIQC + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_pondrAIQC <- rs[1,1]
  
  output=cbind(label,coefs,r_pondrAIQC)  
  output4_SA_DK <- rbind.fill(output4_SA_DK, output)
}
output4_SA_DK$pFDR_pondrAIQC <- p.adjust(output4_SA_DK$p_pondrAIQC, "fdr")

# SA with HPC atlas 
output4_SA_HPC<- data.frame()
for (label in names(df_SA_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ pondrAIQC + age + age2 + sex + (1|site)")), data=df_SA_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_pondrAIQC <- rs[1,1]

  output=cbind(label,coefs,r_pondrAIQC)  
  output4_SA_HPC <- rbind.fill(output4_SA_HPC, output)
}
output4_SA_HPC$pFDR_pondrAIQC <- p.adjust(output4_SA_HPC$p_pondrAIQC, "fdr")

#CV
# CV with DK atlas 
output4_CV_DK<- data.frame()
for (label in names(df_CV_parcs[30:97])) {
  model <- lmer(as.formula(paste(label, "~ pondrAIQC + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_pondrAIQC <- rs[1,1]
  
  output=cbind(label,coefs,r_pondrAIQC)  
  output4_CV_DK <- rbind.fill(output4_CV_DK, output)
}
output4_CV_DK$pFDR_pondrAIQC <- p.adjust(output4_CV_DK$p_pondrAIQC, "fdr")

# CV with HPC atlas 
output4_CV_HPC<- data.frame()
for (label in names(df_CV_parcs[98:457])) {
  model <- lmer(as.formula(paste(label, "~ pondrAIQC + age + age2 + sex + (1|site)")), data=df_CV_parcs)
  #Store coefficients
  coefs<- parseLmer(model)
  param_tab <- parameters::model_parameters(model)
  rs <- t_to_r(param_tab$t[2:5], param_tab$df_error[2:5])
  r_pondrAIQC <- rs[1,1]

  output=cbind(label,coefs,r_pondrAIQC)  
  output4_CV_HPC <- rbind.fill(output4_CV_HPC, output)
}
output4_CV_HPC$pFDR_pondrAIQC <- p.adjust(output4_CV_HPC$p_pondrAIQC, "fdr")




####ggseg 
#for each phenotype, CT, SA and CV combined (total 8 figs)

#FSQC
#DK
output_CT_DK$phenotype = "CT"
output_SA_DK$phenotype = "SA"
output_CV_DK$phenotype = "CV"

output_DK_all = rbind(output_CT_DK, output_SA_DK, output_CV_DK)

subset(output_DK_all, pFDR_FSQC < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = dk, hemisphere = 'left') + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) +
  ggtitle('FSQC DK')
ggsave('FSQC_DK_all_r.png', height = 5, width = 10)

output_DK_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = dk) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('FSQC DK')
ggsave('FSQC_DK_all_allregions.png', height = 5, width = 10)


#HPC
output_CT_HPC$phenotype = "CT" 
output_SA_HPC$phenotype = "SA" 
output_CV_HPC$phenotype = "CV" 

output_HPC_all = rbind(output_CT_HPC, output_SA_HPC, output_CV_HPC)

output_HPC_all$label <- gsub(x = output_HPC_all$label, pattern = "_ROI", replacement = "")  
output_HPC_all$label <- gsub(x = output_HPC_all$label, pattern = "9.", replacement = "9-", fixed=T)  #rename labels to be consistent with glasser atlas
output_HPC_all$label <- gsub(x = output_HPC_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output_HPC_all$label <- gsub(x = output_HPC_all$label, pattern = "2.", replacement = "2-", fixed=T)  

subset(output_HPC_all, pFDR_FSQC < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser, hemisphere = 'left') +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) +
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('FSQC HPC')
ggsave('FSQC_HPC_all_r.png', height = 5, width = 10)

output_HPC_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_FSQC),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('FSQC HPC')
ggsave('FSQC_HPC_all_allregions.png', height = 5, width = 10)



#######
#EULER

#DK
output2_CT_DK$phenotype = "CT" 
output2_SA_DK$phenotype = "SA" 
output2_CV_DK$phenotype = "CV" 

output2_DK_all = rbind(output2_CT_DK, output2_SA_DK, output2_CV_DK)

subset(output2_DK_all, pFDR_surfholes < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_surfholes),  atlas = dk, hemisphere = 'left') + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) +
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('Euler DK')
ggsave('Euler_DK_all_r.png', height = 5, width = 10)

output2_DK_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_surfholes),  atlas = dk) + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('Euler DK')
ggsave('Euler_DK_all_allregions.png', height = 5, width = 10)


#HPC
output2_CT_HPC$phenotype = "CT" 
output2_SA_HPC$phenotype = "SA" 
output2_CV_HPC$phenotype = "CV" 

output2_HPC_all = rbind(output2_CT_HPC, output2_SA_HPC, output2_CV_HPC)

output2_HPC_all$label <- gsub(x = output2_HPC_all$label, pattern = "_ROI", replacement = "")  
output2_HPC_all$label <- gsub(x = output2_HPC_all$label, pattern = "9.", replacement = "9-", fixed=T) 
output2_HPC_all$label <- gsub(x = output2_HPC_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output2_HPC_all$label <- gsub(x = output2_HPC_all$label, pattern = "2.", replacement = "2-", fixed=T)  

subset(output2_HPC_all, pFDR_surfholes < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_surfholes),  atlas = glasser, hemisphere = 'left') +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) +
  ggtitle('Euler HPC')
ggsave('Euler_HPC_all_r.png', height = 5, width = 10)

output2_HPC_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_surfholes),  atlas = glasser) +
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('Euler HPC')
ggsave('Euler_HPC_all_allregions.png', height = 5, width = 10)




#########
#MOTION_QC
#DK
output3_CT_DK$phenotype = "CT" 
output3_SA_DK$phenotype = "SA" 
output3_CV_DK$phenotype = "CV" 

output3_DK_all = rbind(output3_CT_DK, output3_SA_DK, output3_CV_DK)

subset(output3_DK_all, pFDR_MOTION_QC < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_MOTION_QC),  atlas = dk, hemisphere = 'left') + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('MOTION QC DK')
ggsave('MOTION_QC_DK_all_r.png', height = 5, width = 10)

output3_DK_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_MOTION_QC),  atlas = dk) + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('MOTION QC DK')
ggsave('MOTION_QC_DK_all_allregions.png', height = 5, width = 10)


#HPC
output3_CT_HPC$phenotype = "CT" 
output3_SA_HPC$phenotype = "SA" 
output3_CV_HPC$phenotype = "CV" 

output3_HPC_all = rbind(output3_CT_HPC, output3_SA_HPC, output3_CV_HPC)

output3_HPC_all$label <- gsub(x = output3_HPC_all$label, pattern = "_ROI", replacement = "")  
output3_HPC_all$label <- gsub(x = output3_HPC_all$label, pattern = "9.", replacement = "9-", fixed=T)  #some labels were not merging and plotting properly bc had . instead of - (label between my file and glasser ggseg file did not match; replace)
output3_HPC_all$label <- gsub(x = output3_HPC_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output3_HPC_all$label <- gsub(x = output3_HPC_all$label, pattern = "2.", replacement = "2-", fixed=T)  

subset(output3_HPC_all, pFDR_MOTION_QC < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_MOTION_QC),  atlas = glasser, hemisphere = 'left') + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('MOTION QC HPC')
ggsave('MOTION_QC_HPC_all_r.png', height = 5, width = 10)

output3_HPC_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_MOTION_QC),  atlas = glasser) + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('MOTION QC HPC')
ggsave('MOTION_QC_HPC_all_allregions.png', height = 5, width = 10)



########
#pondrAIQC
#DK
output4_CT_DK$phenotype = "CT" 
output4_SA_DK$phenotype = "SA" 
output4_CV_DK$phenotype = "CV" 

output4_DK_all = rbind(output4_CT_DK, output4_SA_DK, output4_CV_DK)

subset(output4_DK_all, pFDR_pondrAIQC < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_pondrAIQC),  atlas = dk, hemisphere = 'left') + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('pondrAI QC DK')
ggsave('pondrAIQC_DK_all_r.png', height = 5, width = 10)

output4_DK_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_pondrAIQC),  atlas = dk) + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('pondrAI QC DK')
ggsave('pondrAIQC_DK_all_allregions.png', height = 5, width = 10)


#HPC
output4_CT_HPC$phenotype = "CT" 
output4_SA_HPC$phenotype = "SA" 
output4_CV_HPC$phenotype = "CV" 

output4_HPC_all = rbind(output4_CT_HPC, output4_SA_HPC, output4_CV_HPC)

output4_HPC_all$label <- gsub(x = output4_HPC_all$label, pattern = "_ROI", replacement = "")  
output4_HPC_all$label <- gsub(x = output4_HPC_all$label, pattern = "9.", replacement = "9-", fixed=T)  #some labels were not merging and plotting properly bc had . instead of - (label between my file and glasser ggseg file did not match; replace)
output4_HPC_all$label <- gsub(x = output4_HPC_all$label, pattern = "6.", replacement = "6-", fixed=T)  
output4_HPC_all$label <- gsub(x = output4_HPC_all$label, pattern = "2.", replacement = "2-", fixed=T)  

subset(output4_HPC_all, pFDR_pondrAIQC < 0.05) %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_pondrAIQC),  atlas = glasser, hemisphere = 'left') + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('pondrAI QC HPC')
ggsave('pondrAIQC_HPC_all_r.png', height = 5, width = 10)

output4_HPC_all %>% 
  group_by(phenotype) %>%
  ggseg(mapping=aes(fill=r_pondrAIQC),  atlas = glasser) + 
  scale_fill_continuous_divergingx(palette = 'RdBu', mid=0,rev = TRUE) +
  facet_wrap("phenotype",nrow = 3) + 
  labs(fill="Partial r") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "right", axis.text = element_blank(), axis.title = element_blank()) + 
  ggtitle('pondrAI QC HPC')
ggsave('pondrAIQC_HPC_all_allregions.png', height = 5, width = 10)



save.image()

