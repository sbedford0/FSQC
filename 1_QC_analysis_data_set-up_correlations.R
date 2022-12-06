
#analysis of ABIDE QC metrics
library(plyr) 
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggpubr) 
library(stringr)
library(purrr)


#read in and format FSQC ratings 
abide_SB_fsqc <- read.csv('path/to/ratings') #if done in batches, read in multiple csvs and combine with rbind

#rename back from MD5 hash
names(abide_SB_fsqc)[names(abide_SB_fsqc) == 'image'] <- 'image_md5'
hashkey <- read.csv('path/to/md5hash_key.csv')
hashkey$image_md5png <- paste(hashkey$image_md5, '.png', sep='')
hashkey$image_md5 <- hashkey$image_md5png
hashkey$image_md5png <- NULL
abide_SB_fsqc <- left_join(abide_SB_fsqc, hashkey, by = 'image_md5')

#convert scores to numeric (good=1, minor error=2, motion=3, bad=4)
abide_SB_fsqc$FSQC <- ifelse(abide_SB_fsqc$rating == "GOOD", 1, ifelse(abide_SB_fsqc$rating == "MINOR_ERROR",2, ifelse(abide_SB_fsqc$rating == "MOTION", 3, 4)))

#calculate average of scores per participant
fsqc_SB_summary <- abide_SB_fsqc %>%
  group_by(ID) %>%
  summarise_at(vars(FSQC), funs(mean(., na.rm=TRUE)))


##
#combine with rater 2 
abide_rb_fsqc <- read.csv('path/to/ratings_2')

#rename back from MD5 hash
names(abide_rb_fsqc)[names(abide_rb_fsqc) == 'image'] <- 'image_md5'
hashkey <- read.csv('path/to/md5hash_key.csv')
hashkey$image_md5png <- paste(hashkey$image_md5, '.png', sep='')
hashkey$image_md5 <- hashkey$image_md5png
hashkey$image_md5png <- NULL
abide_rb_fsqc <- left_join(abide_rb_fsqc, hashkey, by = 'image_md5')

#convert ratings to numeric
abide_rb_fsqc$QC <- ifelse(abide_rb_fsqc$rating == "GOOD", 1, ifelse(abide_rb_fsqc$rating == "MINOR_ERROR",2, ifelse(abide_rb_fsqc$rating == "MOTION", 3, 4)))

#get average of scores per participant
abide_RB_summary <- abide_rb_fsqc %>%
  group_by(ID) %>%
  summarise_at(vars(QC), funs(mean(., na.rm=TRUE)))

rm(hashkey)



##COMBINE AND COMPARE RATINGS    
fsqc_SB_summary <- fsqc_SB_summary %>% rename(FSQC_SB = FSQC) 
abide_RB_summary <- abide_RB_summary %>% rename(FSQC_RB = QC)

abide_comp <- right_join(fsqc_SB_summary, abide_RB_summary)
abide_comp$diff <- abide_comp$FSQC_SB - abide_comp$FSQC_RB #calculate difference between two raters
abide_comp$ave <- (abide_comp$FSQC_SB + abide_comp$FSQC_RB)/2 #calculate average of two raters

#remove individual image QC scores, keep only summaries
rm(abide_rb_fsqc, abide_SB_fsqc)


#correlation between ratings
#mean
cor.test(abide_comp$FSQC_SB, abide_comp$FSQC_RB, method='spearman')
qplot(FSQC_SB, FSQC_RB, data=abide_comp, geom=c("point"))+geom_smooth(method='lm')+ stat_cor(method='spearman')



#FINAL SCORES (average SB and RB):
abide_fsqc <- abide_comp %>% select(ID, FSQC = ave)



##############################
#combine and compare with other QC measures, demos and neuroanatomical measures 

#cortical measures - including Euler (surfholes)
parcs <- read.csv('path/to/cortical_parcellations.csv')
df <- inner_join(abide_fsqc, parcs) #(called qc_metrics in old R file)

#manual motion QC scores
motion_qc <- read.csv('path/to/motion_qc.csv')
df <- left_join(df, motion_qc) 

#PondrAI QC scores
pondrAI_qc <- read.csv('/path/to/pondrAI_qc.csv')
df = left_join(df, pondrAI_qc)



#####################
#PLOTS + CORRELATIONS

##correlations of metrics with each other

#correlation matrix
matrix <- df %>% select(FSQC, surfholes, motionQC, pondrAIQC)

res <- cor(matrix, method = 'spearman')
res <- round(res, 2)

colnames(res) <- c("FSQC", "Euler", "Motion QC", "Pondr-AI QC")
rownames(res) <- c("FSQC", "Euler", "Motion QC", "Pondr-AI QC")

ggcorrplot(res,
           hc.order = TRUE,
           type = "lower",
           lab=T,
           lab_col = 'white') + scale_fill_viridis() + labs(fill='rho') + theme_bw()+ theme(plot.title = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank())+ theme(text = element_text(size = 18)) 


#pairwise plots with FSQC 
cor.test(df$FSQC, df$surfholes, method='spearman')
cor.test(df$FSQC, df$motionQC, method='spearman')
cor.test(df$FSQC, df$pondrAIQC, method='spearman')



##diagnosis effects/differences

library(ggsignif)
library(RColorBrewer)

display.brewer.pal(n = 8, name = 'Dark2')
display.brewer.pal(n = 8, name = 'Paired')
display.brewer.pal(n = 8, name = 'RdYlBu')


ggplot(df,aes(x=dx,y=FSQC,fill=dx)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_jitter(alpha = 0.5,shape=21) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete(labels = c('Autism','Control'))+
  geom_violin(alpha = 0.5) +
  ylim(1,4.2)+
  geom_signif(comparisons = list(c("Autism", "Control")), annotation ='*', textsize = 6, tip_length=0.02) +
  ylab('FSQC QC') + 
  stat_summary(fun.y="mean",color="black", shape=20)+
  theme(axis.text.x = element_text(vjust = 0.5), 
        legend.position = "bottom",
        axis.title.x = element_blank()) 

ggplot(df,aes(x=dx,y=surfholes,fill=dx)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_jitter(alpha = 0.5,shape=21) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete(labels = c('Autism','Control'))+
  geom_violin(alpha = 0.5) +
  ylim(1,4.2)+
  geom_signif(comparisons = list(c("Autism", "Control")), annotation ='*', textsize = 6, tip_length=0.02) +
  ylab('FSQC QC') + 
  stat_summary(fun.y="mean",color="black", shape=20)+
  theme(axis.text.x = element_text(vjust = 0.5), 
        legend.position = "bottom",
        axis.title.x = element_blank()) 

ggplot(df,aes(x=dx,y=motionQC,fill=dx)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_jitter(alpha = 0.5,shape=21) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete(labels = c('Autism','Control'))+
  geom_violin(alpha = 0.5) +
  ylim(1,4.2)+
  geom_signif(comparisons = list(c("Autism", "Control")), annotation ='*', textsize = 6, tip_length=0.02) +
  ylab('FSQC QC') + 
  stat_summary(fun.y="mean",color="black", shape=20)+
  theme(axis.text.x = element_text(vjust = 0.5), 
        legend.position = "bottom",
        axis.title.x = element_blank()) 

ggplot(df,aes(x=dx,y=pondrAIQC,fill=dx)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_jitter(alpha = 0.5,shape=21) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete(labels = c('Autism','Control'))+
  geom_violin(alpha = 0.5) +
  ylim(1,4.2)+
  geom_signif(comparisons = list(c("Autism", "Control")), annotation ='*', textsize = 6, tip_length=0.02) +
  ylab('FSQC QC') + 
  stat_summary(fun.y="mean",color="black", shape=20)+
  theme(axis.text.x = element_text(vjust = 0.5), 
        legend.position = "bottom",
        axis.title.x = element_blank()) 


t.test(df$surfholes[df$dx=="Autism"], df$surfholes[df$dx=="Control"], )
t.test(df$motionQC[df$dx=="Autism"], df$motionQC[df$dx=="Control"], )
t.test(df$FSQC[df$dx=="Autism"], df$FSQC[df$dx=="Control"], )
t.test(df$pondrAIQC[df$dx=="Autism"], df$pondrAIQC[df$dx=="Control"], )



##site effects 
library(viridis)
ggplot(df,aes(x=site,y=FSQC,fill=site)) +
  geom_jitter(alpha = 0.5,shape=21) +
  geom_boxplot(alpha = 0.5) +
  geom_violin(alpha = 0.5) +
  stat_summary(fun.y="mean",color="black", shape=20)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
        legend.position = "bottom",
        axis.title.x = element_blank()) 

#repeat for other metrics


##age effects
qplot(FSQC, age, data=df, main = 'FSQC by age')+geom_smooth(method=lm)
qplot(FSQC, age, data=df, main = 'FSQC by age')+geom_smooth(method="lm", formula=y ~ poly(x, 2, raw=TRUE))

cor.test(df$age, df$FSQC, method='spearman') 
cor.test(df$age2, df$FSQC, method='spearman') #quadratic age term 

#repeat for other metrics



##sex effects
ggplot(df,aes(x=sex,y=FSQC,fill=sex)) +
  geom_jitter(alpha = 0.5,shape=21) +
  geom_boxplot(alpha = 0.5) +
  geom_violin(alpha = 0.5) +
  stat_summary(fun.y="mean",color="black", shape=20)+
  stat_compare_means(method = "t.test")+ # Add pairwise comparisons p-value
theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
        legend.position = "bottom",
        axis.title.x = element_blank()) 

t.test(df$FSQC[df$sex=="M"], df$FSQC[df$sex=="F"], )
cohens_d(FSQC~sex, data=df)

#repeat for other metrics




##linear mixed effects models  
lmer1 <- lmerTest::lmer(FSQC ~ dx + age + age2 + sex + (1|site), data=df)
summary(lmer1) 

library(effectsize)
param_tab <- parameters::model_parameters(lmer1)
d <- t_to_d(param_tab$t[2], param_tab$df_error[2])

#compare linear vs quadratic age using AIC
# lmer:
lmer1 <- lmerTest::lmer(FSQC ~ dx + age + age2 + sex + (1|site), data=df)
lmer2 <- lmerTest::lmer(FSQC ~ dx + age + sex + (1|site), data=df)
AIC(lmer1,lmer2)




#correlations with neuroanatomical measures

#GMV
cor.test(df$FSQC, df$cGMV)
ggplot(df,aes(x=FSQC, y=cGMV)) + geom_point(alpha = .25) + geom_smooth(method=lm) + stat_cor(method='spearman')

#CT
cor.test(df$FSQC, df$meanCT)
ggplot(df,aes(x=FSQC, y=meanCT)) + geom_point(alpha = .25) + geom_smooth(method=lm) + stat_cor(method='spearman')

#repeat for other measures and QC metrics



save.image()



