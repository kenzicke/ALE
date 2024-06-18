#ALE Project 2024
#second attempt at data analysis 4/24/2024
library(ggplot2) 
library(dplyr)
library(nlstools)
library(seacarb)

#carb system parameters using average TA and DIC for each treatment 4/24. 
#NEED TO UPDATE WITH FINAL DIC DATA
#flag = 15  alk, dic in moles.
amb = carb(15, 0.002454, 0.002130, S=34.84, T=25)
ele = carb(15, 0.003623, 0.002975, S=34.76, T=25)
hi = carb(15, 0.004395, 0.003570, S=34.6, T=25)
xhi = carb(15, 0.004589 , 0.003800, S=34.4, T=25)


#import growth data 
bw <- read.csv('data/bw_w_SA.csv', stringsAsFactors = TRUE)
le <- read.csv('data/growth_le_6_12.csv', stringsAsFactors = TRUE)

#check structure
str(bw)
str(le)

#define custom labels for genotype
labels_genotype <- c("C" = "Coopers",
                     "M" = "Marker 9",
                     "S" = "Sunny Isles")

#bw scatter plot w linear regression
bwgenoplot <-  ggplot(aes(x = ta, y = gr_end, colour = geno), data = bw) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~log(x), se = FALSE) +
  geom_smooth(color = 1, method = 'lm', se = FALSE) +
  #scale_x_discrete(labels = custom_labels_x) + # Set custom labels for x-axis (treatment)
  scale_colour_discrete(labels = labels_genotype) +
  xlab('TA (umol)')+
  ylab("Growth rate (mg/g/day)")+
  labs(colour = "Genotype")
#scale_y_continuous(breaks = seq(0, 12, by = 1)) # Add more tick marks on the y-axis

print(bwgenoplot)

#bw scatter plot with non-linear regressoin
#check fit of log function
log.model = lm(gr_end ~ log(ta), data = bw)
summary(log.model)
#LE (standardized) scatter plot w linear regression using Leios data
leplot <- ggplot(aes(x = ta, y = leios_end, colour = geno), data = le)+
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  #scale_x_discrete(labels = custom_labels_x) + # Set custom labels for x-axis (treatment)
  scale_colour_discrete(labels = labels_genotype) +
  xlab('TA (umol)')+
  ylab("Growth rate (mm/cm/day)")+
  labs(colour = "Genotype")

print(leplot)

#LE (not standardized) scatter plot w linear regressoin using Leios data
leplot2 <- ggplot(aes(x = ta, y = leios_mmday, colour = geno), data = le) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_colour_discrete(labels = labels_genotype)+
  xlab('Ta (umol)')+
  ylab('Growth rate (mm/day)')+
  labs(colour = "Genotype")
print(leplot2)

#boxplot bw 
bwbox <- boxplot(bw$gr_end ~ bw$treatment,
        ylab = "mg/g/day", xlab = "treatment") 
print(bwbox)

#boxplot le standardized using leios data
lebox <- boxplot(le$leios_end ~ le$treatment,
        ylab = "mm/cm/day", xlab = "treatment")
print(lebox)

#boxplot le not standardized using leios data
lebox2 <- boxplot(le$leios_mmday ~ le$treatment,
                  ylab = "Growth rate (mm/day)", xlab = "treatment")

print(lebox2)

#Statistics 

#ANOVA for bw
ANOVA_bw <- aov(gr_end ~ treatment, data = bw)
summary(ANOVA_bw)

ANOVA_bw <- aov(gr_end ~ ta, data = bw)
summary(ANOVA_bw)
anova(ANOVA_bw)

#mixed linear model, add rnadom variables that may be impacting results
library(lme4)
library(emmeans)

LM_TA <- lmer(gr_end ~ ta + (1|geno) + (1|tank), data = bw)
summary(LM_TA)
anova(LM_TA)
ranova(LM_TA)
step(LM_TA)

#can try by treatment too. Do same for LE. 
LM_TA <- lmer(gr_end ~ treatme + (1|geno), data = bw)
summary(LM_TA)
#not an actual anova, evaluating your mixed linear model
anova(LM_TA)
#Tukey test will be in emmeans package. Search to figure out how to call
emmeans(LM_TA)
#way to display which gives you the statistical letters
cld

#################################
# Perform Tukey HSD post-hoc test
tukey_result_bw <- TukeyHSD(ANOVA_bw)
print(tukey_result_bw)

#ANOVA for LE standardized using Leios data
ANOVA_le <- aov(leios_end ~ treatment, data = le)
summary(ANOVA_le)

#Tukey 
tukey_result_le <- TukeyHSD(ANOVA_le)
print(tukey_result_le)


#ANOVA LE not-standardized using Leios data
ANOVA_le_nostd <- aov(leios_mmday ~ treatment, data = le)
summary(ANOVA_le_nostd)

tukey_result_le_nostd <- TukeyHSD(ANOVA_le_nostd)
print(tukey_result_le_nostd)


#ANOVA LE using ImageJ

ano_le_img <- aov(imj_end ~ treatment, data = le)
summary(ano_le_img)

#ANOVA LE midway point
ano_le_mid <- aov(mid_gr ~ treatment, data = le)
summary (ano_le_mid)

tukey_le_mid <- TukeyHSD(ano_le_mid)
print(tukey_le_mid)
