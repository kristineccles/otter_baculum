####################################################################
# EDA
# Completed in R. 3.6.0
# By Kristin Eccles
#####################################################################
#Load Libraries
library(corrplot)
library(ggplot2)
library(tidyr)
library(car)
library(lmtest)
library(ggpubr)
library(multcomp)
library(rsq)
library(AICcmodavg)
library(sjPlot)
library(caret)# LOOCV

# Load data
data<- read.csv("summarized_data_pac_metals.csv")

#Reorder data
data$ord_loc <- factor(data$Location_2, levels=c("Control", "SAGD", "MOS", "PAD"))

# Transform Data
# sqrt transform because of zero values
sqrtdata=sqrt(data[,9:94])
colnames(sqrtdata) = paste("sqrt", colnames(sqrtdata), sep="_")
sqrt_df=cbind(data,sqrtdata)

logdata=log10(data[,9:94]+1)
colnames(logdata) = paste("log", colnames(logdata), sep="_")
log_df=cbind(data,logdata)

# Subset Data
adults_sqrt=subset(sqrt_df,Age_coded=="3")
adults_log=subset(log_df,Age_coded=="3")

####################################################################
# Exploratory Data Analysis
#sqrt
cor=cor(adults_sqrt[,96:181])
corrplot(cor,tl.col = "black")
write.csv(cor,"sqrt_all_correlation.csv")

# Significant of correlations
res1 <- cor.mtest(adults_sqrt[,96:181], conf.level = .95)
corrplot(cor,p.mat = res1$p,tl.col = "black")
write.csv(res1,"sqrt_all_correlation_p.csv")

#log
cor=cor(adults_log[,96:181])
corrplot(cor,tl.col = "black")
write.csv(cor,"log_all_correlation.csv")

# Significant of correlations
res1 <- cor.mtest(adults_log[,96:181], conf.level = .95)
corrplot(cor,p.mat = res1$p,tl.col = "black")
write.csv(res1,"logall_correlation_p.csv")