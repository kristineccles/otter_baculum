####################################################################
# Figure 3- health metrics by age group
# Completed in R. 3.6.0
# By Kristin Eccles

# Must Run Exploratory Data Analysis First
####################################################################
# Fix for adding adj.pvalues to plots
# Install required package
#devtools::install_github("kassambara/ggpubr")
#devtools::install_github("kassambara/rstatix")
# Load required package
library(rstatix)

####################################################################

# Denisty
# Test assumptions
shapiro.test(sqrt_df$Density) # pass
leveneTest(sqrt_df$Density~as.factor(sqrt_df$Age_coded)) #pass

stat.test1= tukey_hsd(aov(Density~as.factor(Age_coded), data=sqrt_df))
stat.test1
#No difference between sites
density <- ggplot(sqrt_df, aes(x=as.factor(Age_coded), y=Density)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab("Sqrt Density (ng/g)")+
  stat_compare_means(method="anova", size=4, label.y = 1.85)+
  stat_pvalue_manual(stat.test1[2,], label = "p.adj",
                   y.position = c(1.80))
density

# Peak Load
# Test assumptions
shapiro.test(sqrt_df$PeakLoad) # pass
leveneTest(sqrt_df$PeakLoad~as.factor(sqrt_df$Age_coded)) #pass

stat.test2= tukey_hsd(aov(PeakLoad~as.factor(Age_coded), data=sqrt_df))
stat.test2
#No difference between sites
peakload <- ggplot(sqrt_df, aes(x=as.factor(Age_coded), y=PeakLoad)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab(" Sqrt Peak Load (N)")+
  stat_compare_means(method="anova", size=4, label.y = 1900)+
  stat_pvalue_manual(stat.test2[2:3,], label = "p.adj",
                     y.position = c(1600, 1750))
peakload

# Work to fail
# Test assumptions
shapiro.test(sqrt_df$WorktoFail) # pass
leveneTest(sqrt_df$WorktoFail~as.factor(sqrt_df$Age_coded)) #pass

stat.test3= tukey_hsd(aov(WorktoFail~as.factor(Age_coded), data=sqrt_df))
stat.test3
#No difference between sites
worktofail = ggplot(sqrt_df, aes(x=as.factor(Age_coded), y=WorktoFail)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab("Sqrt Work to Fail (N/mm)")+
  stat_compare_means(method="anova", size=4, label.y = 2900)+
  stat_pvalue_manual(stat.test2[2:3,], label = "p.adj",
                     y.position = c(2500, 2700))
worktofail

# Stiffness
# Test assumptions
shapiro.test(sqrt_df$Stiffness) # fail
hist(sqrt_df$Stiffness)
leveneTest(sqrt_df$Stiffness~as.factor(sqrt_df$Age_coded)) #pass

stat.test4= dunn_test(Stiffness~Age_coded, data=sqrt_df)
stat.test4
#No difference between sites
stiff = ggplot(sqrt_df, aes(x=as.factor(Age_coded), y=Stiffness)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab("Sqrt Stiffness (N/mm)")+
  stat_compare_means(method="kruskal.test", size=4, label.y = 1300)+
  stat_pvalue_manual(stat.test2[2:3,], label = "p.adj",
                     y.position = c(1050, 1200))
stiff


##############################################
#Compile
figure3=ggarrange(density, peakload, worktofail, stiff,
                  labels = c("A", "B", "C","D"),
                  vjust = 1,
                  hjust = -0.5,
                  ncol = 2, nrow = 2,
                  common.legend = FALSE,
                  legend = "right")
figure3
#Plot figures with dpi=300
save_plot("Figure3_revisions.tif", figure3, width = 20, height = 20, dpi = 300)
