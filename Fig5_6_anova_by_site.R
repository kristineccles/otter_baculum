####################################################################
# Baculum metrics by Site ANOVAs
# Completed in R. 3.6.0
# By Kristin Eccles
####################################################################
#Load Libraries
library(ggplot2)
library(tidyr)
library(car)
library(lmtest)
library(ggpubr)
library(multcomp)
library(sjPlot)
library(ggpubr)

# Fix for adding adj.pvalues to plots
# Install required package
#devtools::install_github("kassambara/ggpubr")
#devtools::install_github("kassambara/rstatix")
# Load required package
library(rstatix)

# NOTE: the default of the Levene Test is middle=median, must specify mean

####################################################################
# Plot concentrations of important PAcs by site

# sqrt_C4_Chrysene
# Test assumptions
shapiro.test(adults_sqrt$sqrt_C4Chrysene) # fail
leveneTest(adults_sqrt$sqrt_C4Chrysene~adults_sqrt$ord_loc, center=mean) # pass

stat.test5 =dunn_test(sqrt_C4Chrysene~ord_loc, data=adults_sqrt)
stat.test5
#No difference between sites
PAC1 <- ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_C4Chrysene)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt C4-Chrysene (ng/g)")+
  stat_compare_means(method="kruskal.test", size=3)
PAC1

# sqrt_X3_Methylphenanthrene
# test assumptions 
shapiro.test(adults_sqrt$sqrt_x3Methylphenanthrene) # pass
leveneTest(adults_sqrt$sqrt_x3Methylphenanthrene~adults_sqrt$ord_loc, center=mean) # pass

stat.test6 = tukey_hsd(aov(sqrt_x3Methylphenanthrene~ord_loc, data=adults_sqrt))
stat.test6
# no sig difference between sites
PAC2 = ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_x3Methylphenanthrene)) + 
  stat_boxplot( geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt 3-Methylphenanthrene (ng/g)")+
  stat_compare_means(method="anova",label.y = c(0.5), size=3)
PAC2

# sqrt_X2_Methylnaphthalene
# test assumptions 
shapiro.test(adults_sqrt$sqrt_x2Methylnaphthalene) # fail
leveneTest(adults_sqrt$sqrt_x2Methylnaphthalene~adults_sqrt$ord_loc, center=mean) #pass

stat.test7 = dunn_test(sqrt_x2Methylnaphthalene~ord_loc, data=adults_sqrt)
stat.test7
# effect of location 
PAC3 = ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_x2Methylnaphthalene)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt 2-Methylnaphthalene (ng/g)")+ 
  stat_compare_means(method="kruskal.test", size=3, label.y=2.5)+
  stat_pvalue_manual(stat.test7[c(1,5),], label="p.adj.signif",  y.position = c(1.5, 2.1), size=3)
PAC3
# there is a glitch in the code for dunn_test and the p.adj is not truncated on the plot 

# sqrt_Retene
# test assumptions 
shapiro.test(adults_sqrt$sqrt_Retene) # fail
leveneTest(adults_sqrt$sqrt_Retene~adults_sqrt$ord_loc, center=mean) #pass

stat.test8 = dunn_test(sqrt_Retene~ord_loc, data=adults_sqrt)
stat.test8
# no effect of location
PAC4 <- ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_Retene)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt Retene (ng/g)")+
  stat_compare_means(method="kruskal.test",size=3)
PAC4
# no sig difference between sites

#sqrt pyrene
# test assumptions 
shapiro.test(adults_sqrt$sqrt_C4Pyrene) # fail
leveneTest(adults_sqrt$sqrt_C4Pyrene~adults_sqrt$ord_loc, center=mean) #pass

stat.test8 = dunn_test(sqrt_C4Pyrene~ord_loc, data=adults_sqrt)
stat.test8
# no effect of site
PAC5 <- ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_C4Pyrene)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt C4-Pyrene (ng/g)")+
  stat_compare_means(method="kruskal.test", size=3, label.y=0.17)
PAC5

comp3=ggarrange(PAC1, PAC2, PAC3, PAC4, PAC5,
                labels = c("A", "B", "C","D","E"),
                vjust = 1,
                hjust = -0.5,
                ncol = 3, nrow = 2,
                common.legend = FALSE,
                legend = "right")
comp3
#Plot figures with dpi=300
save_plot("Figure6_revisions.tif", comp3, width = 25, height = 15, dpi = 300)


################################################################
# thallum, cadmium, strontium, iron

# sqrt_Cd
# test assumptions 
shapiro.test(adults_sqrt$sqrt_Cd) # fail
leveneTest(adults_sqrt$sqrt_Cd~adults_sqrt$ord_loc, center=mean) #fail

# difference between site
stat.test = dunn_test(sqrt_Cd~ord_loc, data=adults_sqrt)
stat.test

MET1 <- ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_Cd)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt Cadmium (μg/g)")+
  stat_compare_means(method="kruskal.test", size=4)+
  stat_pvalue_manual(stat.test[1,], label = "p.adj.signif",
                     y.position = c(0.45), size=4)
MET1

# Iron
# test assumptions 
shapiro.test(adults_sqrt$sqrt_Fe) # pass
leveneTest(adults_sqrt$sqrt_Fe~adults_sqrt$ord_loc, center=mean) #pass

stat.test2 = tukey_hsd(aov(sqrt_Fe~ord_loc, data=adults_sqrt))
stat.test2
#no difference between site
MET2 <- ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_Fe)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt Iron (μg/g)")+
  stat_compare_means(method="anova", size=4, label.y=47)
MET2

# Thallium
# test assumptions 
shapiro.test(adults_sqrt$sqrt_Tl) # fail
leveneTest(adults_sqrt$sqrt_Tl~adults_sqrt$ord_loc, center=mean) #pass

stat.test3 = dunn_test(sqrt_Tl~ord_loc, data=adults_sqrt)
stat.test3
#no effect of site
MET3 <- ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_Tl)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt Thallium (μg/g)")+  
  stat_compare_means(method="kruskal.test", label.y=0.14, size=4)
MET3

# Strontium
# test assumptions 
shapiro.test(adults_sqrt$sqrt_Sr) # fail
leveneTest(adults_sqrt$sqrt_Sr~adults_sqrt$ord_loc) #pass

stat.test4 = dunn_test(sqrt_Sr~ord_loc, data=adults_sqrt)
stat.test4
# effect of site
MET4 <- ggplot(adults_sqrt, aes(x=ord_loc, y=sqrt_Sr)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal()+
  xlab("Location")+
  ylab("Sqrt Strontium (μg/g)")+  
  stat_compare_means(method="kruskal.test", label.y=0.75, size=4)+
  stat_pvalue_manual(stat.test4[3,],
                     label = "p.adj.signif",
                     y.position = c(0.6), size=4)
MET4

#Compile
comp4=ggarrange(MET1, MET2, MET3, MET4,
                labels = c("A", "B", "C","D"),
                vjust = 1,
                hjust = -0.5,
                ncol = 2, nrow = 2,
                common.legend = FALSE,
                legend = "right")
comp4
#Plot figures with dpi=300
save_plot("Figure5_revisions.tif", comp4, width = 20, height = 20, dpi = 300)
