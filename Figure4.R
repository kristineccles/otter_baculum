####################################################################
# Baculum metrics by site ADULTS ONLY
# Completed in R. 3.6.0
# By Kristin Eccles

# Must run import datasets first- only use Adult
####################################################################
#Load Libraries

# Fix for adding adj.pvalues to plots
# Install required package
#devtools::install_github("kassambara/ggpubr")
#devtools::install_github("kassambara/rstatix")
# Load required package
library(rstatix)

####################################################

# Denisty
# Test assumptions
shapiro.test(sqrt_df$Density) # pass
leveneTest(sqrt_df$Density~sqrt_df$ord_loc, center=mean) #pass

stat.test1= tukey_hsd(aov(Density~ord_loc, data=adults_sqrt))
stat.test1
#No difference between sites
density <- ggplot(adults_sqrt, aes(x=ord_loc, y=Density)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab("Sqrt Density (ng/g)")+
  stat_compare_means(method="anova", size=4, label.y = 1.75)
density

# Peak Load
# Test assumptions
shapiro.test(sqrt_df$PeakLoad) # pass
leveneTest(sqrt_df$PeakLoad~sqrt_df$ord_loc, center=mean) #fail

stat.test2= dunn_test(PeakLoad~ord_loc, data=adults_sqrt)
stat.test2
#No difference between sites
peakload <- ggplot(adults_sqrt, aes(x=ord_loc, y=PeakLoad)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab(" Sqrt Peak Load (N)")+
  stat_compare_means(method="kruskal.test", size=4, label.y = 1900)
peakload

# Work to fail
# Test assumptions
shapiro.test(sqrt_df$WorktoFail) # pass
leveneTest(sqrt_df$WorktoFail~sqrt_df$ord_loc, center=mean) #pass

stat.test3= tukey_hsd(aov(WorktoFail~ord_loc, data=adults_sqrt))
stat.test3
#No difference between sites
worktofail = ggplot(adults_sqrt, aes(x=ord_loc, y=WorktoFail)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab("Sqrt Work to Fail (N/mm)")+
  stat_compare_means(method="anova", size=4, label.y = 2500)
worktofail


# Stiffness
# Test assumptions
shapiro.test(sqrt_df$Stiffness) #fail
leveneTest(sqrt_df$Stiffness~sqrt_df$ord_loc, center=mean) #fail

stat.test4= dunn_test(Stiffness~ord_loc, data=adults_sqrt)
stat.test4
#No difference between sites
stiff = ggplot(adults_sqrt, aes(x=ord_loc, y=Stiffness)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(fill="lightgrey")+
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab("Sqrt Stiffness (N/mm)")+
  stat_compare_means(method="kruskal.test", size=4, label.y = 1200)+
  stat_pvalue_manual(stat.test4[1,], label = "p.adj.signif",
                     y.position = c(1050))
stiff

##############################################
#Compile
figure4=ggarrange(density, peakload, worktofail, stiff,
                labels = c("A", "B", "C","D"),
                vjust = 1,
                hjust = -0.5,
                ncol = 2, nrow = 2,
                common.legend = FALSE,
                legend = "right")
figure4
#Plot figures with dpi=300
save_plot("Figure4_revisions.tif", figure4, width = 20, height = 20, dpi = 300)
