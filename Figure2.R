####################################################################
# Figure 2
# Completed in R. 3.6.0
# By Kristin Eccles

# Must run import datasets first- only use Adult
####################################################################
#Load Libraries
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(reshape)
library(rstatix)
library(viridis)

# Load data
df2 = read.csv("figure2_data.csv")

#Reorder data
df2$ord_loc <- factor(df2$loc, levels=c("Control", "SAGD", "MOS", "PAD"))

# Transform Data
# sqrt transform because of zero values
sqrtdf2=sqrt(df2[,10:22])
sqrtdf2=cbind(df2[,1:9],sqrtdf2)
#Reorder data
sqrtdf2$ord_loc <- factor(sqrtdf2$loc, levels=c("Control", "SAGD", "MOS", "PAD"))

stack_df2=melt(cbind(sqrtdf2[,13:18], sqrtdf2[,2]), id=7)
colnames(stack_df2)=cbind('site', "variable", "concentration")
# Reorder the sites
stack_df2$site <- factor(stack_df2$site, levels=c("Control", "SAGD", "MOS", "PAD"))

##############################################
# Make Plots

A_boxplot= ggplot(stack_df2, aes(x=site, y=concentration, fill=variable)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot()+
  scale_fill_brewer(palette = "BrBG")+
  #scale_fill_viridis(discrete=TRUE)+
  theme_minimal(base_size = 15)+
  xlab("Location")+
  ylab("Sqrt PAC Concentration (ng/g l.w.)")+
  stat_compare_means(method="anova", size=4, label.y = 1.45)+
  theme(legend.title = element_blank())
A_boxplot


B_boxplot= ggplot(sqrtdf2, aes(x=ord_loc, y=USEPA.PP)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape=NA, fill="lightgrey")+
  coord_cartesian(ylim = c(8, 12.5))+
  theme_minimal(base_size = 15)+
  #stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  stat_compare_means(method="kruskal.test", size=3, label.y = 12)+
  xlab("Location")+
  ylab("Sqrt Sum US EPA Priority Pollutants (ng/g d.w.)")
B_boxplot



##############################################
#Compile
figure2=ggarrange(A_boxplot, B_boxplot,
                  labels = c("A", "B"),
                  vjust = 1,
                  hjust = -0.5,
                  ncol = 2, nrow = 1,
                  widths = c(2, 1),
                  common.legend = FALSE,
                  legend = "right",
                  font.label = list(size = 16))
figure2
#Plot figures with dpi=300
save_plot("Figure2_revisions.tif", figure2, width = 40, height = 20, dpi = 300)

