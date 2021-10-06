# vocabulary-education

This plot code was added from Rstudio to test having a plot on display in the readme file. 


library(ggplot2)
library(ggpubr)
library(RColorBrewer)
#create datasets with results in 
language_MH_results = data.frame(
  cohort=factor(c("BCS","BCS", "MCS",  "MCS")), 
  coeff = c(.00, -.06, .05, -.03), #model coefficients
  lower = c(-.03, -.08, .02, -.05), #lower confidence intervals
  upper = c(.03, -.03, .07, -.01), #upper confidence intervals
  Reporter = factor(c("BCS Self Report", "BCS Parent Report", 
                      "MCS Self Report", "MCS Parent Report"))
)


#plot

language_mh_plot <- ggplot() +
  geom_pointrange(data=language_MH_results, #dataset using to make plot from
                  mapping=aes(x=cohort, y=coeff,ymin=lower, ymax=upper,  group= Reporter, color=Reporter, fill=Reporter, shape=Reporter), #cohort on the X axis, coefficients on the y axis, ymin and ymax create the point range with the lower and upper confidence intervals, group, colour, fill and shape based on repoter - this is how it does the colour coding in the legend
                  size=1.5,  position = position_dodge(width=0.4)) + #size and position of each point
  scale_color_manual(name="Cohort & Reporter", values=c( "#5E3C99",  "#B2ABD2",  "#FDB863", "#FEE0B6" ), #change the colour
                     limits=c("BCS Self Report", "BCS Parent Report", "MCS Self Report", "MCS Parent Report"))+
  scale_fill_manual(name="Cohort & Reporter", values=c(  "#5E3C99",  "#B2ABD2", "#FDB863", "#FEE0B6"), 
                    limits=c("BCS Self Report", "BCS Parent Report", "MCS Self Report", "MCS Parent Report"))+
  scale_shape_manual(name="Cohort & Reporter", values = c(17, 16, 17, 16), #change the shape
                     limits= c("BCS Self Report", "BCS Parent Report", "MCS Self Report", "MCS Parent Report")) +
  theme_classic2()+
  xlab(label="Cohort") +
  ylab(label="Internalizing Symptoms \n (Standardised)")+
  theme(axis.text = element_text(size=15, family = "Times", 
                                 colour = "black"), 
        axis.title = element_text(size=15, 
                                  family = "Times"))+
  theme(legend.text = element_text(size=15, 
                                   family="Times"), 
        legend.title = element_text(size=15, 
                                    family="Times"))+
  geom_vline(xintercept = 1.5, #this adds in the grey line 
             colour="#E0E0E0") +
  ylim(-.08, .08) 
  
language_mh_plot
  