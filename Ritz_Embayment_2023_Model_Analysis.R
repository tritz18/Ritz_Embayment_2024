
################################################################
#### Model selection and analysis for Ritz 2024 Embayment   ####
#### Script created by Thornton Ritz - Last update 10/27/23 ####
################################################################

#### Load packages ####
library(MASS)
library(ggpubr)
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(stats)
library(broom)
library(car)
library(lme4)
library(effects)
library(ggeffects)
library(patchwork)
library(corrplot)
library(lmtest)


#### Read in combined abiotic and biotic data for both locations ####

setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

Model_DF<- read_excel("Ritz_Embayment_2023_Model.xlsx")
Model_DF$Location<- as.factor(Model_DF$Location)
Model_DF$Species<- as.factor(Model_DF$Species)

#### Filter out focal species with enough representation in catch ####
Model_DF <- Model_DF |>
  filter(Species %in% c("LMB", "SMB", "LEPOMIS", "CYPRINIDAE", "ICTALURID"))


#### LMB Model ####
Model_LMB<- Model_DF |>
  filter(Species %in% "LMB") 
Model_CYP<- Model_DF |>
  filter(Species %in% "CYPRINIDAE")
Model_ICT<- Model_DF |>
  filter(Species %in% "ICTALURID")

#### Check distribution, Correlation plot (only first to confirm multicollinearity) ##
## Remove correlated variables listed as the following ##
## DO: DO Mean-DO Max 0.89 (remove DO max, mean more meaningful) ##
## DO: DO Mean-DO Min 0.74 (remove DO min, mean more meaningful) ##
## TEMP: TEMP Mean-TEMP Max 0.81 (remove TEMP max, mean more meaningful) ##
## TEMP: TEMP Mean-TEMP Min 0.87 (remove TEMP min, mean more meaingful)
 
#### Values used to unscale and uncenter data for plots ##
## DO mean = 6.170    DO SD = 2.169 ##
## TEMP mean = 23.020 TEMP SD = 2.234 ##
## DEPTH mean = 74.568 SD = 0.041 ####

ggplot(Model_LMB, aes(Catch))+
  geom_histogram()

Cor<- Model_LMB |>
  select(DO_mean, DO_max, DO_min,
         TEMP_mean, TEMP_max, TEMP_min,
         DEPTH_mean, Study_Day)

cor(Cor)

#### Global model - remove variables that are not signficiant ####
Global_model_LMB<- glm.nb(Catch~ DO_mean + TEMP_mean + DEPTH_mean,
                  data = Model_LMB)
summary(Global_model_LMB)

Effect_LMB<- effect(term= "TEMP_mean", mod=Global_model_LMB)

#### Create dataframe to plot the effects ####                                                                                                          
Effect_LMB<- as.data.frame(Effect_LMB) 

Effect_LMB<- Effect_LMB |>
  mutate(Unscaled=(TEMP_mean*2.2349)+23.025)

Figure5_1<- ggplot()+
  geom_ribbon(data=Effect_LMB, aes(Unscaled, ymin=lower, ymax=upper),
              fill="grey",
              alpha=0.6)+
  geom_point(data=Effect_LMB, aes(Unscaled, y=fit))+
  geom_line(data=Effect_LMB, aes(Unscaled, y=fit), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"),
                   plot.caption = element_text(hjust=.8, vjust = 6))+
  ylab("Total Catch")+xlab("TEMP mean")
  #scale_y_continuous(breaks=seq(0,225, 50), limits = c(0,225))+
  #scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
Figure5_1


## Testing for multicollinarity (VIF <10 ) & autocorrelation (DW test stat between 1.5-2.5) ###
vif(DO_model) 
acf(DO_model$residuals, type = "correlation")
dwtest(DO_model)

## Summary stats for final Eel DO model ##
summary(DO_model)


ggplot(Model_CYP, aes(Catch))+
  geom_histogram()


#### Global model - remove variables that are not signficiant ####
Global_model_CYP<- glm.nb(Catch~ DO_mean + TEMP_mean + DEPTH_mean,
                          data = Model_CYP)
summary(Global_model_CYP)

Effect_CYP<- effect(term= "DO_mean", mod=Global_model_CYP)

#### Create dataframe to plot the effects ####                                                                                                          
Effect_CYP<- as.data.frame(Effect_CYP) 
Effect_CYP<- Effect_CYP |>
  mutate(Unscaled=(DO_mean*2.169)+6.170)

Figure5_1<- ggplot()+
  geom_ribbon(data=Effect_CYP, aes(Unscaled, ymin=lower, ymax=upper),
              fill="grey",
              alpha=0.6)+
  geom_point(data=Effect_CYP, aes(Unscaled, y=fit))+
  geom_line(data=Effect_CYP, aes(Unscaled, y=fit), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"),
                   plot.caption = element_text(hjust=.8, vjust = 6))+
  ylab("Total Catch")+xlab("DO mean")
#scale_y_continuous(breaks=seq(0,225, 50), limits = c(0,225))+
#scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
Figure5_1




ggplot(Model_ICT, aes(Catch))+
  geom_histogram()

#### Global model - remove variables that are not signficiant ####
Global_model_ICT<- glm.nb(Catch~ DO_mean + TEMP_mean + DEPTH_mean,
                          data = Model_ICT)
summary(Global_model_ICT)

Effect_ICT<- effect(term= "TEMP_mean", mod=Global_model_ICT)

Effect_ICT<- as.data.frame(Effect_ICT) 
Effect_ICT<- Effect_ICT |>
  mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Figure5_1<- ggplot()+
  geom_ribbon(data=Effect_ICT, aes(Unscaled, ymin=lower, ymax=upper),
              fill="grey",
              alpha=0.6)+
  geom_point(data=Effect_ICT, aes(Unscaled, y=fit))+
  geom_line(data=Effect_ICT, aes(Unscaled, y=fit), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"),
                   plot.caption = element_text(hjust=.8, vjust = 6))+
  ylab("Total Catch")+xlab("TEMP mean")
#scale_y_continuous(breaks=seq(0,225, 50), limits = c(0,225))+
#scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
Figure5_1
























#### TEMP model ####
TEMP_model<- glm.nb(Catch~Study_Day + Species+ TEMP_mean,
                    data=Model_Eel)
vif(TEMP_model)
acf(TEMP_model$residuals, type = "correlation")
dwtest(TEMP_model)

## Summary Stats for final Eel TEMP model ##
summary(TEMP_model)

#### Depth model ####
DEPTH_model<- glm.nb(Catch~ Study_Day + Species + DEPTH_mean,
                     data=Model_Eel)
vif(DEPTH_model)
acf(DEPTH_model$residuals, type = "correlation")
dwtest(DEPTH_model)

## Summary stats for final Eel DEPTH model ##
summary(DEPTH_model)

#### Global Model ####

Global_model_Eel<- glm.nb(Catch~Study_Day + Species +
                      DEPTH_mean + DO_min,
                      data=Model_Eel)

vif(Global_model_Eel)
dwtest(Global_model_Eel)

## Summary stats for final Eel Global Model ##

summary(Global_model_Eel)

#########################################################################################

#### PLOTS OF SIGNIFICANT VARIABLES EEL Figure 5 ####

## DO model - strong study day effect Panel A ##

DO_Model_SD_Eel<- effect(term= "Study_Day", mod=DO_model)

DO_Model_SD_Eel<- as.data.frame(DO_Model_SD_Eel) 

Figure5_1<- ggplot()+
  geom_point(data=DO_Model_SD_Eel, aes(Study_Day, y=fit))+
  geom_line(data=DO_Model_SD_Eel, aes(Study_Day, y=fit), linewidth=1)+
  geom_ribbon(data=DO_Model_SD_Eel, aes(Study_Day, ymin=lower, ymax=upper),
                          fill="#003366",
                          alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"),
                   plot.caption = element_text(hjust=.8, vjust = 6))+
  ylab("Total Catch")+xlab("Study Day")+
  scale_y_continuous(breaks=seq(0,225, 50), limits = c(0,225))+
  scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
Figure5_1

## Global model - DO min Panel B ## 

Global_DO_min_Eel<- effect(term= "DO_min", mod=Global_model_Eel)
Global_DO_min_Eel<- as.data.frame(Global_DO_min_Eel)


Figure5_2<- ggplot()+
  geom_point(data=Global_DO_min_Eel, aes(DO_min, y=fit))+
  geom_line(data=Global_DO_min_Eel, aes(DO_min, y=fit),linewidth=1)+
  geom_ribbon(data=Global_DO_min_Eel, aes(DO_min, ymin=lower, ymax=upper), 
              fill="#003366",alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("DO Minimum (scaled & centered)")+
  scale_y_continuous(breaks=seq(0,90, 15))
  #scale_x_continuous(breaks=seq(0,6,1))

Figure5_2

#### Global Model - Depth mean Panel C ####

Global_Depth_mean_Eel<- effect(term= "DEPTH_mean", mod=Global_model_Eel)
Global_Depth_mean_Eel<- as.data.frame(Global_Depth_mean_Eel)

Figure5_3<- ggplot()+
  geom_point(data=Global_Depth_mean_Eel, aes(DEPTH_mean, y=fit))+
  geom_line(data=Global_Depth_mean_Eel, aes(DEPTH_mean, y=fit), linewidth=1)+
  geom_ribbon(data=Global_Depth_mean_Eel, aes(DEPTH_mean,ymin=lower, ymax=upper),
              fill="#003366",alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Depth mean (scaled & centered)")+
  scale_y_continuous(breaks=seq(0,300,50))
Figure5_3

##Plot all panals together Figure 5 Complete ####

Figure_5 <- Figure5_1 + Figure5_2 + Figure5_3+
  plot_layout(nrow = 3)

Figure_5

ggsave("Figure_5.png", height = 8, width = 6)
#########################################################################################

#### Appendix model plots EB ###

#### DO models Sig. Covar. Eel Figure1A_1 ###
DO_Model_DOmin_Eel<- effect(term= "DO_min", mod=DO_model)

DO_Model_DOmin_Eel<- as.data.frame(DO_Model_DOmin_Eel) 

FigureA1_1<- ggplot()+
  geom_point(data=DO_Model_DOmin_Eel, aes(DO_min, y=fit))+
  geom_line(data=DO_Model_DOmin_Eel, aes(DO_min, y=fit), linewidth=1)+
  geom_ribbon(data=DO_Model_DOmin_Eel, aes(DO_min, ymin=lower, ymax=upper),
              fill="#003366",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"),
                   plot.caption = element_text(hjust=.8, vjust = 6))+
  ylab("Total Catch")+xlab("DO Minimum (scaled & centered)")+
  scale_y_continuous(breaks=seq(0,150, 25), limits = c(0,150))
FigureA1_1

## Figure A3_1

Depth_Model_Eel<- effect(term= "DEPTH_mean", mod=DEPTH_model)
Depth_Model_Eel<- as.data.frame(Depth_Model_Eel)

FigureA3_1<- ggplot()+
  geom_point(data=Depth_Model_Eel, aes(DEPTH_mean, y=fit))+
  geom_line(data=Depth_Model_Eel, aes(DEPTH_mean, y=fit), linewidth=1)+
  geom_ribbon(data=Depth_Model_Eel, aes(DEPTH_mean,ymin=lower, ymax=upper),
              fill="#003366",alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Depth mean (scaled & centered)")+
  scale_y_continuous(breaks=seq(0,300,50))
FigureA3_1




#######################################################################################
#### Flynn Bay ####

Model_Flynn <- Model_DF |>
  filter(Location %in% "Flynn Bay") 

## DO model ##
DO_model<- glm.nb(Catch~Study_Day + Species+
                    DO_min + DO_mean,
                  data = Model_Flynn)


## Testing for multicollinarity (VIF <10 ) & autocorrelation (DW test stat between 1.5-2.5) ###
vif(DO_model) 
acf(DO_model$residuals, type = "correlation")
dwtest(DO_model)


summary(DO_model)


#### TEMP model ####
TEMP_model<- glm.nb(Catch~Study_Day + Species+ TEMP_mean,
                    data=Model_Flynn)
vif(TEMP_model)
acf(TEMP_model$residuals, type = "correlation")
dwtest(TEMP_model)


summary(TEMP_model)


#### Depth model ####
DEPTH_model<- glm.nb(Catch~ Study_Day + Species + DEPTH_mean,
                     data=Model_Flynn, maxit=100)
vif(DEPTH_model)
acf(DEPTH_model$residuals, type = "correlation")
dwtest(DEPTH_model)

summary(DEPTH_model)


Flynn_Model_Global <- glm.nb(Catch~ Study_Day + Species+ 
                               DO_mean + TEMP_mean + DEPTH_mean, 
                             maxit=200,data=Model_Flynn)
vif(Flynn_Model_Global)

summary(Flynn_Model_Global)


###########################################################################################

#### Aooendix models- DO FigureA_2,3,4 #####

DO_Model_DO_min_Flynn<- effect(term= "DO_min", mod=DO_model)
DO_Model_DO_min_Flynn<- as.data.frame(DO_Model_DO_min_Flynn) 

FigureA1_2<- ggplot()+
  geom_point(data=DO_Model_DO_min_Flynn, aes(DO_min, y=fit))+
  geom_line(data=DO_Model_DO_min_Flynn, aes(DO_min, y=fit), linewidth=1)+
  geom_ribbon(data=DO_Model_DO_min_Flynn, aes(DO_min, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("DO Minimum (scaled & centered)")+
#scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
scale_y_continuous(breaks=seq(0,30,5), limits = c(0,30))
FigureA1_2

DO_Model_SD_Flynn<- effect(term= "Study_Day", mod=DO_model)
DO_Model_SD_Flynn<- as.data.frame(DO_Model_SD_Flynn) 

FigureA1_4<- ggplot()+
  geom_point(data=DO_Model_SD_Flynn, aes(Study_Day, y=fit))+
  geom_line(data=DO_Model_SD_Flynn, aes(Study_Day, y=fit), linewidth=1)+
  geom_ribbon(data=DO_Model_SD_Flynn, aes(Study_Day, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Study Day")+
  #scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
  scale_y_continuous(breaks=seq(0,60,10))
FigureA1_4


DO_Model_Mean_Flynn<- effect(term= "DO_mean", mod=DO_model)
DO_Model_Mean_Flynn<- as.data.frame(DO_Model_Mean_Flynn) 

FigureA1_3<- ggplot()+
  geom_point(data=DO_Model_Mean_Flynn, aes(DO_mean, y=fit))+
  geom_line(data=DO_Model_Mean_Flynn, aes(DO_mean, y=fit), linewidth=1)+
  geom_ribbon(data=DO_Model_Mean_Flynn, aes(DO_mean, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("DO mean (scaled & centered)")+
  #scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
  scale_y_continuous(breaks=seq(0,90,10))
FigureA1_3

FigureA1<-  FigureA1_1 + FigureA1_2 + FigureA1_3 + FigureA1_4+
  plot_layout(nrow = 2)
FigureA1

ggsave("FigureA_1.png",width=10, height=6,dpi=300)

####TEMP model EEL #####

Model_TEMP_Flynn<- effect(term= "TEMP_mean", mod=TEMP_model)
Model_TEMP_Flynn<- as.data.frame(Model_TEMP_Flynn) 

FigureA2_1<- ggplot()+
  geom_point(data=Model_TEMP_Flynn, aes(TEMP_mean, y=fit))+
  geom_line(data=Model_TEMP_Flynn, aes(TEMP_mean, y=fit), linewidth=1)+
  geom_ribbon(data=Model_TEMP_Flynn, aes(TEMP_mean, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Water temperature mean (scaled & centered)")+
  scale_y_continuous(breaks=seq(0,80,20), limits = c(0,80))
FigureA2_1

Model_SD_Flynn<- effect(term= "Study_Day", mod=TEMP_model)
Model_SD_Flynn<- as.data.frame(Model_SD_Flynn) 

FigureA2_2<- ggplot()+
  geom_point(data=Model_SD_Flynn, aes(Study_Day, y=fit))+
  geom_line(data=Model_SD_Flynn, aes(Study_Day, y=fit), linewidth=1)+
  geom_ribbon(data=Model_SD_Flynn, aes(Study_Day, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Study Day")
  #scale_y_continuous(breaks=seq(0,80,20), limits = c(0,80))
FigureA2_2


Figure_A2 <- FigureA2_1 + FigureA2_2 +
  plot_layout(nrow = 1)
Figure_A2

ggsave("FigureA_2.png", dpi = 300, width=10, height=5)

### Depth model Apendix FigureA3_2

Depth_Model_Flynn<- effect(term= "DEPTH_mean", mod=DEPTH_model)
Depth_Model_Flynn<- as.data.frame(Depth_Model_Flynn)

FigureA3_2<- ggplot()+
  geom_point(data=Depth_Model_Flynn, aes(DEPTH_mean, y=fit))+
  geom_line(data=Depth_Model_Flynn, aes(DEPTH_mean, y=fit), linewidth=1)+
  geom_ribbon(data=Depth_Model_Flynn, aes(DEPTH_mean,ymin=lower, ymax=upper),
              fill="#3366CC",alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Depth mean (scaled & centered)")+
  scale_y_continuous(breaks=seq(0,75,25))
FigureA3_2


FigureA3<- FigureA3_1 + FigureA3_2 + 
  plot_layout(nrow = 1)

FigureA3
ggsave("FigureA_3.png", dpi = 300, height=5, width=10)

## DO model - Study Day plot ##

DO_Model_SD_Flynn<- effect(term= "DEPTH_mean", mod=Flynn_Model_Global)
DO_Model_SD_Flynn<- as.data.frame(DO_Model_SD_Flynn) 

Figure6_1<- ggplot()+
  geom_point(data=DO_Model_SD_Flynn, aes(DEPTH_mean, y=fit))+
  geom_line(data=DO_Model_SD_Flynn, aes(DEPTH_mean, y=fit), linewidth=1)+
  geom_ribbon(data=DO_Model_SD_Flynn, aes(DEPTH_mean, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Depth Mean (centered & scaled)")
#scale_x_continuous(breaks=seq(0,42,7), limits=c(0,42))
#scale_y_continuous(breaks=seq(0,300, 50), limits = c(0,300))
Figure6_1




Global_Model_TEMP_Flynn<- effect(term= "TEMP_mean", mod=Flynn_Model_Global)
Global_Model_TEMP_Flynn<- as.data.frame(Global_Model_TEMP_Flynn) 

Figure6_2<- ggplot()+
  geom_point(data=Global_Model_TEMP_Flynn, aes(TEMP_mean, y=fit))+
  geom_line(data=Global_Model_TEMP_Flynn, aes(TEMP_mean, y=fit), linewidth=1)+
  geom_ribbon(data=Global_Model_TEMP_Flynn, aes(TEMP_mean, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("Water temperature mean (scaled & centered)")+
scale_y_continuous(breaks=seq(0,140,20), limits = c(0,140))
Figure6_2


Global_Model_DO_Flynn<- effect(term= "DO_mean", mod=Flynn_Model_Global)
Global_Model_DO_Flynn<- as.data.frame(Global_Model_DO_Flynn) 

Figure6_3<- ggplot()+
  geom_point(data=Global_Model_DO_Flynn, aes(DO_mean, y=fit))+
  geom_line(data=Global_Model_DO_Flynn, aes(DO_mean, y=fit), linewidth=1)+
  geom_ribbon(data=Global_Model_DO_Flynn, aes(DO_mean, ymin=lower, ymax=upper),
              fill="#3366CC",
              alpha=0.6)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=12),
                   axis.title = element_text(size=13, face="bold"))+
  ylab("Total Catch")+xlab("DO mean (scaled & centered)")+
scale_y_continuous(breaks=seq(0,80, 20), limits = c(0,80))
Figure6_3



Figure_6 <- Figure6_3 + Figure6_2 + Figure6_1 +
  plot_layout(nrow = 3)
Figure_6


ggsave("Figure_6.png", height = 8, width = 6)
