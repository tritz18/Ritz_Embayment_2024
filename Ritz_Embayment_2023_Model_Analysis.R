#### Model selection and analysis for Ritz and Farrell 2024 ####

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
options(scipen = 999, digits = 6)

#### Read in combined abiotic and biotic data for both locations ####

setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

Model_DF<- read_excel("Ritz_Embayment_2023_Model.xlsx")
Model_DF$Location<- as.factor(Model_DF$Location)
Model_DF$Species<- as.factor(Model_DF$Species)
Model_DF$Julian_Date<-as.numeric(Model_DF$Julian_Date)

#### Filter out focal species with enough representation in catch ####
Model_DF <- Model_DF |>
  filter(Species %in% c("LMB", "SMB", "LEP", "CYP", "BBH"))

#### Species-Specific Model Dataframe Generation ####
Model_CYP<- Model_DF |>
  filter(Species %in% "CYP")
Model_BBH<- Model_DF |>
  filter(Species %in% "BBH")
Model_LMB<- Model_DF |>
  filter(Species %in% "LMB") 
Model_LEP<- Model_DF |>
  filter(Species %in% "LEP")
Model_SMB<- Model_DF |>
  filter(Species %in% "SMB")

#### Species-Specific Models with Summary, Confidence Intervals, and McFaddens Adjusted R2 ####
Global_model_CYP<- glm.nb(Catch ~ DO_mean + TEMP_mean + DEPTH_mean + Julian_Date,
                          data = Model_CYP)
summary(Global_model_CYP)
confint(Global_model_CYP)
with(summary(Global_model_CYP), 1 - deviance/null.deviance)


Global_model_BBH<- glm.nb(Catch~ DO_mean + TEMP_mean + DEPTH_mean + Julian_Date,
                          data = Model_BBH)
summary(Global_model_BBH)
confint(Global_model_BBH)
with(summary(Global_model_BBH), 1 - deviance/null.deviance)

Global_model_LMB<- glm.nb(Catch~ DO_mean + TEMP_mean + DEPTH_mean + Julian_Date,
                          data = Model_LMB)
summary(Global_model_LMB)
confint(Global_model_LMB)
with(summary(Global_model_LMB), 1 - deviance/null.deviance)

Global_model_LEP<- glm.nb(Catch~ DO_mean + TEMP_mean+ DEPTH_mean+ Julian_Date,
                          data = Model_LEP)
summary(Global_model_LEP)
confint(Global_model_LEP)
with(summary(Global_model_LEP), 1 - deviance/null.deviance)

Global_model_SMB<- glm.nb(Catch~ DO_mean + TEMP_mean+ DEPTH_mean + Julian_Date,
                          data = Model_SMB)
summary(Global_model_SMB)
confint(Global_model_SMB)
with(summary(Global_model_SMB), 1 - deviance/null.deviance)

#### Model Effects for plotting ##
## Values used to unscale and uncenter data for plots ##
## DO mean = 6.170    DO SD = 2.169 ##
## TEMP mean = 23.020 TEMP SD = 2.234 ##
## DEPTH mean = 74.568 SD = 0.041 ####

#### CYP Modeling ####
Effect_CYP_DO<- effect(term= "DO_mean", mod=Global_model_CYP)
Effect_CYP_TEMP<- effect(term= "TEMP_mean", mod=Global_model_CYP)

## Study Day ##
Effect_CYP_SD<- effect(term = "Julian_Date", mod = Global_model_CYP)
Effect_CYP_SD<- as.data.frame(Effect_CYP_SD) |>
  mutate(Species="CYP")
## Water Temp ##
Effect_CYP_TEMP<- as.data.frame(Effect_CYP_TEMP) |> 
  mutate(Species="CYP")
Effect_CYP_TEMP<-Effect_CYP_TEMP |> mutate(Unscaled=(TEMP_mean*2.234)+23.020)
## DO ##
Effect_CYP_DO<- as.data.frame(Effect_CYP_DO) |>
  mutate(Species="CYP")
Effect_CYP_DO<- Effect_CYP_DO |> mutate(Unscaled=(DO_mean*2.169)+6.170)
## Depth ##
Effect_CYP_DEPTH<- effect(term = "DEPTH_mean", mod = Global_model_CYP)
Effect_CYP_DEPTH<- as.data.frame(Effect_CYP_DEPTH) |>
  mutate(Species="CYP")
Effect_CYP_DEPTH<- Effect_CYP_DEPTH |> mutate(Unscaled=(DEPTH_mean*0.041)+74.568)

#### BBH Modeling ####
Effect_BBH_DO<- effect(term= "DO_mean", mod=Global_model_BBH)
Effect_BBH_DO<- as.data.frame(Effect_BBH_DO) |>
  mutate(Species="BBH")
Effect_BBH_DO<- Effect_BBH_DO |> mutate(Unscaled=(DO_mean*2.169)+6.170)

Effect_BBH_SD<- effect(term = "Julian_Date", mod = Global_model_BBH)
Effect_BBH_SD<- as.data.frame(Effect_BBH_SD) |>
  mutate(Species="BBH")

Effect_BBH_TEMP<- effect(term="TEMP_mean", mod=Global_model_BBH)
Effect_BBH_TEMP<- as.data.frame(Effect_BBH_TEMP) |> 
  mutate(Species="BBH")
Effect_BBH_TEMP<-Effect_BBH_TEMP |> mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_BBH_DEPTH<- effect(term = "DEPTH_mean", mod = Global_model_BBH)
Effect_BBH_DEPTH<- as.data.frame(Effect_BBH_DEPTH) |>
  mutate(Species="BBH")

Effect_BBH_DEPTH<- Effect_BBH_DEPTH |> mutate(Unscaled=(DEPTH_mean*0.041)+74.568)

#### LMB Modeling ####
Effect_LMB_DO<- effect(term= "DO_mean", mod=Global_model_LMB)
Effect_LMB_DO<- as.data.frame(Effect_LMB_DO) |>
  mutate(Species="LMB") |>
  mutate(Unscaled=(DO_mean*2.169)+6.170)

Effect_LMB_SD<- effect(term = "Julian_Date", mod = Global_model_LMB)
Effect_LMB_SD<- as.data.frame(Effect_LMB_SD) |>
  mutate(Species="LMB")

Effect_LMB_TEMP<- effect(term= "TEMP_mean", mod=Global_model_LMB)
Effect_LMB_TEMP<- as.data.frame(Effect_LMB_TEMP) |>
  mutate(Species="LMB") |> 
  mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_LMB_DEPTH<- effect(term= "DEPTH_mean", mod=Global_model_LMB)

Effect_LMB_DEPTH<- as.data.frame(Effect_LMB_DEPTH) |> 
  mutate(Unscaled=(DEPTH_mean*0.041)+74.568) |>
  mutate(Species="LMB")

#### LEP Modeling ####
Effect_LEP_TEMP<- effect(term= "TEMP_mean", mod=Global_model_LEP)
Effect_LEP_TEMP<- as.data.frame(Effect_LEP_TEMP) |>
  mutate(Species="LEP") |> 
  mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_LEP_DO<- effect(term= "DO_mean", mod=Global_model_LEP)
Effect_LEP_DO<- as.data.frame(Effect_LEP_DO) |> 
  mutate(Species="LEP") |>
  mutate(Unscaled=(DO_mean*2.169)+6.170)


Effect_LEP_SD<- effect(term= "Julian_Date", mod=Global_model_LEP) 
Effect_LEP_SD<- as.data.frame(Effect_LEP_SD)  |>
mutate(Species="LEP")

Effect_LEP_DEPTH<- effect(term= "DEPTH_mean", mod=Global_model_LEP)
Effect_LEP_DEPTH<- as.data.frame(Effect_LEP_DEPTH) |>
  mutate(Species="LEP")

Effect_LEP_DEPTH<- Effect_LEP_DEPTH |> mutate(Unscaled=(DEPTH_mean*0.041)+74.568)

#### SMB Modeling ####
Effect_SMB_DO<- effect(term= "DO_mean", mod=Global_model_SMB)
Effect_SMB_DO<- as.data.frame(Effect_SMB_DO) |>
  mutate(Species="SMB") |>
  mutate(Unscaled=(DO_mean*2.169)+6.170)

Effect_SMB_TEMP<- effect(term= "TEMP_mean", mod=Global_model_SMB)
Effect_SMB_TEMP<- as.data.frame(Effect_SMB_TEMP)|>
  mutate(Species="SMB") |> 
  mutate(Unscaled=(TEMP_mean*2.234)+23.020)

Effect_SMB_SD<- effect(term = "Julian_Date", mod = Global_model_SMB)
Effect_SMB_SD<- as.data.frame(Effect_SMB_SD) |>
  mutate(Species="SMB")

Effect_SMB_DEPTH<- effect(term= "DEPTH_mean", mod=Global_model_SMB)

Effect_SMB_SD<- effect(term= "Julian_Date", mod=Global_model_SMB)
Effect_SMB_SD<- as.data.frame(Effect_SMB_SD) |>
  mutate(Species="SMB")

Effect_SMB_DEPTH<- as.data.frame(Effect_SMB_DEPTH)
Effect_SMB_DEPTH<- Effect_SMB_DEPTH |>
  mutate(Unscaled=(DEPTH_mean*0.041)+74.568) |>
  mutate(Species="SMB")

#### Study Day Effects Plots ####
                                                                                                       
SD<- bind_rows(Effect_CYP_SD, Effect_BBH_SD, Effect_LMB_SD, Effect_LEP_SD, Effect_SMB_SD)
SD$Species<- factor(SD$Species, 
                    levels=c("CYP","BBH", "LMB",
                             "LEP","SMB"))
Figure_SD<- ggplot()+
  geom_ribbon(data=SD, aes(Julian_Date, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=SD, aes(Julian_Date, y=fit, group=Species))+
  geom_line(data=SD, aes(Julian_Date, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=13),
                   axis.title = element_text(size=14),
                   strip.text = element_text(size=14),
                   legend.text=element_text(size=14),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily Catch")+xlab("Julian Day")+
  facet_wrap(~Species, nrow=1, scales = "free")
Figure_SD

ggsave("Figure_4.png", dpi = 300, height = 2.5, width = 10)

#### Water TEMP Effects Plots ####
TEMP<- bind_rows(Effect_CYP_TEMP,Effect_BBH_TEMP, Effect_LMB_TEMP, 
                 Effect_LEP_TEMP, Effect_SMB_TEMP)
TEMP$Species<- factor(TEMP$Species, 
                    levels=c("CYP","BBH", "LMB",
                             "LEP","SMB"))
Figure_TEMP<- ggplot()+
  geom_ribbon(data=TEMP, aes(Unscaled, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=TEMP, aes(Unscaled, y=fit, group=Species))+
  geom_line(data=TEMP, aes(Unscaled, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=13),
                   axis.title = element_text(size=14),
                   strip.text = element_text(size=14),
                   legend.text=element_text(size=14),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily Catch")+xlab("Daily Mean Water Temp. (°C)")+
  facet_wrap(~Species, scales = "free", nrow = 1)
Figure_TEMP

#### DO Effects Plots ####

DO<- bind_rows(Effect_CYP_DO, Effect_BBH_DO, Effect_LMB_DO, 
                 Effect_LEP_DO, Effect_SMB_DO)
DO$Species<- factor(SD$Species, 
                    levels=c("CYP","BBH", "LMB",
                             "LEP","SMB"))
Figure_DO<- ggplot()+
  geom_ribbon(data=DO, aes(Unscaled, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=DO, aes(Unscaled, y=fit, group=Species))+
  geom_line(data=DO, aes(Unscaled, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=13),
                   axis.title = element_text(size=14),
                   strip.text = element_text(size=14),
                   legend.text=element_text(size=14),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily Catch")+xlab("Daily Mean Dissolved Oxygen (mg/l)")+
  facet_wrap(~Species, scales = "free", nrow = 1)
Figure_DO

Figure_5<- Figure_DO / Figure_TEMP
Figure_5
ggsave("Figure_5.png", dpi = 300, height = 5, width = 10)


#### Depth Effects Plots ####
Depth<- bind_rows(Effect_CYP_DEPTH, Effect_BBH_DEPTH, Effect_LEP_DEPTH, Effect_LMB_DEPTH, Effect_SMB_DEPTH)
Depth$Species<- factor(Depth$Species, 
                    levels=c("CYP","BBH", "LMB",
                             "LEP","SMB"))

Figure_Depth<- ggplot()+
  geom_ribbon(data=Depth, aes(Unscaled, ymin=lower, ymax=upper, group=Species), 
              fill="grey",
              alpha=0.6)+
  geom_point(data=Depth, aes(Unscaled, y=fit, group=Species))+
  geom_line(data=Depth, aes(Unscaled, y=fit, group=Species), linewidth=1)+
  theme_bw()+theme(legend.position = "none",
                   axis.text = element_text(size=13),
                   axis.title = element_text(size=14),
                   strip.text = element_text(size=14),
                   legend.text=element_text(size=14),
                   strip.background = element_rect (fill = "white"),
                   plot.title = element_text(hjust=0.5, size=14, face="bold"))+
  ylab("Daily Catch")+xlab("Daily Mean Water Level (m)")+
  facet_wrap(~Species, nrow=1, scales = "free")
Figure_Depth


ggsave("Figure_7_Appendix.png", dpi = 300, height = 2.5, width = 12)
