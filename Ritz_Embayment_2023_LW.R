################################################################################
####              Fish Dissection Data                                         #### 
####              Flynn Bay, Eel Bay, Clayton, NY completed by TR           ####               
#################################################################################

#### Packages used for the following data preparation and analysis ####
library(dplyr)
library(ggpubr)
library(zoo)
library(tidyverse)
library(lubridate)
library(stats)
library(broom)
library(ggpubr)
library(readxl)
library(writexl)
library(tidymodels)
library(ggpmisc)
library(patchwork)
options(scipen=999, digits = 2) 

##################

###############################################################################################################################
################                                             ###############
###############################################################################################################################

#### set working directory and make sure dissection data file in order ####

setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")
LW_YOY_Raw<- read_excel("LW_YOY_2021.xlsx")
LW_YOY_Raw$Date<-as.POSIXct(LW_YOY_Raw$Date, format="%Y-%m-%d")
LW_YOY_Raw$Species<- as.factor(LW_YOY_Raw$Species) 
LW_YOY_Raw$Location<- as.factor(LW_YOY_Raw$Location)

#### Filter species of interest and create study data variable 1-Start of sampling ##
LW_YOY<- LW_YOY_Raw |>
  select(-Notes)|>
  arrange(Date) |>
  group_by(Date) |>
  mutate(Study_Day= cur_group_id()) |>
  filter(Species %in% c("ICTALURID", "CYPRINIDAE", 
                        "LEPOMIS", "LMB", "SMB")) |>
  mutate(K = Weight/(Length^3) * 100,0000) |>
  filter(Study_Day <=42)

#### Linear model with total length ~ study date ####

LW_YOY_SUM<- LW_YOY |>
  group_by(Species, Location) |>
  summarise(Mean_Length=mean(Length), Mean_Weight=mean(Weight),
            Length_sd=sd(Length), Weight_sd=sd(Weight))

shapiro.test(LW_YOY$Length)


ggplot(LW_YOY, aes(log(Length)))+
  geom_histogram(bins=20)+
  facet_wrap(~Species)

Model_LM<- LW_YOY |>
  group_by(Location, Species) |>
  mutate(Log_length=log(Length), Log_weight=log(Weight)) |>
  group_modify(~ broom::tidy(lm(Study_Day ~ Log_length, data = .x)))


LW_YOY$Date<-as.Date(LW_YOY$Date,format="%M-%d")

LW_YOY_EEL<- LW_YOY |>
  filter(Location %in% "Eel Bay")

EEL<- ggplot(LW_YOY_EEL, aes(Date, log(Length)))+
  geom_smooth(method="lm", color="black")+
  geom_point()+
  theme_bw()+
  theme(legend.position = "bottom", legend.text = element_text(size=14), legend.title = element_text(size = 15),
        axis.text = element_text(size=13),
         axis.title = element_text(size=14),
         strip.text = element_text(size=14), axis.title.x = element_blank(),
        strip.background = element_rect (fill = "white"),
        plot.title = element_text(hjust = 0.5,size=14),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_poly_eq(use_label(c("R2", "p"))) +ylab("Log Total Length (mm)")+
  scale_x_date(date_breaks = "5 days", 
               minor_breaks = "1 days",
               date_labels = "%b %d",
               limits=as.Date(c("2021-06-11","2021-07-22")))+
  scale_y_continuous(breaks = seq(2,4,0.4), limits=c(2.3,4))+
  ggtitle("Eel Bay")+
  facet_wrap(~Species, nrow=5, scales="free_y")

LW_YOY_FLYNN<- LW_YOY |>
  filter(Location %in% "Flynn Bay")

FLYNN<- ggplot(LW_YOY_FLYNN, aes(Date, log(Length)))+
  geom_smooth(method="lm", color="black")+
  geom_point()+
  theme_bw()+
  theme(legend.position = "bottom", legend.text = element_text(size=14), legend.title = element_text(size = 15),
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        strip.text = element_text(size=14), axis.title.x = element_blank(),
        strip.background = element_rect (fill = "white"),
        plot.title = element_text(hjust = 0.5,size=14),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_poly_eq(use_label(c("R2", "p"))) +ylab("Log Total Length (mm)")+
  scale_x_date(date_breaks = "5 days", 
               minor_breaks = "1 days",
               date_labels = "%b %d",
               limits=as.Date(c("2021-06-11","2021-07-22")))+
  scale_y_continuous(breaks = seq(2,4,0.4), limits=c(2.3,4))+
  ggtitle("Flynn Bay")+
  facet_wrap(~Species, nrow=5, scales="free_y")


EEL+FLYNN

ggsave("Figure_4.png", dpi=300, height=8, width=10)

