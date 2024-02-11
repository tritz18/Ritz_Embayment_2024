#### Abiotic Data Data Generation for Figure 2 Ritz & Farrell 2024 ##
## Data Source : 4 Onset U26 data loggers (2 per wetland) - Water TEMP and DO ##
## Alexandria Bay NOAA Water Level Gauge Station ALXN6 - 8311062 ####

#### Packages used in the following analysis ####
library(zoo)
library(dplyr)
library(Rmisc)
library(ggpubr)
library(plotly)
library(ggfortify)
library(lubridate)
library(tidyverse)
library(readxl)
library(stats)
library(car)
library(patchwork)

####

#### Set working directory and read in data files ####
options(scipen = 999, digits = 6)
setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

#### Read in Abiotic File (DO, TEMP, DEPTH) ##
## This file contains pooled daily logger data from each wetland ##
## Depth data - Daily mean & mean change calculated ####

Abiotic<- read_excel("Ritz_Embayment_2023_Abiotic.xlsx")
Abiotic$Location<- as.factor(Abiotic$Location)

Depth_Raw<- read_excel("Ritz_Embayment_2023_ABAY.xlsx")

Depth<- Depth_Raw |>
  arrange(Date) |>
  group_by(Location) |>
  mutate(DEPTH_mean_change= (DEPTH_mean-dplyr::lag(DEPTH_mean, n=2))) |>
  group_by(Date) |> 
  dplyr::mutate(Study_Day = cur_group_id()) 

Depth<- Depth |>
  mutate_at(c("DEPTH_mean_change"), ~replace_na(.,0))


#### Statistical testing on abiotic data led to non-normal data, ##
## where transformations could not fix. For example DO min lots of zeros ##
## numerous ties within data set so no non parametric comparisons. ####

#### Dissolved Oxygen BOXPLOT ####

Abiotic_DO<- Abiotic |>
 select("DO Mean", "DO Max.", "DO Min.", "Location") |>
  gather(key = "DO_measurement", value = "value", -Location)

DO_plot<-ggplot(Abiotic_DO, aes(DO_measurement, value, color=Location))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  theme_bw()+
  theme(legend.title = element_blank(), 
        legend.position="none",
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13, face="bold"),
        axis.title.x = element_blank(),
        strip.text = element_text(size=13),
        strip.background = element_rect (fill = "white"))+
  ylab("Dissolved Oxygen (mg/l)")+
  scale_color_manual(values=c("#003366", "#3366CC"))+
  scale_y_continuous(breaks=seq(0,20,2), limits=c(0,20))+
  facet_wrap(~Location, scales="free")


#### Water temperature Boxplot ####

Abiotic_TEMP<- Abiotic |>
  select("TEMP Mean", "TEMP Max.", "TEMP Min.", "Location") |>
  gather(key = "TEMP_measurement", value = "value", -Location)

TEMP_Plot<-ggplot(Abiotic_TEMP, aes(TEMP_measurement, value, color=Location))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  theme_bw()+
  theme(legend.title = element_blank(), 
        legend.position="none",
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13, face="bold"),
        axis.title.x = element_blank(),
        strip.text = element_text(size=13),
        strip.background = element_rect (fill = "white"))+
  ylab("Water Temperature (°C)")+
  scale_color_manual(values=c("#003366", "#3366CC"))+
  scale_y_continuous(breaks=seq(14,30,2), limits=c(14,30))+
  facet_wrap(~Location, scales="free")

Figure_2<- DO_plot / TEMP_Plot

ggsave("Figure_2.png", dpi=300, width=8, height=6)



######################################################################################
DO<- Abiotic |>
  filter(Type %in% c("DO_mean","DO_min", "DO_max"))

DO_plot<- ggplot(DO, aes(Date, Measurement, group=interaction(Location, Type), 
                         color=Location, lty=Type))+
  geom_point()+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  scale_color_manual(values=c("#003366", "#3366CC"))+
  theme_bw()+ylab("DO (mg/l)")+xlab("Date")+
  scale_y_continuous(breaks=seq(0,20,2), limits=c(0,20))+
  scale_x_date(date_breaks = "7 days", 
               minor_breaks = "1 days",
               date_labels = "%b %d")+
  theme(legend.title = element_blank(), 
        legend.position="bottom",
        legend.text = element_text(size=11),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12, face="bold"),
        axis.title.x = element_blank(),
        strip.text = element_text(size=12),
        strip.background = element_rect (fill = "white"))+
  facet_wrap(~Location)


DO_plot<- DO_plot+guides(colour="none",
               lty=guide_legend(order=1))
DO_plot
#### TEMP Stats and Graph (Figure 2) #### 


shapiro.test(Abiotic$TEMP_mean)
with(Abiotic, leveneTest(TEMP_mean, Location))

TEMP_mean_TT<- t.test(TEMP_mean ~ Location, data = Abiotic) 
TEMP_mean_TT


shapiro.test(Abiotic$TEMP_min)
with(Abiotic, leveneTest(TEMP_min, Location))

TEMP_min_TT<- t.test(TEMP_min ~ Location, data = Abiotic, var.equal=FALSE) 
TEMP_min_TT

shapiro.test(Abiotic$TEMP_max)
with(Abiotic, leveneTest(TEMP_max, Location))

TEMP_max_TT<- t.test(TEMP_max ~ Location, data = Abiotic, var.equal=FALSE) 
TEMP_max_TT

TEMP<- Abiotic |>
  filter(Type %in% c("TEMP_mean","TEMP_min", "TEMP_max" ))

TEMP_plot<- ggplot(TEMP, aes(Date, Measurement, group=interaction(Location, Type), 
                            color=Location, lty=Type))+
  geom_point()+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  scale_color_manual(values=c("#003366", "#3366CC"))+
  theme_bw()+ylab("Water Temp. (°C)")+xlab("Date")+
  scale_y_continuous(breaks=seq(14,34,2), limits=c(14,34))+
  scale_x_date(date_breaks = "7 days", 
               minor_breaks = "1 days",
               date_labels = "%b %d")+
  theme(legend.title = element_blank(), 
        legend.position="bottom",
        legend.text = element_text(size=11),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12, face="bold"),
        axis.title.x = element_blank(),
        strip.text = element_text(size=12),
        strip.background = element_rect (fill = "white"))+
  facet_wrap(~Location)

TEMP_plot<- TEMP_plot+
  guides(colour="none",
         lty=guide_legend(order=1))


Depth$Date <- as.Date(Depth$Date,  format="%M-%d")

ggplot(Depth, aes(DEPTH_mean))+
  geom_histogram()



with(Abiotic, leveneTest(TEMP_mean, Location))

DEPTH_plot<- ggplot(Depth, aes(Date, DEPTH_mean))+
  geom_point()+
  geom_line(linewidth=1)+
  #scale_linetype_manual(values=c("solid","twodash"))+
  #scale_color_manual(values=c("#003366", "#3366CC"))+
  theme_bw()+ylab("Water Depth (m)")+xlab("Date")+
  scale_x_date(date_breaks = "7 days", 
               minor_breaks = "1 days",
               date_labels = "%b %d")+
  #scale_y_continuous(breaks=seq(-40,120,20), limits=c(-40,122))+
  theme(legend.title = element_blank(), 
        legend.position="bottom",
        legend.text = element_text(size=11),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12, face="bold"),
        axis.title.x = element_blank(),
        strip.text = element_text(size=12))
DEPTH_plot



Combined_Plot<- DO_plot + TEMP_plot + DEPTH_plot + plot_layout(nrow = 3, byrow = TRUE)


Combined_Plot

ggsave("Figure_2.png", dpi=300, height = 8, width=12)






