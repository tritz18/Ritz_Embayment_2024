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

#### Set working directory and read in data files (Data available upon request) ####
options(scipen = 999, digits = 6)
setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

#### Read in Abiotic File (DO, TEMP, DEPTH) ##
## This file contains pooled daily logger data from each wetland ##
## Depth data - Daily mean & mean change calculated ####

Abiotic<- read_excel("Ritz_Embayment_2023_Abiotic_ALL.xlsx")
Abiotic$Location<- as.factor(Abiotic$Location)
Abiotic$Date<- as.Date(Abiotic$Date)

Depth<- read_excel("Ritz_Embayment_2023_ABAY.xlsx")
Depth <- Depth |>
  mutate(Date=format(Date, "%m-%d"))
Depth$Date<-as.Date(Depth$Date, format = "%m-%d")

Depth$Location<- as.factor(Depth$Location)
Depth$Year<- as.factor(Depth$Year)

shaprio.test(Abiotic$`DO Mean`)
ggplot(Abiotic, aes(DO_mean))+
  geom_histogram()

#### Statistical testing on abiotic data led to non-normal data, ##
## where transformations could not fix. For example DO min lots of zeros ##
## numerous ties within data set so no non parametric comparisons. ####

#### Eel Bay Abiotic Data Filtering & Visualization ####
Abiotic_DO_Eel<- Abiotic |>
  filter(Location %in% "Eel Bay") |>
  select("DO Mean", "DO Max.", "DO Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_DO_Eel$Type<- as.factor(Abiotic_DO_Eel$Type)

Abiotic_TEMP_Eel<- Abiotic |>
  filter(Location %in% "Eel Bay") |>
  select("TEMP Mean", "TEMP Max.", "TEMP Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_TEMP_Eel$Type<- as.factor(Abiotic_TEMP_Eel$Type)

annotations_DO <- data.frame(xmin = c("2021-06-11"),xmax = c("2021-07-22"),
                          Type=c("DO Max.", "DO Mean", "DO Min."))
annotations_DO$xmin<- as.Date(annotations_DO$xmin)
annotations_DO$xmax<- as.Date(annotations_DO$xmax)

DO_Eel_plot<- ggplot(Abiotic_DO_Eel, aes(Date, Measurement, 
                  group=Type, lty=Type))+
  geom_rect(data=annotations_DO, aes(xmin = xmin, xmax = xmax, 
                                     ymin = -Inf, ymax = Inf,x = NULL, y = NULL), 
            fill = "grey",alpha=0.5) +
  geom_point(size=0.8)+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  geom_hline(yintercept = 3,color="#666666", linewidth=1)+
  theme_bw()+ylab("Dissolved Oxygen (mg/l)")+xlab("Date")+
  scale_y_continuous(breaks=seq(0,20,4), limits=c(0,20))+
  scale_x_date(date_breaks = "12 days", 
               minor_breaks = "2 days",
               date_labels = "%b %d")+
  theme(legend.title = element_blank(), 
        legend.position =c(0.76,0.92), 
        legend.text = element_text(size=11), axis.text.x = element_blank(),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+guides(lty = guide_legend(nrow = 1))
  
DO_Eel_plot

annotations_TEMP <- data.frame(xmin = c("2021-06-11"),xmax = c("2021-07-22"),
                             Type=c("TEMP Max.", "TEMP Mean", "TEMP Min."))
annotations_TEMP$xmin<- as.Date(annotations_TEMP$xmin)
annotations_TEMP$xmax<- as.Date(annotations_TEMP$xmax)


TEMP_Eel_plot<- ggplot(Abiotic_TEMP_Eel, aes(Date, Measurement, 
                                         group=Type, lty=Type))+
  geom_rect(data=annotations_TEMP, aes(xmin = xmin, xmax = xmax, 
            ymin = -Inf, ymax = Inf,x = NULL, y = NULL), 
            fill = "grey",alpha=0.5) +
  geom_point(size=0.8)+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  theme_bw()+ylab("Water Temperature (°C)")+xlab("Date")+
  scale_y_continuous(breaks=seq(14,34,4), limits=c(14,34))+
  scale_x_date(date_breaks = "12 days", 
               minor_breaks = "2 days",
               date_labels = "%b %d")+
  theme(legend.title = element_blank(), legend.position =c(0.75,0.92),
        legend.text = element_text(size=11),
        axis.text = element_text(size=13), axis.text.x = element_blank(),
        axis.title = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+guides(lty = guide_legend(nrow = 1))

TEMP_Eel_plot

annotations_Depth <- data.frame(xmin = c("2024-06-11"),xmax = c("2024-07-22"),
                               Type=c("DEPTH Mean"),
                               Year=c("2018", "2019", "2020", "2021"))
annotations_Depth$xmin<- as.Date(annotations_Depth$xmin)
annotations_Depth$xmax<- as.Date(annotations_Depth$xmax)

DEPTH_plot<- ggplot(Depth, aes(Date, DEPTH_mean, group=Year, lty=Year))+
  geom_rect(data=annotations_Depth, aes(xmin = xmin, xmax = xmax, 
        ymin = -Inf, ymax = Inf,x = NULL, y = NULL), 
            fill = "grey",alpha=0.5) +
  geom_point(size=0.8)+
  geom_line(linewidth=1)+
  theme_bw()+ylab("Water Level (m)")+xlab("Date")+
  scale_x_date(date_breaks = "12 days", 
               minor_breaks = "2 days",
               date_labels = "%b %d")+
  scale_y_continuous(breaks=seq(74.50,75.75,.2), limits=c(74.48,75.75))+
  scale_linetype_manual(values=c(1,3,6,4))+
  theme(legend.title = element_blank(), 
        legend.position =c(0.75,0.92),
        legend.text = element_text(size=8),
        axis.text = element_text(size=14),
        axis.title = element_text(size=15),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))+guides(lty = guide_legend(nrow = 1))
DEPTH_plot

EEL<- DO_Eel_plot / TEMP_Eel_plot / DEPTH_plot + guides(colour="none",
               lty=guide_legend(order=1, nrow = 1))+plot_annotation(title = "Eel Bay") &
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
EEL

#### Flynn Bay Abiotic Data Filtering and Visulization #### 

Abiotic_DO_Flynn<- Abiotic |>
  filter(Location %in% "Flynn Bay") |>
  select("DO Mean", "DO Max.", "DO Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_DO_Flynn$Type<- as.factor(Abiotic_DO_Flynn$Type)

Abiotic_TEMP_Flynn<- Abiotic |>
  filter(Location %in% "Flynn Bay") |>
  select("TEMP Mean", "TEMP Max.", "TEMP Min.", "Location", "Date") |>
  gather(key = "Type", value = "Measurement", -Location, -Date)
Abiotic_TEMP_Flynn$Type<- as.factor(Abiotic_TEMP_Flynn$Type)

DO_Flynn_plot<- ggplot(Abiotic_DO_Flynn, aes(Date, Measurement, 
                                         group=Type, lty=Type))+
  geom_rect(data=annotations_DO, aes(xmin = xmin, xmax = xmax, 
                                        ymin = -Inf, ymax = Inf,x = NULL, y = NULL), 
            fill = "grey",alpha=0.5) +
  geom_point(size=0.8)+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  geom_hline(yintercept = 3,color="#666666", linewidth=1)+
  theme_bw()+ylab("Dissolved Oxygen (mg/l)")+xlab("Date")+
  scale_y_continuous(breaks=seq(0,20,4), limits=c(0,20))+
  scale_x_date(date_breaks = "12 days", 
               minor_breaks = "2 days",
               date_labels = "%b %d")+
  theme(legend.title = element_blank(), 
        legend.position =c(0.76,0.92), 
        legend.text = element_text(size=11), axis.text.x = element_blank(),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        axis.title.x = element_blank())+guides(lty = guide_legend(nrow = 1))

DO_Flynn_plot

TEMP_Flynn_plot<- ggplot(Abiotic_TEMP_Flynn, aes(Date, Measurement, 
                     group=Type, lty=Type))+
  geom_rect(data=annotations_TEMP, aes(xmin = xmin, xmax = xmax, 
                                        ymin = -Inf, ymax = Inf,x = NULL, y = NULL), 
            fill = "grey",alpha=0.5) +
  geom_point(size=0.8)+
  geom_line(linewidth=1)+
  scale_linetype_manual(values=c("dotted","solid","twodash"))+
  theme_bw()+ylab("Water Temperature (°C)")+xlab("Date")+
  scale_y_continuous(breaks=seq(14,34,4), limits=c(14,34))+
  scale_x_date(date_breaks = "12 days", 
               minor_breaks = "2 days",
               date_labels = "%b %d")+
  theme(legend.title = element_blank(), legend.position =c(0.75,0.92), 
        legend.text = element_text(size=11),
        axis.text = element_text(size=13), axis.text.x = element_blank(),
        axis.title = element_text(size=15),
        axis.title.x = element_blank())+guides(lty = guide_legend(nrow = 1))

TEMP_Flynn_plot

FLYNN<- DO_Flynn_plot / TEMP_Flynn_plot / DEPTH_plot + 
  guides(colour="none",lty=guide_legend(order=1, nrow = 1))+plot_annotation(title = "Flynn Bay") &
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
FLYNN

Figure_2<- wrap_elements(EEL) | wrap_elements(FLYNN) 
Figure_2

ggsave("Figure_2.png", dpi=300, width=18, height=12)

