#### Abiotic Data Cleaning and Data Generation for Ritz & Farrell 2024 ##
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
## This file contains pooled daily loggeer data from each wetland ##
## Depth data - Daily mean & mean change calculated ####

Abiotic<- read_excel("Ritz_Embayment_2023_Abiotic.xlsx")

Depth_Raw<- read_excel("Ritz_Embayment_2023_ABAY.xlsx")

Depth<- Depth_Raw |>
  arrange(Date) |>
  group_by(Location) |>
  mutate(DEPTH_mean_change= (DEPTH_mean-dplyr::lag(DEPTH_mean, n=2))) |>
  group_by(Date) |> 
  dplyr::mutate(Study_Day = cur_group_id()) 

Depth<- Depth |>
  mutate_at(c("DEPTH_mean_change"), ~replace_na(.,0))


#### Statistical testing on abiotic data with the following steps ##
## Check for normality using Shapiro Test and Levene Test ##
## Most if not all abiotic data not normally distributed, transformations did not help ##
## PCA chosen to help visiulize differences ##

#### Dissolved Oxygen PCA ####

Abiotic_DO<- Abiotic |>
 select(DO_mean, DO_max, DO_min, -Location) 

pca_res_DO <- prcomp(Abiotic_DO, scale. = TRUE)
summary(pca_res_DO)

DO_df <- cbind(pca_res_DO$x[,1:3], Abiotic)
PCAloadings_DO <- data.frame(Variables = rownames(pca_res_DO$rotation), pca_res_DO$rotation)

DO_PCA<-ggplot(DO_df, aes(PC1, PC2, color = Location)) + 
  geom_point() +
  stat_ellipse()+
  geom_segment(data = PCAloadings_DO, aes(x = 0, y = 0, xend = (PC1*5),
  yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")),color = "black") +
  annotate("text", x = (PCAloadings_DO$PC1*5), y = (PCAloadings_DO$PC2*5),
           label = PCAloadings_DO$Variables)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title = element_text(size=13, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13))+
  scale_color_manual(values=c("#003366", "#3366CC"))+
  scale_fill_manual(values=c("#003366", "#3366CC"))+
  xlab("PCA1 (81%)")+ylab("PCA2 (16%)")


#### Water Temperature PCA ####

Abiotic_TEMP<- Abiotic |>
  select(TEMP_mean, TEMP_max, TEMP_min, -Location) 

pca_res_TEMP <- prcomp(Abiotic_TEMP, scale. = TRUE)
summary(pca_res_TEMP)

TEMP_df <- cbind(pca_res_TEMP$x[,1:3], Abiotic)
PCAloadings_TEMP <- data.frame(Variables = rownames(pca_res_TEMP$rotation), pca_res_TEMP$rotation)

TEMP_PCA<-ggplot(TEMP_df, aes(PC1, PC2, color = Location)) + 
  geom_point() +
  stat_ellipse()+
  geom_segment(data = PCAloadings_TEMP, aes(x = 0, y = 0, xend = (PC1*5),
                                          yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")),color = "black") +
  annotate("text", x = (PCAloadings_TEMP$PC1*5), y = (PCAloadings_TEMP$PC2*5),
           label = PCAloadings_TEMP$Variables)+
  theme_bw()+
  theme(legend.position = "right",
        axis.text = element_text(size=12),
        axis.title = element_text(size=13, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13))+
  scale_color_manual(values=c("#003366", "#3366CC"))+
  scale_fill_manual(values=c("#003366", "#3366CC"))+
  xlab("PCA1 (82%)")+ylab("PCA2 (17%)")

Figure_2<- DO_PCA + TEMP_PCA
Figure_2

ggsave("Figure_2.png", dpi=300, width=8, height=4)

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






