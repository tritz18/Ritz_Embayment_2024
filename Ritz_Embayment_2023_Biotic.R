#### Biotic Catch Data analysis for Ritz & Farrell 2024 ####

#### Packages used ####
library(zoo)
library(ggpubr)
library(readxl)
library(car)
library(purrr)
library(tidyverse)
library(writexl)
library(ggh4x)
library(patchwork)
library(dplyr)
options(scipen = 999, digits = 6)

##### Set working directory #### 
setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

#### Catch Data file preparation for figures ####
Catch_Raw<- read_excel("Ritz_Embayment_2023_Catch.xlsx")
Catch_Raw$Species<- as.factor(Catch_Raw$Species)
Catch_Raw$Location<- as.factor(Catch_Raw$Location)

Catch_Sum<- Catch_Raw |>
  group_by(Species,Location, Date) |>
  filter(Species %in% c("CYP", "LMB", "LEP","BBH", "SMB")) |>
  summarize(Total_Catch=sum(Catch), Catch_Per_Day=(sum(Catch)/42))

#### Table 2 data generation ####
Table2<- Catch_Sum |>
  group_by(Species, Location) |>
  summarize(MeanCPD= mean(Catch_Per_Day), SD=sd(Catch_Per_Day))

#### Figure 2 data preperation and plotting ####
Catch_Plot<- Catch_Raw |>
  filter(Catch >0) |>
  filter(Species %in% c("CYP", "LMB", "LEP","BBH", "SMB"))

Catch_Plot$Date<- as.Date(Catch_Plot$Date,  format="%M-%d")
Catch_Plot$Species<- factor(Catch_Plot$Species, 
                            levels=c("CYP","BBH", "LMB",
                                     "LEP","SMB"))
#### Eel Bay Plot ####
annotations <- data.frame(
  xmin = c("2021-06-11", "2021-06-11", "2021-06-15", "2021-06-17", "2021-06-25"),
  xmax = c("2021-06-21", "2021-07-03", "2021-06-29", "2021-07-10", "2021-07-10"),
  Species = c("CYP", "LMB", "BBH", "LEP","SMB"))
annotations$xmin<- as.Date(annotations$xmin)
annotations$xmax<- as.Date(annotations$xmax)
annotations$Species<-as.factor(annotations$Species)

Catch_Eel<- Catch_Plot |>
  filter(Location %in% "Eel Bay")

Catch_Eel_Plot<- ggplot(Catch_Eel, aes(Date, Catch))+
  geom_col(color="black", fill="#CCCCCC")+
  geom_rect(data=annotations, aes(xmin = xmin, xmax = xmax, 
           ymin = -Inf, ymax = Inf,x = NULL, y = NULL), 
            fill = "grey",alpha=0.5) +
  theme_bw()+
  theme(axis.text = element_text(size=13 ),
        axis.title = element_text(size=14), 
        axis.title.x = element_blank(),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),
        strip.background = element_rect (fill = "white"),
        plot.title = element_text(hjust = 0.5,size=14, face="bold"),
        legend.position="bottom", legend.title = element_text(size = 14))+
  ylab("Daily Catch")+ 
  scale_x_date(date_breaks = "5 days", 
               minor_breaks = "1 days",
               date_labels = "%b %d",
               limits=as.Date(c("2021-06-11","2021-07-22")))+
  facet_wrap(~Species, scales="free_y", nrow = 5)
Catch_Eel_Plot

EEL<-Catch_Eel_Plot +
  facetted_pos_scales(
    y = list(
      Species == "LEP" ~ scale_y_continuous(limits = c(0,60)),
      Species == "CYP" ~ scale_y_continuous(limits = c(0,2000)),
      Species == "BBH" ~ scale_y_continuous(limits=c(0,1000)),
      Species == "SMB" ~ scale_y_continuous(limits=c(0,300))))+
  ggtitle("Eel Bay")
EEL

#### Flynn Bay Plot ####
annotations_Flynn <- data.frame(
  xmin = c("2021-06-11", "2021-06-11", "2021-06-15", "2021-06-27", "2021-06-26"),
  xmax = c("2021-06-13", "2021-07-04", "2021-06-29", "2021-07-16", "2021-07-04"),
  Species = c("CYP", "LMB", "BBH", "LEP","SMB"))
annotations_Flynn$xmin<- as.Date(annotations_Flynn$xmin)
annotations_Flynn$xmax<- as.Date(annotations_Flynn$xmax)
annotations_Flynn$Species<-as.factor(annotations_Flynn$Species)

Catch_Flynn<- Catch_Plot |>
  filter(Location %in% "Flynn Bay")

Catch_Flynn_Plot<- ggplot(Catch_Flynn, aes(Date, Catch))+
  geom_col(color="black", fill="grey")+
  geom_rect(data=annotations_Flynn, aes(xmin = xmin, xmax = xmax, 
                                  ymin = -Inf, ymax = Inf,x = NULL, y = NULL), 
            fill = "grey",alpha=0.5) +
  theme_bw()+
  theme(axis.text = element_text(size=13 ),
        axis.title = element_text(size=14), 
        axis.title.x = element_blank(),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),
        strip.background = element_rect (fill = "white"),
        plot.title = element_text(hjust = 0.5, size=14, face="bold"))+
  ylab("Daily Catch")+ 
  scale_x_date(date_breaks = "5 days", 
               minor_breaks = "1 days",
               date_labels = "%b %d",
               limits=as.Date(c("2021-06-11","2021-07-22")))+
  facet_wrap(~Species, scales="free_y", nrow = 5)
Catch_Flynn_Plot

FLYNN<-Catch_Flynn_Plot +
  facetted_pos_scales(
    y = list(
      Species == "LEP" ~ scale_y_continuous(limits = c(0,8)),
      Species == "CYP" ~ scale_y_continuous(limits = c(0,2000)),
      Species == "BBH" ~ scale_y_continuous(limits=c(0,1000)),
      Species == "SMB" ~ scale_y_continuous(limits=c(0,30))))+
  ggtitle("Flynn Bay")
FLYNN

#### Combine plots for Figure 2 ####
EEL+FLYNN

ggsave("Figure_3.png", dpi=300, height = 8, width=12)

