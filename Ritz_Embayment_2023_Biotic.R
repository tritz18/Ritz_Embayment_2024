#### Biotic Data analysis for Ritz & Farrell 2024 ##
## Data dervied from daily catch data ####

#### Packages used ####
library(dplyr)
library(zoo)
library(ggpubr)
library(readxl)
library(stats)
library(broom)
library(mgcv)
library(car)
library(purrr)
library(ggeffects)
library(tidyverse)
library(rstatix)
library(ggh4x)
library(writexl)

#############
options(scipen = 999, digits = 6)
##### Set working directory #### 
setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

#### Catch Data file preperation for figures ####

Catch_Raw<- read_excel("Ritz_Embayment_2023_Catch.xlsx")
Catch_Raw$Species<- as.factor(Catch_Raw$Species)
Catch_Raw$Location<- as.factor(Catch_Raw$Location)

Catch_Plot<- Catch_Raw |>
  filter(Catch >0)

Catch_Plot$Date<- as.Date(Catch_Plot$Date,  format="%M-%d")


Catch<- ggplot(Catch_Plot, aes(Date, Catch, group= interaction(Location,Species), 
                              fill=Location))+
  geom_col()+
  theme_bw()+scale_fill_manual(values=c("#003366", "#3366CC"))+
  theme(axis.text = element_text(size=13 ),
        axis.title = element_text(size=14, face="bold"), 
        axis.title.x = element_blank(),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),
        strip.background = element_rect (fill = "white"),
        legend.position=c(0.75,0.10), legend.text = element_text(size=13), legend.title = element_text(size = 14))+
  ylab("Daily Catch")+ 
  scale_x_date(date_breaks = "14 days", 
               minor_breaks = "7 days",
               date_labels = "%b %d",
               limits=as.Date(c("2021-06-11","2021-07-22")))+
  facet_wrap(~Species, scales="free", nrow = 4)
Catch

Catch +
  facetted_pos_scales(
    y = list(
      Species == "LEPOMIS" ~ scale_y_continuous(limits = c(0,60)),
      Species == "CARP" ~ scale_y_continuous(limits = c(0,20)),
      Species == "CYPRINIDAE" ~ scale_y_continuous(limits = c(0,2000)),
      Species == "ICTALURID" ~ scale_y_continuous(limits=c(0,1000)),
      Species == "SMB" ~ scale_y_continuous(limits=c(0,300))))


ggsave("Figure_3.png", dpi=300, height = 8, width=12)

