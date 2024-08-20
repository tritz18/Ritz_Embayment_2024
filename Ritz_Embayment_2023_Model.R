#### Model data standardization, generation, and pooling for Ritz and Farrell 2024 ####
library(zoo)
library(MASS)
library(ggpubr)
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(stats)
library(broom)
library(mgcv)
library(car)
library(lme4)
library(sjPlot)
library(dplyr)

#### Set working directory  and read in catch data and abiotic data #### 
setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

Catch_DF<- read_excel("Ritz_Embayment_2023_Catch.xlsx")
Catch_DF$Date<-as.Date(Catch_DF$Date, format="%Y-%m-%d")
Catch_DF$Location<-as.factor(Catch_DF$Location)

Catch_DF$Species<- as.factor(Catch_DF$Species)

Abiotic_DF<- read_excel("Ritz_Embayment_2023_Abiotic.xlsx")
Abiotic_DF$Date<-as.Date(Abiotic_DF$Date, format="%Y-%m-%d")
Abiotic_DF$Location<-as.factor(Abiotic_DF$Location)

Depth_DF<- read_excel("Ritz_Embayment_2023_ABAY.xlsx")
Depth_DF$Date<-as.Date(Depth_DF$Date, format="%Y-%m-%d")
Depth_DF$Location<-as.factor(Depth_DF$Location)

Depth_DF<- Depth_DF |>
  arrange(Date) |>
  group_by(Location) |>
  mutate(DEPTH_mean_change= DEPTH_mean-dplyr::lag(DEPTH_mean, lag=1)) |>
  arrange(Date) |>
  group_by(Date) |> 
  mutate(Study_Day = cur_group_id()) |>
  filter(Study_Day <=42) |>
  ungroup()

Depth_DF[is.na(Depth_DF)] <- 0

#### Join logger data and gauge dataframes for modeling ####

Abiotic <- left_join(Abiotic_DF, Depth_DF, by=c("Location", "Study_Day", "Date")) 

Abiotic$Study_Day <- as.factor(Abiotic$Study_Day)

#### Scale and center covariates ####
Abiotic_Scaled<- Abiotic |>
mutate(across(where(is.numeric), scale))

Abiotic_Scaled$Study_Day<- as.numeric(Abiotic_Scaled$Study_Day)

#### Join together abiotic and biotic dataframes #### 

Model<- left_join(Catch_DF, Abiotic_Scaled, by=c("Location", "Study_Day", "Date"))

Model<- as.data.frame(Model)

Model<- Model |>
  mutate(Julian_Date=format(Date, "%j")) 
  
write_xlsx(Model, "Ritz_Embayment_2023_Model.xlsx")


#### Summary statistics on abiotic data ####

DO_Mean<- mean(Abiotic$`DO Mean`)
DO_SD<- sd(Abiotic$`DO Mean`)

TEMP_mean<- mean(Abiotic$`TEMP Mean`)
TEMP_SD<- sd(Abiotic$`TEMP Mean`)

DEPTH_mean<- mean(Depth_DF$DEPTH_mean)
DEPTH_sd<- sd(Depth_DF$DEPTH_mean)
