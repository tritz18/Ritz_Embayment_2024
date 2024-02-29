
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
options(scipen = 999, digits = 6)
library(ggfortify)
library(tidyverse)
library(readxl)
library(gridExtra)
library(factoextra)
library(patchwork)
#### Read in combined abiotic and biotic data for both locations ####

setwd("/Users/thorn/OneDrive/Desktop/Ritz_Embayment_2023/Manuscript_Data_Files/")

Model_DF<- read_excel("Ritz_Embayment_2023_Model.xlsx")
Model_DF$Location<- as.factor(Model_DF$Location)
Model_DF$Species<- as.factor(Model_DF$Species)
Model_DF$Julian_Day<-as.numeric(Model_DF$Julian_Day)

#### Filter out focal species with enough representation in catch ####
Model_DF <- Model_DF |>
  filter(Species %in% c("LMB", "SMB", "LEPOMIS", "CYPRINIDAE", "ICTALURID"))


#### Species Model ####
Model_CYP<- Model_DF |>
  filter(Species %in% "CYPRINIDAE")
Model_ICT<- Model_DF |>
  filter(Species %in% "ICTALURID")
Model_LMB<- Model_DF |>
  filter(Species %in% "LMB") 
Model_LEP<- Model_DF |>
  filter(Species %in% "LEPOMIS")
Model_SMB<- Model_DF |>
  filter(Species %in% "SMB")


PCA_CYP <- Model_CYP |>
  select(-Species,-Location, -Date)

pca_res_CYP <- prcomp(~Catch + DO_mean + 
                        TEMP_mean + 
                        DEPTH_mean + Julian_Day,
                      data=PCA_CYP, scale = TRUE, center=TRUE)


Biplot_CYP<-fviz_pca_biplot(pca_res_CYP, addEllipses = TRUE,ggtheme=theme_bw(),
                habillage=Model_CYP$Location, label = "var",
                palette = c("#666666", "#111111"), col.var = "#111111",
                title="CYPRINIDS")
Biplot_CYP

res.var_CYP <- get_pca_var(pca_res_CYP)
res.var_CYP$coord          # Coordinates
res.var_CYP$contrib        # Contributions to the PCs
res.var_CYP$cos2           # Quality of representation 


PCA_ICT <- Model_ICT |>
  select(-Species,-Location, -Date)

pca_res_ICT <- prcomp(~Catch + DO_mean + 
                        TEMP_mean + 
                        DEPTH_mean + Julian_Day,
                      data=PCA_ICT, scale = TRUE, center=TRUE)


Biplot_ICT<-fviz_pca_biplot(pca_res_ICT, addEllipses = TRUE,ggtheme=theme_bw(),
                      habillage=Model_ICT$Location, label = "var",
                      palette = c("#666666", "#111111"), col.var = "#111111",
                      title="ICTALURIDS")
Biplot_ICT

res.var_ICT <- get_pca_var(pca_res_ICT)
res.var_ICT$coord          # Coordinates
res.var_ICT$contrib        # Contributions to the PCs
res.var_ICT$cos2           # Quality of representation 

          # Quality of representation 

PCA_LEP <- Model_LEP |>
  select(-Species,-Location, -Date)

pca_res_LEP <- prcomp(~Catch + DO_mean + 
                        TEMP_mean + 
                        DEPTH_mean + Julian_Day,
                      data=PCA_LEP, scale = TRUE, center=TRUE)


Biplot_LEP<-fviz_pca_biplot(pca_res_LEP, addEllipses = TRUE,ggtheme=theme_bw(),
                            habillage=Model_LEP$Location, label = "var",
                            palette = c("#666666", "#111111"), col.var = "#111111",
                            title="LEPOMIS")
Biplot_LEP

res.var_LEP <- get_pca_var(pca_res_LEP)
res.var_LEP$coord          # Coordinates
res.var_LEP$contrib        # Contributions to the PCs
res.var_LEP$cos2           # Quality of representation 


PCA_LMB <- Model_LMB |>
  select(-Species,-Location, -Date)

pca_res_LMB <- prcomp(~Catch + DO_mean + 
                        TEMP_mean + 
                        DEPTH_mean+ Julian_Day,
                      data=PCA_LMB, scale = TRUE, center=TRUE)


Biplot_LMB<-fviz_pca_biplot(pca_res_LMB, addEllipses = TRUE,ggtheme=theme_bw(),
                            habillage=Model_LMB$Location, label = "var",
                            palette = c("#666666", "#111111"), col.var = "#111111",
                            title="LMB")
Biplot_LMB

res.var_LMB <- get_pca_var(pca_res_LMB)
res.var_LMB$coord          # Coordinates
res.var_LMB$contrib        # Contributions to the PCs
res.var_LMB$cos2           # Quality of representation 




PCA_SMB <- Model_SMB |>
  select(-Species,-Location, -Date)

pca_res_SMB <- prcomp(~Catch + DO_mean + 
                        TEMP_mean + 
                        DEPTH_mean + Julian_Day,
                      data=PCA_SMB, scale = TRUE, center=TRUE)


Biplot_SMB<-fviz_pca_biplot(pca_res_SMB, addEllipses = TRUE,ggtheme=theme_bw(),
                            habillage=Model_SMB$Location, label = "var",
                            palette = c("#666666", "#111111"), col.var = "#111111",
                            title="SMB")
Biplot_SMB

res.var_SMB <- get_pca_var(pca_res_SMB)
res.var_SMB$coord          # Coordinates
res.var_SMB$contrib        # Contributions to the PCs
res.var_SMB$cos2           # Quality of representation 



PCA_ALL<-Biplot_CYP + Biplot_ICT + Biplot_LEP + Biplot_LMB + Biplot_SMB

ggsave("Figure_7A.png", dpi=300, height = 10, width=14)


