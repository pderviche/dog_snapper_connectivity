##
# Patrick Derviche


#########################
#Select data
#########################
library(dplyr)
library(vegan)
library(factoextra)
library(FactoMineR)


#################################################
# PCA - All data
#################################################

#########
#1.1 Edge
#########

#Clean R environment 
rm(list = ls())

#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")

#Read data
edge <- read.csv2("data_edge.csv")

length(unique(edge$ID))
table(unique(edge$Site))
table(unique(edge$Habitat))

edge <- edge %>%
  mutate(
    Habitat = case_when(
      Site %in% c("GU", "MA") ~ "Upwelling AG",
      Site %in% c("AB", "AK", "FR") ~ "Non-upwelling AG",
      Site == "GR" ~ "Marine NH",
      Site %in% c("SM", "IT") ~ "Brackish ENH",
      Site %in% c("PA", "CA") ~ "Saline ENH",
      TRUE ~ NA_character_ ))

str(edge)

#Set as numeric
edge <- mutate_at(edge, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Ba138, Pb208)), as.numeric)

library(corrplot)

#Significant elements
edge <- edge[,c("Site","Habitat","Mg24","Al27","Cr52","Fe57","Cu65","Zn66","Ba138","Pb208")]

edge[is.na(edge)] <- 0

#Scale the variables
edge$Mg24 <- log(edge$Mg24 +1)
edge$Al27 <- log(edge$Al27 +1)
edge$Cr52 <- log(edge$Cr52 +1)
edge$Fe57 <- log(edge$Fe57 +1)
edge$Cu65 <- log(edge$Cu65 +1)
edge$Zn66 <- log(edge$Zn66 +1)
edge$Ba138 <- log(edge$Ba138 +1)
edge$Pb208 <- log(edge$Pb208 +1)

#Rename
names(edge)[names(edge) == "Mg24"] <- "Mg"
names(edge)[names(edge) == "Al27"] <- "Al"
names(edge)[names(edge) == "Cr52"] <- "Cr"
names(edge)[names(edge) == "Fe57"] <- "Fe"
names(edge)[names(edge) == "Cu65"] <- "Cu"
names(edge)[names(edge) == "Zn66"] <- "Zn"
names(edge)[names(edge) == "Ba138"] <- "Ba"
names(edge)[names(edge) == "Pb208"] <- "Pb"


#Function to identify outliers
identify_outliers <- function(x, threshold = 3) {
  z_scores <- scale(x)
  outliers <- abs(z_scores) > threshold
  return(outliers)}

summary(identify_outliers(edge[, 3:10]))

#Function to replaces outliers within individual samples
replace_outliers_with_na <- function(x, threshold = 3) {
  z_scores <- scale(x)
  x[abs(z_scores) > threshold] <- NA
  return(x)
}

#Identify numeric columns
edge_elements <- sapply(edge, is.numeric)

# Apply the function only to numeric columns
data_edge <- edge  # Create a copy to keep original data structure
data_edge[edge_elements] <- lapply(edge[edge_elements], replace_outliers_with_na)

data_edge <- na.omit(data_edge)
str(data_edge)



#PCA
res_pca_edge <- PCA(data_edge[, 3:10], graph = FALSE,    
               scale.unit = TRUE,           
               ncp = 5)                    
res_pca_edge
groups_edge <- as.factor(data_edge$Habitat[1:224])
groups_edge <- factor(data_edge$Habitat, levels = c("Brackish ENH","Saline ENH","Marine NH", "Non-upwelling AG", "Upwelling AG"))

edge_pca <- fviz_pca_biplot(res_pca_edge, 
                col.ind = groups_edge, 
                addEllipses = TRUE, 
                ellipse.type = "confidence",  ellipse.level = 0.95,
                palette = c("#f8766d", "#a3a500", "#66FF66", "#e876f3", "#01b0f6"),
                legend.title = "Habitat",
                geom.ind = "point",
                pointsize = 1.5,
                repel = TRUE,
                alpha.var = 0.5,
                alpha.ind = 0.2,
                title = "Otolith edge signature")+
  theme_light() 

edge_pca

get_eig(res_pca)
#Dim.1 29.23%
#Dim.2 26.04%
#Total 55.27%






##########
#1.2 Core
##########

#Read data
core <- read.csv2("data_core.csv")

length(unique(core$ID))
table(unique(core$Site))
table(unique(core$Habitat))

core <- core %>%
  mutate(
    Habitat = case_when(
      Site %in% c("GU", "MA") ~ "Upwelling AG",
      Site %in% c("AB", "AK", "FR") ~ "Non-upwelling AG",
      Site == "GR" ~ "Marine NH",
      Site %in% c("SM", "IT") ~ "Brackish ENH",
      Site %in% c("PA", "CA") ~ "Saline ENH",
      TRUE ~ NA_character_  # Add a catch-all condition to handle any unexpected values
    )
  )


#Set as numeric
core <- mutate_at(core, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Ba138, Pb208)), as.numeric)

#Significant elements
core <- core[,c("Site","Habitat","Mg24","Al27","Cr52","Fe57","Cu65","Zn66","Ba138","Pb208")]

core[is.na(core)] <- 0

#Scale the variables
core$Mg24 <- log(core$Mg24 +1)
core$Al27 <- log(core$Al27 +1)
core$Cr52 <- log(core$Cr52 +1)
core$Fe57 <- log(core$Fe57 +1)
core$Cu65 <- log(core$Cu65 +1)
core$Zn66 <- log(core$Zn66 +1)
core$Ba138 <- log(core$Ba138 +1)
core$Pb208 <- log(core$Pb208 +1)

#Rename
names(core)[names(core) == "Mg24"] <- "Mg"
names(core)[names(core) == "Al27"] <- "Al"
names(core)[names(core) == "Cr52"] <- "Cr"
names(core)[names(core) == "Fe57"] <- "Fe"
names(core)[names(core) == "Cu65"] <- "Cu"
names(core)[names(core) == "Zn66"] <- "Zn"
names(core)[names(core) == "Ba138"] <- "Ba"
names(core)[names(core) == "Pb208"] <- "Pb"

#Function to identify outliers
identify_outliers <- function(x, threshold = 3) {
  z_scores <- scale(x)
  outliers <- abs(z_scores) > threshold
  return(outliers)}

summary(identify_outliers(core[, 3:10]))

#Function to replaces outliers within individual samples
replace_outliers_with_na <- function(x, threshold = 3) {
  z_scores <- scale(x)
  x[abs(z_scores) > threshold] <- NA
  return(x)
}

#Identify numeric columns
core_elements <- sapply(core, is.numeric)

# Apply the function only to numeric columns
data_core <- core  # Create a copy to keep original data structure
data_core[core_elements] <- lapply(core[core_elements], replace_outliers_with_na)

data_core <- na.omit(data_core)
str(data_core)


#PCA
res_pca_core <- PCA(data_core[, 3:10], graph = FALSE,    
               scale.unit = TRUE,           
               ncp = 5)                    
res_pca_core
groups_core <- as.factor(core$Habitat[1:238])
groups_core <- factor(groups_core, levels = c("Brackish ENH","Saline ENH","Marine NH", "Non-upwelling AG", "Upwelling AG"))

core_pca <- fviz_pca_biplot(res_pca_core, 
                col.ind = groups_core, 
                addEllipses = TRUE, 
                ellipse.type = "confidence", 
                palette = c("#f8766d", "#a3a500", "#66FF66", "#e876f3", "#01b0f6"),
                legend.title = "Habitat",
                geom.ind = "point",
                pointsize = 1.5,
                repel = TRUE,
                alpha.var = 0.5,
                alpha.ind = 0.2,
                title = "Otolith core signature")+
  theme_light() 

core_pca

get_eig(res_pca)

#Dim.1 29.23%
#Dim.2 26.04%
#Total 55.27%













############
#2.3 Water
############

#Read data
data_water <- read.csv2("water_mol.csv")

data_water <- data_water[,c('Sample', 'Site','Month', 'Year', 'Season', 'tide',
                            "MgCa","AlCa","CrCa","FeCa","CuCa","ZnCa","BaCa","PbCa",
                            'Temp', 'Sal')]

table(unique(data_water$Site))

data_water <- data_water[data_water$Site != 'Santa Maria', ]
data_water <- data_water[data_water$Site != 'Regencia', ]
data_water <- data_water %>%  filter(!(Sample == 'A076'))


data_water <- data_water %>%
  mutate(
    Site = case_when(
      Site == "Caravelas" ~ "CA",
      Site == "Piraque-acu" ~ "PA",
      Site == "Sao Mateus" ~ "SM",
      Site == "Itapemirim" ~ "IT",
      Site == "Gramute" ~ "GR",
      Site == "Guarapari" ~ "GU",
      Site == "Tres Ilhas" ~ "GU",
      Site == "Rasa de Dentro" ~ "GU",
      Site == "Abrolhos Bank" ~ "AK",
      Site == "PARNA Abrolhos" ~ "AB",
      Site == "Esquecidos" ~ "FR",
      Site == "Marataizes" ~ "MA",
      TRUE ~ Site),
    Habitat = case_when(
      Site %in% c("GU", "MA") ~ "Upwelling AG",
      Site %in% c("AB", "AK", "FR") ~ "Non-upwelling AG",
      Site == "GR" ~ "Marine NH",
      Site %in% c("SM", "IT") ~ "Brackish ENH",
      Site %in% c("PA", "CA") ~ "Saline ENH",
      TRUE ~ NA_character_
    )
  )

table(unique(data_water$Habitat))

data_water <- mutate_at(data_water, vars(c("MgCa","AlCa","CrCa","FeCa","CuCa","ZnCa","BaCa","PbCa",
                                            'Temp', 'Sal')), as.numeric)


str(data_water)

library(corrplot)

#Elements
water <- data_water[,c("Site","Habitat","MgCa","AlCa","CrCa","FeCa","CuCa","ZnCa","BaCa","PbCa")]

#Scale the variables
water$MgCa <- log(water$MgCa +1)
water$AlCa <- log(water$AlCa +1)
water$CrCa <- log(water$CrCa +1)
water$FeCa <- log(water$FeCa +1)
water$CuCa <- log(water$CuCa +1)
water$ZnCa <- log(water$ZnCa +1)
water$BaCa <- log(water$BaCa +1)
water$PbCa <- log(water$PbCa +1)

#Rename
names(water)[names(water) == "MgCa"] <- "Mg"
names(water)[names(water) == "AlCa"] <- "Al"
names(water)[names(water) == "CrCa"] <- "Cr"
names(water)[names(water) == "FeCa"] <- "Fe"
names(water)[names(water) == "CuCa"] <- "Cu"
names(water)[names(water) == "ZnCa"] <- "Zn"
names(water)[names(water) == "BaCa"] <- "Ba"
names(water)[names(water) == "PbCa"] <- "Pb"

#PCA
res_pca_water <- PCA(water[, 3:10], graph = FALSE,    
               scale.unit = TRUE,           
               ncp = 5)                    
res_pca_water
groups_water <- as.factor(water$Habitat[1:77])
groups_water <- factor(water$Habitat, levels = c("Brackish ENH","Saline ENH","Marine NH", "Non-upwelling AG", "Upwelling AG"))

water_pca <- fviz_pca_biplot(res_pca_water, 
                         col.ind = groups_water, 
                         addEllipses = TRUE, 
                         ellipse.type = "confidence",  ellipse.level = 0.95,
                         palette = c("#f8766d", "#a3a500", "#66FF66", "#e876f3", "#01b0f6"),
                         legend.title = "Habitat",
                         geom.ind = "point",
                         pointsize = 1.5,
                         repel = TRUE,
                         alpha.var = 0.5,
                         alpha.ind = 0.2,
                         title = "Water chemistry")+
  theme_light() 

water_pca


library(ggpubr)

ggarrange(edge_pca, water_pca,
          labels = c("A","B"),
          ncol = 1, nrow = 2,  common.legend = TRUE,
          legend = "right")
#550 x 700
