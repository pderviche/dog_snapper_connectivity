##################
#Boxplots
##################

#packages
library(dplyr)
library(ggplot2)
library(ggbreak)

#Clean R environment 
rm(list = ls())

#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")

#Read data
data <- read.csv2("data_dog_snappers.csv")

length(unique(data$ID))
table(unique(data$Site))


# Combined update of `Site` and `Habitat`
data <- data %>%
  mutate(
    # Update `Site` first, ensuring all site names are transformed correctly
    Site = case_when(
      Site == "Caravelas" ~ "CA",
      Site == "Piraque-acu" ~ "PA",
      Site == "Sao Mateus" ~ "SM",
      Site == "Itapemerim" ~ "IT",
      Site == "Gramute" ~ "GR",
      Site == "Guarapari" ~ "GU",
      Site == "Abrolhos Bank" ~ "AK",
      Site == "PARNA Abrolhos" ~ "AB",
      Site == "Forgotten reefs" ~ "FR",
      Site == "Marataizes" ~ "MA",
      TRUE ~ Site
    ),
    
    # Update `Habitat` based on the updated `Site`
    Habitat = case_when(
      Site %in% c("SM", "IT") ~ 'Brackish',
      Site %in% c("CA", "PA") ~ 'Saline',
      Site == "GR" ~ 'Marine',
      Site %in% c("GU", "MA") ~ 'Upwelling',
      Site %in% c("AB", "FR","AK") ~ 'Non-upwelling'
    )
  )

data <- mutate_at(data, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Rb85, Sr87, Ba138, Pb208, 
                               TL, FL, SL, Weight)), as.numeric)


#####
#Edge
#####

#Select the last rows in each ID
edge <- data %>%
  group_by(ID) %>%
  slice_tail(n = 5)

#Set as numeric
edge <- mutate_at(edge, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                                           Zn66, Rb85, Sr87, Ba138, Pb208, 
                                           TL, FL, SL, Weight)), as.numeric)

#Using dplyr to calculate mean
edge <- edge %>%
  group_by(Site, ID) %>%
  summarise(Mg24 = mean(Mg24),
            Al27 = mean(Al27),
            Cr52 = mean(Cr52),
            Fe57 = mean(Fe57),
            Cu65 = mean(Cu65),
            Zn66 = mean(Zn66),
            Rb85 = mean(Rb85),
            Sr87 = mean(Sr87),
            Ba138 = mean(Ba138),
            Pb208 = mean(Pb208))

#Categoric information
info <- data %>%
  select(ID, Habitat, Day, Month, Year, Season, lat, lon, TL, FL, SL, Weight)

#Exclude duplicates
info <- distinct(info, ID, .keep_all = TRUE)

#Merge datasets
data_edge <- left_join(edge, info, by = "ID")
data_edge <- data_edge %>% group_by(Site, ID)

data_edge$Fulton <- 100*data_edge$Weight/(data_edge$TL*0.1)^3

#Reorganize
data_edge <- data_edge[, c("ID","Site","Habitat",
                                           "Mg24", "Al27", "Cr52", "Fe57", "Cu65",
                                           "Zn66", "Rb85", "Sr87", "Ba138", "Pb208", 
                                           "TL", "FL", "SL", "Weight", "Fulton", "Season","Month","Year")]

str(data_edge)
#write.table(data_edge,"data_edge.csv", sep=";", dec=".",row.names = F)



#####
#Core
#####

#Select the last rows in each ID
core <- data %>%
  group_by(ID) %>%
  slice_head(n = 5)

#Set as numeric
core <- mutate_at(core, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Rb85, Sr87, Ba138, Pb208, 
                               TL, FL, SL, Weight)), as.numeric)

#Using dplyr to calculate mean
core <- core %>%
  group_by(Site, ID) %>%
  summarise(Mg24 = mean(Mg24),
            Al27 = mean(Al27),
            Cr52 = mean(Cr52),
            Fe57 = mean(Fe57),
            Cu65 = mean(Cu65),
            Zn66 = mean(Zn66),
            Rb85 = mean(Rb85),
            Sr87 = mean(Sr87),
            Ba138 = mean(Ba138),
            Pb208 = mean(Pb208))

#Categoric information
info <- data %>%
  select(ID, Habitat, Day, Month, Year, Season, lat, lon, TL, FL, SL, Weight)

#Exclude duplicates
info <- distinct(info, ID, .keep_all = TRUE)

#Merge datasets
data_core <- left_join(core, info, by = "ID")
data_core <- data_core %>% group_by(Site, ID)

data_core$Fulton <- 100*data_core$Weight/(data_core$TL*0.1)^3

#Reorganize
data_core <- data_edge[, c("ID","Site","Habitat",
                           "Mg24", "Al27", "Cr52", "Fe57", "Cu65",
                           "Zn66", "Rb85", "Sr87", "Ba138", "Pb208", 
                           "TL", "FL", "SL", "Weight", "Fulton", "Season","Month","Year")]

str(data_core)
#write.table(data_core,"data_core.csv", sep=";", dec=".",row.names = F)

###################################
###################################




###################################
# Box plots - otolith edge
###################################

#Clean R environment 
rm(list = ls())

#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")

#Read data
data <- read.csv2("data_edge.csv")

length(unique(data$ID))
table(unique(data$Site))

#Set as numeric
data <- mutate_at(data, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Rb85, Sr87, Ba138, Pb208, 
                               TL, FL, SL, Weight, Fulton)), as.numeric)

data$Site <- factor(data$Site, levels = c("SM","IT","CA","PA","GR","AB", "AK", "FR","GU", "MA"))

data <- data %>%
  mutate(
    # Update `Site` first, ensuring all site names are transformed correctly
    Habitat = case_when(
      Habitat == "Brackish" ~ "Brackish ENH",
      Habitat == "Saline" ~ "Saline ENH",
      Habitat == "Marine" ~ "Marine NH",
      Habitat == "Non-upwelling" ~ "Non-upwelling AG",
      Habitat == "Upwelling" ~ "Upwelling AG"))

data$Habitat <- factor(data$Habitat, levels = c("Brackish ENH","Saline ENH","Marine NH","Non-upwelling AG","Upwelling AG"))

habitat_colors <- c("Upwelling AG" = "#01b0f6", 
                    "Non-upwelling AG" = "#e876f3", 
                    "Marine NH" = "#66FF66",
                    "Saline ENH" = "#a3a500",
                    "Brackish ENH" = "#f8766d")

#packages
library(FSA)
library(rcompanion)

#########
#Mg24
#########

data_Mg <- data
data_Mg <- data_Mg %>%  filter(!(ID == 'ID335'))

#letters based on post hoc Dunn's test
dunn_Mg <- dunnTest(Mg24 ~ Site, data = data_Mg, method = "bonferroni")$res
dunn_Mg
cld_Mg <- cldList(P.adj ~ Comparison, data=dunn_Mg)
cld_Mg
cld_Mg <- as.data.frame(cld_Mg)                    
cld_Mg <- cld_Mg %>%
  rename(Site = Group)
cld_Mg

letter_Mg <- group_by(data_Mg, Site, Habitat) %>%
  summarise(mean=mean(Mg24), quant = quantile(Mg24, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Mg <- as.data.frame(letter_Mg)                    
letter_Mg

letter_Mg <- merge(letter_Mg, cld_Mg, by = "Site")
letter_Mg

Mg24<-ggplot(data_Mg, aes(x=Site, y=Mg24, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.4) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Mg:Ca[otolith]))+
  geom_text(data = letter_Mg, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Mg24

#########
#Al27
#########

data_Al <- data

data_Al <- data_Al %>%  filter(!(ID == 'ID204'))

#letters based on post hoc Dunn's test
dunn_Al <- dunnTest(Al27 ~ Site, data = data_Al, method = "bonferroni")$res
dunn_Al
cld_Al <- cldList(P.adj ~ Comparison, data=dunn_Al)
cld_Al
cld_Al <- as.data.frame(cld_Al)                    
cld_Al <- cld_Al %>%
  rename(Site = Group)
cld_Al

letter_Al <- group_by(data_Al, Site, Habitat) %>%
  summarise(mean=mean(Al27), quant = quantile(Al27, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Al <- as.data.frame(letter_Al)                    
letter_Al

letter_Al <- merge(letter_Al, cld_Al, by = "Site")
letter_Al

Al27 <-  ggplot(data_Al, aes(x=Site, y=Al27, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Al:Ca[otolith]))+
  geom_text(data = letter_Al, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Al27

#########
#Cr52 
#########

data_Cr <- data

#letters based on post hoc Dunn's test
dunn_Cr <- dunnTest(Cr52 ~ Site, data = data_Cr, method = "bonferroni")$res
dunn_Cr
cld_Cr <- cldList(P.adj ~ Comparison, data=dunn_Cr)
cld_Cr
cld_Cr <- as.data.frame(cld_Cr)                    
cld_Cr <- cld_Cr %>%
  rename(Site = Group)
cld_Cr

letter_Cr <- group_by(data_Cr, Site, Habitat) %>%
  summarise(mean=mean(Cr52), quant = quantile(Cr52, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Cr <- as.data.frame(letter_Cr)                    
letter_Cr

letter_Cr <- merge(letter_Cr, cld_Cr, by = "Site")
letter_Cr

Cr52 <-  ggplot(data_Cr, aes(x=Site, y=Cr52, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Cr:Ca[otolith]))+
  geom_text(data = letter_Cr, aes(x = Site, y = quant, label = Letter), vjust=-6, size = 4.5)

Cr52


#########
#Fe57
#########

data_Fe <- data
data_Fe <- data[!is.na(data$Fe57), ]

data_Fe <- data_Fe %>%  filter(!(ID == 'ID432'))
data_Fe <- data_Fe %>%  filter(!(ID == 'ID319'))
data_Fe <- data_Fe %>%  filter(!(ID == 'ID441'))
data_Fe <- data_Fe %>%  filter(!(ID == 'ID279'))
data_Fe <- data_Fe %>%  filter(!(ID == 'ID192'))
data_Fe <- data_Fe %>%  filter(!(ID == 'ID338'))

sum(is.na(data_Fe$Fe57))

#letters based on post hoc Dunn's test
dunn_Fe <- dunnTest(Fe57 ~ Site, data = data_Fe, method = "bonferroni")$res
dunn_Fe
cld_Fe <- cldList(P.adj ~ Comparison, data=dunn_Fe)
cld_Fe
cld_Fe <- as.data.frame(cld_Fe)                    
cld_Fe <- cld_Fe %>%
  rename(Site = Group)
cld_Fe

letter_Fe <- group_by(data_Fe, Site, Habitat) %>%
  summarise(mean=mean(Fe57), quant = quantile(Fe57, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Fe <- as.data.frame(letter_Fe)                    
letter_Fe

letter_Fe <- merge(letter_Fe, cld_Fe, by = "Site")
letter_Fe

Fe57 <-  ggplot(data_Fe, aes(x=Site, y=Fe57, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Fe:Ca[otolith]))+
  geom_text(data = letter_Fe, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Fe57



#########
#Cu65
#########

data_Cu <- data
data_Cu <- data_Cu %>%  filter(!(ID == 'ID076'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID502'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID482'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID202'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID307'))

#letters based on post hoc Dunn's test
dunn_Cu <- dunnTest(Cu65 ~ Site, data = data_Cu, method = "bonferroni")$res
dunn_Cu
cld_Cu <- cldList(P.adj ~ Comparison, data=dunn_Cu)
cld_Cu
cld_Cu <- as.data.frame(cld_Cu)                    
cld_Cu <- cld_Cu %>%
  rename(Site = Group)
cld_Cu

letter_Cu <- group_by(data_Cu, Site, Habitat) %>%
  summarise(mean=mean(Cu65), quant = quantile(Cu65, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Cu <- as.data.frame(letter_Cu)                    
letter_Cu

letter_Cu <- merge(letter_Cu, cld_Cu, by = "Site")
letter_Cu

Cu65 <-  ggplot(data_Cu, aes(x=Site, y=Cu65, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Cu:Ca[otolith]))+
  geom_text(data = letter_Cu, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Cu65


#########
#Zn66
#########

data_Zn <- data
data_Zn <- data_Zn %>%  filter(!(ID == 'ID259'))
data_Zn <- data_Zn %>%  filter(!(ID == 'ID274'))
data_Zn <- data_Zn %>%  filter(!(ID == 'ID076'))

#letters based on post hoc Dunn's test
dunn_Zn <- dunnTest(Zn66 ~ Site, data = data_Zn, method = "bonferroni")$res
dunn_Zn
cld_Zn <- cldList(P.adj ~ Comparison, data=dunn_Zn)
cld_Zn
cld_Zn <- as.data.frame(cld_Zn)                    
cld_Zn <- cld_Zn %>%
  rename(Site = Group)
cld_Zn

letter_Zn <- group_by(data_Zn, Site, Habitat) %>%
  summarise(mean=mean(Zn66), quant = quantile(Zn66, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Zn <- as.data.frame(letter_Zn)                    
letter_Zn

letter_Zn <- merge(letter_Zn, cld_Zn, by = "Site")
letter_Zn

Zn66 <-  ggplot(data_Zn, aes(x=Site, y=Zn66, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Zn:Ca[otolith]))+
  geom_text(data = letter_Zn, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Zn66

#########
#Rb85
#########

data_Rb <- data

#letters based on post hoc Dunn's test
dunn_Rb <- dunnTest(Rb85 ~ Site, data = data_Rb, method = "bonferroni")$res
dunn_Rb
cld_Rb <- cldList(P.adj ~ Comparison, data=dunn_Rb)
cld_Rb
cld_Rb <- as.data.frame(cld_Rb)                    
cld_Rb <- cld_Rb %>%
  rename(Site = Group)
cld_Rb

letter_Rb <- group_by(data_Rb, Site, Habitat) %>%
  summarise(mean=mean(Rb85), quant = quantile(Rb85, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Rb <- as.data.frame(letter_Rb)                    
letter_Rb

letter_Rb <- merge(letter_Rb, cld_Rb, by = "Site")
letter_Rb

Rb85 <-  ggplot(data_Rb, aes(x=Site, y=Rb85, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Rb:Ca[otolith]))+
  geom_text(data = letter_Rb, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Rb85


#########
#Sr87
#########

data_Sr <- data
data_Sr$Sr87m <- data$Sr87/1000

#letters based on post hoc Dunn's test
dunn_Sr <- dunnTest(Sr87m ~ Site, data = data_Sr, method = "bonferroni")$res
dunn_Sr
cld_Sr <- cldList(P.adj ~ Comparison, data=dunn_Sr)
cld_Sr
cld_Sr <- as.data.frame(cld_Sr)                    
cld_Sr <- cld_Sr %>%
  rename(Site = Group)
cld_Sr

letter_Sr <- group_by(data_Sr, Site, Habitat) %>%
  summarise(mean=mean(Sr87m), quant = quantile(Sr87m, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Sr <- as.data.frame(letter_Sr)                    
letter_Sr

letter_Sr <- merge(letter_Sr, cld_Sr, by = "Site")
letter_Sr

Sr87 <-  ggplot(data_Sr, aes(x=Site, y=Sr87m, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=15), 
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Sr:Ca[otolith]~(mmol~mol^-1)))+
  geom_text(data = letter_Sr, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5) 

Sr87


#########
#Ba138
#########

data_Ba <- data

#letters based on post hoc Dunn's test
dunn_Ba <- dunnTest(Ba138 ~ Site, data = data_Ba, method = "bonferroni")$res
dunn_Ba
cld_Ba <- cldList(P.adj ~ Comparison, data=dunn_Ba)
cld_Ba
cld_Ba <- as.data.frame(cld_Ba)                    
cld_Ba <- cld_Ba %>%
  rename(Site = Group)
cld_Ba

letter_Ba <- group_by(data_Ba, Site, Habitat) %>%
  summarise(mean=mean(Ba138), quant = quantile(Ba138, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Ba <- as.data.frame(letter_Ba)                    
letter_Ba

letter_Ba <- merge(letter_Ba, cld_Ba, by = "Site")
letter_Ba

Ba138 <-  ggplot(data_Ba, aes(x=Site, y=Ba138, fill=Habitat)) +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  theme_light() +
  scale_fill_manual(values = habitat_colors) +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text = element_text(size = 15), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + 
  ylab(expression(Ba:Ca[otolith])) +
  geom_text(data = letter_Ba, aes(x = Site, y = quant, label = Letter), vjust = -6, size = 4.5) +
  scale_y_break(c(3, 10), scales = 1.5)

Ba138


data_Ba %>%
  group_by(Habitat) %>%
  summarise(
    mean = mean(Ba138, na.rm = TRUE),
    median = median(Ba138, na.rm = TRUE),
    se = std.error(Ba138, na.rm = TRUE),
    sd = sd(Ba138, na.rm = TRUE))


data_Ba2 <- data_Ba
data_Ba2 <- data_Ba2 %>% filter(!(Site %in% c('SM', 'IT')))
data_Ba2 <- data_Ba2 %>% filter(!(Habitat  == ('Brackish NH')))
data_Ba2 <- data_Ba2 %>% filter(!(ID == 'ID104' & TL == '189'))
data_Ba2 <- data_Ba2 %>% filter(!(ID == 'ID096' & TL == '212'))
data_Ba2 <- data_Ba2 %>% filter(!(ID == 'ID408' & TL == '34'))
data_Ba2 <- data_Ba2 %>% filter(!(ID == 'ID194' & TL == '128'))


data_Ba2 %>%
  group_by(Habitat) %>%
  summarise(
    mean = mean(Ba138, na.rm = TRUE),
    median = median(Ba138, na.rm = TRUE),
    se = std.error(Ba138, na.rm = TRUE),
    sd = sd(Ba138, na.rm = TRUE))

Ba138_2 <-  ggplot(data_Ba2, aes(x=Site, y=Ba138, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Ba:Ca[otolith]))+
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'cyan2', size =1) 

Ba138_2

#########
#Pb208
#########

data_Pb <- data
data_Pb <- data[!is.na(data$Pb208), ]
data_Pb <- data_Pb %>%  filter(!(ID == 'ID202'))
data_Pb <- data_Pb %>%  filter(!(ID == 'ID016'))
data_Pb <- data_Pb %>%  filter(!(ID == 'ID076'))
data_Pb <- data_Pb %>%  filter(!(ID == 'ID047'))
data_Pb <- data_Pb %>%  filter(!(ID == 'ID072'))

#letters Pbsed on post hoc Dunn's test
dunn_Pb <- dunnTest(Pb208 ~ Site, data = data_Pb, method = "bonferroni")$res
dunn_Pb
cld_Pb <- cldList(P.adj ~ Comparison, data=dunn_Pb)
cld_Pb
cld_Pb <- as.data.frame(cld_Pb)                    
cld_Pb <- cld_Pb %>%
  rename(Site = Group)
cld_Pb

letter_Pb <- group_by(data_Pb, Site, Habitat) %>%
  summarise(mean=mean(Pb208), quant = quantile(Pb208, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Pb <- as.data.frame(letter_Pb)                    
letter_Pb

letter_Pb <- merge(letter_Pb, cld_Pb, by = "Site")
letter_Pb

Pb208 <-  ggplot(data_Pb, aes(x=Site, y=Pb208, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Pb:Ca[otolith]))+
  geom_text(data = letter_Pb, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Pb208


#####
# Figure
#####

library(ggpubr)

ggarrange(Mg24, Al27, Cr52, Fe57,
          Cu65, Zn66, Zn66, Pb208,
          labels = c("A","B","C","D","E","F","G","H"),
          ncol = 2, nrow =4,
          legend = "right", common.legend = T)
#1000 x 1200

Ba138
#550 x 366












###################################
# Box plots - otolith core
###################################

#Clean R environment 
rm(list = ls())

#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")

#Read data
data <- read.csv2("data_core.csv")

length(unique(data$ID))
table(unique(data$Site))

#Set as numeric
data <- mutate_at(data, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Rb85, Sr87, Ba138, Pb208, 
                               TL, FL, SL, Weight,Fulton)), as.numeric)

data$Site <- factor(data$Site, levels = c("CA","PA","SM","IT","GR","AB", "AK", "FR","GU", "MA"))

habitat_colors <- c("Upwelling" = "#56B4E9", 
                    "Non-upwelling" = "#fbb4af", 
                    "Marine" = "aquamarine2",
                    "Brackish" = "chartreuse1",
                    "Saline" = "cyan4")

#packages
library(FSA)
library(rcompanion)

#########
#Mg24
#########

data_Mg <- data

#letters based on post hoc Dunn's test
dunn_Mg <- dunnTest(Mg24 ~ Site, data = data_Mg, method = "bonferroni")$res
dunn_Mg
cld_Mg <- cldList(P.adj ~ Comparison, data=dunn_Mg)
cld_Mg
cld_Mg <- as.data.frame(cld_Mg)                    
cld_Mg <- cld_Mg %>%
  rename(Site = Group)
cld_Mg

letter_Mg <- group_by(data_Mg, Site, Habitat) %>%
  summarise(mean=mean(Mg24), quant = quantile(Mg24, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Mg <- as.data.frame(letter_Mg)                    
letter_Mg

letter_Mg <- merge(letter_Mg, cld_Mg, by = "Site")
letter_Mg

Mg24<-ggplot(data_Mg, aes(x=Site, y=Mg24, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.4) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Mg:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Mg, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Mg24

#########
#Al27
#########

data_Al <- data

data_Al <- data_Al %>%  filter(!(ID == 'ID427'))

#letters based on post hoc Dunn's test
dunn_Al <- dunnTest(Al27 ~ Site, data = data_Al, method = "bonferroni")$res
dunn_Al
cld_Al <- cldList(P.adj ~ Comparison, data=dunn_Al)
cld_Al
cld_Al <- as.data.frame(cld_Al)                    
cld_Al <- cld_Al %>%
  rename(Site = Group)
cld_Al

letter_Al <- group_by(data_Al, Site, Habitat) %>%
  summarise(mean=mean(Al27), quant = quantile(Al27, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Al <- as.data.frame(letter_Al)                    
letter_Al

letter_Al <- merge(letter_Al, cld_Al, by = "Site")
letter_Al

Al27 <-  ggplot(data_Al, aes(x=Site, y=Al27, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Al:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Al, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Al27

#########
#Cr52 
#########

data_Cr <- data

#letters based on post hoc Dunn's test
dunn_Cr <- dunnTest(Cr52 ~ Site, data = data_Cr, method = "bonferroni")$res
dunn_Cr
cld_Cr <- cldList(P.adj ~ Comparison, data=dunn_Cr)
cld_Cr
cld_Cr <- as.data.frame(cld_Cr)                    
cld_Cr <- cld_Cr %>%
  rename(Site = Group)
cld_Cr

letter_Cr <- group_by(data_Cr, Site, Habitat) %>%
  summarise(mean=mean(Cr52), quant = quantile(Cr52, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Cr <- as.data.frame(letter_Cr)                    
letter_Cr

letter_Cr <- merge(letter_Cr, cld_Cr, by = "Site")
letter_Cr

Cr52 <-  ggplot(data_Cr, aes(x=Site, y=Cr52, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Cr:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Cr, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Cr52


#########
#Fe57
#########

data_Fe <- data
data_Fe <- data[!is.na(data$Fe57), ]

data_Fe <- data_Fe %>%  filter(!(ID == 'ID279'))
data_Fe <- data_Fe %>%  filter(!(ID == 'ID102'))
data_Fe <- data_Fe %>%  filter(!(ID == 'ID430'))

sum(is.na(data_Fe$Fe57))

#letters based on post hoc Dunn's test
dunn_Fe <- dunnTest(Fe57 ~ Site, data = data_Fe, method = "bonferroni")$res
dunn_Fe
cld_Fe <- cldList(P.adj ~ Comparison, data=dunn_Fe)
cld_Fe
cld_Fe <- as.data.frame(cld_Fe)                    
cld_Fe <- cld_Fe %>%
  rename(Site = Group)
cld_Fe

letter_Fe <- group_by(data_Fe, Site, Habitat) %>%
  summarise(mean=mean(Fe57), quant = quantile(Fe57, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Fe <- as.data.frame(letter_Fe)                    
letter_Fe

letter_Fe <- merge(letter_Fe, cld_Fe, by = "Site")
letter_Fe

Fe57 <-  ggplot(data_Fe, aes(x=Site, y=Fe57, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Fe:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Fe, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Fe57



#########
#Cu65
#########

data_Cu <- data
data_Cu <- data_Cu %>%  filter(!(ID == 'ID102'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID158'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID504'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID502'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID305'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID072'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID427'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID508'))
data_Cu <- data_Cu %>%  filter(!(ID == 'ID258'))

#letters based on post hoc Dunn's test
dunn_Cu <- dunnTest(Cu65 ~ Site, data = data_Cu, method = "bonferroni")$res
dunn_Cu
cld_Cu <- cldList(P.adj ~ Comparison, data=dunn_Cu)
cld_Cu
cld_Cu <- as.data.frame(cld_Cu)                    
cld_Cu <- cld_Cu %>%
  rename(Site = Group)
cld_Cu

letter_Cu <- group_by(data_Cu, Site, Habitat) %>%
  summarise(mean=mean(Cu65), quant = quantile(Cu65, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Cu <- as.data.frame(letter_Cu)                    
letter_Cu

letter_Cu <- merge(letter_Cu, cld_Cu, by = "Site")
letter_Cu

Cu65 <-  ggplot(data_Cu, aes(x=Site, y=Cu65, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Cu:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Cu, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Cu65


#########
#Zn66
#########

data_Zn <- data
data_Zn <- data_Zn %>%  filter(!(ID == 'ID427'))

#letters based on post hoc Dunn's test
dunn_Zn <- dunnTest(Zn66 ~ Site, data = data_Zn, method = "bonferroni")$res
dunn_Zn
cld_Zn <- cldList(P.adj ~ Comparison, data=dunn_Zn)
cld_Zn
cld_Zn <- as.data.frame(cld_Zn)                    
cld_Zn <- cld_Zn %>%
  rename(Site = Group)
cld_Zn

letter_Zn <- group_by(data_Zn, Site, Habitat) %>%
  summarise(mean=mean(Zn66), quant = quantile(Zn66, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Zn <- as.data.frame(letter_Zn)                    
letter_Zn

letter_Zn <- merge(letter_Zn, cld_Zn, by = "Site")
letter_Zn

Zn66 <-  ggplot(data_Zn, aes(x=Site, y=Zn66, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Zn:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Zn, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Zn66

#########
#Rb85
#########

data_Rb <- data

#letters based on post hoc Dunn's test
dunn_Rb <- dunnTest(Rb85 ~ Site, data = data_Rb, method = "bonferroni")$res
dunn_Rb
cld_Rb <- cldList(P.adj ~ Comparison, data=dunn_Rb)
cld_Rb
cld_Rb <- as.data.frame(cld_Rb)                    
cld_Rb <- cld_Rb %>%
  rename(Site = Group)
cld_Rb

letter_Rb <- group_by(data_Rb, Site, Habitat) %>%
  summarise(mean=mean(Rb85), quant = quantile(Rb85, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Rb <- as.data.frame(letter_Rb)                    
letter_Rb

letter_Rb <- merge(letter_Rb, cld_Rb, by = "Site")
letter_Rb

Rb85 <-  ggplot(data_Rb, aes(x=Site, y=Rb85, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Rb:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Rb, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Rb85


#########
#Sr87
#########

data_Sr <- data
data_Sr$Sr87m <- data$Sr87/1000

#letters based on post hoc Dunn's test
dunn_Sr <- dunnTest(Sr87m ~ Site, data = data_Sr, method = "bonferroni")$res
dunn_Sr
cld_Sr <- cldList(P.adj ~ Comparison, data=dunn_Sr)
cld_Sr
cld_Sr <- as.data.frame(cld_Sr)                    
cld_Sr <- cld_Sr %>%
  rename(Site = Group)
cld_Sr

letter_Sr <- group_by(data_Sr, Site, Habitat) %>%
  summarise(mean=mean(Sr87m), quant = quantile(Sr87m, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Sr <- as.data.frame(letter_Sr)                    
letter_Sr

letter_Sr <- merge(letter_Sr, cld_Sr, by = "Site")
letter_Sr

Sr87 <-  ggplot(data_Sr, aes(x=Site, y=Sr87m, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Sr:Ca[otolith]~(mmol~mol^-1)))+
  geom_text(data = letter_Sr, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5) +
  ylim(1.8,4)

Sr87


#########
#Ba138
#########

data_Ba <- data

#letters based on post hoc Dunn's test
dunn_Ba <- dunnTest(Ba138 ~ Site, data = data_Ba, method = "bonferroni")$res
dunn_Ba
cld_Ba <- cldList(P.adj ~ Comparison, data=dunn_Ba)
cld_Ba
cld_Ba <- as.data.frame(cld_Ba)                    
cld_Ba <- cld_Ba %>%
  rename(Site = Group)
cld_Ba

letter_Ba <- group_by(data_Ba, Site, Habitat) %>%
  summarise(mean=mean(Ba138), quant = quantile(Ba138, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Ba <- as.data.frame(letter_Ba)                    
letter_Ba

letter_Ba <- merge(letter_Ba, cld_Ba, by = "Site")
letter_Ba

Ba138 <-  ggplot(data_Ba, aes(x=Site, y=Ba138, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Ba, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Ba138


#########
#Pb208
#########

data_Pb <- data
data_Pb <- data[!is.na(data$Pb208), ]
data_Pb <- data_Pb %>%  filter(!(ID == 'ID432'))
data_Pb <- data_Pb %>%  filter(!(ID == 'ID278'))


#letters Pbsed on post hoc Dunn's test
dunn_Pb <- dunnTest(Pb208 ~ Site, data = data_Pb, method = "bonferroni")$res
dunn_Pb
cld_Pb <- cldList(P.adj ~ Comparison, data=dunn_Pb)
cld_Pb
cld_Pb <- as.data.frame(cld_Pb)                    
cld_Pb <- cld_Pb %>%
  rename(Site = Group)
cld_Pb

letter_Pb <- group_by(data_Pb, Site, Habitat) %>%
  summarise(mean=mean(Pb208), quant = quantile(Pb208, probs = 0.75)) %>%
  arrange(desc(mean))
letter_Pb <- as.data.frame(letter_Pb)                    
letter_Pb

letter_Pb <- merge(letter_Pb, cld_Pb, by = "Site")
letter_Pb

Pb208 <-  ggplot(data_Pb, aes(x=Site, y=Pb208, fill=Habitat)) +
  geom_boxplot(alpha = 0.5,width=0.5) +
  theme_light() +
  scale_fill_manual(values=habitat_colors) +
  theme(legend.position="none",
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15)) +
  xlab("") + ylab(expression(Pb:Ca[otolith]~(mu~mol~mol^-1)))+
  geom_text(data = letter_Pb, aes(x = Site, y = quant, label = Letter), vjust=-6,  size = 4.5)

Pb208


#####
#Select the elements for further analysis
#####

Mg24      
Al27      
Cr52      
Fe57      
Cu65      
Zn66     
Ba138    
Pb208     

library(ggpubr)

ggarrange(Mg24, Al27, Cr52, Fe57,
          Cu65, Zn66, Zn66, Pb208,
          labels = c("A","B","C","D","E","F","G","H"),
          ncol = 2, nrow =3,
          legend = "none")
#1200 x 1200




