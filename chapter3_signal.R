#packages
library(dplyr)
library(ggplot2)
library(plotrix)

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
                               TL, FL, SL, Weight, Time)), as.numeric)


summary(data$Ba138)
length(unique(data$ID))
table(unique(data$Habitat))

###########
# Threshold
###########

data$Habitat <- factor(data$Habitat, levels = c("Brackish","Saline","Marine","Non-upwelling", "Upwelling"))

#Select the last rows in each ID
edge <- data %>%
  group_by(ID) %>%
  slice_tail(n = 5)

#Set as numeric
edge <- mutate_at(edge, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Rb85, Sr87, Ba138, Pb208, 
                               TL, FL, SL, Weight)), as.numeric)

edge %>%
  group_by(Habitat) %>%
  summarise(
    mean = mean(Ba138, na.rm = TRUE),
    median = median(Ba138, na.rm = TRUE),
    se = std.error(Ba138, na.rm = TRUE),
    sd = sd(Ba138, na.rm = TRUE))

#  Habitat         mean median     se     sd
#1 Brackish      22.7   14.2   0.851  22.2  
#2 Saline         5.15   1.14  1.19   16.4  
#3 Marine         4.23   0.689 1.23   10.6  
#4 Non-upwelling  0.740  0.574 0.0373  0.554
#5 Upwelling      1.37   0.946 0.117   1.30

####
#mean – (2 * SE) 
5.15 - (2*1.19) #= 2.77
4.23 - (2*1.23) #= 1.77

##Thrsholds

#Estuarine    >2.77
#Transition   between 2.77 and 1.77
#Marine       <1.77


######
#Profiles
######

#ID, Site, Month, Year, Length, Weigth

#ID012 São Mateus	July	2022	187	103.18
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID012")) + 
  geom_line(data=subset(data,ID %in% "ID012")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +  
  theme_light() +
  ggtitle("ID012 São Mateus	JUL 2022	TL187	W103.18")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID013 São Mateus	July	2022	180	89.09
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID013")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +   
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  geom_line(data=subset(data,ID %in% "ID013")) + 
  ylim(0, 50) +  
  theme_light() +
  ggtitle("") +
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID014 São Mateus	July	2022	147	42.38
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID014")) + 
  geom_line(data=subset(data,ID %in% "ID014")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +  
  theme_light() +
ggtitle("") +
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID015 São Mateus	July	2022	260	270.16
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID015")) + 
  geom_line(data=subset(data,ID %in% "ID015")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +  
  theme_light() +
ggtitle("") +
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID016 São Mateus	July	2022	165	71.29
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID016")) + 
  geom_line(data=subset(data,ID %in% "ID016")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +  
  theme_light() +
ggtitle("") +
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID018 São Mateus	July	2022	178	80.86
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID018")) + 
  geom_line(data=subset(data,ID %in% "ID018")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +  
  theme_light() +
ggtitle("") +
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID019 São Mateus	July	2022	167	75.03
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID019"), size = 3) + 
  geom_line(data=subset(data,ID %in% "ID019"), size =1) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +  
  theme_light() +
ggtitle("") +
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID022 São Mateus	July	2022	138	42.35
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID022")) + 
  geom_line(data=subset(data,ID %in% "ID022")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() +
ggtitle("") +   
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID023 São Mateus	July	2022	176	77.56
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID023")) + 
  geom_line(data=subset(data,ID %in% "ID023")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() +
ggtitle("") +   
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID032 São Mateus	July	2022	116	26.24
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID032")) + 
  geom_line(data=subset(data,ID %in% "ID032")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() +
ggtitle("") +   
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID044 São Mateus	September	2022	190	101.78
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID044")) + 
  geom_line(data=subset(data,ID %in% "ID044")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() +
ggtitle("") +   
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID047 São Mateus	September	2022	173	84.67
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID047")) + 
  geom_line(data=subset(data,ID %in% "ID047")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() +
ggtitle("") +   
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID048 São Mateus	September	2022	154	63.25
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID048")) + 
  geom_line(data=subset(data,ID %in% "ID048")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() +
ggtitle("") +  
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

#ID050 São Mateus	September	2022	126	32.33
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID050")) + 
  geom_line(data=subset(data,ID %in% "ID050")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID051 São Mateus	September	2022	92	13.05
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID051")) + 
  geom_line(data=subset(data,ID %in% "ID051")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID052 São Mateus	September	2022	91	12.43
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID052")) + 
  geom_line(data=subset(data,ID %in% "ID052")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID053 São Mateus	September	2022	72	5.81
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID053")) + 
  geom_line(data=subset(data,ID %in% "ID053")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID069 Gramuté	June	2022	33	0.14
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID069")) + 
  geom_line(data=subset(data,ID %in% "ID069")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID070 São Mateus	October	2022	180	80.87
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID070")) + 
  geom_line(data=subset(data,ID %in% "ID070")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID072 São Mateus	August	2022	190	119.96
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID072")) + 
  geom_line(data=subset(data,ID %in% "ID072")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID073 São Mateus	August	2022	182	100.45
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID073")) + 
  geom_line(data=subset(data,ID %in% "ID073")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID074 São Mateus	August	2022	178	98.82
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID074")) +
  geom_line(data=subset(data,ID %in% "ID074")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID075 São Mateus	August	2022	182	108.24
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID075")) + 
  geom_line(data=subset(data,ID %in% "ID075")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID076 São Mateus	August	2022	147	59.15
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID076")) + 
  geom_line(data=subset(data,ID %in% "ID076")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID077 São Mateus	August	2022	150	58.66
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID077")) + 
  geom_line(data=subset(data,ID %in% "ID077")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +
  theme_light() 

#ID078 São Mateus	August	2022	134	42.22
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID078")) +
  geom_line(data=subset(data,ID %in% "ID078")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID079 São Mateus	August	2022	137	47.25
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID079")) + 
  geom_line(data=subset(data,ID %in% "ID079")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID080 São Mateus	August	2022	140	44.55
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID080")) + 
  geom_line(data=subset(data,ID %in% "ID080")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID081 São Mateus	August	2022	143	45.89
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID081")) + 
  geom_line(data=subset(data,ID %in% "ID081")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID082 São Mateus	August	2022	152	65.07
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID082")) + 
  geom_line(data=subset(data,ID %in% "ID082")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID083 São Mateus	August	2022	132	41.52
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID083")) +
  geom_line(data=subset(data,ID %in% "ID083")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID092 Caravelas	August	2022	221	150.95
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID092")) +
  geom_line(data=subset(data,ID %in% "ID092")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID093 Caravelas	August	2022	218	157.27
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID093")) + 
  geom_line(data=subset(data,ID %in% "ID093")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID094 Caravelas	August	2022	219	167.65	1
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID094")) +
  geom_line(data=subset(data,ID %in% "ID094")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID095 Caravelas	August	2022	189	119.43
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID095")) + 
  geom_line(data=subset(data,ID %in% "ID095")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID096 Caravelas	August	2022	212	147.18
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID096")) +
  geom_line(data=subset(data,ID %in% "ID096")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID097 Caravelas	August	2022	197	115.95
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID097")) +
  geom_line(data=subset(data,ID %in% "ID097")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID098 Caravelas	August	2022	195	124.59
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID098")) + 
  geom_line(data=subset(data,ID %in% "ID098")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID099 Caravelas	August	2022	193	119.61
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID099")) +
  geom_line(data=subset(data,ID %in% "ID099")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID100 Caravelas	August	2022	185	95.47
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID100")) +
  geom_line(data=subset(data,ID %in% "ID100")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) + 
  theme_light() 

#ID101 Caravelas	August	2022	195	121.16
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID101")) + 
  geom_line(data=subset(data,ID %in% "ID101")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID102 Caravelas	August	2022	194	117.71
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID102")) +
  geom_line(data=subset(data,ID %in% "ID102")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID103 Caravelas	August	2022	193	118.70
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID103")) + 
  geom_line(data=subset(data,ID %in% "ID103")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID104 Caravelas	August	2022	189	107.11
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID104")) +
  geom_line(data=subset(data,ID %in% "ID104")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID105 Caravelas	August	2022	193	11.18
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID105")) +
  geom_line(data=subset(data,ID %in% "ID105")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID106 Caravelas	August	2022	180	99.89
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID106")) +
  geom_line(data=subset(data,ID %in% "ID106")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID110 Abrolhos Bank	July	2022	630	2965.48
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID110")) +
  geom_line(data=subset(data,ID %in% "ID110")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID111 Abrolhos Bank	November	2022	745	6000.00
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID111")) + 
  geom_line(data=subset(data,ID %in% "ID111")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID112 Abrolhos Bank	November	2022	790	7000.00
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID112")) +
  geom_line(data=subset(data,ID %in% "ID112")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID121 São Mateus	August	2022	122	26.30
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID121")) + 
  geom_line(data=subset(data,ID %in% "ID121")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID142 Abrolhos Bank	November	2022	430	950.00
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID142")) + 
  geom_line(data=subset(data,ID %in% "ID142")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID143 Abrolhos Bank	November	2022	440	1150.00
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID143")) + 
  geom_line(data=subset(data,ID %in% "ID143")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID144 Abrolhos Bank	NOV	2022	490	1500
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID144")) + 
  geom_line(data=subset(data,ID %in% "ID144")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID158 Abrolhos Bank	NOV	2022	490	1500
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID158")) + 
  geom_line(data=subset(data,ID %in% "ID158")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID159 Sao Mateus	NOV	2022	221	158.08
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID159")) + 
  geom_line(data=subset(data,ID %in% "ID159")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID160 Sao Mateus	NOV	2022	194	113.54
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID160")) +
  geom_line(data=subset(data,ID %in% "ID160")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID161 Sao Mateus	NOV	2022	202	128.24
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID161")) + 
  geom_line(data=subset(data,ID %in% "ID161")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID162 Sao Mateus	NOV	2022	182	102.44	11.3
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID162")) +
  geom_line(data=subset(data,ID %in% "ID162")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID163 Sao Mateus	DEC	2022	222	173.01
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID163")) + 
  geom_line(data=subset(data,ID %in% "ID163")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID164 Sao Mateus	DEC	2022	166	72.52
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID164")) + 
  geom_line(data=subset(data,ID %in% "ID164")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID165 Sao Mateus	DEC	2022	162	61.8
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID165")) +
  geom_line(data=subset(data,ID %in% "ID165")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID170 Sao Mateus	AUG	2022	190	101.8
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID170")) + 
  geom_line(data=subset(data,ID %in% "ID170")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID173 Sao Mateus	AUG	2022	197	115.45
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID173")) + 
  geom_line(data=subset(data,ID %in% "ID173")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID174 Sao Mateus	AUG	2022	185	91.11
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID174")) + 
  geom_line(data=subset(data,ID %in% "ID174")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID186 Gramute	OCT	2022	131	36.21
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID186")) + 
    geom_line(data=subset(data,ID %in% "ID186")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID188 Gramute	OCT	2022	134	42.66	6.8
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID188")) + 
  geom_line(data=subset(data,ID %in% "ID188")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID192 Gramute	OCT	2022	115	25.82
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID192")) + 
  geom_line(data=subset(data,ID %in% "ID192")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) + 
  theme_light() 

#ID193 Gramute	OCT	2022	123	35.14
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID193")) + 
  geom_line(data=subset(data,ID %in% "ID193")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID194 Gramute	OCT	2022	128	38.29
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID194")) + 
  geom_line(data=subset(data,ID %in% "ID194")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID197 Sao Mateus	OCT	2022	196	127.96
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID197")) + 
  geom_line(data=subset(data,ID %in% "ID197")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID198 Sao Mateus	OCT	2022	133	40.13
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID198")) + 
  geom_line(data=subset(data,ID %in% "ID198")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID199 Sao Mateus	OCT	2022	139	43.52
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID199")) + 
  geom_line(data=subset(data,ID %in% "ID199")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID201 Sao Mateus	OCT	2022	160	66.69
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID201")) + 
  geom_line(data=subset(data,ID %in% "ID201")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID202 Sao Mateus	OCT	2022	149	52.31
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID202")) +
  geom_line(data=subset(data,ID %in% "ID202")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID204 Sao Mateus	OCT	2022	203	142.02
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID204")) + 
  geom_line(data=subset(data,ID %in% "ID204")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID205 Sao Mateus	OCT	2022	230	195.02
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID205")) + 
  geom_line(data=subset(data,ID %in% "ID205")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID209 Sao Mateus	SEP	2022	190	112.78
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID209")) +
  geom_line(data=subset(data,ID %in% "ID209")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID211 Sao Mateus	SEP	2022	160	57.56
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID211")) +
  geom_line(data=subset(data,ID %in% "ID211")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID212 Sao Mateus	SEP	2022	165	62.42
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID212")) + 
  geom_line(data=subset(data,ID %in% "ID212")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID220 Sao Mateus	NOV	2022	189	122.73
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID220")) + 
  geom_line(data=subset(data,ID %in% "ID220")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID221 Sao Mateus	NOV	2022	173	81.92
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID221")) + 
  geom_line(data=subset(data,ID %in% "ID221")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID222 Sao Mateus	NOV	2022	104	17.56
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID222")) + 
  geom_line(data=subset(data,ID %in% "ID222")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID224 Gramute	NOV	2022	105	17.38
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID224")) +
  geom_line(data=subset(data,ID %in% "ID224")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID226 Gramute	NOV	2022	109	22.77
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID226")) + 
    geom_line(data=subset(data,ID %in% "ID226")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID235 Sao Mateus	OCT	2022	186	113.11
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID235")) +
    geom_line(data=subset(data,ID %in% "ID235")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID236 Sao Mateus	OCT	2022	190	125.1
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID236")) + 
    geom_line(data=subset(data,ID %in% "ID236")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID241 Esquecidos	JAN	2023	385	786
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID241")) + 
  geom_line(data=subset(data,ID %in% "ID241")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID247 Sao Mateus	JAN	2023	192	117.52
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID247")) + 
    geom_line(data=subset(data,ID %in% "ID247")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID248 Sao Mateus	JAN	2023	170	71.74
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID248")) + 
    geom_line(data=subset(data,ID %in% "ID248")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID249 Sao Mateus	JAN	2023	193	123.13
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID249")) + 
    geom_line(data=subset(data,ID %in% "ID249")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID251 Sao Mateus	JAN	2023	172	74.81
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID251")) + 
    geom_line(data=subset(data,ID %in% "ID251")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID253 Sao Mateus	JAN	2023	172	74.81
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID253")) + 
  geom_line(data=subset(data,ID %in% "ID253")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID254 Sao Mateus	JAN	2023	172	71.07
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID254")) + 
    geom_line(data=subset(data,ID %in% "ID254")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID255 Sao Mateus	JAN	2023	162	60.07
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID255")) + 
  geom_line(data=subset(data,ID %in% "ID255")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID258 Sao Mateus	JAN	2023	141	42.58
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID258")) + 
  geom_line(data=subset(data,ID %in% "ID258")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID259 Sao Mateus	JAN	2023	129	30.43
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID259")) +
  geom_line(data=subset(data,ID %in% "ID259")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID260 Sao Mateus	JAN	2023	114	20.57
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID260")) + 
  geom_line(data=subset(data,ID %in% "ID260")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID262  Esquecidos	FEB	2023	660	4925
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID262")) +
    geom_line(data=subset(data,ID %in% "ID262")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID263 Esquecidos	FEB	2023	610	4402	38.5
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID263")) +
    geom_line(data=subset(data,ID %in% "ID263")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID264 Esquecidos	FEB	2023	570	2878
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID264")) + 
  geom_line(data=subset(data,ID %in% "ID264")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID266 Esquecidos	FEB	2023	534	2212
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID266")) +
    geom_line(data=subset(data,ID %in% "ID266")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID267 Esquecidos	FEB	2023	510	2210.82
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID267")) + 
  geom_line(data=subset(data,ID %in% "ID267")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID268 Esquecidos	FEB	2023	369	673.74
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID268")) + 
    geom_line(data=subset(data,ID %in% "ID268")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID272 Sao Mateus	FEB	2023	199	134.04
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID272")) + 
    geom_line(data=subset(data,ID %in% "ID272")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID273 Sao Mateus	FEB	2023	200	136.63
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID273")) + 
    geom_line(data=subset(data,ID %in% "ID273")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID274 Sao Mateus	FEB	2023	205	134.37
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID274")) + 
    geom_line(data=subset(data,ID %in% "ID274")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID275 Sao Mateus	FEB	2023	200	125.87
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID275")) + 
    geom_line(data=subset(data,ID %in% "ID275")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID277 Sao Mateus	FEB	2023	175	95.32
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID277")) + 
  geom_line(data=subset(data,ID %in% "ID277")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID278 Sao Mateus	FEB	2023	174	81.01
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID278")) + 
  geom_line(data=subset(data,ID %in% "ID278")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID279 Sao Mateus	FEB	2023	165	71.9
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID279")) + 
  geom_line(data=subset(data,ID %in% "ID279")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID280 Sao Mateus	FEB	2023	165	66.95
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID280")) + 
  geom_line(data=subset(data,ID %in% "ID280")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID281 Sao Mateus	FEB	2023	160	71.34
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID281")) + 
  geom_line(data=subset(data,ID %in% "ID281")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID282 Sao Mateus	FEB	2023	161	59.34
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID282")) + 
  geom_line(data=subset(data,ID %in% "ID282")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID283 Sao Mateus	FEB	2023	155	62.84
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID283")) + 
  geom_line(data=subset(data,ID %in% "ID283")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) + 
  theme_light() 

#ID284 Sao Mateus	FEB	2023	157	71.77
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID284")) + 
  geom_line(data=subset(data,ID %in% "ID284")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID285 Sao Mateus	FEB	2023	150	55.37
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID285")) + 
  geom_line(data=subset(data,ID %in% "ID285")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID286 Sao Mateus	FEB	2023	140	42.22
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID286")) +
  geom_line(data=subset(data,ID %in% "ID286")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID287 Sao Mateus	FEB	2023	125	32.73
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID287")) + 
  geom_line(data=subset(data,ID %in% "ID287")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID292 PARNA Abrolhos	FEB	2023	676	4467
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID292")) + 
  geom_line(data=subset(data,ID %in% "ID292")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID293 PARNA Abrolhos	FEB	2023	600	2774
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID293")) +
  geom_line(data=subset(data,ID %in% "ID293")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID294 PARNA Abrolhos	FEB	2023	751	6194
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID294")) +
    geom_line(data=subset(data,ID %in% "ID294")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID295 PARNA Abrolhos	FEB	2023	616	3820
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID295")) +
  geom_line(data=subset(data,ID %in% "ID295")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID296 PARNA Abrolhos	FEB	2023	557	2490
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID296")) +
    geom_line(data=subset(data,ID %in% "ID296")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID297 PARNA Abrolhos	FEB	2023	465	1376
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID297")) + 
  geom_line(data=subset(data,ID %in% "ID297")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID298 PARNA Abrolhos	FEB	2023	433	1235	36.2
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID298")) + 
  geom_line(data=subset(data,ID %in% "ID298")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID299 PARNA Abrolhos	FEB	2023	581	3149
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID299")) +
  geom_line(data=subset(data,ID %in% "ID299")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID300 PARNA Abrolhos	FEB	2023	513	1971
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID300")) + 
  geom_line(data=subset(data,ID %in% "ID300")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID301 PARNA Abrolhos	FEB	2023	515	2207
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID301")) +
  geom_line(data=subset(data,ID %in% "ID301")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID302 PARNA Abrolhos	FEB	2023	551	2291
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID302")) + 
  geom_line(data=subset(data,ID %in% "ID302")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID303 PARNA Abrolhos	FEB	2023	372	775
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID303")) + 
  geom_line(data=subset(data,ID %in% "ID303")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID304 PARNA Abrolhos	FEB	2023	641	3952
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID304")) + 
  geom_line(data=subset(data,ID %in% "ID304")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID305 PARNA Abrolhos	FEB	2023	556	2432
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID305")) +
  geom_line(data=subset(data,ID %in% "ID305")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID306 PARNA Abrolhos	FEB	2023	488	1704
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID306")) + 
  geom_line(data=subset(data,ID %in% "ID306")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID307 PARNA Abrolhos	FEB	2023	440	1222
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID307")) + 
  geom_line(data=subset(data,ID %in% "ID307")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID308 PARNA Abrolhos	FEB	2023	529	2282
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID308")) + 
  geom_line(data=subset(data,ID %in% "ID308")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID309 PARNA Abrolhos	FEB	2023	475	1847
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID309")) +
  geom_line(data=subset(data,ID %in% "ID309")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID310 PARNA Abrolhos	FEB	2023	475	1847
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID310")) + 
  geom_line(data=subset(data,ID %in% "ID310")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID311 PARNA Abrolhos	FEB	2023	655	4170
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID311")) + 
  geom_line(data=subset(data,ID %in% "ID311")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID312 PARNA Abrolhos	FEB	2023	598	3030
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID312")) +
  geom_line(data=subset(data,ID %in% "ID312")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID313 PARNA Abrolhos	FEB	2023	541	2424
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID313")) + 
  geom_line(data=subset(data,ID %in% "ID313")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID314 PARNA Abrolhos	FEB	2023	586	3004
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID314")) +
  geom_line(data=subset(data,ID %in% "ID314")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID315 PARNA Abrolhos	FEB	2023	635	3943
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID315")) + 
  geom_line(data=subset(data,ID %in% "ID315")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID316 PARNA Abrolhos	FEB	2023	516	1730
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID316")) + 
  geom_line(data=subset(data,ID %in% "ID316")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID317 PARNA Abrolhos	FEB	2023	532	2333
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID317")) + 
  geom_line(data=subset(data,ID %in% "ID317")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID318 PARNA Abrolhos	FEB	2023	492	1560
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID318")) + 
  geom_line(data=subset(data,ID %in% "ID318")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID319 PARNA Abrolhos	FEB	2023	557	2555
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID319")) + 
  geom_line(data=subset(data,ID %in% "ID319")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID320 PARNA Abrolhos	FEB	2023	425	1261
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID320")) +
    geom_line(data=subset(data,ID %in% "ID320")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID321 PARNA Abrolhos	FEB	2023	442	1167
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID321")) + 
    geom_line(data=subset(data,ID %in% "ID321")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID322 Sao Mateus	MAR	2023	226	187.93
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID322")) + 
    geom_line(data=subset(data,ID %in% "ID322")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID323 Sao Mateus	MAR	2023	197	151.43
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID323")) + 
    geom_line(data=subset(data,ID %in% "ID323")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID324 Sao Mateus	MAR	2023	190	122.99
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID324")) + 
  geom_line(data=subset(data,ID %in% "ID324")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) + 
  theme_light() 

#ID326 Sao Mateus	MAR	2023	200	137.53
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID326")) +
  geom_line(data=subset(data,ID %in% "ID326")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID327 Sao Mateus	MAR	2023	190	112.58
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID327")) + 
    geom_line(data=subset(data,ID %in% "ID327")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID328 Sao Mateus	MAR	2023	189	109.14
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID328")) + 
    geom_line(data=subset(data,ID %in% "ID328")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID331 Sao Mateus	MAR	2023	177	101.88
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID331")) + 
  geom_line(data=subset(data,ID %in% "ID331")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 250) + 
  theme_light() 

#ID333 Sao Mateus	MAR	2023	212	148.74
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID333")) + 
  geom_line(data=subset(data,ID %in% "ID333")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID335 Sao Mateus	MAR	2023	181	98.22
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID335")) +
    geom_line(data=subset(data,ID %in% "ID335")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) +   
  theme_light() 

#ID336 Sao Mateus	MAR	2023	160	66.79
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID336")) + 
  geom_line(data=subset(data,ID %in% "ID336")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID337 Piraque-acu	MAR	2023	230	243.59
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID337")) +
  geom_line(data=subset(data,ID %in% "ID337")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) + 
  theme_light() 

#ID338 Piraque-acu	MAR	2023	235	254.30
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID338")) +
  geom_line(data=subset(data,ID %in% "ID338")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) + 
  theme_light() 

#ID339 Piraque-acu	MAR	2023	244	266.00
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID339")) + 
    geom_line(data=subset(data,ID %in% "ID339")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID340 Piraque-acu	MAR	2023	230	207.36
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID340")) +
    geom_line(data=subset(data,ID %in% "ID340")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID341 Piraque-acu	MAR	2023	230	207.36
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID341")) + 
    geom_line(data=subset(data,ID %in% "ID341")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() 

#ID342 Piraque-acu	MAR	2023	214	154.43
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID342")) +
  geom_line(data=subset(data,ID %in% "ID342")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) + 
  theme_light() 

#ID343 Piraque-acu	MAR	2023	200	119.85
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID343")) + 
    geom_line(data=subset(data,ID %in% "ID343")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID344 Piraque-acu	MAR	2023	215	140.84
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID344")) + 
  geom_line(data=subset(data,ID %in% "ID344")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID345 Piraque-acu	MAR	2023	190	121.19
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID345")) +
  geom_line(data=subset(data,ID %in% "ID345")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) + 
  theme_light() 

#ID346 Piraque-acu	MAR	2023	200	118.14
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID346")) + 
    geom_line(data=subset(data,ID %in% "ID346")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 6) +   
  theme_light() 

#ID347 Piraque-acu	MAR	2023	205	134.41
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID347")) +
    geom_line(data=subset(data,ID %in% "ID347")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID348 Piraque-acu	MAR	2023	200	112.47
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID348")) +
    geom_line(data=subset(data,ID %in% "ID348")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID349 Piraque-acu	MAR	2023	215	139.6884
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID349")) + 
    geom_line(data=subset(data,ID %in% "ID349")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID350 Piraque-acu	MAR	2023	144	46.29
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID350")) +
    geom_line(data=subset(data,ID %in% "ID350")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID351 Piraque-acu	MAR	2023	133	38.05
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID351")) + 
  geom_line(data=subset(data,ID %in% "ID351")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) + 
  theme_light() 

#ID352 Caravelas	FEB	2023	232	167.11	15.8
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID352")) +
    geom_line(data=subset(data,ID %in% "ID352")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID353 Caravelas	FEB	2023	215	130.57
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID353")) + 
    geom_line(data=subset(data,ID %in% "ID353")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID354 Caravelas	FEB	2023	205	130.8
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID354")) + 
  geom_line(data=subset(data,ID %in% "ID354")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID355 Caravelas	FEB	2023	215	142.73
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID355")) +
  geom_line(data=subset(data,ID %in% "ID355")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID356 Caravelas	FEB	2023	198	104.23
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID356")) + 
  geom_line(data=subset(data,ID %in% "ID356")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID357 Caravelas	FEB	2023	198	104.23
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID357")) + 
    geom_line(data=subset(data,ID %in% "ID357")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID358 Caravelas	FEB	2023	215	128.49
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID358")) + 
    geom_line(data=subset(data,ID %in% "ID358")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 4) +   
  theme_light() 

#ID359 Caravelas	FEB	2023	230	165.47
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID359")) + 
  geom_line(data=subset(data,ID %in% "ID359")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID374 Sao Mateus	APR	2023	235	212.5
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID374")) +
    geom_line(data=subset(data,ID %in% "ID374")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID376 Sao Mateus	APR	2023	223	192.46
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID376")) +
  geom_line(data=subset(data,ID %in% "ID376")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID378 Sao Mateus	APR	2023	215	128.54
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID378")) +
  geom_line(data=subset(data,ID %in% "ID378")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID381 Sao Mateus	APR	2023	233	210.75
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID381")) + 
  geom_line(data=subset(data,ID %in% "ID381")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID382 Sao Mateus	APR	2023	210	174.49
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID382")) + 
    geom_line(data=subset(data,ID %in% "ID382")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) +   
  theme_light() 

#ID383 Sao Mateus	APR	2023	198	147.37
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID383")) + 
    geom_line(data=subset(data,ID %in% "ID383")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) +   
  theme_light() 

#ID385 Sao Mateus	APR	2023	200	184.81
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID385")) + 
    geom_line(data=subset(data,ID %in% "ID385")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID386 Sao Mateus	APR	2023	230	204.86
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID386")) + 
    geom_line(data=subset(data,ID %in% "ID386")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID388 Sao Mateus	APR	2023	185	107.91
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID388")) + 
    geom_line(data=subset(data,ID %in% "ID388")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID390 Sao Mateus	APR	2023	129	35.91
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID390")) +
    geom_line(data=subset(data,ID %in% "ID390")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID391 Rasas island	APR	2023	410	1255
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID391")) +
  geom_line(data=subset(data,ID %in% "ID391")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID392 Rasas island	APR	2023	380	905
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID392")) + 
    geom_line(data=subset(data,ID %in% "ID392")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID393 Rasas island	APR	2023	390	918
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID393")) + 
    geom_line(data=subset(data,ID %in% "ID393")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID394 Rasas island	APR	2023	380	891
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID394")) + 
  geom_line(data=subset(data,ID %in% "ID394")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID395 Rasas island	APR	2023	380	891
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID395")) + 
    geom_line(data=subset(data,ID %in% "ID395")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID396 Rasas island	APR	2023	410	1155
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID396")) + 
  geom_line(data=subset(data,ID %in% "ID396")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID399 Itapemerim	APR	2023	158	19.93
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID399")) + 
    geom_line(data=subset(data,ID %in% "ID399")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID403 Guarapari shelf	FEB	2023	515	2200
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID403")) + 
    geom_line(data=subset(data,ID %in% "ID403")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID405 Gramute	FEB	2023	67	7.293
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID405")) + 
    geom_line(data=subset(data,ID %in% "ID405")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID408 Gramute	FEB	2023	34	0.702
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID408")) + 
    geom_line(data=subset(data,ID %in% "ID408")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 70) +   
  theme_light() 

#ID409 Gramute	FEB	2023	39	1.209
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID409")) +
  geom_line(data=subset(data,ID %in% "ID409")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID410 Gramute	FEB	2023	32	0.689
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID410")) + 
    geom_line(data=subset(data,ID %in% "ID410")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID413 Gramute	FEB	2023	38	1.105
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID413")) + 
    geom_line(data=subset(data,ID %in% "ID413")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID416 Gramute	FEB	2023	40	1.222
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID416")) + 
    geom_line(data=subset(data,ID %in% "ID416")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID425 Gramute	FEB	2023	19	0.169
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID425")) + 
    geom_line(data=subset(data,ID %in% "ID425")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID427 Marataizes	APR	2023	520	2237.28
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID427")) + 
    geom_line(data=subset(data,ID %in% "ID427")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) +   
  theme_light() 

#ID428 Marataizes	APR	2023	522	1999.62
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID428")) + 
    geom_line(data=subset(data,ID %in% "ID428")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID429 Marataizes	APR	2023	440	1318.2
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID429")) + 
    geom_line(data=subset(data,ID %in% "ID429")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID430 Marataizes	APR	2023	499	1563
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID430")) + 
    geom_line(data=subset(data,ID %in% "ID430")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID431 Marataizes	APR	2023	540	2257.25
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID431")) + 
    geom_line(data=subset(data,ID %in% "ID431")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID432 Marataizes	APR	2023	415	2873.03
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID432")) + 
  geom_line(data=subset(data,ID %in% "ID432")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID433 Marataizes	APR	2023	580	3226.02
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID433")) +
    geom_line(data=subset(data,ID %in% "ID433")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID434 Marataizes	APR	2023	540	2035.48
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID434")) + 
    geom_line(data=subset(data,ID %in% "ID434")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID435 Marataizes	APR	2023	486	1415.92
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID435")) + 
    geom_line(data=subset(data,ID %in% "ID435")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID436 Marataizes	APR	2023	544	2115.85
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID436")) + 
    geom_line(data=subset(data,ID %in% "ID436")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID437 Marataizes	APR	2023	486	2271.01
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID437")) + 
    geom_line(data=subset(data,ID %in% "ID437")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID438 Marataizes	APR	2023	544	1549.66
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID438")) +
    geom_line(data=subset(data,ID %in% "ID438")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID439 Marataizes	MAY	2023	541	1822.3
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID439")) +
    geom_line(data=subset(data,ID %in% "ID439")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID440 Marataizes	APR	2023	479	2715.1
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID440")) +
  geom_line(data=subset(data,ID %in% "ID440")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID441 Marataizes	APR	2023	482	1694.73
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID441")) + 
    geom_line(data=subset(data,ID %in% "ID441")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID442 Marataizes	APR	2023	485	2062.774
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID442")) + 
    geom_line(data=subset(data,ID %in% "ID442")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID443 Marataizes	APR	2023	597	3197.69
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID443")) +
    geom_line(data=subset(data,ID %in% "ID443")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID444 Itapemerim	MAY	2023	223	163.9
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID444")) +
  geom_line(data=subset(data,ID %in% "ID444")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) + 
  theme_light() 

#ID445 Itapemerim	MAY	2023	214	175.69
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID445")) + 
  geom_line(data=subset(data,ID %in% "ID445")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID446 Itapemerim	MAY	2023	210	168.86
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID446")) + 
  geom_line(data=subset(data,ID %in% "ID446")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) +   
  theme_light() 

#ID447 Itapemerim	MAY	2023	210	177.82
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID447")) +
  geom_line(data=subset(data,ID %in% "ID447")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID449 Itapemerim	MAY	2023	175	98.02
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID449")) + 
  geom_line(data=subset(data,ID %in% "ID449")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID454 Itapemerim	MAY	2023	168	86.9
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID454")) + 
  geom_line(data=subset(data,ID %in% "ID454")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID460 Itapemerim	MAY	2023	185	118.4
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID460")) + 
  geom_line(data=subset(data,ID %in% "ID460")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 200) + 
  theme_light() 

#ID461 Itapemerim	MAY	2023	190	123.01
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID461")) + 
  geom_line(data=subset(data,ID %in% "ID461")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID462 Itapemerim	MAY	2023	180	102.9
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID462")) +
  geom_line(data=subset(data,ID %in% "ID462")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID463 Itapemerim	MAY	2023	183	124.9
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID463")) + 
  geom_line(data=subset(data,ID %in% "ID463")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID465 Itapemerim	MAY	2023	169	88.9
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID465")) +
  geom_line(data=subset(data,ID %in% "ID465")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID470 Itapemerim	MAY	2023	162	81.02
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID470")) + 
  geom_line(data=subset(data,ID %in% "ID470")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID472 Itapemerim	MAY	2023	141	51.87
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID472")) + 
  geom_line(data=subset(data,ID %in% "ID472")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID473 Itapemerim	MAY	2023	145	56.5
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID473")) +
  geom_line(data=subset(data,ID %in% "ID473")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID480 Sao Mateus	MAY	2023	225	207.01
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID480")) + 
  geom_line(data=subset(data,ID %in% "ID480")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID481 Sao Mateus	MAY	2023	205	144.96
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID481")) +
  geom_line(data=subset(data,ID %in% "ID481")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID482 Sao Mateus	MAY	2023	180	100.04
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID482")) +
  geom_line(data=subset(data,ID %in% "ID482")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID487 Sao Mateus	MAY	2023	160	72.32
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID487")) + 
  geom_line(data=subset(data,ID %in% "ID487")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID488 Sao Mateus	MAY	2023	137	46.67
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID488")) +
  geom_line(data=subset(data,ID %in% "ID488")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID489 Sao Mateus	MAY	2023	130	39.36
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID489")) + 
  geom_line(data=subset(data,ID %in% "ID489")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID494 Sao Mateus	MAY	2023	205	125.87
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID494")) +
  geom_line(data=subset(data,ID %in% "ID494")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID495 Sao Mateus	MAY	2023	210	162.5
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID495")) + 
  geom_line(data=subset(data,ID %in% "ID495")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID496 Sao Mateus	MAY	2023	205	129.01
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID496")) + 
  geom_line(data=subset(data,ID %in% "ID496")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 150) + 
  theme_light() 

#ID497 Sao Mateus	MAY	2023	185	110.38
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID497")) + 
  geom_line(data=subset(data,ID %in% "ID497")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID501 Sao Mateus	JUN	2023	194	109.79
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID501")) +
  geom_line(data=subset(data,ID %in% "ID501")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID502 Sao Mateus	JUN	2023	185	98.67
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID502")) + 
  geom_line(data=subset(data,ID %in% "ID502")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID503 Sao Mateus	JUN	2023	184	99.45
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID503")) + 
  geom_line(data=subset(data,ID %in% "ID503")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID504 Sao Mateus	JUN	2023	184	87.88
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID504")) + 
  geom_line(data=subset(data,ID %in% "ID504")) + 
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID506 Sao Mateus	JUN	2023	181	89.65
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID506")) + 
  geom_line(data=subset(data,ID %in% "ID506")) + 
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID508 Sao Mateus	JUN	2023	165	66.77
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID508")) + 
  geom_line(data=subset(data,ID %in% "ID508")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() 

#ID509 Sao Mateus	JUN	2023	200	112.52
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID509")) + 
  geom_point(data=subset(data,ID %in% "ID509")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) + 
  theme_light() 

#ID510 Sao Mateus	JUN	2023	178	84.35
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID510")) + 
  geom_point(data=subset(data,ID %in% "ID510")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID511 Sao Mateus	JUN	2023	159	64.04
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID511")) +
  geom_line(data=subset(data,ID %in% "ID511")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) + 
  theme_light() 

#ID514 Sao Mateus	JUN	2023	138	38.6
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID514")) +
  geom_line(data=subset(data,ID %in% "ID514")) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

#ID516 Marataizes	JAN	2023	690	8000
ggplot(data, aes(x=Time, y=Ba138)) +
  geom_point(data=subset(data,ID %in% "ID516")) + 
  geom_line(data=subset(data,ID %in% "ID516")) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() 

