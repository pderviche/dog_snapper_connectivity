##################
#Water
##################

#packages
library(dplyr)
library(ggplot2)

#Clean R environment 
rm(list = ls())

#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")

#Read data
data <- read.csv2("water_mol.csv")
str(data)

#Elements

data <- data[,c('Sample', 'Site','Month', 'Year', 'Season', 'tide',
                'MgCa', 'AlCa', 'CrCa', 'MnCa', 'FeCa', 'CuCa',	'ZnCa', 'AsCa',	'BaCa', 'PbCa',
                'Temp', 'Sal')]

table(unique(data$Site))

gradient <- data %>%
  filter(Site %in% c('Sao Mateus','Caravelas','Itapemirim','Piraque-acu')) %>%
  filter(tide == "gradient") %>%
  filter(!(Site == 'Sao Mateus' & Season != 'Late wet'))

gradient <- gradient %>%
  mutate(
    Site = case_when(
      Site == "Caravelas" ~ "CA",
      Site == "Piraque-acu" ~ "PA",
      Site == "Sao Mateus" ~ "SM",
      Site == "Itapemirim" ~ "IT",
      TRUE ~ Site
    ))
    
table(unique(gradient$Site))

gradient <- mutate_at(gradient, vars(c('MgCa', 'AlCa', 'CrCa', 'MnCa', 'FeCa', 'CuCa',	'ZnCa', 'AsCa',	'BaCa', 'PbCa',
                                       'Temp', 'Sal')), as.numeric)

gradient$Site <- factor(gradient$Site, levels = c('CA','SM','PA','IT'))

gradient <- gradient %>%  filter(!(Sample == 'A076'))



#####
#Mg
#####

gradient$MgCam <- gradient$MgCa/1000000 #transform umol mol-1 to mol mol-1

str(gradient)

Mg <- ggplot(data = gradient, aes(y = MgCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=2)  +
  scale_shape_manual(values = c(21, 22, 23, 24)) + 
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Mg:Ca[water]~ ('mol'~'mol'^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed")) 

Mg

#####
#Al
#####

gradient$AlCam <- gradient$AlCa/1000000 #transform umol mol-1 to mol mol-1

str(gradient)

Al <- ggplot(data = gradient, aes(y = AlCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=2)  +
  scale_shape_manual(values = c(21, 22, 23, 24)) + 
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Al:Ca[water]~ ('mol'~'mol'^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed")) 

Al

#####
#Cr
#####

gradient$CrCam <- gradient$CrCa/1000 #transform umol mol-1 to mmol mol-1

Cr <- ggplot(data = gradient, aes(y = CrCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=3)  +
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Cr:Ca[water]~ (mmol~mol^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed")) 

Cr


#####
#Fe
#####

gradient$FeCam <- gradient$FeCa/1000000 #transform umol mol-1 to mol mol-1


Fe <- ggplot(data = gradient, aes(y = FeCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=3)  +
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Fe:Ca[water]~ ('mol'~'mol'^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan'))+
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed")) 

Fe

#####
#Cu
#####

gradient$CuCam <- gradient$CuCa/1000 #transform umol mol-1 to mmol mol-1


Cu <- ggplot(data = gradient, aes(y = CuCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=3)  +
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Cu:Ca[water]~ (mmol~mol^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  labs(colour = NULL)+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan'))+
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed")) 

Cu

#####
#Zn
#####

gradient$ZnCam <- gradient$ZnCa/1000 #transform umol mol-1 to mmol mol-1

Zn <- ggplot(data = gradient, aes(y = ZnCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=3)  +
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Zn:Ca[water]~ (mmol~mol^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan'))+
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed"))

Zn


#####
#Ba
#####

gradient$BaCam <- gradient$BaCa/1000 #transform umol mol-1 to mmol mol-1


Ba <- ggplot(data = gradient, aes(y = BaCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=3)  +
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Ba:Ca[water]~ ('mmol'~'mol'^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan'))+
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed"))

Ba

#####
#Pb
#####

gradient$PbCam <- gradient$PbCa/1000 #transform umol mol-1 to mmol mol-1

Pb <- ggplot(data = gradient, aes(y = PbCam, x = Sal, fill = Site)) +
  geom_point(aes(colour = factor(Site)), alpha = 0.3, size=3)  +
  geom_smooth(aes(colour = factor(Site), linetype = Site), method="lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
  labs(title = " ",x = "Salinity", y = expression(Pb:Ca[water]~ (mmol~mol^-1)))  +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 35, by = 5)) + 
  guides(linetype = FALSE, fill = "none")+
  theme(legend.position="right", legend.title = element_blank(),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = 15),
        legend.text=element_text(size=15)) + 
  scale_fill_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan')) +
  scale_colour_manual(values = c('cyan4', 'cyan3', 'cyan2', 'cyan'))+
  scale_linetype_manual(values = c("CA" = "solid", "SM" = "dashed", "PA" = "solid", "IT" = "dashed"))

Pb


#####
#Figure
#####

Mg
Al
Cr
Fe
Cu
Zn
Ba
Pb

library(ggpubr)

ggarrange(Mg, Al, Cr, Fe,
          Cu, Zn, Ba, Pb,
          labels = c("A","B","C","D","E","F","G","H"),
          ncol = 2, nrow =4,  common.legend = TRUE,
          legend = "right")
#1000 x 1200




