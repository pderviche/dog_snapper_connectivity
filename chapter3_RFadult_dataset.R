
#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")


#Clean R environment 
rm(list = ls())

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

# Adult IDs
selected_ids <- c("ID110", "ID111", "ID112", "ID142", "ID143", "ID144", "ID158",
                  "ID241", "ID242", "ID243", "ID262", "ID263", "ID264", "ID265", 
                  "ID266", "ID267", "ID268", "ID292", "ID293", "ID294", "ID295", 
                  "ID296", "ID297", "ID298", "ID299", "ID300", "ID301", "ID302", 
                  "ID303", "ID304", "ID305", "ID306", "ID307", "ID308", "ID309", 
                  "ID310", "ID311", "ID312", "ID313", "ID314", "ID315", "ID316", 
                  "ID317", "ID318", "ID319", "ID320", "ID321", "ID367", "ID368", 
                  "ID369", "ID370", "ID371", "ID372", "ID373", "ID391", "ID392", 
                  "ID393", "ID394", "ID395", "ID396", "ID403", "ID404", "ID427", 
                  "ID428", "ID429", "ID430", "ID431", "ID432", "ID433", "ID434", 
                  "ID435", "ID436", "ID437", "ID438", "ID439", "ID440", "ID441", 
                  "ID442", "ID443", "ID516")

data <- data[data$ID %in% selected_ids, ]
data
length(unique(data$ID))

#Set as numeric
data <- mutate_at(data, vars(c(Mg24, Sr87, Ba138, Time)), as.numeric)



######
#Profiles
######

#ID, Site, Month, Year, Length, Weigth

#ID110 Abrolhos Bank	July	2022	630	2965.48
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID110")) + 
  geom_point(data = subset(data, ID %in% "ID110"), size = 3,
             aes(color = ifelse(Time >= 0 & Time <= 125.7, "red", "purple")),
             show.legend = FALSE) +  
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID110 (AK)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID110 <- data %>% filter(Time >= 0 & Time <= 125.7, ID %in% c("ID110"))

#ID111 Abrolhos Bank	November	2022	745	6000.00
ID111 <- ggplot(data, aes(x=Time*10, y=Ba138)) +
          geom_line(data=subset(data,ID %in% "ID111")) + 
          geom_point(data = subset(data, ID %in% "ID111"), size = 3,
                     aes(color = ifelse(Time >= 269 & Time <= max(Time), "red", 
                                        ifelse(Time >= 18 & Time <= 85.5, "purple", "green"))),
                     show.legend = FALSE) +  
          geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
          geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
          ylim(0, 4.5) + 
          theme_light() +
          ggtitle("ID111 captured on Non-upwelling AG")+ 
          ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1))) +
          scale_x_continuous(expression(Distance~from~core~(mu~m)) )

ID111

ID111 <- data %>% filter(Time >= 18 & Time <= 85.5, ID %in% c("ID111"))

#700 x 400




#ID112 Abrolhos Bank	November	2022	790	7000.00
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID112")) + 
  geom_point(data = subset(data, ID %in% "ID112"), size = 3,
             aes(color = ifelse(Time >= 8 & Time <= 86.6, "red", "purple")),
             show.legend = FALSE) +  
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID112 (AK)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID112 <- data %>% filter(Time >= 8 & Time <= 86.6, ID %in% c("ID112"))

#ID142 Abrolhos Bank	November	2022	430	950.00
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID142")) + 
  geom_point(data = subset(data, ID %in% "ID142"), size = 3,
             aes(color = ifelse(Time >= 0 & Time <= 96.05, "red", "purple")),
             show.legend = FALSE) +  
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID142 (AK)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID142 <- data %>% filter(Time >= 0 & Time <= 96.05, ID %in% c("ID142"))

#ID143 Abrolhos Bank	November	2022	440	1150.00
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID143")) + 
  geom_point(data = subset(data, ID %in% "ID143"), size = 3,
             aes(color = ifelse(Time >= 0 & Time <= 106.04, "red", "purple")),
             show.legend = FALSE) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID143 (AK)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID143 <- data %>% filter(Time >= 0 & Time <= 106.04, ID %in% c("ID143"))

#ID144 Abrolhos Bank	NOV	2022	490	1500
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID144")) + 
  geom_point(data = subset(data, ID %in% "ID144"), size = 3,
             aes(color = ifelse(Time >= 15 & Time <= 94.06, "red", "purple")),
             show.legend = FALSE) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID144 (AK)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID144 <- data %>% filter(Time >= 15 & Time <= 94.06, ID %in% c("ID144"))

#ID158 Abrolhos Bank	NOV	2022	490	1500
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID158")) + 
  geom_point(data = subset(data, ID %in% "ID158"), size = 3,
             aes(color = ifelse(Time >= 8 & Time <= 104.32, "red", "purple")),
             show.legend = FALSE) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID158 (AK)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID158 <- data %>% filter(Time >= 8 & Time <= 104.32, ID %in% c("ID158"))

#ID241 Forgotten Reefs	JAN	2023	385	786
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID241")) + 
  geom_point(data = subset(data, ID %in% "ID241"), size = 3,
             aes(color = ifelse(Time >= 5 & Time <= 93.59, "red", "purple")),
             show.legend = FALSE) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 35) + 
  theme_light() +
  ggtitle("ID241 (FR)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID241 <- data %>% filter(Time >= 5 & Time <= 93.59, ID %in% c("ID241"))


#ID262  Forgotten Reefs	FEB	2023	660	4925
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID262")) +
  geom_point(data = subset(data, ID %in% "ID262"), size = 3,
             aes(color = ifelse(Time >= 0 & Time <= 99.95, "red", "purple")),
             show.legend = FALSE) + 
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID262 (FR)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID262 <- data %>% filter(Time >= 0 & Time <= 99.95, ID %in% c("ID262"))

#ID263 Forgotten Reefs	FEB	2023	610	4402	38.5
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  geom_line(data=subset(data,ID %in% "ID263")) + 
  geom_point(data = subset(data, ID %in% "ID263"), size = 3,
             aes(color = ifelse(Time >= 195 & Time <= max(Time), "red", 
                                ifelse(Time >= 18 & Time <= 98.12, "purple", "green"))),
             show.legend = FALSE) +  

  ylim(0, 4.5) +   
  theme_light() +
  ggtitle("ID263 captured on Upwelling AG")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID263 <- data %>% filter(ID %in% c("ID263"))
ID263 <- data %>% filter(Time >= 20 & Time <= 98.12, ID %in% c("ID263"))


#ID264 Forgotten Reefs	FEB	2023	570	2878
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID264")) + 
  geom_point(data = subset(data, ID %in% "ID264"), size = 3,
             aes(color = ifelse(Time >=  28 & Time <= 102.01, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID264 (FR)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID264 <- data %>% filter(Time >= 28 & Time <= 102.01, ID %in% c("ID264"))

#ID266 Forgotten Reefs	FEB	2023	534	2212
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID266")) + 
  geom_point(data = subset(data, ID %in% "ID266"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 77.22, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 40) +   
  theme_light() +
  ggtitle("IDXXX (FR)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID266 <- data %>% filter(Time >= 0 & Time <= 77.22, ID %in% c("ID266"))


#ID267 Forgotten Reefs	FEB	2023	510	2210.82
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID267")) + 
  geom_point(data = subset(data, ID %in% "ID267"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 85.66, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 20) + 
  theme_light() +
  ggtitle("ID267 (FR)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID267 <- data %>% filter(Time >= 0 & Time <= 85.66, ID %in% c("ID267"))

#ID268 Forgotten Reefs	FEB	2023	369	673.74
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID268")) + 
  geom_point(data = subset(data, ID %in% "ID268"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 70.73, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID268 (FR)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID268 <- data %>% filter(Time >= 0 & Time <= 70.73, ID %in% c("ID268"))

#ID292 PARNA Abrolhos	FEB	2023	676	4467
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID292")) +
  geom_point(data = subset(data, ID %in% "ID292"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 111.57, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID292 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID292 <- data %>% filter(Time >= 0 & Time <= 111.57, ID %in% c("ID292"))

#ID293 PARNA Abrolhos	FEB	2023	600	2774
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID293")) + 
  geom_point(data = subset(data, ID %in% "ID293"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 84.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID293 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID293 <- data %>% filter(Time >= 0 & Time <= 84.5, ID %in% c("ID293"))

#ID294 PARNA Abrolhos	FEB	2023	751	6194
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID294")) + 
  geom_point(data = subset(data, ID %in% "ID294"), size = 3,
             aes(color = ifelse(Time >=  5 & Time <= 86.65, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID294 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID294 <- data %>% filter(Time >= 5 & Time <= 86.65, ID %in% c("ID294"))

#ID295 PARNA Abrolhos	FEB	2023	616	3820
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID295")) + 
  geom_point(data = subset(data, ID %in% "ID295"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 91.39, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID295 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID295 <- data %>% filter(Time >= 0 & Time <= 91.39, ID %in% c("ID295"))

#ID296 PARNA Abrolhos	FEB	2023	557	2490
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID296")) + 
  geom_point(data = subset(data, ID %in% "ID296"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID296 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID296 <- data %>% filter(Time >= 0 & Time <= 88.5, ID %in% c("ID296"))

#ID297 PARNA Abrolhos	FEB	2023	465	1376
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID297")) + 
  geom_point(data = subset(data, ID %in% "ID297"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID297 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID297 <- data %>% filter(Time >= 0 & Time <= 88.5, ID %in% c("ID297"))

#ID298 PARNA Abrolhos	FEB	2023	433	1235	36.2
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID298")) + 
  geom_point(data = subset(data, ID %in% "ID298"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 100.26, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID298 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID298 <- data %>% filter(Time >= 0 & Time <= 100.26, ID %in% c("ID298"))

#ID299 PARNA Abrolhos	FEB	2023	581	3149
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID299")) + 
  geom_point(data = subset(data, ID %in% "ID299"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 97.01, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID299 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID299 <- data %>% filter(Time >= 0 & Time <= 97.01, ID %in% c("ID299"))

#ID300 PARNA Abrolhos	FEB	2023	513	1971
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID300")) + 
  geom_point(data = subset(data, ID %in% "ID300"), size = 3,
             aes(color = ifelse(Time >=  4 & Time <= 97.78, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID300 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID300 <- data %>% filter(Time >= 4 & Time <= 100, ID %in% c("ID300"))

#ID301 PARNA Abrolhos	FEB	2023	515	2207
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID301")) + 
  geom_point(data = subset(data, ID %in% "ID301"), size = 3,
             aes(color = ifelse(Time >=  15 & Time <= 86.37, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID301 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID301 <- data %>% filter(Time >= 15 & Time <= 86.37, ID %in% c("ID301"))

#ID302 PARNA Abrolhos	FEB	2023	551	2291
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID302")) + 
  geom_point(data = subset(data, ID %in% "ID302"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 99.81, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID302 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID302 <- data %>% filter(Time >= 0 & Time <= 99.81, ID %in% c("ID302"))

#ID303 PARNA Abrolhos	FEB	2023	372	775
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID303")) +
  geom_point(data = subset(data, ID %in% "ID303"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 93.83, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID303 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID303 <- data %>% filter(Time >= 0 & Time <= 93.83, ID %in% c("ID303"))

#ID304 PARNA Abrolhos	FEB	2023	641	3952
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID304")) + 
  geom_point(data = subset(data, ID %in% "ID304"), size = 3,
             aes(color = ifelse(Time >=  53 & Time <= 94.76, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID304 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID304 <- data %>% filter(Time >= 53 & Time <= 94.76, ID %in% c("ID304"))

#ID305 PARNA Abrolhos	FEB	2023	556	2432
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID305")) + 
  geom_point(data = subset(data, ID %in% "ID305"), size = 3,
             aes(color = ifelse(Time >=  15 & Time <= 74.64, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID305 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID305 <- data %>% filter(Time >= 15 & Time <= 74.64, ID %in% c("ID305"))

#ID306 PARNA Abrolhos	FEB	2023	488	1704
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID306")) + 
  geom_point(data = subset(data, ID %in% "ID306"), size = 3,
             aes(color = ifelse(Time >=  18 & Time <= 85.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID306 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID306 <- data %>% filter(Time >= 18 & Time <= 85.5, ID %in% c("ID306"))

#ID307 PARNA Abrolhos	FEB	2023	440	1222
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID307")) + 
  geom_point(data = subset(data, ID %in% "ID307"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 82.36, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID307 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID307 <- data %>% filter(Time >= 0 & Time <= 82.36, ID %in% c("ID307"))

#ID308 PARNA Abrolhos	FEB	2023	529	2282
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID308")) + 
  geom_point(data = subset(data, ID %in% "ID308"), size = 3,
             aes(color = ifelse(Time >=  46 & Time <= 94.86, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 75) + 
  theme_light() +
  ggtitle("ID308 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID308 <- data %>% filter(Time >= 46 & Time <= 94.86, ID %in% c("ID308"))

#ID309 PARNA Abrolhos	FEB	2023	475	1847
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID309")) + 
  geom_point(data = subset(data, ID %in% "ID309"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 90.4, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID309 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID309 <- data %>% filter(Time >= 0 & Time <= 90.4, ID %in% c("ID309"))

#ID310 PARNA Abrolhos	FEB	2023	475	1847
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID310")) + 
  geom_point(data = subset(data, ID %in% "ID310"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 99.13, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID310 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID310 <- data %>% filter(Time >= 0 & Time <= 99.13, ID %in% c("ID310"))

#ID311 PARNA Abrolhos	FEB	2023	655	4170
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID311")) + 
  geom_point(data = subset(data, ID %in% "ID311"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 65.39, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID311 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID311 <- data %>% filter(Time >= 0 & Time <= 65.39, ID %in% c("ID311"))

#ID312 PARNA Abrolhos	FEB	2023	598	3030
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID312")) + 
  geom_point(data = subset(data, ID %in% "ID312"), size = 3,
             aes(color = ifelse(Time >=  5 & Time <= 90.95, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID312 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID312 <- data %>% filter(Time >= 5 & Time <= 90.95, ID %in% c("ID312"))

#ID313 PARNA Abrolhos	FEB	2023	541	2424
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID313")) + 
  geom_point(data = subset(data, ID %in% "ID313"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 60.6, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID313 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID313 <- data %>% filter(Time >= 0 & Time <= 60.6, ID %in% c("ID313"))

#ID314 PARNA Abrolhos	FEB	2023	586	3004
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID314")) + 
  geom_point(data = subset(data, ID %in% "ID314"), size = 3,
             aes(color = ifelse(Time >=  11 & Time <= 64.48, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID314 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID314 <- data %>% filter(Time >= 11 & Time <= 64.48, ID %in% c("ID314"))

#ID315 PARNA Abrolhos	FEB	2023	635	3943
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID315")) + 
  geom_point(data = subset(data, ID %in% "ID315"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 83.25, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID315 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID315 <- data %>% filter(Time >= 0 & Time <= 83.25, ID %in% c("ID315"))

#ID316 PARNA Abrolhos	FEB	2023	516	1730
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID316")) + 
  geom_point(data = subset(data, ID %in% "ID316"), size = 3,
             aes(color = ifelse(Time >=  24 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID316 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID316 <- data %>% filter(Time >= 24 & Time <= 88.5, ID %in% c("ID316"))

#ID317 PARNA Abrolhos	FEB	2023	532	2333
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID317")) + 
  geom_point(data = subset(data, ID %in% "ID317"), size = 3,
             aes(color = ifelse(Time >=  6 & Time <= 96.95, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID317 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID317 <- data %>% filter(Time >= 6 & Time <= 96.95, ID %in% c("ID317"))

#ID318 PARNA Abrolhos	FEB	2023	492	1560
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID318")) + 
  geom_point(data = subset(data, ID %in% "ID318"), size = 3,
             aes(color = ifelse(Time >=  10 & Time <= 94.65, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID318 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID318 <- data %>% filter(Time >= 10 & Time <= 94.65, ID %in% c("ID318"))

#ID319 PARNA Abrolhos	FEB	2023	557	2555
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID319")) +
  geom_point(data = subset(data, ID %in% "ID319"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 99.45, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID319 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID319 <- data %>% filter(Time >= 0 & Time <= 99.45, ID %in% c("ID319"))

#ID320 PARNA Abrolhos	FEB	2023	425	1261
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID320")) + 
  geom_point(data = subset(data, ID %in% "ID320"), size = 3,
             aes(color = ifelse(Time >=  33 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID320 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID320 <- data %>% filter(Time >= 33 & Time <= 88.5, ID %in% c("ID320"))

#ID321 PARNA Abrolhos	FEB	2023	442	1167
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID321")) + 
  geom_point(data = subset(data, ID %in% "ID321"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 71.68, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID321 (AB)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID321 <- data %>% filter(Time >= 0 & Time <= 71.68, ID %in% c("ID321"))


#ID391 Guarapari	APR	2023	410	1255
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID391")) + 
  geom_point(data = subset(data, ID %in% "ID391"), size = 3,
             aes(color = ifelse(Time >=  5 & Time <= 86.16, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 20) + 
  theme_light() +
  ggtitle("ID391 (GU)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID391 <- data %>% filter(Time >= 5 & Time <= 86.16, ID %in% c("ID391"))

#ID392 Guarapari	APR	2023	380	905
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID392")) +
  geom_point(data = subset(data, ID %in% "ID392"), size = 3,
             aes(color = ifelse(Time >=  13 & Time <= 110.69, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 15) +   
  theme_light() +
  ggtitle("ID392 (GU)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID392 <- data %>% filter(Time >= 18 & Time <= 110.69, ID %in% c("ID392"))

#ID393 Guarapari	APR	2023	390	918
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID393")) + 
  geom_point(data = subset(data, ID %in% "ID393"), size = 3,
             aes(color = ifelse(Time >=  30 & Time <= 79.64, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID393 (GU)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID393 <- data %>% filter(Time >= 30 & Time <= 79.64, ID %in% c("ID393"))

#ID394 Guarapari	APR	2023	380	891
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID394")) + 
  geom_point(data = subset(data, ID %in% "ID394"), size = 3,
             aes(color = ifelse(Time >=  3 & Time <= 83.43, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID394 (GU)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID394 <- data %>% filter(Time >= 3 & Time <= 83.43, ID %in% c("ID394"))

#ID395 Guarapari	APR	2023	380	891
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID395")) + 
  geom_point(data = subset(data, ID %in% "ID395"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 71.25, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 90) +   
  theme_light() +
  ggtitle("ID395 (GU)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID395 <- data %>% filter(Time >= 0 & Time <= 71.25, ID %in% c("ID395"))

#ID396 Guarapari	APR	2023	410	1155
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID396")) + 
  geom_point(data = subset(data, ID %in% "ID396"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 86.61, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) + 
  theme_light() +
  ggtitle("ID396 (GU)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID396 <- data %>% filter(Time >= 0 & Time <= 86.61, ID %in% c("ID396"))


#ID403 Guarapari shelf	FEB	2023	515	2200
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID403")) + 
  geom_point(data = subset(data, ID %in% "ID403"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID403 (GU)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID403 <- data %>% filter(Time >= 0 & Time <= 88.5, ID %in% c("ID403"))


#ID427 Marataizes	APR	2023	520	2237.28
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID427")) +
  geom_point(data = subset(data, ID %in% "ID427"), size = 3,
             aes(color = ifelse(Time >=  10 & Time <= 77.99, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 170) +   
  theme_light() +
  ggtitle("ID427 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID427 <- data %>% filter(Time >= 18 & Time <= 77.99, ID %in% c("ID427"))

#ID428 Marataizes	APR	2023	522	1999.62
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID428")) +
  geom_point(data = subset(data, ID %in% "ID428"), size = 3,
             aes(color = ifelse(Time >=  2 & Time <= 92.99, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID428 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID428 <- data %>% filter(Time >= 2 & Time <= 92.99, ID %in% c("ID428"))

#ID429 Marataizes	APR	2023	440	1318.2
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID429")) + 
  geom_point(data = subset(data, ID %in% "ID429"), size = 3,
             aes(color = ifelse(Time >=  10 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 125) +   
  theme_light() +
  ggtitle("ID429 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID429 <- data %>% filter(Time >= 10 & Time <= 88.5, ID %in% c("ID429"))

#ID430 Marataizes	APR	2023	499	1563
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID430")) + 
  geom_point(data = subset(data, ID %in% "ID430"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 86.05, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 35) +   
  theme_light() +
  ggtitle("ID430 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID430 <- data %>% filter(Time >= 0 & Time <= 86.05, ID %in% c("ID430"))

#ID431 Marataizes	APR	2023	540	2257.25
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID431")) + 
  geom_point(data = subset(data, ID %in% "ID431"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 60) +   
  theme_light() +
  ggtitle("ID431 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID431 <- data %>% filter(Time >= 0 & Time <= 88.5, ID %in% c("ID431"))

#ID432 Marataizes	APR	2023	415	2873.03
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID432")) + 
  geom_point(data = subset(data, ID %in% "ID432"), size = 3,
             aes(color = ifelse(Time >=  15 & Time <= 102.92, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 25) + 
  theme_light() +
  ggtitle("ID432 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID432 <- data %>% filter(Time >= 15 & Time <= 102.92, ID %in% c("ID432"))

#ID433 Marataizes	APR	2023	580	3226.02
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID433")) + 
  geom_point(data = subset(data, ID %in% "ID433"), size = 3,
             aes(color = ifelse(Time >=  15 & Time <= 107.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 25) +   
  theme_light() +
  ggtitle("ID433 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID433 <- data %>% filter(Time >= 15 & Time <= 107.5, ID %in% c("ID433"))

#ID434 Marataizes	APR	2023	540	2035.48
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID434")) + 
  geom_point(data = subset(data, ID %in% "ID434"), size = 3,
             aes(color = ifelse(Time >=  18 & Time <= 65.08, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 50) +   
  theme_light() +
  ggtitle("ID434 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID434 <- data %>% filter(Time >= 18 & Time <= 65.08, ID %in% c("ID434"))

#ID435 Marataizes	APR	2023	486	1415.92
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID435")) +
  geom_point(data = subset(data, ID %in% "ID435"), size = 3,
             aes(color = ifelse(Time >=  3 & Time <= 59.86, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID435 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID435 <- data %>% filter(Time >= 3 & Time <= 59.86, ID %in% c("ID435"))

#ID436 Marataizes	APR	2023	544	2115.85
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID436")) + 
  geom_point(data = subset(data, ID %in% "ID436"), size = 3,
             aes(color = ifelse(Time >=  7 & Time <= 53.35, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 30) +   
  theme_light() +
  ggtitle("ID436 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID436 <- data %>% filter(Time >= 7 & Time <= 53.35, ID %in% c("ID436"))

#ID437 Marataizes	APR	2023	486	2271.01
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID437")) + 
  geom_point(data = subset(data, ID %in% "ID437"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 79.98, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 20) +   
  theme_light() +
  ggtitle("ID437 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID437 <- data %>% filter(Time >= 0 & Time <= 79.98, ID %in% c("ID437"))

#ID438 Marataizes	APR	2023	544	1549.66
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID438")) +
  geom_point(data = subset(data, ID %in% "ID438"), size = 3,
             aes(color = ifelse(Time >=  18 & Time <= 59.22, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 10) +   
  theme_light() +
  ggtitle("ID438 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID438 <- data %>% filter(Time >= 18 & Time <= 59.22, ID %in% c("ID438"))

#ID439 Marataizes	MAY	2023	541	1822.3
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID439")) + 
  geom_point(data = subset(data, ID %in% "ID439"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 30) +   
  theme_light() +
  ggtitle("ID439 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID439 <- data %>% filter(Time >= 0 & Time <= 88.5, ID %in% c("ID439"))

#ID440 Marataizes	APR	2023	479	2715.1
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID440")) + 
  geom_point(data = subset(data, ID %in% "ID440"), size = 3,
             aes(color = ifelse(Time >=  37 & Time <= 88.5, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 35) + 
  theme_light() +
  ggtitle("ID440 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID440 <- data %>% filter(Time >= 37 & Time <= 88.5, ID %in% c("ID440"))

#ID441 Marataizes	APR	2023	482	1694.73
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID441")) + 
  geom_point(data = subset(data, ID %in% "ID441"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 100.01, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 100) +   
  theme_light() +
  ggtitle("ID441 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID441 <- data %>% filter(Time >= 0 & Time <= 100.01, ID %in% c("ID441"))

#ID442 Marataizes	APR	2023	485	2062.774
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID442")) +
  geom_point(data = subset(data, ID %in% "ID442"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 108.7, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 65) +   
  theme_light() +
  ggtitle("ID442 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID442 <- data %>% filter(Time >= 0 & Time <= 108.7, ID %in% c("ID442"))

#ID443 Marataizes	APR	2023	597	3197.69
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID443")) +
  geom_point(data = subset(data, ID %in% "ID443"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 93.22, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 75) +   
  theme_light() +
  ggtitle("ID443 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID443 <- data %>% filter(Time >= 0 & Time <= 93.22, ID %in% c("ID443"))


#ID516 Marataizes	JAN	2023	690	8000
ggplot(data, aes(x=Time*10, y=Ba138)) +
  geom_line(data=subset(data,ID %in% "ID516")) +
  geom_point(data = subset(data, ID %in% "ID516"), size = 3,
             aes(color = ifelse(Time >=  0 & Time <= 82.58, "red", "purple")),
             show.legend = FALSE) + geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  ylim(0, 15) +   
  theme_light() +
  ggtitle("ID516 (MA)")+
  xlab(expression(Distance~from~core~(mu~m))) + ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1)))

ID516 <- data %>% filter(Time >= 0 & Time <= 82.58, ID %in% c("ID516"))

#####
# Combine datasets
#####
adult_core_RF <- rbind(ID110, ID111, ID112, ID142, ID143, ID144, ID158,
                       ID241, ID262, ID263, ID264, ID266, ID267, ID268, 
                       ID292, ID293, ID294, ID295, ID296, ID297, ID298, 
                       ID299, ID300, ID301, ID302, ID303, ID304, ID305, 
                       ID306, ID307, ID308, ID309, ID310, ID311, ID312, 
                       ID313, ID314, ID315, ID316, ID317, ID318, ID319, 
                       ID320, ID321, ID391, ID392, ID393, ID394, ID395, 
                       ID396, ID403, ID427, ID428, ID429, ID430, ID431, 
                       ID432, ID433, ID434, ID435, ID436, ID437, ID438, 
                       ID439, ID440, ID441, ID442, ID443, ID516)

dim(adult_core_RF)
table(unique(adult_core_RF$ID))
length(unique(adult_core_RF$ID))

#Write table
#write.table(adult_core_RF,"adult_core_RF.csv", sep=";", dec=".",row.names = F)








#####
#Figure 2
######


#Clean R environment 
rm(list = ls())

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

str(data)
data$Time <- as.numeric(data$Time)
data$Ba138 <- as.numeric(data$Ba138)

#550 x 400

#ID111 Abrolhos Bank	November	2022	745	6000.00
ID111 <- data %>% filter(ID %in% c("ID111"))

ID111 <- ggplot(data, aes(x=Time, y=Ba138)) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  geom_line(data=subset(data,ID %in% "ID111")) + 
  geom_point(data = subset(data, ID %in% "ID111"), size = 3,
             aes(color = ifelse(Time >= 269 & Time <= max(Time), "red", 
                                ifelse(Time >= 18 & Time <= 85.5, "purple", "green"))),
             show.legend = FALSE) +  
  ylim(0, 4.5) +
  theme_light() +
  ggtitle("ID111 captured on Non-upwelling AG")+ 
  ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1))) +
  scale_x_continuous(expression(Distance~from~core~(mu~m)), limits = c(0, 280),
                     breaks = seq(0, 300, by = 50))+
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))

ID111

#ID263 Forgotten Reefs	FEB	2023	610	4402	38.5
ID263 <- ggplot(data, aes(x=Time, y=Ba138)) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
  geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
  geom_line(data=subset(data,ID %in% "ID263")) + 
  geom_point(data = subset(data, ID %in% "ID263"), size = 3,
             aes(color = ifelse(Time >= 195 & Time <= max(Time), "red", 
                                ifelse(Time >= 18 & Time <= 98.12, "purple", "green"))),
             show.legend = FALSE) +  
  
  ylim(0, 4.5) +   
  theme_light() +
  ggtitle("ID263 captured on Upwelling AG")+
  ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1))) +
  scale_x_continuous(expression(Distance~from~core~(mu~m)), limits = c(0, 280),
                     breaks = seq(0, 300, by = 50))+
  theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 14))

ID263

library(ggbreak)

#ID511 Sao Mateus	JUN	2023	159	64.04
ID511 <- ggplot(data, aes(x=Time, y=Ba138)) +
          geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
          geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
          geom_line(data=subset(data,ID %in% "ID511")) + 
          geom_point(data = subset(data, ID %in% "ID511"), size = 3,
                     aes(color = ifelse(Time >= 70 & Time <= max(Time), "red", "purple")),
                     show.legend = FALSE) +  
          ylim(0, 35) + 
          theme_light() +
  ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1))) +
  ggtitle("ID511 captured on Brakish ENH")+
          scale_x_continuous(expression( ), limits = c(0, 280),
                     breaks = seq(0, 300, by = 50))+
          theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14)) +
          scale_y_break(c(4.5, 15), scales = 1)

ID511


#ID188 Gramute	OCT	2022	134	42.66	6.8
ID188 <- ggplot(data, aes(x=Time, y=Ba138)) +
          geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
          geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
          geom_line(data=subset(data,ID %in% "ID188")) + 
          geom_point(data = subset(data, ID %in% "ID188"), size = 3,
                     aes(color = ifelse(Time >= 48 & Time <= max(Time), "red", "purple")),
                     show.legend = FALSE) +  
          ylim(0, 4.5) +   
          theme_light() +
  ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1))) +
  ggtitle("ID188 captured on Marine NH")+
          scale_x_continuous(expression(Distance~from~core~(mu~m)), limits = c(0, 280),
                     breaks = seq(0, 300, by = 50))+          theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14))

ID188



#ID348 Piraque-acu	MAR	2023	200	112.47
ID348 <- data %>% filter(ID %in% c("ID348"))
ID348 <- as.data.frame(ID348)
ID348$Time <- ID348$Time -11.3
ID348$Time <- as.numeric(ID348$Time)
ID348$Ba138 <- as.numeric(ID348$Ba138)

ID348 <- ggplot(ID348, aes(x=Time, y=Ba138)) +
          geom_hline(yintercept =2.77, linetype = 'dashed', color = 'chartreuse3', size =1) +
          geom_hline(yintercept =1.77, linetype = 'dashed', color = 'cyan2', size =1) +
          geom_line(data=subset(ID348,ID %in% "ID348")) + 
          geom_point(data = subset(ID348, ID %in% "ID348"), size = 3,
                     aes(color = ifelse(Time >= 72 & Time <= max(Time), "red", "purple")),
                     show.legend = FALSE) +  
          ylim(0, 4.5) +   
          theme_light() +
  ylab(expression(Ba:Ca[otolith]~(mu~mol~mol^-1))) +
  ggtitle("ID348 captured on Saline ENH")+
          scale_x_continuous(expression(Distance~from~core~(mu~m)), limits = c(0, 280),
                     breaks = seq(0, 300, by = 50))+          theme(axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14))
ID348



