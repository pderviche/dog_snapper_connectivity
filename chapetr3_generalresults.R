#########################
#Read dataset
#########################


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

length(unique(edge$ID))


#Set as numeric
edge <- mutate_at(edge, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Rb85, Sr87, Ba138, Pb208, 
                               TL, FL, SL, Weight)), as.numeric)





#edge <- edge[edge$Site != 'AK', ]

summary(is.na(edge))
edge[is.na(edge)] <- 0

edge$Site <- factor(edge$Site, levels = c('CA','PA','SM','IT','GR','MA','GU','FR','AB','AK'))


###
# General results
###

# TL Mean SD
edge %>%
  group_by(Site) %>%
  summarise(
    mean_TL = mean(TL, na.rm = TRUE),
    sd_TL = sd(TL, na.rm = TRUE),
    max_TL = max(TL, na.rm = TRUE),
    min_TL = min(TL, na.rm = TRUE))


