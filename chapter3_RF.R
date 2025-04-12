#################

# Load the randomForest package
library(randomForest)
library(dplyr)

#########################
#Read dataset
#########################


#Clean R environment 
rm(list = ls())

#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")

#Read data
edge <- read.csv2("data_dog_snappers.csv")

length(unique(edge$ID))
table(unique(edge$Site))


# Combined update of `Site` and `Habitat`
edge <- edge %>%
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


#Select the last rows in each ID
edge <- edge %>%
  group_by(ID) %>%
  slice_tail(n = 5)

#Set as numeric
edge <- mutate_at(edge, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Rb85, Sr87, Ba138, Pb208, 
                               TL, FL, SL, Weight)), as.numeric)


edge <- edge[,c("ID","Site","Habitat","Mg24", "Al27","Cr52", "Fe57", "Cu65", "Zn66", "Ba138", "Pb208")]

summary(is.na(edge))
edge[is.na(edge)] <- 0


# Random selected SM IDs
selected_ids <- c("ID173", "ID326", "ID053", "ID072", "ID161", "ID253", "ID327", 
                  "ID159", "ID048", "ID273", "ID076", "ID381", "ID204", "ID281", 
                  "ID081", "ID385", "ID511", "ID277", "ID235", "ID164", "ID260", 
                  "ID388", "ID324", "ID018", "ID488", "ID278", "ID489", "ID160", 
                  "ID075", "ID079")

sm30 <- edge[edge$ID %in% selected_ids, ]
sm30

edge <- edge %>%
  filter(Site != "SM")

edge <- bind_rows(edge, sm30)


edge %>%
  group_by(Site) %>%
  summarise(count = n_distinct(ID))
length(unique(edge$ID))

edge %>%
  group_by(Habitat) %>%
  summarise(count = n_distinct(ID))
length(unique(edge$ID))

######
#Ranfom Forest
######

# Train-test split
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(edge), 0.70 * nrow(edge)) # 70% training data
train_data <- edge[train_index, ]
train_data <- train_data [,-2]
test_data <- edge[-train_index, ]

summary(train_data$Habitat)
summary(test_data$Habitat)

train_data$Habitat<-as.factor(train_data$Habitat)
test_data$Habitat<-as.factor(test_data$Habitat)


test_data
test_data %>%
  group_by(Habitat) %>%
  summarise(n = n())

#####
#Models
#####

# Build a Random Forest classifier
rf_model <- randomForest(Habitat ~ Mg24 + Al27 + Cr52 + Fe57 + Cu65 + Zn66 + Ba138 + Pb208, 
                         data = train_data)

# Predict using the Random Forest model
predictions <- predict(rf_model, test_data)

predictions_data_frame <- as.data.frame(predictions)
predictions_data_frame

# Evaluate the model
confusion_matrix <- table(predictions, test_data$Habitat)
confusion_matrix

#n = 835
#70% (n = 584 multi-elemental signatures) for training and 20% (n = 251) for testing
#predictions     Brackish Marine Non-upwelling Saline Upwelling
#Brackish            63      3             0      5         2
#Marine               0      9             0      0         1
#Non-upwelling        0      8            55      3        12
#Saline               4      2             1     49         6
#Upwelling            3      0             3      4        18

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

#n = 835
#overall classification accuracy of 0.7729084

# Model
rf_model


# Calculate the column totals for each habitat type
column_totals <- colSums(confusion_matrix)
column_totals

# Repeat the row 5 times to create the matrix
repetitive_matrix <- matrix(rep(column_totals, time = 5), 
                            nrow = 5, byrow = TRUE,
                            dimnames = list(c("Brackish", "Marine", "Non-upwelling", "Saline", "Upwelling"),
                                            c("Brackish", "Marine", "Non-upwelling", "Saline", "Upwelling")))
repetitive_matrix

# Convert each element to percentage of its respective column
confusion_matrix_percent <- round((confusion_matrix / repetitive_matrix) * 100, 1)

# Display the matrix in percentages
confusion_matrix_percent

#predictions     Brackish Marine Non-upwelling Saline Upwelling
#Brackish          90.0   13.6           0.0    8.2       5.1
#Marine             0.0   40.9           0.0    0.0       2.6
#Non-upwelling      0.0   36.4          93.2    4.9      30.8
#Saline             5.7    9.1           1.7   80.3      15.4
#Upwelling          4.3    0.0           5.1    6.6      46.2


#####
#Element variable importance
#####

# Get variable importance measures
importance_measures <- importance(rf_model)

# Print variable importance
round(importance_measures,1)

# Plot variable importance
varImpPlot(rf_model)

# Extract variable importance
importance_data <- as.data.frame(importance(rf_model))
importance_data$Variable <- rownames(importance_data)
importance_data

importance_data$Variable <- gsub("Al27", "Al:Ca", importance_data$Variable)
importance_data$Variable <- gsub("Pb208", "Pb:Ca", importance_data$Variable)
importance_data$Variable <- gsub("Cu65", "Cu:Ca", importance_data$Variable)
importance_data$Variable <- gsub("Fe57", "Fe:Ca", importance_data$Variable)
importance_data$Variable <- gsub("Zn66", "Zn:Ca", importance_data$Variable)
importance_data$Variable <- gsub("Cr52", "Cr:Ca", importance_data$Variable)
importance_data$Variable <- gsub("Mg24", "Mg:Ca", importance_data$Variable)
importance_data$Variable <- gsub("Ba138", "Ba:Ca", importance_data$Variable)

# Create the ggplot
ggplot(importance_data, aes(x = reorder(Variable, -MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "cyan4", color = "aquamarine2", alpha = 0.5,width=0.7) +
  coord_flip() +
  theme_bw() +
  labs(x = " ", y = "Mean decrease in Gini's index") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

#500 x 400

importance_data
total_gini <- sum(importance_data$MeanDecreaseGini)
importance_data$Percentage <- (importance_data$MeanDecreaseGini / total_gini) * 100
importance_data$Percentage <- round(importance_data$Percentage,1)
importance_data

#Write table
#write.table(test_data,"test_data.csv", sep=";", dec=".",row.names = F)
#write.table(train_data,"train_data.csv", sep=";", dec=".",row.names = F)



#####
# True Skill Statistic (TSS)
#####

confusion_matrix

#predictions     Brackish Marine Non-upwelling Saline Upwelling
#Brackish            63      3             0      5         2
#Marine               0      9             0      0         1
#Non-upwelling        0      8            55      3        12
#Saline               4      2             1     49         6
#Upwelling            3      0             3      4        18

#a - correct prediction of presence at re-assignment 
#b - correct prediction of absence
#c - incorrect prediction of presence
#d - incorrect predictions of absence

#Brackish
a = 63                      #correctly predicted as Brackish
c = 0 + 0 + 4 + 3           #actual Brackish classified as other habitats
d = 3 + 0 + 5 + 2           #other habitats predicted as Brackish
b = 251 - (a + c + d)       #the correct prediction of absence

tss_brackish <- (a * b - c * d) / ((a + d) * (c + b))
round(tss_brackish,3)
#TSS Brackish = 0.824
 
#Marine
a = 9                         #correctly predicted as Marine
c = 3 + 8 + 2 + 0             #actual Marine classified as other habitats
d = 0 + 0 + 0 + 1             #other habitats predicted as Marine
b = 251 - (a + c + d)         #the correct prediction of absence

tss_marine <- (a * b - c * d) / ((a + d) * (c + b))
round(tss_marine,3)
#TSS Marine = 0.846

#Non-upwelling
a = 55                  #correctly predicted as Non-upwelling
c = 0 + 0 + 1 + 3       #actual Non-upwelling classified as other habitats
d = 0 + 8 + 3 + 12       #other habitats predicted as Non-upwelling
b = 251 - (a + c + d)   #the correct prediction of absence

tss_nonup <- (a * b - c * d) / ((a + d) * (c + b))
round(tss_nonup, 3)
#TSS = Non-upwelling 0.682

#Saline
a = 49                         #correctly predicted as Saline
c = 5 + 0 + 3 + 4              #actual Saline classified as other habitats
d = 4 + 2 + 1 + 6              #other habitats predicted as Saline
b = 251 - (a + c + d)          #the correct prediction of absence

tss_saline <- (a * b - c * d) / ((a + d) * (c + b))
round(tss_saline,3)
#TSS Saline = 0.727

#Upwelling
a = 18                    #correctly predicted as Upwelling)
c = 2 + 1 + 12 + 6         #actual Upwelling classified as other habitats
d = 3 + 0 + 3 + 4         #other habitats predicted as Upwelling
b = 251 - (a + c + d)     #the correct prediction of absence

tss_up <- (a * b - c * d) / ((a + d) * (c + b))
round(tss_up, 3)
#TSS Upwelling = 0.549


########################
#predictions     Brackish Marine Non-upwelling Saline Upwelling
#Brackish            63      3             0      5         2
#Marine               0      9             0      0         1
#Non-upwelling        0      8            55      3        12
#Saline               4      2             1     49         6
#Upwelling            3      0             3      4        18

#Overall
a = 63 + 9 + 55 + 49 + 18 

c = 0 + 0 + 4 + 3 +
    3 + 8 + 2 + 0 +
    0 + 0 + 1 + 3 +
    5 + 0 + 3 + 4 +
    2 + 1 + 12 + 6

d = 3 + 0 + 5 + 2 +
    0 + 0 + 0 + 1 +
    0 + 8 + 3 + 12 +
    4 + 2 + 1 + 6 +
    3 + 0 + 3 + 4

b = (251*5) - (a + c + d)  

tss_overall <- (a * b - c * d) / ((a + d) * (c + b))
round(tss_overall,3)
#TSS overall = 0.716

round(tss_saline,3)
round(tss_brackish,3)
round(tss_marine,3)
round(tss_up,3)
round(tss_nonup,3)
round(tss_overall,3)

#TSS values range from -1 to 1
#1: A perfect score 
#0: Random performance 
#0.5 or higher: An acceptable model performance 
#< 0: A performance no better than random

#TSS Brackish = 0.824
#TSS Saline = 0.727
#TSS Marine = 0.846
#TSS Upwelling = 0.549
#TSS Non-upwelling = 0.682
#TSS overall = 0.716


#########
# Save model
#########

#save(rf_model, file = "rf_model.RData")
#load("random_forest_model.RData")










#########
# Juvenile habitat assignment for sub-adult and adult fish
#########

#Read data
adult_core_RF <- read.csv2("adult_core_RF.csv")
head(adult_core_RF)

adult_core_RF <- adult_core_RF[,c("ID","Site","Habitat","Mg24", "Al27","Cr52", "Fe57", "Cu65", "Zn66", "Ba138", "Pb208")]

#Set as numeric
adult_core_RF <- mutate_at(adult_core_RF, vars(c(Mg24, Al27, Cr52, Fe57, Cu65,
                               Zn66, Ba138, Pb208)), as.numeric)

summary(is.na(adult_core_RF))
adult_core_RF[is.na(adult_core_RF)] <- 0


# Apply the Random Forest model to the new dataset, getting probabilities
pred_adult_core <- predict(rf_model, adult_core_RF)
pred_adult_core <- as.data.frame(pred_adult_core)
head(pred_adult_core)

RF_habitat_assigned_adult_core <- bind_cols(adult_core_RF, pred_adult_core)
head(RF_habitat_assigned_adult_core)

RF_habitat_assigned_adult_core %>%
  group_by(pred_adult_core) %>% 
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Define the threshold function
assign_habitat <- function(predictions, threshold = 0.5) {
  # Calculate the proportion of each class
  prop <- table(predictions) / length(predictions)
  
  # Check if the maximum proportion is greater than or equal to the threshold
  if (max(prop) >= threshold) {
    # Return the habitat with the highest proportion
    return(names(prop)[which.max(prop)])
  } else {
    return(NA) # If no habitat exceeds the threshold, assign NA
  }
}

# Apply the function to each ID
assignments <- RF_habitat_assigned_adult_core %>%
  group_by(ID) %>%
  summarise(Habitat_assigned = assign_habitat(pred_adult_core))

# View the final dataset with assignments
assignments

assignments %>%
  group_by(Habitat_assigned) %>% 
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Select only the necessary columns ('ID', 'Site', 'Habitats') from 'adult_core_RF'
adult_core_RF_selected <- adult_core_RF %>%
  select(ID, Site, Habitat) %>%
  distinct(ID, .keep_all = TRUE)

# Perform a left join to keep all rows from 'assignments' and bring in matching 'Site' and 'Habitats' values from 'adult_core_RF'
RF_habitat_assigned_ID <- adult_core_RF_selected %>%
  left_join(assignments, by = "ID")

RF_habitat_assigned_ID

#Write table
#write.table(RF_habitat_assigned_ID,"RF_habitat_assigned_ID.csv", sep=";", dec=".",row.names = F)
#write.table(RF_habitat_assigned_adult_core,"RF_habitat_assigned_adult_core.csv", sep=";", dec=".",row.names = F)







####
# Figure
####

# Load required libraries
library(ggplot2)
library(webr)
library(dplyr)

# Calculate the proportions of each Habitat_assigned within each Habitat
habitat_data <- RF %>%
  count(Habitat, Habitat_assigned) %>%
  group_by(Habitat) %>%
  mutate(percentage = n / sum(n) * 100)

palette_colors <- c(
  rgb(248/255, 118/255, 109/255, alpha = 0.5),
  rgb(163/255, 165/255, 0/255, alpha = 0.5),
  rgb(102/255, 255/255, 102/255, alpha = 0.5),
  rgb(232/255, 118/255, 243/255, alpha = 0.5),
  rgb(1/255, 176/255, 246/255, alpha = 0.5)
)

table(unique(habitat_data$Habitat_assigned))

habitat_data$Habitat_assigned <- factor(habitat_data$Habitat_assigned, levels = c("Brackish","Saline","Marine", "Non-upwelling", "Upwelling"))


# Create the pie chart
ggplot(habitat_data, aes(x = "", y = percentage, fill = Habitat_assigned)) +
  geom_bar(stat = "identity", width = 10, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ Habitat) +  # Separate pie charts for each Habitat category
  labs(title = " ",
       fill = "Habitat assigned") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),  # Size of facet labels (Habitat)
        plot.title = element_text(size = 16, face = "bold")) +
  theme_void() +
  theme(legend.position = "right")+
  scale_fill_manual(values = c("#f8766d", "#e876f3", "#a3a500", "#66FF66", "#01b0f6")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 4, 
            label.padding = unit(0.5, "lines"),  # Adds some padding around the label
            label.size = 0.3) 

habitat_data
habitat_data$percentage <- round(habitat_data$percentage,1)

###
#Upwelling
###

Upwelling <- habitat_data %>%
  filter(Habitat != "Non-upwelling")

Upwelling

Upwelling <- Upwelling %>%
  mutate(across(everything(), ~replace(as.character(.), is.na(.), "Unidentified")))

Upwelling <- Upwelling %>%
  mutate(Habitat_type = case_when(
    Habitat_assigned == "Brackish" ~ "Estuary",
    Habitat_assigned == "Upwelling" ~ "Sea",
    Habitat_assigned == "Non-upwelling" ~ "Sea",
    Habitat_assigned == "Unidentified" ~ "Unidentified",
    TRUE ~ as.character(Habitat_assigned)  # Retain other values if any
  ))

Upwelling
Upwelling$n <- as.numeric(Upwelling$n)
Upwelling$Upwelling <- Upwelling$Habitat_type


PieDonut(Upwelling, aes(Upwelling, Habitat_assigned, count=n), title = NULL,
         explode = 1:2, 
         color = "gray",
         r0=0.5,
         start=5.4,
         ratioByGroup=FALSE,
         pieLabelSize = 4, donutLabelSize = 4)


###
#Non-upwelling
###

Nonupwelling <- habitat_data %>%
  filter(Habitat != "Upwelling")

Nonupwelling

Nonupwelling <- Nonupwelling %>%
  mutate(across(everything(), ~replace(as.character(.), is.na(.), "Unidentified")))

Nonupwelling <- Nonupwelling %>%
  mutate(Habitat_type = case_when(
    Habitat_assigned == "Saline" ~ "Estuary",
    Habitat_assigned == "Brackish" ~ "Estuary",
    Habitat_assigned == "Marine" ~ "Sea",
    Habitat_assigned == "Non-upwelling" ~ "Sea",
    Habitat_assigned == "Upwelling" ~ "Sea",
      Habitat_assigned == "Unidentified" ~ "Unidentified",
    TRUE ~ as.character(Habitat_assigned)  # Retain other values if any
  ))

Nonupwelling
Nonupwelling$n <- as.numeric(Nonupwelling$n)
Nonupwelling$Non_upwelling <- Nonupwelling$Habitat_type

PieDonut(Nonupwelling, aes(Non_upwelling, Habitat_assigned, count=n), title = NULL,
         explode = 1:3, 
         color = "gray",
         r0=0.5,
         start=1.4,
         ratioByGroup=FALSE,
         pieLabelSize = 4, donutLabelSize = 4)


