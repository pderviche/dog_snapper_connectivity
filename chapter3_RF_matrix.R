# Load necessary libraries
library(ggplot2)
library(reshape2)


#Set working directory
setwd("C:/Users/patri/OneDrive/Documentos/UFES/Tese/Connectivity/R")


confusion_matrix_percent
order <- c("Brackish", "Saline", "Marine", "Upwelling", "Non-upwelling")

# Reorder rows and columns
confusion_matrix_percent_order <- confusion_matrix_percent[order, order]
confusion_matrix_percent_order

#predictions     Brackish Saline Marine Upwelling Non-upwelling
#Brackish          90.0    8.2   13.6       5.1           0.0
#Saline             5.7   80.3    9.1      15.4           1.7
#Marine             0.0    0.0   40.9       2.6           0.0
#Upwelling          4.3    6.6    0.0      46.2           5.1
#Non-upwelling      0.0    4.9   36.4      30.8          93.2

# Create the data frame for the cross-assignment table
data <- data.frame(
  Assignment = c("Brackish ENH", "Saline ENH", "Marine NH", "Upwelling AG", "Non-upwelling AG"),
  `Brackish ENH` = c(90.0, 5.7, 0.0, 4.3, 0.0),
  `Saline ENH` = c(8.2, 80.3, 0.0, 6.6, 4.9),
  `Marine NH` = c(13.6, 9.1, 40.9,  0.0, 36.4),
  `Upwelling AG` = c(5.1, 15.4, 2.6, 46.2, 30.8),
  `Non-upwelling AG` = c(0.0, 1.7, 0.0, 5.1, 93.2)
)

# Reshape the data frame for ggplot2
data_melt <- melt(data, id.vars = "Assignment", variable.name = "Habitat", value.name = "Percentage")

data_melt <- data_melt %>%
  mutate(Habitat = recode(Habitat, 
                          "Brackish.ENH" = "Brackish ENH", 
                          "Saline.ENH" = "Saline ENH",
                          "Marine.NH" = "Marine NH",
                          "Non.upwelling.AG" = "Non-upwelling AG",
                          "Upwelling.AG" = "Upwelling AG"))

# Specify the order for the x and y axes
data_melt$Assignment <- factor(data_melt$Assignment, levels = c("Non-upwelling AG", "Upwelling AG", "Marine NH", "Saline ENH", "Brackish ENH"))
data_melt$Habitat <- factor(data_melt$Habitat, levels = c("Brackish ENH", "Saline ENH", "Marine NH",  "Upwelling AG", "Non-upwelling AG"))

# Plot the heatmap with a color gradient
ggplot(data_melt, aes(x = Habitat, y = Assignment, fill = Percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "aquamarine2", high = "cyan4", midpoint = 50, limits = c(0, 100), name = "Percentage") +
  geom_text(aes(
    label = sprintf("%.1f%%", Percentage),
    fontface = ifelse(Percentage > 40, "bold", "plain") 
  ), color = "gray20", size = 4) + 
  labs(x = "Actual habitat type", y = "Assignment") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "cm")
  )

