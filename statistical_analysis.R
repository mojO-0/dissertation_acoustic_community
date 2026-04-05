# 1. Load necessary libraries
# install.packages(c("tidyverse", "corrplot", "ggpubr")) # Run this if you don't have them
library(tidyverse)
library(corrplot)
library(ggpubr)

# 2. Load the dataset
df <- read.csv("guquarter.csv")

# 3. Data Inspection
print("--- Data Summary ---")
summary(df)

# 4. Boxplots for each index by Replicate
# Reshape data for easier plotting with ggplot
df_long <- df %>%
  pivot_longer(cols = c(ACI_avg, BI_avg, NDSI_avg, Anthrophony_avg, Biophony_avg),
               names_to = "Index", 
               values_to = "Value")

ggplot(df_long, aes(x = Replicate, y = Value, fill = Replicate)) +
  geom_boxplot() +
  facet_wrap(~Index, scales = "free") +
  theme_minimal() +
  labs(title = "Acoustic Indices by Replicate", y = "Value") +
  scale_fill_brewer(palette = "Set3")

# 5. Correlation Matrix
# Select only numeric columns
numeric_cols <- df %>% select(ACI_avg, BI_avg, NDSI_avg, Anthrophony_avg, Biophony_avg)
cor_matrix <- cor(numeric_cols)

# Plot Heatmap
corrplot(cor_matrix, method = "color", addCoef.col = "black", 
         type = "upper", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", mar = c(0,0,1,0))

# 6. Statistical Analysis: ANOVA and Tukey HSD
indices <- c("ACI_avg", "BI_avg", "NDSI_avg", "Anthrophony_avg", "Biophony_avg")

cat("\n--- Statistical Test Results ---\n")

for (idx in indices) {
  cat("\n----------------------------------\n")
  cat("Testing Index:", idx, "\n")
  
  # Run ANOVA
  formula <- as.formula(paste(idx, "~ Replicate"))
  res_anova <- aov(formula, data = df)
  
  # Print ANOVA summary
  print(summary(res_anova))
  
  # If p-value < 0.05, run Tukey HSD
  p_val <- summary(res_anova)[[1]][["Pr(>F)"]][1]
  if (p_val < 0.05) {
    cat("\nSignificant result found. Running Tukey HSD:\n")
    print(TukeyHSD(res_anova))
  } else {
    cat("\nNo significant difference (p > 0.05)\n")
  }
}

# 7. Grouped Means Summary Table
replicate_summary <- df %>%
  group_by(Replicate) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

print(replicate_summary)


# 1. Load necessary libraries
library(tidyverse) # includes ggplot2, tidyr, and dplyr

# 2. Load your data
df <- read.csv("guquarter.csv")

# 3. Reshape the data from 'wide' to 'long' format
# This is required for ggplot to create facets automatically
df_long <- df %>%
  pivot_longer(
    cols = c(ACI_avg, BI_avg, NDSI_avg, Anthrophony_avg, Biophony_avg),
    names_to = "Index", 
    values_to = "Value"
  ) %>%
  # Ensure Replicates are in the correct order on the X-axis
  mutate(Replicate = factor(Replicate, levels = c("Rep1", "Rep2", "Rep3")))

# 4. Create the faceted boxplot
ggplot(df_long, aes(x = Replicate, y = Value, fill = Replicate)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2) +
  # Create a 2x3 grid (matching the Python layout)
  facet_wrap(~Index, scales = "free_y", ncol = 3) + 
  # Apply a clean white grid theme similar to Seaborn's whitegrid
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12), # Style of subplot titles
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "none", # Hide legend since X-axis labels are sufficient
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  labs(
    title = "Acoustic Indices Across Replicates",
    x = "Replicate",
    y = "Average Index Value"
  ) +
  # Optional: Choose a color palette
  scale_fill_brewer(palette = "Pastel1")

# 5. Save the plot
ggsave("indices_by_replicate_R2.png", width = 12, height = 8, dpi = 300)