#R version 4.2.1 (2022-06-23 ucrt)

# Load necessary libraries
library(readxl)
library(data.table)
library(multcomp)

# Read data from Excel file
data <- read_excel("path/to/file.xlsx", 
                   sheet = "Sheet1",
                   range = "A1:B5")  # Replace with your desired range

# Reshape data for ANOVA test
data_melted <- melt(data, id.vars = NULL)
setnames(data_melted, c("variable", "value"), c("Conditions", "Value"))
data_melted$Conditions <- as.factor(data_melted$Conditions)

# Perform one-way ANOVA
model_data_melted <- aov(Value ~ Conditions, data = data_melted)
summary(model_data_melted)

# Perform Tukey's HSD post-hoc test
TukeyHSD_model_data_melted <- glht(model_data_melted,
                                   linfct = mcp(Conditions = "Tukey"))
summary(TukeyHSD_model_data_melted)

# Export ANOVA and Tukey's HSD results to separate files
capture.output(summary(model_data_melted), file = "data_anova.txt")
capture.output(summary(TukeyHSD_model_data_melted), file = "data_tukey.txt")
