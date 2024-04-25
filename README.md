# BUS-446-FINAL-PROJECT-GRAPES
This repository is created as part of the final project of BUS 446 class in spring 2024. 
#Final Project BUS 446-Grapes 

# Load the dataset into R
lung_data <- read.csv("/Users/saharsana/Desktop/lung_capacity_data.csv")

# View the structure of the dataset
str(lung_data)

# Summary statistics of the dataset
summary(lung_data) 

#Question1

# Load necessary libraries
library(dplyr)

# 1. Subset the data by smoking status and gender
male_smokers <- lung_data %>% filter(Gender == "male", Smoke == "yes")
female_smokers <- lung_data %>% filter(Gender == "female", Smoke == "yes")

male_non_smokers <- lung_data %>% filter(Gender == "male", Smoke == "no")
female_non_smokers <- lung_data %>% filter(Gender == "female", Smoke == "no")

# 2. Calculate correlation coefficients
correlation_male_smokers <- cor(male_smokers$LungCap, male_smokers$Height)
correlation_female_smokers <- cor(female_smokers$LungCap, female_smokers$Height)

correlation_male_non_smokers <- cor(male_non_smokers$LungCap, male_non_smokers$Height)
correlation_female_non_smokers <- cor(female_non_smokers$LungCap, female_non_smokers$Height)

# 3. Compare correlations
correlation_male_smokers
correlation_female_smokers
correlation_male_non_smokers
correlation_female_non_smokers

# Load necessary libraries
library(ggplot2)

# Scatter plots with linear regression lines
ggplot(lung_data, aes(x = Height, y = LungCap)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Gender ~ Smoke) +
  labs(title = "Scatter Plot of Lung Capacity vs. Height by Gender and Smoking Status",
       x = "Height",
       y = "Lung Capacity") +
  theme_minimal()


#QUESTION2

# Load necessary libraries
library(ggplot2)

# 1. Subset the data by smoking status and gender
male_smokers <- lung_data[lung_data$Smoke == "yes" & lung_data$Gender == "male", ]
male_non_smokers <- lung_data[lung_data$Smoke == "no" & lung_data$Gender == "male", ]

female_smokers <- lung_data[lung_data$Smoke == "yes" & lung_data$Gender == "female", ]
female_non_smokers <- lung_data[lung_data$Smoke == "no" & lung_data$Gender == "female", ]

# 2. Calculate summary statistics by age group for male smokers and non-smokers
summary_male_smokers <- aggregate(LungCap ~ Age, data = male_smokers, FUN = summary)
summary_male_non_smokers <- aggregate(LungCap ~ Age, data = male_non_smokers, FUN = summary)

# 3. Visualize the data for male smokers and non-smokers
# Box plot or violin plot
ggplot(data = lung_data, aes(x = factor(Age), y = LungCap, fill = Smoke)) +
  geom_boxplot() +
  facet_wrap(~Gender) +
  labs(title = "Lung Capacity Variation Between Smokers and Non-Smokers by Age and Gender",
       x = "Age Group",
       y = "Lung Capacity",
       fill = "Smoking Status") +
  theme_minimal()

# 4. Analyze trends over time for male smokers and non-smokers
# Scatter plot with regression line
ggplot(data = lung_data, aes(x = Age, y = LungCap, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Smoke) +
  labs(title = "Trends in Lung Capacity by Age and Gender, Stratified by Smoking Status",
       x = "Age",
       y = "Lung Capacity",
       color = "Gender") +
  theme_minimal()



#QUESTION3


# Load necessary libraries
library(ggplot2)

# 1. Analyze Anatomical and Physiological Differences
# Visualize and compare lung capacity distributions between males and females
ggplot(lung_data, aes(x = Gender, y = LungCap, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Distribution of Lung Capacity by Gender",
       x = "Gender",
       y = "Lung Capacity") +
  theme_minimal()

# 2. Investigate Hormonal Factors
# Compare lung capacity between males and females, stratified by age groups
ggplot(lung_data, aes(x = Age, y = LungCap, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Lung Capacity by Age and Gender",
       x = "Age",
       y = "Lung Capacity",
       color = "Gender") +
  theme_minimal()

# 3. Consider Behavioral and Lifestyle Factors
# Compare lung capacity between smokers and non-smokers, stratified by gender
ggplot(lung_data, aes(x = Smoke, y = LungCap, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Distribution of Lung Capacity by Smoking Status and Gender",
       x = "Smoking Status",
       y = "Lung Capacity",
       fill = "Gender") +
  theme_minimal()

#or: 


# Load necessary libraries
library(ggplot2)

# Visualize lung capacity by gender, age, smoking status, and their interactions
ggplot(lung_data, aes(x = Age, y = LungCap, color = Smoke, shape = Smoke)) +
  geom_point() +
  facet_grid(Gender ~ Smoke) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Lung Capacity by Gender, Age, and Smoking Status",
       x = "Age",
       y = "Lung Capacity",
       color = "Smoking Status",
       shape = "Smoking Status") +
  theme_minimal()











