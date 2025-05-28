
# Chapter 8 - Predicting Heating Oil Usage with Linear Regression
# Author: Data Mining Student

### 1. Import Data ###
# Import training data
ch8Train <- read.csv(file.choose(), header=TRUE)

# Import scoring data
ch8Score <- read.csv(file.choose(), header=TRUE)

### 2. Describe and Compare Ranges ###
# Load psych library
library(psych)

# Descriptive statistics
describe(ch8Train)
describe(ch8Score)

# Remove scoring data with out-of-range Home_Size
ch8Score <- subset(ch8Score, Home_Size >= 489 & Home_Size <= 7081)

### 3. Build the Linear Regression Model ###
# Load MASS library
library(MASS)

# Initial model with all variables
ch8Model <- lm(Heating_Oil_Used ~ Insulation_Rating + Outdoor_Temp + Num_Occupants + Home_Age + Home_Size, data=ch8Train)

# View model summary
summary(ch8Model)

# Refined model (removing non-significant Num_Occupants)
ch8Model <- lm(Heating_Oil_Used ~ Insulation_Rating + Outdoor_Temp + Home_Age + Home_Size, data=ch8Train)

# View updated model summary
summary(ch8Model)

### 4. Make Predictions ###
# Generate predictions for scoring data
ch8Predictions <- predict(ch8Model, ch8Score)

# Combine predictions with scoring data
ch8PredOutput <- data.frame(ch8Predictions, ch8Score)

# View combined output
View(ch8PredOutput)

### 5. Summarize Predictions ###
# Total predicted heating oil usage
sum(ch8PredOutput$ch8Predictions)

# Average predicted usage per household
mean(ch8PredOutput$ch8Predictions)

### Optional: Manual Calculation ###
# Example: Predict for one observation manually
# y = (3.329 * Insulation) + (-0.886 * Temp) + (2.005 * Age) + (0.004 * Size) + 160.634
# Replace values below with actual inputs
# (3.329*6) + (-0.886*67) + (2.005*35) + (0.004*5286) + 160.634
