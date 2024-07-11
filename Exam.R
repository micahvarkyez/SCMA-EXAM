# Load necessary packages and libraries
install.packages("tidyverse")
install.packages("caret")
install.packages("car")
install.packages("broom")
install.packages("e1071")
library(tidyverse)
library(caret)
library(car)
library(broom)
library(e1071)
library(readr)

# Load the dataset
setwd("C:\\Users\\Dell\\Desktop\\MICAH")
data <- read_csv("cancer_reg.csv")

# View the structure of the data
str(data)

# Summary of the data
summary(data)

# Check for missing values
colSums(is.na(data))

# Handle missing values (example: using median imputation)
data <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Manage categorical variables (convert factors)
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Identify and handle outliers (example: capping)
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(. > quantile(., 0.99), quantile(., 0.99), .)))

# Split the data into training and testing sets
set.seed(123)
training_indices <- createDataPartition(data$TARGET_deathRate, p = 0.8, list = FALSE)
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# Build the multivariate OLS regression model
model <- lm(TARGET_deathRate ~ ., data = train_data)

# Model summary
summary(model)

# Predict on the test data
predictions <- predict(model, newdata = test_data)

# Calculate RMSE
rmse <- RMSE(predictions, test_data$TARGET_deathRate)
cat("RMSE: ", rmse)

# Adjusted R-squared
adj_r_squared <- summary(model)$adj.r.squared
cat("Adjusted R-squared: ", adj_r_squared)

# Linearity
plot(model$fitted.values, model$residuals)
abline(h = 0, col = "red")

# Serial independence of errors (Durbin-Watson test)
durbinWatsonTest(model)

# Multicollinearity (Variance Inflation Factor)
vif(model)
# Normality of residuals (Q-Q plot and Shapiro-Wilk test)
qqnorm(model$residuals)
qqline(model$residuals, col = "red")
shapiro.test(model$residuals)
