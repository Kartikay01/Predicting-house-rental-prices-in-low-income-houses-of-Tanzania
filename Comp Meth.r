# Load the necessary libraries
library(tidyverse)

# Load the data into R (replace "data.csv" with the actual filename and path of your data)
data <- read_csv("data.csv")

install.packages("gbm")

head(data)

colnames(data)

nrow(data)
ncol(data)

df <- data

df <- df %>% select(-electricity_cost_period)

colnames(df)

head(df)

for (col_name in colnames(df)) {
  # Do something with each column, for example print the column name and number of NA values
  if (any(is.na(df[[col_name]]))) {
    cat("Column", col_name, "has", sum(is.na(df[[col_name]])), "NA values.\n")
  } else {
    cat("Column", col_name, "has no NA values.\n")
  }
}

mode_x2 <- names(which.max(table(df$electricity_ESCOM)))

# Perform mode imputation
imputed_data <- df
imputed_data$electricity_ESCOM[is.na(imputed_data$electricity_ESCOM)] <- mode_x2
df <- imputed_data

mode_x2 <- names(which.max(table(df$source_drinkingWater)))

# Perform mode imputation
imputed_data <- df
imputed_data$source_drinkingWater[is.na(imputed_data$source_drinkingWater)] <- mode_x2
df <- imputed_data

mode_x2 <- names(which.max(table(df$cooking_fuel)))

# Perform mode imputation
imputed_data <- df
imputed_data$cooking_fuel[is.na(imputed_data$cooking_fuel)] <- mode_x2
df <- imputed_data

multiply_area <- function(df_temp) {
  df_temp$area_property[df_temp$area_property_unit == 1 & !is.na(df$area_property)] <- df_temp$area_property[df_temp$area_property_unit == 1 & !is.na(df$area_property)] * 4046.856422 
  df_temp$area_property[df_temp$area_property_unit == 2 & !is.na(df$area_property)] <- df_temp$area_property[df_temp$area_property_unit == 2 & !is.na(df$area_property)] *  10000
  df_temp$area_property[df_temp$area_property_unit == 4 & !is.na(df$area_property)] <- df_temp$area_property[df_temp$area_property_unit == 4 & !is.na(df$area_property)] *  0.836126983  
  return(df_temp)
}

multiply_rent <- function(df){
    df$Rent[df$timeunit_rent == 3 & !is.na(df$timeunit_rent)] <- df$Rent[df$timeunit_rent == 3 & !is.na(df$timeunit_rent)]*365
    df$Rent[df$timeunit_rent == 4 & !is.na(df$timeunit_rent)] <- df$Rent[df$timeunit_rent == 4 & !is.na(df$timeunit_rent)]*52
    df$Rent[df$timeunit_rent == 5 & !is.na(df$timeunit_rent)] <- df$Rent[df$timeunit_rent == 5 & !is.na(df$timeunit_rent)]*12
    return(df)
}

df <- multiply_area(df)

df <- df %>% select(-area_property_unit)

head(df)

df <- multiply_rent(df)

df <- df %>% select(-timeunit_rent)

head(df)

colnames(df)

mean_impute <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)  # Calculate mean, ignoring NA values
  x[is.na(x)] <- mean_x            # Replace NA values with mean
  return(x)
}

df$housing_price <- mean_impute(df$housing_price)
df$Rent <- mean_impute(df$Rent)
df$area_property <- mean_impute(df$area_property)

head(df)

for (col_name in colnames(df)) {
  # Do something with each column, for example print the column name and number of NA values
  if (any(is.na(df[[col_name]]))) {
    cat("Column", col_name, "has", sum(is.na(df[[col_name]])), "NA values.\n")
  } else {
    cat("Column", col_name, "has no NA values.\n")
  }
}

df$cooking_fuel <- as.numeric(as.character(df$cooking_fuel))
df$housing_price <- as.numeric(as.character(df$housing_price))
df$electricity_ESCOM <- as.numeric(as.character(df$electricity_ESCOM))
df$source_drinkingWater <- as.numeric(as.character(df$source_drinkingWater))

str(df)

df_temp <- df %>% select(-household_id)
cor(df_temp)

# Install and load required packages
install.packages("ggcorrplot")
library(ggcorrplot)

# Compute correlation matrix
cor_mat <- cor(df)

# Create correlation plot
ggcorrplot(cor_mat, 
           type = "upper", 
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"), 
           title = "Correlation Matrix")

model <- lm(housing_price ~ Rent + area_property + electricity + electricity_ESCOM + toilet_facility + cooking_fuel + MTLtelephone + source_drinkingWater + disposal_facility + use_bednet, data = df)

predicted <- predict(model)
actual <- df$housing_price

# Calculate the RMSE value
rmse <- sqrt(mean((actual - predicted)^2))

# Create a data frame with predicted and residual values
residuals <- actual - predicted
pred_df <- data.frame(predicted, actual, residuals)

# Print the summary of the model and the RMSE value
cat("Summary of the model:\n")
summary(model)
cat("\nRMSE value:", rmse, "\n")

ggplot(pred_df, aes(predicted, residuals)) +
  geom_point(color = 'blue') +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residual Plot") +
  xlab("Predicted Values") +
  ylab("Residuals") +
  ylim(limits = c(-1e7, 1e7))

# Load the necessary libraries
library(tidyverse)
library(rpart)

# Convert the housing_price column to a numeric variable
df$housing_price <- as.numeric(df$housing_price)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(df), nrow(df) * 0.7)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train the tree-based regression model
model <- rpart(housing_price ~ Rent + area_property + electricity + electricity_ESCOM + toilet_facility + cooking_fuel + MTLtelephone + source_drinkingWater + disposal_facility + use_bednet, data = train_data)

# Make predictions on the test data
predicted <- predict(model, newdata = test_data)
rmse <- sqrt(mean((predicted - test_data$housing_price)^2))

# Calculate the residuals
residuals <- test_data$housing_price - predicted

rmse



# Create a residual plot
ggplot(data = data.frame(predicted = predicted, residuals = residuals), aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.5,color = "blue") +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")


# Load the necessary libraries
library(caret)
library(randomForest)

# Prepare the data
X <- df[, c("area_property","electricity","Rent" ,"toilet_facility" ,"cooking_fuel" , "MTLtelephone" , "source_drinkingWater" ,"disposal_facility" ,"use_bednet")]
y <- df$housing_price

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- X[train_index, ]
train_label <- y[train_index]
test_data <- X[-train_index, ]
test_label <- y[-train_index]

# Perform Random Forest regression
model <- randomForest(x = train_data, y = train_label, ntree = 500, mtry = 2)

# Make predictions on the test data
predicted <- predict(model, test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_label - predicted)^2))

# Print the RMSE
print(paste0("RMSE: ", rmse))

# Load the necessary libraries
library(ggplot2)

# Create a data frame with predicted and residual values
pred_df <- data.frame(predicted = predicted, residual = test_label - predicted)

# Create a residual plot
ggplot(pred_df, aes(x = predicted, y = residual)) +
  geom_point(color = "blue") +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Residual plot")

# Load the necessary libraries
library(tidyverse)
library(glmnet)

# Load the data into R (replace "data.csv" with the actual filename and path of your data)

# Convert data to matrix format
x <- as.matrix(df[, c("area_property","electricity" ,"toilet_facility" ,"Rent", "cooking_fuel" , "MTLtelephone" , "source_drinkingWater" ,"disposal_facility" ,"use_bednet")]) # Replace with actual predictor names
y <- as.matrix(df$housing_price) # Replace with actual dependent variable name

# Perform Ridge regression
lambda <- 0.1 # Choose the appropriate value of lambda
ridge_model <- glmnet(x, y, alpha = 0, lambda = lambda)

# Calculate RMSE
rmse <- sqrt(mean((predicted - df$housing_price)^2))
print(paste0("RMSE: ", rmse))

library(gbm)

# Convert the species column to a factor
# df$housing_price <- as.factor(df$housing_price)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(df), nrow(df) * 0.7)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train the gradient boosting model
gbm_model <- gbm(housing_price ~ Rent + area_property + electricity + electricity_ESCOM + toilet_facility + cooking_fuel + MTLtelephone + source_drinkingWater + disposal_facility + use_bednet, data = train_data, n.trees = 100, interaction.depth = 2,distribution = "gaussian")

# Make predictions on the test data
predicted <- predict(gbm_model, newdata = test_data, n.trees = 100, type = "response")

#Evaluate the model using accuracy
# accuracy <- sum(predictions == test_data$housing_price) / nrow(test_data)
# print(paste0("Accuracy: ", accuracy))


predicted <- predict(model, newdata = test_data)
rmse <- sqrt(mean((predicted - test_data$housing_price)^2))

# Calculate the residuals
residuals <- test_data$housing_price - predicted

# Create a residual plot
ggplot(data = data.frame(predicted = predicted, residuals = residuals), aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.5,color = "blue") +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals")

rmse

# Load ggplot2 package
library(ggplot2)


x <- c(1, 2, 3, 4, 5)
y <- c(1558824, 1838892, 1504464, 1439510, 1253958)
data <- data.frame(x, y)

# Create plot
ggplot(data, aes(x, y)) +
  geom_line() +
  labs(title = "Error Comparison in various Algorithms", x = "Algo No.", y = "RMSE value")


