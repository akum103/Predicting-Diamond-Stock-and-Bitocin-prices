setwd("C:\\Users\\ankit\\OneDrive\\Desktop\\wORK fiLES\\NCI Files\\Semester I\\DMML")


################################################################################
# LIBRARIES
################################################################################
# Importing the 'zoo' library for time series data manipulation
library(zoo)

# Importing the 'ggplot2' library for creating visually appealing plots
library(ggplot2)

# Importing the 'caret' library for machine learning modeling and evaluation
library(caret)

# Importing the 'dplyr' library for data manipulation
library(dplyr)

# Importing the 'data.table' library for efficient data manipulation
library(data.table)

# Importing the 'corrplot' library for plotting correlation matrices
library(corrplot)

# Importing the 'car' library for statistical tools
library(car)

# Importing the 'forecast' library for time series forecasting
library(forecast)

# Importing the 'Metrics' library for model evaluation metrics
library(Metrics)

# Importing the 'psych' library for statistical functions and data analysis
library(psych)


################################################################################
# IMPORTING DATA
################################################################################
apple_stock <- read.csv("AAPL.csv")
summary(apple_stock)

describe(apple_stock)

################################################################################
# DATA PROCESSING
################################################################################

#Check values in categorical columns
table(apple_stock$Brand.Name)
table(apple_stock$Ticker)
table(apple_stock$Country)

# Removing brand name, country and ticker
apple_stock <- subset(apple_stock, select = -c(Brand.Name, Ticker, Country))


any_missing <- apply(apple_stock, 2, function(x) any(is.na(x)))

# Print columns with missing values
print(names(apple_stock)[any_missing])
str(apple_stock)

# Formatting Date
apple_stock$Date <- as.Date(apple_stock$Date, format="%d/%m/%Y")
summary(apple_stock)

################################################################################
# FEATURE ENGINEERING
################################################################################

apple_stock$Year <- format(apple_stock$Date, "%Y")
apple_stock$Month <- format(apple_stock$Date, "%m")
apple_stock$Day <- format(apple_stock$Date, "%d")
summary(apple_stock)


# Convert Month, Day of month and Year to factors

# Month
table(apple_stock$Month)
custom_month_order <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
apple_stock$Month <- factor(apple_stock$Month, levels = custom_month_order)


# Day of the month
table(apple_stock$Day)
apple_stock$Day <- factor(apple_stock$Day)

# Year
table(apple_stock$Year)
# 1980 has 13 values. Removing them
apple_stock <- apple_stock[year(apple_stock$Date) != 1980, ]

# As factor
apple_stock$Year_factor <- factor(apple_stock$Year)
summary(apple_stock$Year_factor)

# As Numeric
apple_stock$Year_numeric <- as.numeric(apple_stock$Year)


################################################################################
# VIZ
################################################################################

# Trend Analysis
ggplot(apple_stock, aes(x=Date, y=Adj.Close)) +
  geom_line(color="blue") +
  ggtitle("Apple Stock Price") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()


# Create a scatter plot
plot(scale(apple_stock$Volume), apple_stock$Close, col = "darkgreen", pch = 16, xlab = "Volume", ylab = "Closing Price", main = "Scatter Plot: Closing Price vs. Volume", las = 2)



str(select(apple_stock, -c(Date, Year_factor, Year)))

corr_apple_stock <- apple_stock

corr_apple_stock$Month <- as.numeric(corr_apple_stock$Month)
corr_apple_stock$Day <- as.numeric(corr_apple_stock$Day)

correlation_matrix <- cor(select(corr_apple_stock, -c(Date,Year_factor, Year)))
corrplot(correlation_matrix, method = "circle", type = "upper", col = colorRampPalette(c("blue", "white", "red"))(50), main = "Correlation Matrix")
print(correlation_matrix)



################################################################################
# NORMALIZATION
################################################################################

# variables on which I will do normalization
numeric_columns <- c("Open", "High", "Low", "Close", "Day.Difference", "Adj.Close", "Volume")

# Min Max scaling
apple_stock[, numeric_columns] <- scale(apple_stock[, numeric_columns], center = FALSE, scale = apply(apple_stock[, numeric_columns], 2, function(x) diff(range(x))))


summary(apple_stock)


################################################################################
# Simple Moving average time Series Model - Predicting Close Price
################################################################################

# Split the data into training (Up to 2022) and testing (2023) sets
train_data_ma <- subset(apple_stock, Date >= "1981-01-02" & Date <= "2022-12-31")
test_data_ma <- subset(apple_stock, Date >= "2023-01-01")

#Summary of Test and Train
summary(train_data_ma)
summary(test_data_ma)

# Fit a simple moving average model
simple_ma_model <- ma(train_data_ma$Close, order = 3)

# Print the summary of the model
summary(simple_ma_model)

# Plot the fitted model on the training data
plot(train_data_ma$Date, train_data_ma$Close, type = 'l', 
     xlab = 'Date', ylab = 'Close Price', main = 'Simple Moving Average Model on Training Data')

# Calculate the simple moving average manually
fitted_values <- ma(train_data_ma$Close, order = 3)

# Lines for fitted values
lines(train_data_ma$Date, fitted_values, col = 'red', lty = 2)

# Legend
legend("topright", legend = c("Actual", "Fitted"), col = c("black", "red"), lty = 1:2)



# Forecast using the simple moving average model on the testing data
forecast_values <- ma(test_data_ma$Close, order = 3)

summary(forecast_values)

# Plot the actual values and forecasted values
plot(test_data_ma$Date, test_data_ma$Close, type = 'l', 
     xlab = 'Date', ylab = 'Close Price', main = 'Forecasting with Simple Moving Average Model on Testing Data')
lines(test_data_ma$Date, forecast_values, col = 'red', lty = 2)

# Legend
legend("topright", legend = c("Actual", "Forecasted"), col = c("black", "red"), lty = 1:2)


# Remove missing values
test_data_cleaned <- na.omit(test_data_ma$Close)
forecast_values_cleaned <- na.omit(forecast_values)

# Trim both vectors to the minimum length
min_length <- min(length(test_data_cleaned), length(forecast_values_cleaned))
test_data_cleaned <- head(test_data_cleaned, min_length)
forecast_values_cleaned <- head(forecast_values_cleaned, min_length)

# Calculate evaluation metrics using cleaned data
mae_value <- mae(test_data_cleaned, forecast_values_cleaned)
mse_value <- mse(test_data_cleaned, forecast_values_cleaned)
rmse_value <- rmse(test_data_cleaned, forecast_values_cleaned)

# Print the evaluation metrics
cat("Mean Absolute Error (MAE):", mae_value, "\n")
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")


################################################################################
# REGRESSION MODEL
################################################################################

# Linear Regression
###################

# Set a seed for reproducibility
set.seed(582)

# Create an index for splitting the data
index <- sample(1:nrow(apple_stock), 0.8 * nrow(apple_stock))

# Create training and testing sets
train_data <- apple_stock[index, ]
test_data <- apple_stock[-index, ]

# Linear Regression with Factor 'Year' on training data
model_factor <- lm(Close ~ + Volume + as.factor(Year_factor) + Month + Day, data = train_data)

# Linear Regression with Numeric 'Year' on training data
model_numeric <- lm(Close ~  Volume + Year_numeric + Month + Day, data = train_data)

# Assess the models on the test data
predictions_factor <- predict(model_factor, newdata = test_data)
predictions_numeric <- predict(model_numeric, newdata = test_data)


# Print summaries or relevant information
summary(model_factor)

summary(model_numeric)


# to check variance inflation pattern
data.frame("VIF1"=vif(model_factor))



# Calculate R-squared,MSE,RMSE and MAE for the test data1
rsquared_test1 <- 1 - sum((test_data$Close - predictions_factor)^2) / sum((test_data$Close - mean(test_data$Close))^2)
mse1 <- mean((test_data$Close - predictions_factor)^2)
rmse1 <- sqrt(mse1)
mae1 <- mean(abs(test_data$Close - predictions_factor))

# Print all the values
cat("R-squared for Test Data1:", rsquared_test1, "\n")
cat("Mean Squared Error1:", mse1, "\n")
cat("Root Mean Squared Error1:", rmse1, "\n")
cat("Mean Absolute Error1 (MAE1):", mae1, "\n")



# KNN REGRESSION
################

# Create a data frame with the predictor variables and the target variable
knn_data <- apple_stock[, c("Open", "High", "Low", "Volume", "Year_factor", "Month", "Day", "Close")]


# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(knn_data$Close, p = 0.8, list = FALSE)
train_data <- knn_data[train_index, ]
test_data <- knn_data[-train_index, ]

# Train KNN regression model
knn_model <- train(
  Close ~ .,
  data = train_data,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5)
)

# Make predictions on the test set
predictions_knn <- predict(knn_model, newdata = test_data)

# Evaluate the KNN model
rsquared_knn <- cor(predictions_knn, test_data$Close)^2
mse_knn <- mean((predictions_knn - test_data$Close)^2)
rmse_knn <- sqrt(mse_knn)
mae_knn <- mean(abs(predictions_knn - test_data$Close))

# Print the evaluation metrics
cat("R-squared for KNN Regression:", rsquared_knn, "\n")
cat("Mean Squared Error (MSE) for KNN Regression:", mse_knn, "\n")
cat("Root Mean Squared Error (RMSE) for KNN Regression:", rmse_knn, "\n")
cat("Mean Absolute Error (MAE) for KNN Regression:", mae_knn, "\n")



# Create a scatter plot of actual vs. predicted values
plot(test_data$Close, predictions_knn, 
     xlab = "Actual Close Price", ylab = "Predicted Close Price",
     main = "KNN Regression: Actual vs. Predicted Values",
     col = "blue", pch = 16)

# Add a 45-degree reference line for comparison
abline(0, 1, col = "red")

# Add a legend
legend("topright", legend = c("Predictions", "45-degree line"), 
       col = c("blue", "red"), pch = c(16, NA))
