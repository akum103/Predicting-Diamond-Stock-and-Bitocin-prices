setwd("C:\\Users\\ankit\\OneDrive\\Desktop\\wORK fiLES\\NCI Files\\Semester I\\DMML")

################################################################################
# IMPORTING LIBRARIES
################################################################################

# Importing the 'dplyr' library for data manipulation
library(dplyr)

# Importing the 'randomForest' library for building random forest models
library(randomForest)

# Importing the 'psych' library for statistical functions and data analysis
library(psych)

# Importing the 'caret' library for machine learning modeling and evaluation
library(caret)

# Importing the 'rpart' library for recursive partitioning (decision trees)
library(rpart)

# Importing the 'rpart.plot' library for plotting rpart models
library(rpart.plot)

# Importing the 'partykit' library for visualizing tree-structured models
library(partykit)

# Importing the 'plotly' library for interactive plots
library(plotly)

# Importing the 'ggplot2' library for creating visually appealing plots
library(ggplot2)

# Importing the 'GGally' library for extension to 'ggplot2' for matrix plots
library(GGally)

# Importing the 'corrplot' library for plotting correlation matrices
library(corrplot)


bitcoin_data <- read.csv("bitcoin.csv")
summary(bitcoin_data)

describe(bitcoin_data)
 

################################################################################
# DATA CLEANING
################################################################################

# Converting time stamp to Date Time format
bitcoin_data$timestamp <- as.POSIXct(bitcoin_data$timestamp, format="%Y-%m-%d %H:%M:%S")


# removing data between 2017 to 2022. Data is huge so need to remove them
bitcoin_data <- subset(bitcoin_data, timestamp >= as.POSIXct("2023-01-01"))

missing_value <- colSums(is.na(bitcoin_data))
print(missing_value)


# Check rows with volume = 0
zero_volume_rows <- bitcoin_data[bitcoin_data$volume == 0, ]
print(zero_volume_rows)


# add new features - Day of week, hours of day and month
bitcoin_data$day_of_week <- weekdays(bitcoin_data$timestamp)
bitcoin_data$hour_of_day <- format(bitcoin_data$timestamp, "%H")
bitcoin_data$month <- months(bitcoin_data$timestamp)


# Convert day_of_week, hour_of_day, and month to factors

# Day of week
bitcoin_data$day_of_week <- as.numeric(factor(bitcoin_data$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))


# Month
table(bitcoin_data$month)
custom_month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
bitcoin_data$month <- as.numeric(factor(bitcoin_data$month, levels = custom_month_order))

# Hour Of Day
bitcoin_data$hour_of_day <- as.numeric(bitcoin_data$hour_of_day)


# Check the summary again
summary(bitcoin_data)



################################################################################
# CREATING A NEW DEPENDENT VARIABLE
################################################################################

# Create a binary variable 'trading_signal' (1 for Buy, 0 for Sell)
bitcoin_data$trading_signal <- c(0, ifelse(diff(bitcoin_data$close, lag = 1) > 0, 1, 0))


# Remove the last row to match the length of 'trading_signal'
bitcoin_data <- bitcoin_data[-nrow(bitcoin_data), ]
table(bitcoin_data$trading_signal)


################################################################################
# VISUALIZATION
################################################################################
summary (bitcoin_data)

plot(bitcoin_data$timestamp, bitcoin_data$close, type = "l", xlab = "Timestamp", ylab = "Close Price", main = "Bitcoin Close Price Over Time")


# Create a histogram of close prices
ggplot(bitcoin_data, aes(x = close)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Close Prices", x = "Close Price", y = "Frequency")



# Create a boxplot of close prices by day of the week
ggplot(bitcoin_data, aes(x = day_of_week, y = close, fill = factor(day_of_week))) +
  geom_boxplot() +
  labs(title = "Boxplot of Close Prices by Day of the Week",
       x = "Day of the Week", y = "Close Price", fill = "day")


# Create a boxplot of close prices by hour of the day
ggplot(bitcoin_data, aes(x = hour_of_day, y = close, fill = factor(hour_of_day))) +
  geom_boxplot() +
  labs(title = "Boxplot of Close Prices by Hour of the Day",
       x = "Hour of the Day", y = "Close Price", fill = "Hour")


################################################################################
# CORRELATION MATRIX
################################################################################
df_bitcoin <- bitcoin_data

# Numeric variables
numeric_features <- c("open", "high", "low", "close", "volume", "number_of_trades",
                      "day_of_week", "hour_of_day", "month", "trading_signal", 
                      "quote_asset_volume", "taker_buy_base_asset_volume", "taker_buy_quote_asset_volume" )

# Convert relevant columns to numeric
df_bitcoin[, numeric_features] <- sapply(df_bitcoin[, numeric_features], as.numeric)


# Calculate correlation matrix for numeric variables
cor_matrix_numeric <- cor(df_bitcoin[, numeric_features])


# Display the correlation matrix with corrplot
corrplot(cor_matrix_numeric, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)

# Print the correlation coefficients
print(cor_matrix_numeric)



################################################################################
# REMOVING HIGHLIY CORRELATED VARIABLES
################################################################################

# From the correlation plot, we can see that High, Low, Close, open are highly correlated. As I have used closed to create
# trading signal, I will remove Close, High and Low and keep Open. Similarly, Take Buy base asset volume and taker
# buy quote asset colue are highly correlated. I will keep Base Asset Volume. 


# Remove the columns that we dont need
remove <- c("taker_buy_quote_asset_volume","close","high", "low")
df_bitcoin_clean <- bitcoin_data[, !(names(bitcoin_data) %in% remove)]

summary(df_bitcoin_clean)




################################################################################
# NORMALIZING VARIABLES
################################################################################

# Function to perform Min-Max scaling
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


# Columns that I want to normalize


numeric_columns <-  c("open", "volume", "quote_asset_volume", "number_of_trades","taker_buy_base_asset_volume")


# Apply the normalization
df_bitcoin_clean[numeric_columns] <- lapply(df_bitcoin_clean[numeric_columns], min_max_scaling)
summary(df_bitcoin_clean)

################################################################################
# DECISION TREE - CLASSIFICATION
################################################################################

dt_model_1 <- df_bitcoin_clean


features <-  c("open", "volume", "quote_asset_volume", "number_of_trades", "taker_buy_base_asset_volume",
               "day_of_week", "hour_of_day", "month")


# Remove the last row to match the length of 'trading_signal'
dt_model_1 <- dt_model_1[-nrow(dt_model_1), ]
table(dt_model_1$trading_signal)

set.seed(572)  # for reproducibility
train_size <- 0.8
train_index <- seq(1, floor(train_size * nrow(dt_model_1)))
train_data_1 <- dt_model_1[train_index, ]
test_data_1 <- dt_model_1[-train_index, ]


# check the dimensions of the test and train data
dim(train_data_1)
dim(test_data_1)

train_data_1$trading_signal <- factor(train_data_1$trading_signal, levels = c(0, 1))


# Decision tree Model

# Convert 'trading_signal' to binary factor
train_data_1$trading_signal <- factor(train_data_1$trading_signal, levels = c(0, 1))
test_data_1$trading_signal <- factor(test_data_1$trading_signal, levels = c(0, 1))


# Define control parameters for train function
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train decision tree using train function from caret
tree_model_1 <- train(
  x = train_data_1[, features],
  y = train_data_1$trading_signal,
  method = "rpart",
  trControl = ctrl
)

# Predict on test data
test_data_1$predicted <- predict(tree_model_1, newdata = test_data_1)

# Print confusion matrix
confusion_matrix <- table(test_data_1$trading_signal, test_data_1$predicted)
print(confusion_matrix)

# Extract the rpart model from the caret object
final_rpart_model <- tree_model_1$finalModel

# Plot the decision tree
prp(final_rpart_model, extra = 1, main = "Decision Tree for trading_signal")


# Extract variable importance scores
var_importance <- varImp(tree_model_1, scale = FALSE)

# Print variable importance scores
print(var_importance)


# The node starts as quote asset column and if the quote asset volume is less than .0013,
#it follows yes, else no. Yes here is buy and no here is sell. Based on this, the tree branches off.
#Combining the decision tree with the important variables, we can see that open, day of the week and 
#hour of the day are not as important parameters in deciding whether to buy or sell, rather trading 
#volume and surprisingly month are very important variables.


################################################################################
# RANDOM FOREST - CLASSIFICATION
################################################################################

rf_df_bitcoin <- df_bitcoin_clean


# Specify features (customize based on your data set)
new_features <- c("open", "volume", "quote_asset_volume", "number_of_trades", "taker_buy_base_asset_volume", "month")

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_size <- 0.8
train_index <- seq(1, floor(train_size * nrow(rf_df_bitcoin)))
train_data <- rf_df_bitcoin[train_index, ]
test_data <- rf_df_bitcoin[-train_index, ]


# Different trees to find optimal
ntree_values <- c(50,100,150,200)


# finding appropriate tree size
for(ntree_value in ntree_values) {
  rf_model <- randomForest(
    formula = as.factor(trading_signal) ~ ., 
    data = train_data[, c(new_features, "trading_signal")],
    ntree = 200,
    importance = TRUE
)
  
  # print OOB
  cat("Number of Trees:", ntree_value, "  OOB Error:", rf_model$err.rate[nrow(rf_model$err.rate), "OOB"], "\n")
  
}


# Final Training of the data
ntree_optimal <- 100

rf_model_final <- randomForest(
  formula = as.factor(trading_signal) ~ ., 
  data = train_data[, c(new_features, "trading_signal")],
  ntree = ntree_optimal,
  importance = TRUE
)


rf_model_final


varImpPlot(rf_model_final)

# Specify features for prediction (use the same features used in training)
test_features <- test_data[, new_features]

summary(test_features)

# Make predictions on the test set
predictions <- predict(rf_model_final, newdata = test_features)

# Print the confusion matrix
confusion_matrix_test <- table(test_data$trading_signal, predictions)
print(confusion_matrix_test)



# Function to calculate precision, recall, and F1 score
calculate_metrics <- function(confusion_matrix) {
  TN <- confusion_matrix[1, 1]
  FP <- confusion_matrix[1, 2]
  FN <- confusion_matrix[2, 1]
  TP <- confusion_matrix[2, 2]
  
  # Accuracy
  accuracy <- (TP + TN) / sum(confusion_matrix)
  
  # Precision
  precision <- TP / (TP + FP)
  
  # Recall
  recall <- TP / (TP + FN)
  
  # F1 Score
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(list(accuracy = accuracy, precision = precision, recall = recall, f1_score = f1_score))
}

# Calculate metrics for the test set
metrics <- calculate_metrics(confusion_matrix_test)
print(metrics)




