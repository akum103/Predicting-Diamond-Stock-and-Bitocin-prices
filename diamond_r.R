################################################################################
# 1 - LOADING LIBRARIES
################################################################################

# Importing the 'tidyverse' library for comprehensive data manipulation and analysis.
library(tidyverse)

# Importing the 'hrbrthemes' library to enhance ggplot2 visualizations with custom themes.
library(hrbrthemes)

# Importing the 'ggplot2' library for creating sophisticated and customizable plots.
library(ggplot2)

# Importing the 'plotly' library for generating interactive and dynamic plots.
library(plotly)

# Importing the 'corrplot' library for visualizing correlation matrices.
library(corrplot)

# Importing the 'caret' library for machine learning model training and evaluation.
library(caret)

# Importing the 'class' library for implementing various classification algorithms.
library(class)

# Importing the 'cowplot' library for creating complex ggplot2-based layouts.
library(cowplot)

# Importing the 'patchwork' library to seamlessly combine multiple ggplot2 plots.
library(patchwork)

# Importing the 'pROC' library for evaluating and visualizing ROC curves for model performance.
library(pROC)

# Importing the 'psych' library for conducting various statistical analyses and psychometrics.
library(psych)


################################################################################
# LOADING DATA
################################################################################

diamond_data <- read.csv("diamonds.csv")
summary(diamond_data)

describe(diamond_data)

################################################################################
#2 -  DATA CLEANING
################################################################################

diamond_data$X <- NULL
summary(diamond_data)

# After looking at the initial summary, we found out that there were 0s in X, Y and Z. Let's see them one by one

# Zeroes in x = 8
zeroes_in_x <- sum(diamond_data$x == 0)


# Zeroes in y = 7
zeroes_in_y <- sum(diamond_data$y == 0)


#Zeroes in z = 20
zeroes_in_z <- sum(diamond_data$z == 0)



# Subset non zero in X rather than deleting them

df_diamond_data <- subset(diamond_data, diamond_data$x != 0)

# Subset non zero in Z rather than deleting them
df_diamond_data <- subset(diamond_data, diamond_data$z != 0)
summary(df_diamond_data)

################################################################################
#3 - CATEGORICAL VALUES INSIGHTS
################################################################################

# Total count of categories in "Cut" variable
df_diamond_data_cut <- table(df_diamond_data$cut)
print(df_diamond_data_cut)


# Total count of categories in "color" variable
df_diamond_data_color <- table(df_diamond_data$color)
print(df_diamond_data_color)

# Total count of categories in "clarity" variable
df_diamond_data_clarity <- table(df_diamond_data$clarity)
print(df_diamond_data_clarity)

################################################################################
#4 CONVERTING CATEGORICAL TO NUMERICAL
################################################################################

# Converting Color grade to numeric
df_diamond_data$color <- as.numeric(factor(df_diamond_data$color, levels = c("D","E","F","G","H","I","J")))

#Converting "Cut" to factors
df_diamond_data$cut <- as.numeric(factor(df_diamond_data$cut, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal" )))

#Converting "Clarity" to factors

# Manually assign numeric values to clarity levels based on your criteria
df_diamond_data$clarity <- as.numeric(factor(df_diamond_data$clarity, levels = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1","IF")))



summary(df_diamond_data)



################################################################################
# CREATING THE DEPENDENT VARIABLE
################################################################################

# For this case, we I have created a new variable - Affordability. for me, anything upto 5000 is affordable and anything more than
# that is not affordable

df_diamond_data$affordability <- ifelse(df_diamond_data$price < 5000, 1, 0)
table(df_diamond_data$affordability)
summary(df_diamond_data)

#Factoring it
df_diamond_data$affordability <- as.factor(df_diamond_data$affordability)


################################################################################
#5 VISUALIZATIONS
################################################################################

# Box Plot for Cut

cut_plot <- df_diamond_data %>%
  ggplot(aes(x = "cut", y = cut, fill = "cut")) +
  geom_boxplot(fill = "darkblue") + 
  ggtitle("Box plot for Diamond cut")+
  xlab("Cut")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(cut_plot)
plotly_plot
  
# Box Plot for Carat

carat_plot <- df_diamond_data %>%
  ggplot(aes(x = "carat", y = carat, fill = "carat")) +
  geom_boxplot(fill = "green") + 
  ggtitle("Box plot for Diamond carat")+
  xlab("Carat")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(carat_plot)
plotly_plot

# Box Plot for color

color_plot <- df_diamond_data %>%
  ggplot(aes(x = "color", y = color, fill = "color")) +
  geom_boxplot(fill = "orange") + 
  ggtitle("Box plot for Diamond color")+
  xlab("color")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(color_plot)
plotly_plot

# Box Plot of Clarity 

clarity_plot <- df_diamond_data %>%
  ggplot(aes(x = "clarity", y = clarity, fill = "clarity")) +
  geom_boxplot(fill = "maroon") + 
  ggtitle("Box plot for Diamond clarity")+
  xlab("clarity")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(clarity_plot)
plotly_plot

# Box Plot of depth 

depth_plot <- df_diamond_data %>%
  ggplot(aes(x = "depth", y = depth, fill = "depth")) +
  geom_boxplot(fill = "cyan") + 
  ggtitle("Box plot for Diamond depth")+
  xlab("Depth")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(depth_plot)
plotly_plot

# Box Plot of price 

price_plot <- df_diamond_data %>%
  ggplot(aes(x = "price", y = price, fill = "price")) +
  geom_boxplot(fill = "gray") + 
  ggtitle("Box plot for Diamond price")+
  xlab("Price")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(price_plot)
plotly_plot


length_plot <- df_diamond_data %>%
  ggplot(aes(x = "x", y = x, fill = "x")) +
  geom_boxplot(fill = "blue") + 
  ggtitle("Box plot for Diamond length")+
  xlab("length")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(length_plot)
plotly_plot


width_plot <- df_diamond_data %>%
  ggplot(aes(x = "y", y = y, fill = "y")) +
  geom_boxplot(fill = "pink") + 
  ggtitle("Box plot for Diamond width")+
  xlab("Width")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(width_plot)
plotly_plot


height_plot <- df_diamond_data %>%
  ggplot(aes(x = "z", y = z, fill = "z")) +
  geom_boxplot(fill = "lavender") + 
  ggtitle("Box plot for Diamond Height")+
  xlab("Height")+
  ylab("Frequency")+
  theme_ipsum() +
  theme_minimal() +
  theme(legend.position = "none")

plotly_plot <- plotly::ggplotly(height_plot)
plotly_plot



# Plot in Grid

plot_grid(
  cut_plot, carat_plot,
  color_plot, clarity_plot,
  depth_plot, price_plot,
  ncol = 2
)



# Barplot of categorical variables - cut, color, clarity, affordability


par(mfrow = c(2,2))

# Bar Plot for Cut
barplot(table(df_diamond_data$cut), col = "darkblue", main = "Bar Plot for Diamond Cut", xlab = "Cut")

# Bar Plot for Clarity
barplot(table(df_diamond_data$clarity), col = "maroon", main = "Bar Plot for Diamond Clarity", xlab = "Clarity")

# Bar Plot for Color
barplot(table(df_diamond_data$color), col = "orange", main = "Bar Plot for Diamond Color", xlab = "Color")

# Bar Plot for Affordability
barplot(table(df_diamond_data$affordability), col = "green", main = "Bar Plot for Affordability", xlab = "Affordability")


summary(df_diamond_data)

# Violin plot of price and other independent variables - carat, cut, color, clarity, depth, table, x,y,z with affordability



# Carat VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = carat, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Carat") +
  xlab("Affordability") +
  ylab("Diamond Carat") +
  theme_ipsum()

#  Cut VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = cut, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Cut") +
  xlab("Affordability") +
  ylab("Diamond Cut") +
  theme_ipsum()

#  Color VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = color, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Color") +
  xlab("Affordability") +
  ylab("Diamond Color") +
  theme_ipsum()

#  Clarity VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = clarity, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Clarity") +
  xlab("Affordability") +
  ylab("Diamond Clarity") +
  theme_ipsum()

#  Depth VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = depth, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Depth") +
  xlab("Affordability") +
  ylab("Diamond Depth") +
  theme_ipsum()

#  Table VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = table, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Table") +
  xlab("Affordability") +
  ylab("Diamond Table") +
  theme_ipsum()

#  Length (x) VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = x, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Length") +
  xlab("Affordability") +
  ylab("Diamond Length") +
  theme_ipsum()


#  width (y) VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = df_diamond_data$y, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Width") +
  xlab("Affordability") +
  ylab("Diamond Width") +
  theme_ipsum()

#  Height (z) VS affordability

df_diamond_data %>%
  ggplot(aes(x = affordability, y = df_diamond_data$z, fill = affordability)) +
  geom_violin() +
  ggtitle("Violin plot of Price by Affordability VS Height") +
  xlab("Affordability") +
  ylab("Diamond Height") +
  theme_ipsum()



################################################################################
# NORMALIZATION OF THE DATA
################################################################################


df_diamond_data$table <- df_diamond_data$table/100
summary(df_diamond_data)


# Function to perform Min-Max scaling
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


# Columns that I want to normalize

numeric_columns <- c("carat", "depth", "x", "y", "z")


# Apply Z-score standardization to numeric columns excluding "table"
df_diamond_data[numeric_columns] <- lapply(df_diamond_data[numeric_columns], min_max_scaling)
summary(df_diamond_data)



# Visualize normalized data and other variables as well

par(mfrow=c(2, 2))

# Plot histogram for "carat"
hist(df_diamond_data$carat, main="Histogram for Carat", xlab="Carat", col = "skyblue")

# Plot histogram for "cut"
hist(df_diamond_data$cut, main="Histogram for Cut", xlab="Cut", col = "darkgreen")

# Plot histogram for "color"
hist(df_diamond_data$color, main="Histogram for Color", xlab="Color", col = "orange")

# Plot histogram for "clarity"
hist(df_diamond_data$clarity, main="Histogram for Clarity", xlab="Clarity", col = "maroon")

# Plot histogram for "depth"
hist(df_diamond_data$depth, main="Histogram for Depth", xlab="Depth", col = "cyan")

# Plot histogram for "Table"
hist(df_diamond_data$table, main="Histogram for Table", xlab="Table", col = "purple")

# Plot histogram for "Price"
hist(df_diamond_data$price, main="Histogram for Price", xlab="Price", col = "gray")

# Plot histogram for "x"
hist(df_diamond_data$x, main="Histogram for Length of Diamond (x)", xlab="Length of Diamond", col = "darkblue")

# Plot histogram for "y"
hist(df_diamond_data$y, main="Histogram for Width of Diamond (y)", xlab="Width of Diamond", col = "darkred")

# Plot histogram for "z"
hist(df_diamond_data$z, main="Histogram for Height of Diamond (z)", xlab="Height of Diamond", col = "darksalmon")



# Correlation plot


numeric_features <- c("carat", "cut", "color", "clarity", "depth", "table", "x", "y", "z", "affordability")

# Convert relevant columns to numeric
df_diamond_data[, numeric_features] <- sapply(df_diamond_data[, numeric_features], as.numeric)



# Calculate correlation matrix
cor_matrix <- cor(df_diamond_data[, numeric_features])

# Display the correlation coefficient table
corrplot(cor_matrix, type = "lower", method = "number", order = "hclust")

rounded_cor_matrix <- round(cor_matrix, 2)

# Print the rounded correlation matrix
print(rounded_cor_matrix)



# After looking at the model, x,y,z and carat are highly correlated. we will create different models to choose
# appropriate features. Let's dive into it one by one



################################################################################
#LOGISTIC REGRESSION
################################################################################

# Model 1 (Remove Carat and keep x,y,z)
#######################################

df_diamond_data$affordability <- ifelse(df_diamond_data$price < 5000, 1, 0)
table(df_diamond_data$affordability)
summary(df_diamond_data)


df_lr_model1 <- df_diamond_data

df_lr_model1 <- subset(df_lr_model1, select = -c(carat, price))



#Factoring it
df_diamond_data$affordability <- as.factor(df_diamond_data$affordability)

# check if carat was removed
summary(df_lr_model1)

# Ensure reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
index <- createDataPartition(df_lr_model1$affordability, p = 0.7, list = FALSE)
train_data_1 <- df_lr_model1[index, ]
test_data_1 <- df_lr_model1[-index, ]



# Check the dimensions of the training and testing sets
dim(train_data_1)
dim(test_data_1)



# Fit logistic regression model
logistic_model_1 <- glm(affordability ~ cut + color + clarity + depth + table + x, 
                      data = train_data_1, family = "binomial", maxit = 100 )

summary(logistic_model_1)
# Make predictions on the test set
predictions <- predict(logistic_model_1, newdata = test_data_1, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

test_data_1$affordability <- factor(test_data_1$affordability, levels = c("0", "1"))

predicted_classes <- factor(predicted_classes, levels = levels(test_data_1$affordability))

# Evaluate the model
confusion_matrix <- confusionMatrix(predicted_classes, test_data_1$affordability)
confusion_matrix

#residual plot comparing all variables

# Create a data frame with actual and predicted values
residual_data <- data.frame(
  Actual = as.numeric(as.character(test_data_1$affordability)),
  Predicted = predictions
)


test_data_1$affordability <- as.numeric(as.character(test_data_1$affordability))

# Calculate residuals
residual_data$Residuals <- residual_data$Actual - residual_data$Predicted

# Plot residuals against predicted values
plot(residual_data$Predicted, residual_data$Residuals,
     main = "Residual Plot Model 1",
     xlab = "Predicted Values",
     ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)


# Model 2 (Remove carat and X; keep y and z)
############################################

df_lr_model2 <- df_diamond_data

df_lr_model2 <- subset(df_lr_model2, select = -c(carat, x, price))

# check if carat and x were removed
summary(df_lr_model2)

# Ensure reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
index <- createDataPartition(df_lr_model2$affordability, p = 0.7, list = FALSE)
train_data_2 <- df_lr_model2[index, ]
test_data_2 <- df_lr_model2[-index, ]



# Check the dimensions of the training and testing sets
dim(train_data_2)
dim(test_data_2)



# Fit logistic regression model
logistic_model_2 <- glm(affordability ~ cut + color + clarity + depth + table + y + z, 
                      data = train_data_2, family = "binomial")

summary(logistic_model_2)
# Make predictions on the test set
predictions_2 <- predict(logistic_model_2, newdata = test_data_2, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_classes_2 <- ifelse(predictions_2 > 0.5, 1, 0)

test_data_2$affordability <- factor(test_data_2$affordability, levels = c("0", "1"))

predicted_classes_2 <- factor(predicted_classes_2, levels = levels(test_data_2$affordability))

# Evaluate the model
confusion_matrix_2 <- confusionMatrix(predicted_classes_2, test_data_2$affordability)
confusion_matrix_2

#residual plot comparing all variables

# Create a data frame with actual and predicted values
residual_data_2 <- data.frame(
  Actual_2 = as.numeric(as.character(test_data_2$affordability)),
  Predicted_2 = predictions_2
)


test_data_2$affordability <- as.numeric(as.character(test_data_2$affordability))

# Calculate residuals
residual_data_2$Residuals <- residual_data_2$Actual - residual_data_2$Predicted

# Plot residuals against predicted values
plot(residual_data_2$Predicted, residual_data_2$Residuals,
     main = "Residual Plot Model 2",
     xlab = "Predicted Values",
     ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)




# Model 3 (Remove carat, x and y; keep z)
#########################################

df_lr_model3 <- df_diamond_data

df_lr_model3 <- subset(df_lr_model3, select = -c(carat, x, price, y))

# check if carat and x were removed
summary(df_lr_model3)

# Ensure reproducibility
set.seed(967)

# Split the data into training (70%) and testing (30%) sets
index <- createDataPartition(df_lr_model3$affordability, p = 0.7, list = FALSE)
train_data_3 <- df_lr_model3[index, ]
test_data_3 <- df_lr_model3[-index, ]



# Check the dimensions of the training and testing sets
dim(train_data_3)
dim(test_data_3)



# Fit logistic regression model
logistic_model_3 <- glm(affordability ~ cut + color + clarity + depth + table + z, 
                        data = train_data_3, family = "binomial")

summary(logistic_model_3)
# Make predictions on the test set
predictions_3 <- predict(logistic_model_3, newdata = test_data_3, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_classes_3 <- ifelse(predictions_3 > 0.5, 1, 0)

test_data_3$affordability <- factor(test_data_3$affordability, levels = c("0", "1"))

predicted_classes_3 <- factor(predicted_classes_3, levels = levels(test_data_3$affordability))

# Evaluate the model
confusion_matrix_3 <- confusionMatrix(predicted_classes_3, test_data_3$affordability)
confusion_matrix_3

#residual plot comparing all variables

# Create a data frame with actual and predicted values
residual_data_3 <- data.frame(
  Actual_3 = as.numeric(as.character(test_data_3$affordability)),
  Predicted_3 = predictions_3
)


test_data_3$affordability <- as.numeric(as.character(test_data_3$affordability))

# Calculate residuals
residual_data_3$Residuals <- residual_data_3$Actual - residual_data_3$Predicted

# Plot residuals against predicted values
plot(residual_data_3$Predicted, residual_data_3$Residuals,
     main = "Residual Plot Model 3",
     xlab = "Predicted Values",
     ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)



# Model 4 (Remove x and z; keep y and carat)
#########################################

df_lr_model4 <- df_diamond_data

df_lr_model4 <- subset(df_lr_model4, select = -c(x, price, z))

# check if carat and x were removed
summary(df_lr_model4)

# Ensure reproducibility
set.seed(426)

# Split the data into training (70%) and testing (30%) sets
index <- createDataPartition(df_lr_model4$affordability, p = 0.7, list = FALSE)
train_data_4 <- df_lr_model4[index, ]
test_data_4 <- df_lr_model4[-index, ]



# Check the dimensions of the training and testing sets
dim(train_data_4)
dim(test_data_4)



# Fit logistic regression model
logistic_model_4 <- glm(affordability ~ cut + color + clarity + depth + table + y, 
                        data = train_data_4, family = "binomial")

summary(logistic_model_4)
# Make predictions on the test set
predictions_4 <- predict(logistic_model_4, newdata = test_data_4, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_classes_4 <- ifelse(predictions_4 > 0.5, 1, 0)

test_data_4$affordability <- factor(test_data_4$affordability, levels = c("0", "1"))

predicted_classes_4 <- factor(predicted_classes_4, levels = levels(test_data_4$affordability))

# Evaluate the model
confusion_matrix_4 <- confusionMatrix(predicted_classes_4, test_data_4$affordability)
confusion_matrix_4

#residual plot comparing all variables

# Create a data frame with actual and predicted values
residual_data_4 <- data.frame(
  Actual_4 = as.numeric(as.character(test_data_4$affordability)),
  Predicted_4 = predictions_4
)


test_data_4$affordability <- as.numeric(as.character(test_data_4$affordability))

# Calculate residuals
residual_data_4$Residuals <- residual_data_4$Actual - residual_data_4$Predicted

# Plot residuals against predicted values
plot(residual_data_4$Predicted, residual_data_4$Residuals,
     main = "Residual Plot Model 4",
     xlab = "Predicted Values",
     ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)


################################################################################
# MODEL PERFORMANCE
################################################################################

# List to store results
results <- list()

# Loop through models
for (i in 1:4) {
  
  # Select the appropriate model and data
  model <- get(paste("logistic_model_", i, sep=""))
  test_data <- get(paste("test_data_", i, sep=""))
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = test_data, type = "response")
  
  # Convert probabilities to binary predictions (0 or 1)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  # Factorize variables
  test_data$affordability <- factor(test_data$affordability, levels = c("0", "1"))
  predicted_classes <- factor(predicted_classes, levels = levels(test_data$affordability))
  
  # Evaluate the model
  confusion_matrix <- confusionMatrix(predicted_classes, test_data$affordability)
  
  # Store results
  results[[paste("Model", i)]] <- confusion_matrix$overall
  
}

# Combine results into a data frame
result_table <- do.call(rbind, results)

# Print the results in tabular format
print(result_table)




################################################################################
#KNN CLASSIFICATION
################################################################################
df_knn <- df_diamond_data


# Split the data into training and testing sets
set.seed(694)  # Setting seed for reproducibility
indices <- sample(1:nrow(df_knn), 0.7 * nrow(df_knn))
train_data_knn <- df_knn[indices, ]
test_data_knn <- df_knn[-indices, ]


# Define the independent variables (features) and the dependent variable (target)
features <- c("cut", "color", "clarity", "depth", "table", "z")
target <- "affordability"

# Train the KNN model
knn_model <- knn(train = train_data_knn[, features], test = test_data_knn[, features], cl = train_data_knn[, target], k = 5)

# Evaluate the model
confusion_matrix <- table(knn_model, test_data_knn[, target])
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
cat("Accuracy:", accuracy, "\n")


# Plotting ROC

roc_curve <- roc(test_data_knn$affordability, (as.numeric(as.factor(knn_model)) -1))
auc(roc_curve)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
