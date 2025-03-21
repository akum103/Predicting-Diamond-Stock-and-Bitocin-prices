Predicting Bitcoin, Stock, and Diamond Prices using Machine Learning

📌 Overview

This project applies Machine Learning models to predict prices in three different domains:

Bitcoin Price Prediction using Decision Trees & Random Forest.

Apple Stock Price Prediction using Simple Moving Average & Multiple Linear Regression.

Diamond Price Affordability Prediction using Logistic Regression & KNN Classification.

By leveraging various ML techniques, this project provides insights into financial forecasting and consumer behavior analysis.

📂 Datasets

Bitcoin Price Dataset (Time-series data of Bitcoin trading volume and prices)

Apple Stock Price Dataset (Stock price trends of AAPL from Kaggle)

Diamond Price Dataset (Diamonds dataset with factors like Cut, Clarity, Color, Carat, and Price)

🏗️ Tech Stack

Programming Language: R

Libraries Used: dplyr, randomForest, psych, caret, rpart, ggplot2, corrplot, plotly

Machine Learning Models: Decision Tree, Random Forest, Moving Average, Logistic Regression, KNN

🚀 Features & Workflow

1️⃣ Bitcoin Price Prediction

Data Cleaning: Removed large timeframes, formatted timestamps, handled missing values.

Feature Engineering: Created trading_signal (1 = Buy, 0 = Sell) based on price trends.

Exploratory Data Analysis (EDA):

Time-series visualization of Bitcoin closing prices.

Boxplots for price variations based on time of day & day of the week.

Correlation matrix to identify redundant variables.

Machine Learning Models:

Decision Tree for classification

Random Forest for improved accuracy

Key Findings:

Trading volume and monthly trends impact Bitcoin trading decisions significantly.

Model performed well in predicting Buy signals but struggled with Sell signals due to class imbalance.

2️⃣ Apple Stock Price Prediction

Data Cleaning & Feature Engineering:

Extracted year, month, day features from timestamps.

Normalized numerical values for better ML performance.

Machine Learning Models:

Simple Moving Average (SMA) for short-term trend prediction.

Multiple Linear Regression (MLR) using volume, month, and year factors.

Key Findings:

SMA successfully captured stock price trends with low error margins.

MLR performed well but had a risk of overfitting.

Sentiment analysis could further improve stock price predictions.

3️⃣ Diamond Price Affordability Classification

Business Problem: Predict whether a diamond is affordable (< $5000) or not.

Data Cleaning:

Removed redundant columns (x, y, z) due to high correlation with Carat.

Converted Cut, Color, and Clarity to ranked categorical variables.

Machine Learning Models:

Logistic Regression (4 variations tested for best variable selection).

KNN Classification for final price prediction.

Key Findings:

Carat size & clarity are the strongest indicators of price.

KNN achieved 92.88% accuracy in classifying affordable vs. unaffordable diamonds.

🛠️ Installation & Setup

1️⃣ Clone the Repository

git clone https://github.com/akum103/predictive_analysis.git
cd predictive_analysis

2️⃣ Install Dependencies in R

install.packages(c("dplyr", "randomForest", "caret", "rpart", "ggplot2", "corrplot", "psych", "plotly"))

3️⃣ Run the R Scripts

source("bitcoin.R")

📊 Results & Performance Metrics

Model

Accuracy

Precision

Recall

F1 Score

Bitcoin (Random Forest)

49.9%

49.3%

51.4%

50.3%

Apple (SMA)

N/A

N/A

N/A

RMSE: 0.008

Diamond (KNN)

92.88%

91.3%

94.1%

92.7%

📄 Project Report

📥 Download the full report

🚀 Future Improvements

Balance class imbalance in Bitcoin trading signals

Apply deep learning (LSTM/RNN) to stock market trends

Perform Sentiment Analysis for Stock Price Prediction

Improve affordability classification for rare diamonds

💡 Author: akum103🎯 GitHub Repo: predictive_analysis
