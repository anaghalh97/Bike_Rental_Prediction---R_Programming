# 🚲 Bike Rental Count Prediction using R

## 📌 Project Overview
Bike-sharing systems allow users to rent and return bikes seamlessly across locations.  

This project analyzes a bike-sharing dataset to understand how environmental conditions and seasonal factors influence daily bike rentals. Using exploratory data analysis (EDA) and machine learning techniques, the project predicts the total number of bikes rented per day.

---

## 🎯 Objectives
- Perform exploratory data analysis (EDA) to identify trends and patterns  
- Visualize the effect of seasonal and environmental variables on bike rentals  
- Build a machine learning model to predict daily bike rental counts  

---

## ❓ Problem Statement
A bike rental company wants to accurately predict daily bike demand based on weather conditions, seasons, and calendar-related variables.  

These predictions can help in:
- Better resource allocation  
- Improved operational planning  
- Data-driven decision-making  

---

## 📊 Dataset Description

The dataset contains daily bike rental data with the following key features:

### 🕒 Time-Based Variables
- Date  
- Year  
- Month  
- Weekday  
- Season  

### 🌦 Weather-Related Variables
- Weather situation  
- Temperature  
- Perceived temperature  
- Humidity  
- Windspeed  

### 👥 User Information
- Casual users  
- Registered users  

### 🎯 Target Variable
- Total bike rentals (`cnt`)

---

## 📈 Exploratory Data Analysis (EDA)

- Checked and converted data types where required  
- Analyzed missing values  
- Visualized:
  - Monthly and yearly trends in bike rentals  
  - Distribution of rentals across seasons  
  - Boxplots to identify potential outliers  

---

## ⚙️ Modeling Approach

1. Split the dataset into training and testing sets  
2. Built a **Random Forest** model using R  
3. Predicted bike rental counts on the test dataset  
4. Evaluated model performance using prediction accuracy metrics  

---

## 🛠 Tools & Technologies

- R Programming  
- Exploratory Data Analysis  
- Data Manipulation  
- Data Visualization  
- Machine Learning (Random Forest)  

---

## ✅ Conclusion
The analysis shows that weather and seasonal factors significantly impact bike rental demand.  
The **Random Forest model** provides reliable predictions and can support data-driven decision-making for bike-sharing services.

The analysis shows that weather and seasonal factors significantly impact bike rental demand.  

The **Random Forest model** provides reliable predictions and can support data-driven decision-making for bike-sharing services.
