########################  QUESTIONS ####################
# 1. Exploratory data analysis
#    a. Load dataset and libraries
#    b. Perform data type conversion of the attributes
#    c. Carry out the missing value analysis
# 2. Attributes distributions and trends
#    a. Plot monthly distribution of the total number of bikes rented
#    b. Plot yearly distribution of the total number of bikes rented
#    c. Plot boxplot for outliers analysis
# 3. Split the dataset into train and test dataset
# 4. Create a model using the random forest algorithm
# 5. Predict the performance of the model on the test dataset

#######################  SOLUTIONS  ########################

# 1. Exploratory data analysis
#    a. Load dataset and libraries
getwd()
setwd("C:/Users/Anagha LH/OneDrive/Documents/Datasets")
Bike_Rental=read.csv("Bike_Rental.csv")
View(Bike_Rental)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caTools)
library(randomForest)
library(rpart)
install.packages("future")
install.packages("data.table")
install.packages("caret", dependencies = TRUE)
library(caret)


# 1. Exploratory data analysis
#    b. Perform data type conversion of the attributes
str(Bike_Rental)
Bike_Rental$dteday=as.Date(Bike_Rental$dteday, format = "%d-%m-%Y")
View(Bike_Rental)
str(Bike_Rental)


# 1. Exploratory data analysis
#    c. Carry out the missing value analysis

Bike_Rental=na.omit(Bike_Rental)
View(Bike_Rental)

################################### 2. Attributes distributions and trends#########################
#    a. Plot monthly distribution of the total number of bikes rented

#STEPS:
# Step 1: Convert year and month into full values
# 'yr' is 0 for 2011 and 1 for 2012, so convert it
Bike_Rental$year=ifelse(Bike_Rental$yr == 0, 2011, 2012)
# Step 2: Create a YearMonth column in the format "YYYY-MM"
Bike_Rental$YearMonth=paste(Bike_Rental$year, sprintf("%02d", Bike_Rental$mnth), sep = "-")
# Step 3: Aggregate total rentals per YearMonth
monthly_rentals=aggregate(cnt ~ YearMonth, data=Bike_Rental, sum)
# Step 4: Convert YearMonth to date format for proper plotting
monthly_rentals$YearMonth <- as.Date(paste0(monthly_rentals$YearMonth, "-01"))
# Step 5: Plot the trend using a line chart
ggplot(monthly_rentals, aes(x = YearMonth, y = cnt)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Monthly Distribution of Total Bike Rentals",
       x = "Month",
       y = "Total Rentals") +
  theme_minimal()



#2. Attributes distributions and trends
#     b. Plot yearly distribution of the total number of bikes rented

#STEPS:
# Step 1: Aggregate total rentals by year
yearly_rentals=aggregate(cnt ~ year, data = Bike_Rental, sum)
# Step 2: Plot a bar chart
ggplot(yearly_rentals, aes(x = factor(year), y = cnt, fill = factor(year))) +
  geom_bar(stat = "identity") +
  labs(title = "Yearly Distribution of Total Bike Rentals",
       x = "Year",
       y = "Total Rentals") +
  theme_minimal() +
  scale_fill_manual(values = c("2011" = "skyblue", "2012" = "orange")) +
  theme(legend.position = "none")

#2. Attributes distributions and trends
#    c. Plot boxplot for outliers analysis

#STEPS:
# Step 1: Reshape data to long format for ggplot
box_data <- Bike_Rental %>%
  select(casual, registered, cnt) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Step 2: Plot boxplot
ggplot(box_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot for Outlier Analysis",
       x = "Variable",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")


########################### 3. Split the dataset into train and test dataset#########################

#STEPS:
# STEP 1: Split the data (70% for training)
split_data=sample.split(Bike_Rental$cnt, SplitRatio = 0.7)
Training_data=Bike_Rental[split_data,]
View(Training_data)
Test_data=Bike_Rental[!split_data,]
View(Test_data)

# STEP 2: Split the data (80% for training)
split_data=sample.split(Bike_Rental$cnt, SplitRatio = 0.8)
Training_data=Bike_Rental[split_data,]
View(Training_data)
Test_data=Bike_Rental[!split_data,]
View(Test_data)

########################### 4. Create a model using the random forest algorithm###################
Bike_Rental_RF=randomForest(cnt ~ ., data = Training_data,type="class")
Bike_Rental_RF
summary(Bike_Rental_RF)

########################## 5. Predict the performance of the model on the test dataset############
#STEPS:
# Step 1: Predict using the trained Random Forest model
Prediction_RF=predict(Bike_Rental_RF,Test_data,type = 'class')
# Step 2: Get actual values
Actual_RF=Test_data$cnt
# Step 3: Calculate MAPE
mape_values = abs(Actual_RF - Prediction_RF) / Actual_RF
mape = mean(mape_values, na.rm = TRUE) * 100  # Convert to percentage
# Step 4: Print result
cat("MAPE of Random Forest model on Test Data:", round(mape, 2), "%\n")

View(Bike_Rental)








