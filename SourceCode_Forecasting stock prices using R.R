##### K194141740 - Tran Thanh Phuc

########## 1. Perform literature review 


########## 2. Data collection and input 

# Import library
library(readxl)
library(ggplot2) # for plotting
library(tidyverse)  # for dataframe manipulation
#install.packages("zoo")  # Install zoo package
library("zoo")  # dealing with missing values
#install.packages("SciViews")  # Install SciViews package
library("SciViews")
#install.packages("cowplot")  Install cowplot package
library("cowplot")
#install.packages("Hmisc")
library("Hmisc")



# Get data
data = read_excel('K194141740.xlsx', sheet = 'Data')
View(data)


# Preprocessing data
# Replace missing values by median values
print(paste('There are/is ', sum(is.na(data)), 'missing values'))
for (i in names(data)){
  data[i] = na.locf(data[i])
}
print(paste('There are/is ', sum(is.na(data)), 'missing values')) # check misssing values
# Correlation
corr <- rcorr(as.matrix(data[,2:ncol(data)]))
print(corr)


# Calculate Asset Structure, Firm Size, Current Ratio, Leverage
df = data.frame(data$Date)
colnames(df)[1] = 'Date'
df$ROA = data$ROA
df$AssetStructure = data$FixedAssets/data$CurrentAssets
df$FirmSize = ln(data$TotalAssets)
df$CurrentRatio = data$CurrentAssets/data$CurrentLiabilites
df$Leverage = data$TotalDebts/data$TotalAssets
df = mutate(df, Period = NA)
head(df)

View(df)

########## 3. Provide descriptive statistics of all the variables for BEFORE and AFTER periods

# Create Period column to verify data before 2020 and after 2020
for (i in 1:nrow(df)){
  if (i <= 33){
    df[i,'Period'] = 'before Covid-19 pandemic'
  } else {
    df[i,'Period'] = 'after Covid-19 pandemic'
  }
}

# Descriptive statistics of ROA
ROA_statistics = df %>% 
  group_by(Period) %>% 
  summarise(Median = median(ROA),
            Mean = mean(ROA),
            Min = min(ROA),
            Max = max(ROA),
            Standard_Deviation = sd(ROA))
View(ROA_statistics, 'ROA')
# Descriptive statistics of AssetStructure
AssetStructure_statistics = df %>% 
  group_by(Period) %>% 
  summarise(Median = median(AssetStructure),
            Mean = mean(AssetStructure),
            Min = min(AssetStructure),
            Max = max(AssetStructure),
            Standard_Deviation = sd(AssetStructure))
View(AssetStructure_statistics, 'AssetStructure')
# Descriptive statistics of FirmSize
FirmSize_statistics = df %>% 
  group_by(Period) %>% 
  summarise(Median = median(FirmSize),
            Mean = mean(FirmSize),
            Min = min(FirmSize),
            Max = max(FirmSize),
            Standard_Deviation = sd(FirmSize))
View(FirmSize_statistics, 'FirmSize')
# Descriptive statistics of CurrentRatio
CurrentRatio_statistics = df %>% 
  group_by(Period) %>% 
  summarise(Median = median(CurrentRatio),
            Mean = mean(CurrentRatio),
            Min = min(CurrentRatio),
            Max = max(CurrentRatio),
            Standard_Deviation = sd(CurrentRatio))
View(CurrentRatio_statistics, 'CurrentRatio')
# Descriptive statistics of Leverage
Leverage_statistics = df %>% 
  group_by(Period) %>% 
  summarise(Median = median(Leverage),
            Mean = mean(Leverage),
            Min = min(Leverage),
            Max = max(Leverage),
            Standard_Deviation = sd(Leverage))
View(Leverage_statistics, 'Leverage')


########## 4. Provide box & whisker plot and histogram of Leverage
# Box & Whisker plot of Leverage for the entire period
df %>% 
  ggplot(aes(x = Leverage, y = Period, fill = Period)) +
  geom_boxplot() +
  coord_flip()
# Histogram plot of Leverage for the entire period
htg_before = df %>% 
  filter(Period == 'before Covid-19 pandemic') %>% 
  ggplot(aes(x = Leverage)) +
  geom_histogram(fill = 'blue')
htg_after = df %>% 
  filter(Period == 'after Covid-19 pandemic') %>% 
  ggplot(aes(x = Leverage)) +
  geom_histogram(fill = 'red')
plot_grid(htg_before, htg_after, labels=c("Before Covid-19 pandemic", "After Covid-19 pandemic"), 
          label_size = 7, ncol = 1, nrow = 2)


########## 5. Perform multiple regression to determine the significant determinants of Leverage
# task 1
model_1<-lm(Leverage ~ ROA  + AssetStructure + FirmSize + CurrentRatio, data = df)
summary(model_1)
# task 2
colnames(df)[ncol(df)] = 'Covid'
for (i in 1:nrow(df)){
  if (df[i, 'Covid'] == 'before Covid-19 pandemic'){
    df[i, 'Covid'] = 0
  } else {
    df[i, 'Covid'] = 1
  }
}
print(head(df))
model_2<-lm(Leverage ~ ROA  + AssetStructure + FirmSize + CurrentRatio + Covid, data = df)
summary(model_2)
# task 3
df_new = df
df_new$Date = NULL
df_new$Leverage = NULL
df_new$Covid = NULL
df$Predict = predict(model_1, newdata = df_new)
View(df[, c('Leverage', 'Predict')], 'Predict')

# RMSE, MSE 
#install.packages('Metrics')
library("Metrics")
rmse(df$Leverage , df$Predict)
mse(df$Leverage , df$Predict)


########## 6. Perform ARIMA model to predict the variable of interest for the 4 quarters in 2022 

#install.packages('forecast')
library(forecast) #forecast, accuracy
#install.packages('quantmod')
library(quantmod) #getSymbols
#install.packages('tseries')
library(tseries) #adf.test
#install.packages('lmtest')
library(lmtest) #coeftest
#install.packages('stats')
library(stats) #Box.test

#adjust plot margins to avoid the error in plot.new() figure margins too large
par(mar = c(1, 1, 1, 1))
#create scatterplot
plot(1:30)

#use auto.arima function to determine best P, D, Q
dif = data.frame(diff(df$Leverage, differences = 2))
auto=auto.arima(dif,seasonal=F,trace = T,max.order=4,ic='aic')
coeftest(auto.arima(dif,seasonal=F))
acf(auto$residuals)
pacf(auto$residuals)
Box.test(auto$residuals,lag=20,type='Ljung-Box')

#prediction
term=4
fcastauto=forecast(auto,h=term)
fcastauto #fcastauto is the predicted values for 44 terms
plot(fcastauto)
# check accuracy
accuracy(fcastauto)






