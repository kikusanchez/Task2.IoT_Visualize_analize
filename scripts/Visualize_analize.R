#### 00. INCLUDES ---------------------------------------------------------------------
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,plotly,caret,rpart,
               gdata,chron,lubridate,padr,TTR,forecast,opera,GeomComb,zoo,imputeTS)


#Setting my Plotly API
#Sys.setenv("plotly_username"="kikusanchez")
#Sys.setenv("plotly_api_key"="Uky3F2ELrykJSTQHGBiP")

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#DATA SETS

#load data frame
load("../Task1.IoT_Exploratory_data_analysis/datasets/entire_ok.Rda")




#### 01. PRE-PROCESS ---------------------------------------------------------------------

# MANAGING WITH NA'S #

# Searching for NA's -> 363560
sum(is.na(entire_ok))


# METHOD 1: Replacing NA's with the MEAN of the variable #

# Charging zoo library to get na.aggregate function
#library(zoo)

# Replacing NA's with the mean and creating the concerning dataset
#NA_mean <- replace(entire_ok, TRUE, lapply(entire_ok, na.aggregate))


# METHOD 2: Replacing NA's depending on number of consecutive NA's #

# Creating a table with DateTime and Global Active Power NA's

test_nas <-entire_ok %>%
  filter(is.na(entire_ok$Global_active_power))%>%
  select(DateTime, Global_active_power)


# FYI: lag & lead are for checking previous and next rows
lag(test_nas$DateTime)[2] #this check the 1st row
lead(test_nas$DateTime)[2] # this check the 3rd row


# Creating a column and assigning the sum of NA's for each day
# (checking if the previous row is equal to the current row minus 1 minute (60 seconds))

number_nas <- 1

for (i in 1:nrow(test_nas)) {
  
  if (i==1){
    
    test_nas$number_nas[i] <- number_nas
  }
  
  else if (lag(test_nas$DateTime)[i] == (test_nas$DateTime[i] -60 )){
    
    number_nas <- number_nas +1
    
    test_nas$number_nas[i] <- number_nas
  }
  
  else {
    
    number_nas <- 1
    
    test_nas$number_nas[i] <- number_nas
  }
  
}

# Creating a replica df only with DateTime and the number of consecutive NA's minutes

grouped_nas <- test_nas[FALSE,] # same df but with empty values
grouped_nas$Global_active_power <- NULL #removing Global_active_power column


for (i in 1:nrow(test_nas)) {
  
  if (i==1){
    
    grouped_nas[1,] <- test_nas[1, c("DateTime", "number_nas") ]
  }
  
  else if (test_nas$number_nas[i] == 1){
  
    grouped_nas <- rbind(grouped_nas, test_nas[i, c("DateTime", "number_nas")])
    
  }
  
  else if (test_nas$number_nas[i] != 1){
    
    grouped_nas[nrow(grouped_nas), "number_nas"] <- grouped_nas[nrow(grouped_nas), "number_nas"] +1
    }
}


## if number_nas is less than X, do this, if not, do that 

for (i in 1:10) {
  
  if (grouped_nas$number_nas[i] >= 1 & grouped_nas$number_nas[i] <= 60)
    
    na.aggregate(test_nas$number_nas)      
  
}






entire_ok2 <- ifelse (grouped_nas$number_nas >= 1 & grouped_nas$number_nas <= 60,
        replace(entire_ok2$Global_active_power, TRUE, lapply(entire_ok$Global_active_power, na.mean)), 
        replace(entire_ok$Global_active_power, TRUE, lapply(entire_ok$Global_active_power, na.locf)))






# Global <- Global %>%
#   group_by(year, month) %>%
#   mutate(Global_active_power= replace(Global_active_power, is.na(Global_active_power), mean(Global_active_power, na.rm=TRUE))) %>%
#   mutate(Sub_metering_1= replace(Sub_metering_1, is.na(Sub_metering_1), mean(Sub_metering_1, na.rm=TRUE))) %>%
#   mutate(Sub_metering_2= replace(Sub_metering_2, is.na(Sub_metering_2), mean(Sub_metering_2, na.rm=TRUE))) %>%
#   mutate(Sub_metering_3= replace(Sub_metering_3, is.na(Sub_metering_3), mean(Sub_metering_3, na.rm=TRUE)))





# Number of missing rows per day
NA_adjusted <- ifelse (entire_ok$hour_minute >= hm("6:30") & entire_years$hour_minute <= hm("22:30"),
                             0.158, 0.123) 






plotNA.distribution(entire_ok$Global_active_power)

library(imputeTS)



# Searching for NA's -> 0
sum(is.na(NA_mean))






# Granularity -> month (for each year) & total power
monthly_power <- NA_mean %>% 
  group_by(year, month) %>%
  summarise(month_total = sum(Global_active_power))


# Granularity -> day (for each month and year) & total power
daily_power <- NA_mean %>% 
  group_by(year, month, day) %>%
  summarise(day_total = sum(Global_active_power))

# Granularity -> monthly price (for each year)
monthly_price <- NA_mean %>% 
  group_by(year, month) %>%
  summarise(month_bill = sum(Global_active_power*rate))

write.csv(monthly_power, file =  "monthly_power_1.csv")
getwd()

# Creating time series
ts_monthly <- ts(monthly_power$month_total, frequency = 12, start=c(2007,1))

ts_daily <- ts(daily_power$day_total, frequency = 365, start=c(2007,1))

ts_bill <- ts(monthly_price$month_bill, frequency = 12, start = c(2007,1))

# Plotting my ts
autoplot(ts_monthly)
autoplot(ts_bill)

# Decomposing

dec_ts_monthly<-stl(ts_monthly, s.window = "periodic")

dec_ts_daily <- stl(ts_daily, s.window = "periodic")

dec_ts_bill <- stl(ts_bill, s.window = "periodic")


autoplot(dec_ts_monthly)
autoplot(dec_ts_daily)
autoplot(dec_ts_bill)



# variance for each variable
apply(dec_ts_monthly$time.series, 2, var)/var(ts_monthly)

apply(dec_ts_daily$time.series, 2, var)/var(ts_daily) #difficult to predict due to its high remainder

apply(dec_ts_bill$time.series, 2, var)/var(ts_bill)







Global <- entire_ok %>%
  group_by(year, month, day) %>%
  mutate(Global_active_power = replace(Global_active_power, is.na(Global_active_power), mean(Global_active_power, na.rm=TRUE))) %>%
  mutate(Sub_metering_1 = replace(Sub_metering_1, is.na(Sub_metering_1), mean(Sub_metering_1, na.rm=TRUE))) %>%
  mutate(Sub_metering_2 = replace(Sub_metering_2, is.na(Sub_metering_2), mean(Sub_metering_2, na.rm=TRUE))) %>%
  mutate(Sub_metering_3 = replace(Sub_metering_3, is.na(Sub_metering_3), mean(Sub_metering_3, na.rm=TRUE))) %>%
  mutate(residual = replace(residual, is.na(residual), mean(residual, na.rm=TRUE))) %>%
  mutate(cost_total = replace(cost_total, is.na(cost_total), mean(cost_total, na.rm=TRUE))) %>%
  mutate(cost_residual = replace(cost_residual, is.na(cost_residual), mean(cost_residual, na.rm=TRUE))) %>%
  mutate(cost_sub1 = replace(cost_sub1, is.na(cost_sub1), mean(cost_sub1, na.rm=TRUE))) %>%
  mutate(cost_sub2 = replace(cost_sub2, is.na(cost_sub2), mean(cost_sub2, na.rm=TRUE))) %>%
  mutate(cost_sub3 = replace(cost_sub3, is.na(cost_sub3), mean(cost_sub3, na.rm=TRUE)))
  






#dayly_na <- ifelse(is.na(dayly_data$dayly_power) == 1, mean(dayly_data$dayly_power), dayly_data$dayly_power)



# Grouping data by days
dayly_data2 <- Global %>%
  group_by(year, month, day) %>%
  summarise(dayly_power = sum(Global_active_power),
            dayly_kitchen = sum(Sub_metering_1),
            dayly_laundry = sum(Sub_metering_2),
            dayly_ewac = sum(Sub_metering_3),
            dayly_residual = sum(residual),
            dayly_cost_total = sum(cost_total),
            dayly_cost_kitchen = sum(cost_sub1),
            dayly_cost_laundry = sum(cost_sub2),
            dayly_cost_ewac = sum(cost_sub3),
            dayly_cost_residual = sum(cost_residual))

dayly_data2$DateTime <- paste(dayly_data2$day, dayly_data2$month, dayly_data2$year)

dayly_data2$DateTime <- lubridate::dmy(dayly_data2$DateTime)

dayly_data2$day <- NULL
dayly_data2$month <- NULL
dayly_data2$year <- NULL

# Looking for NA's
sum(is.na(dayly_data2)) # -> 90


# Loop for checking all NA's and replacing them by the mean
for(i in 1:ncol(dayly_data2)){
  for(y in 1:nrow(dayly_data2)) {
    dayly_data2[y,i] = ifelse(is.na(dayly_data2[y,i]) == TRUE, sum(dayly_data2[,i] / 1426, na.rm=TRUE), dayly_data2[y,i])
  }
}

# Looking for NA's
sum(is.na(dayly_data2)) # -> 0


write.csv2(dayly_data2, file = "dayly_data2.csv", fileEncoding = "UTF-8")

getwd()

#### 02. MODELLING ---------------------------------------------------------------------

#### 02.1 Monthly power ####

#train and test sets for total power monthly forecast
train <- window(ts_monthly, start=c(2007, 1), end=c(2009,12))
test <- window(ts_monthly, start=c(2010, 1))


# Creating models
HW_monthly_total <- HoltWinters(train)

Arima_monthly_total <- auto.arima(train)

ets_monthly_total <- ets(train)

tslm_monthly_total <- tslm(train ~ trend + season)

# Creating a vector with the same length of the h parameter
h <- length(test)

# Creating forecasts
FC_hw_monthly_total<-forecast:::forecast.HoltWinters(HW_monthly_total, h)

FC_arima_monthly <- forecast:::forecast.Arima(Arima_monthly_total, h)

FC_ets_monthly <- forecast:::forecast.ets(ets_monthly_total, h)

FC_tslm_monthly <- forecast(tslm_monthly_total, h=h)


# Checking for residuals of my models
checkresiduals(FC_hw_monthly_total)
checkresiduals(FC_ets_monthly)
checkresiduals(FC_arima_monthly)

# Plotting my forecast comparing with my total time observed
autoplot(train)+
  autolayer(FC_hw_monthly_total$mean)+
  autolayer(FC_arima_monthly$mean) +
  autolayer(FC_ets_monthly$mean) + 
  autolayer(FC_tslm_monthly$mean)


# Checking the accuracy of the models on my test dataset
accuracy_hw_monthly <- accuracy(FC_hw_monthly_total, test)

accuracy_arima_monthly <- accuracy(FC_arima_monthly, test)

accuracy_arima_monthly <- accuracy(FC_ets_monthly, test)

# MIXTURE FORECAST #

# Creating a vector with the results of all my forecasts
FC_results <- cbind(FC_hw_monthly_total$mean, FC_arima_monthly$mean, FC_ets_monthly$mean, FC_tslm_monthly$mean)

# Creating a data frame of my test with the results of all my forecasts
results_df <- cbind(test, FC_results)

# Assigning names to the columns
colnames(results_df) <- c("Real data - test size","HW","ARIMA","ETS","TSLM")

# Plotting all forecasts
autoplot(results_df) +
  xlab("Year") + ylab(expression("Total power consumption"[2])) +
  scale_color_manual(labels = c("Real data - test size","HW","ARIMA","ETS","TSLM"),
                     values=c("black", "red", "pink", "blue", "green")) +
  aes(linetype = series,
      size = series) +
  scale_linetype_manual(labels = c("Real data - test size","HW","ARIMA","ETS","TSLM"),
                        values = c(1, 2, 2, 2, 2)) +
  scale_size_manual(labels = c("Real data - test size","HW","ARIMA","ETS","TSLM"),
                    values = c(2, 1, 1, 1, 1))


# The mixture function from the opera package computes weights
#when combining the forecasts based on how well it has done up to that point.

#creating mixture model based on MLpol model
MLpol0 <- mixture(model = "MLpol", loss.type = "square")

# assigning weigths to your model on the testing size
weights <- predict(MLpol0, FC_results, test, type='weights')

head(weights)
tail(weights)

# Creating mixture forecast
FC_mixture_monthly <- ts(predict(MLpol0, FC_results, test, type='response'), start=c(2010,1), freq=12)

# Saving results of mixture forecast and holtwinters (best models) in a dataframe and renaming their columns
bestmodels_df <- cbind(test, FC_mixture_monthly, FC_hw_monthly_total$mean)
colnames(bestmodels_df) <- c("Real data - test size","Mixture forecast", "Holtwinters forecast")


# Plotting the dataframe with mixture forecast and holtwinters results
autoplot(bestmodels_df) +
  xlab("Year") + ylab(expression("Total power consumption"[2])) +
  scale_color_manual(labels = c("Real data - test size","Mixture forecast", "Holtwinters forecast"),
                     values=c("black", "blue", "red")) +
  aes(linetype = series, size = series) +
  scale_linetype_manual(labels = c("Real data - test size","Mixture forecast", "Holtwinters forecast"),
                        values = c(1, 2, 2)) +
  scale_size_manual(labels = c("Real data - test size","Mixture forecast", "Holtwinters forecast"),
                    values = c(2, 1, 1))


# HYBRID FORECAST #
library(thief)
library(forecastHybrid)

fit1 <- hybridModel(train, weights="equal")

fit2 <- hybridModel(train, weights="insample")

fc1 <- forecast(fit1, h=h)
fc2 <- forecast(fit2, h=h)


accuracy_fc1_monthly <- accuracy(fc1, test)
accuracy_fc2_monthly <- accuracy(fc2, test)




autoplot(fc1) + ggtitle("Hybrid 1") + xlab("Year") +
  ylab(expression("Monthly power consumption"[2])) 

#hybrid models dataframe
hybrid_df <- cbind(Data=ts_monthly, Hybrid1=fc1$mean, Hybrid2=fc2$mean)

autoplot(hybrid_df) +
  xlab("Year") + ylab(expression("Monthly power consumption"[2])) +
  scale_color_manual(labels = c("Real data - test size","Hybrid 1", "Hybrid 2"),
                     values=c("black", "blue", "red")) +
  aes(linetype = series, size = series) +
  scale_linetype_manual(labels = c("Real data - test size","Hybrid 1", "Hybrid 2"),
                        values = c(1, 2, 2)) +
  scale_size_manual(labels = c("Real data - test size","Hybrid 1", "Hybrid 2"),
                    values = c(2, 1, 1))

# MSE
mse <- c(Opera=mean((test-FC_mixture_monthly)^2),
         Hybrid1=mean((test - fc1$mean)^2),
         Hybrid2=mean((test - fc2$mean)^2))

round(mse,2)

#MSE2
mse2 <- c(FC_ets_monthly=mean((test-FC_ets_monthly$mean)^2),
          FC_arima_monthly=mean((test-FC_arima_monthly$mean)^2),
          FC_tslm_monthly=mean((test-FC_tslm_monthly$mean)^2))

round(mse2,2)


#### 02.2 Monthly bill ####

#train and test sets for monthly forecast
train_bill <- window(ts_bill, start=c(2007, 1), end=c(2009,12))
test_bill <- window(ts_bill, start=c(2010, 1))


# Creating models
HW_monthly_bill <- HoltWinters(train_bill)

Arima_monthly_bill <- auto.arima(train_bill)

ets_monthly_bill <- ets(train_bill)

tslm_monthly_bill <- tslm(train_bill ~ trend + season)

# Creating a vector with the same length of the h parameter
h <- length(test)

# Creating forecasts
FC_hw_monthly_bill<-forecast:::forecast.HoltWinters(HW_monthly_bill, h)

FC_arima_bill <- forecast:::forecast.Arima(Arima_monthly_bill, h)

FC_ets_bill <- forecast:::forecast.ets(ets_monthly_bill, h)

FC_tslm_bill <- forecast(tslm_monthly_bill, h=h)


# Checking for residuals of my models
checkresiduals(FC_hw_monthly_bill)
checkresiduals(FC_ets_bill)
checkresiduals(FC_arima_bill)

# Plotting my forecast comparing with my total time observed
autoplot(train_bill)+
  autolayer(FC_hw_monthly_bill$mean)+
  autolayer(FC_arima_bill$mean) +
  autolayer(FC_ets_bill$mean) + 
  autolayer(FC_tslm_bill$mean)


# Checking the accuracy of the models on my test dataset
accuracy(FC_hw_monthly_bill, test_bill)
accuracy(FC_arima_bill, test_bill)



# MIXTURE FORECAST #

# Creating a vector with the results of all my forecasts
FC_results_bill <- cbind(FC_hw_monthly_bill$mean, FC_arima_bill$mean, FC_ets_bill$mean, FC_tslm_bill$mean)

# Creating a data frame of my test with the results of all my forecasts
results_df_bill <- cbind(test_bill, FC_results_bill)

# Assigning names to the columns
colnames(results_df_bill) <- c("Real data - test size","HW","ARIMA","ETS","TSLM")

# Plotting all forecasts
autoplot(results_df_bill) +
  xlab("Year") + ylab(expression("Total monthly price"[2])) +
  scale_color_manual(labels = c("Real data - test size","HW","ARIMA","ETS","TSLM"),
                     values=c("black", "red", "pink", "blue", "green")) +
  aes(linetype = series,
      size = series) +
  scale_linetype_manual(labels = c("Real data - test size","HW","ARIMA","ETS","TSLM"),
                        values = c(1, 2, 2, 2, 2)) +
  scale_size_manual(labels = c("Real data - test size","HW","ARIMA","ETS","TSLM"),
                    values = c(2, 1, 1, 1, 1))



# The mixture function from the opera package computes weights
#when combining the forecasts based on how well it has done up to that point.

#creating mixture model based on MLpol model
MLpol0 <- mixture(model = "MLpol", loss.type = "square")

# assigning weigths to your model on the testing size
weights_bill <- predict(MLpol0, FC_results_bill, test_bill, type='weights')

head(weights)
tail(weights)

# Creating mixture forecast
FC_mixture_bill <- ts(predict(MLpol0, FC_results_bill, test_bill, type='response'), start=c(2010,1), freq=12)

# Saving results of mixture forecast and holtwinters (best models) in a dataframe and renaming their columns
bestmodels_df_bill <- cbind(test_bill, FC_mixture_bill, FC_hw_monthly_bill$mean)
colnames(bestmodels_df_bill) <- c("Real data - test size","Mixture forecast", "Holtwinters forecast")


# Plotting the dataframe with mixture forecast and holtwinters results
autoplot(bestmodels_df_bill) +
  xlab("Year") + ylab(expression("Total monthly price"[2])) +
  scale_color_manual(labels = c("Real data - test size","Mixture forecast", "Holtwinters forecast"),
                     values=c("black", "blue", "red")) +
  aes(linetype = series, size = series) +
  scale_linetype_manual(labels = c("Real data - test size","Mixture forecast", "Holtwinters forecast"),
                        values = c(1, 2, 2)) +
  scale_size_manual(labels = c("Real data - test size","Mixture forecast", "Holtwinters forecast"),
                    values = c(2, 1, 1))


# HYBRID FORECAST #
library(thief)
library(forecastHybrid)

fit1_bill <- hybridModel(train_bill, weights="equal")

fit2_bill <- hybridModel(train_bill, weights="insample")

fc1_bill <- forecast(fit1_bill, h=h)
fc2_bill <- forecast(fit2_bill, h=h)

autoplot(fc1_bill) + ggtitle("Hybrid 1") + xlab("Year") +
  ylab(expression("Monthly total price"[2])) 

#hybrid models dataframe
hybrid_df_bill <- cbind(Data=ts_bill, Hybrid1=fc1_bill$mean, Hybrid2=fc2_bill$mean)

autoplot(hybrid_df_bill) +
  xlab("Year") + ylab(expression("Monthly total price"[2])) +
  scale_color_manual(labels = c("Real data - test size","Hybrid 1", "Hybrid 2"),
                     values=c("black", "blue", "red")) +
  aes(linetype = series, size = series) +
  scale_linetype_manual(labels = c("Real data - test size","Hybrid 1", "Hybrid 2"),
                        values = c(1, 2, 2)) +
  scale_size_manual(labels = c("Real data - test size","Hybrid 1", "Hybrid 2"),
                    values = c(2, 1, 1))

# MSE
mse_bill <- c(Opera=mean((test_bill-FC_mixture_bill)^2),
         Hybrid1=mean((test_bill - fc1_bill$mean)^2),
         Hybrid2=mean((test_bill - fc2_bill$mean)^2))

round(mse_bill,2)

#MSE2
mse2_bill <- c(FC_ets_bill=mean((test_bill-FC_ets_bill$mean)^2),
          FC_arima_bill=mean((test_bill-FC_arima_bill$mean)^2),
          FC_tslm_bill=mean((test_bill-FC_tslm_bill$mean)^2))

round(mse2_bill,2)















# Apply time series linear regression to my ts object
fitSM3 <- tslm(Stalin ~ trend + season)

# obtain R2 and RMSE from the model you built
summary(fitSM3)

# Create the forecast ahead 3 time periods 
forecastfitSM3 <- forecast(fitSM3, h=12)

# Plotting the forecast
plot(forecastfitSM3)
autoplot(forecastfitSM3)


## Remove seasonal components



tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)





# PLAN OF ATTACK: #

## Plot all of sub-meter 1
plot(yourData$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(entire_ok, year == 2008 & week == 2)

## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(entire_ok, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1 subset:houseday (9th January 2008)
plot_ly(houseDay, 
        x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        type = 'scatter', 
        mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)"))


## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(entire_ok, year == 2008 & month == 1 & day == 9 & 
                       (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)"))


# Subset the 'Blue Monday' week of 2009 - 1 hour frequency (only o'clock hours)
blue_week <- filter(entire_ok, year == 2009 & month == 1 & week == 4 & minute == 0)

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(blue_week, x = blue_week$DateTime, y = blue_week$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = blue_week$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = blue_week$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Blue Week, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)"))

# Subset the 'Yellow week' of 2009 - 1 hour frequency (only o'clock hours)
yellow_week <- filter(entire_ok, year == 2009 & week == 25 & minute == 0)

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 1 hour frequency
plot_ly(yellow_week, x = yellow_week$DateTime, y = yellow_week$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = yellow_week$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = yellow_week$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Yellow Week, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)"))

# Subset Winter period 2007 - 2008 (december 2007 - march 2008) - 1 hour frequency
winter_0708 <- filter(entire_ok, (year == 2007 & month == 12) | 
                        (year == 2008 & month == 1) |
                        (year == 2008 & month == 2) |
                        (year == 2008 & month == 3) & 
                        (hour == 1))# | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))


## Plot sub-meter 1, 2 and 3 with title, legend and labels - winter 07-08 - 1 hour frequency
plot_ly(winter_0708, x = winter_0708$DateTime, y = winter_0708$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = winter_0708$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = winter_0708$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Winter, 2007-08",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)"))

# Subset Summer period 2008 (june - september) - 1 hour frequency
summer_08 <- filter(entire_ok, (year == 2008 & month == 6 & minute == 0) |
                      (year == 2008 & month == 7 & minute == 0) | 
                      (year == 2008 & month == 8 & minute == 0) |
                      (year == 2008 & month == 9 & minute == 0))








#### TEST AREA ---------------------------------------------------------------------------------------
test <- houseDay %>% dplyr::group_by(hour)%>%
  dplyr::summarise(consumption = sum(Global_active_power),
                   sub1 = sum(Sub_metering_1))


ggplot(test, aes(hour, consumption))+
  geom_line()+
  geom_line(aes(y=sub1))




