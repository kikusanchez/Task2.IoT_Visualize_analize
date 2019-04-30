#### 00. INCLUDES -----------------------------------------------------------------------------------
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,plotly,caret,rpart,gdata,chron,lubridate,padr,TTR,forecast)
library(lubridate)

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




#### 01. PRE-PROCESS --------------------------------------------------------------------------------

# Filling NA's
# Searching for NA's -> 116844
sum(is.na(entire_ok))

# Replace NA's with the mean of the variable
# Charging zoo library to obtain na.aggregate function
library(zoo) 
# Replacing NA's with the mean and creating the concerning dataset
NA_mean <- replace(test, TRUE, lapply(test, na.aggregate))
# Searching for NA's -> 0
sum(is.na(NA_mean))



# Granularity -> month (for each year) & total power
monthly_power <- NA_mean %>% 
  group_by(year, month) %>%
  summarise(month_total = sum(Global_active_power))

# Granularity -> month (for each year) & total power
daily_power <- NA_mean %>% 
  group_by(year, month, day) %>%
  summarise(day_total = sum(Global_active_power))



# Creating time series
ts_monthly <- ts(monthly_power$month_total, frequency = 12, start=c(2007,1))

ts_daily <- ts(daily_power$day_total, frequency = 365, start=c(2007,1))



# Plotting my ts
autoplot(ts_monthly)


# Decomposing
#dec_ts_monthly<-decompose(ts_monthly)
dec_ts_monthly<-stl(ts_monthly, s.window = "periodic")

dec_ts_daily <- stl(ts_daily, s.window = "periodic")


# variance for each variable
apply(dec_ts_monthly$time.series, 2, var)/var(ts_monthly)
apply(dec_ts_daily$time.series, 2, var)/var(ts_daily)

#train and test sets
train_monthly_total <- window(ts_monthly, start=c(2007, 1), end=c(2008,12))
test_monthly_total <- window(ts_monthly, start=c(2009, 1))
  
# Creating the model
HW_monthly_total <- HoltWinters(train_monthly_total)

# Creating Holt Winters forecast
FC_monthly_total<-forecast:::forecast.HoltWinters(HW_monthly_total, h=12)

# Checking for residuals of my model
checkresiduals(HW_monthly_total)

# Plotting my forecast comparing with my total time observed
autoplot(ts_monthly)+
  autolayer(FC_monthly_total$mean)

# Checking for the accuracy of my forecast concerning all time observed
HW_accuracy <- accuracy(FC_monthly_total, test_monthly_total)



# Plotting decompose
autoplot(dec_ts_monthly)

# If the remainder mean is close to 0 is a good forecast
mean(dec_ts_monthly$time.series[,"remainder"])











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




