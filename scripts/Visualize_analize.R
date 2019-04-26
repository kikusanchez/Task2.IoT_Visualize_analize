#### 00. INCLUDES -----------------------------------------------------------------------------------
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,plotly,caret,rpart,gdata,chron,lubridate,padr)


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




