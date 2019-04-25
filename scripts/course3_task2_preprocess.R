#### 00. INCLUDES ####
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi,dplyr,tidyr,ggplot2,caret,rpart,gdata,chron,lubridate,padr)


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


#### 01. PRE-PROCESS ####









