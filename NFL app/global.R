options(shiny.trace = F)  

#####---base
library(shiny)
library(shiny.semantic)    
library(semantic.dashboard) 
library(readxl)   
library(lubridate)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(ggimage)
library(DT)

source("modules/Dashboard_file.R",local=TRUE)    

pace_data = read.csv(paste(getwd(),"data_folder/pace_data.csv", sep = "/"))

