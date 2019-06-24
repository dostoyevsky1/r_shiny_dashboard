library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(RColorBrewer)
library(leaflet)
library(sp)
library(rgdal)
library(lubridate)
library(tseries)
library(forecast)
library(pracma)
library(rsconnect)

traffic_nyc <- read.csv('./traffic_nyc.csv')
traffic_nyc <- traffic_nyc %>% select(-X)


nyc_shape <- readOGR('./nyc_shape.shp')
nyc <- read.csv('./nyc_shape_data.csv')
nyc_shape@data <- nyc %>% select(-X)

tseries_df <- read.csv('./tseries.csv')
tseries_df <- tseries_df %>% select(-X) %>% mutate(Date = as.Date(as.character(Date)))

