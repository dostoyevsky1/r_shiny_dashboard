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



traffic_nyc <- read.csv('C:/Users/MDrozd/Downloads/traffic_nyc.csv')
traffic_nyc <- traffic_nyc %>% select(-X)

nyc_shape <- readOGR('C:/Users/MDrozd/Downloads/dashboardApp/r_shiny_dashboard/nyc_boroughs_shapefiles/nyc_shape.shp')
nyc <- read.csv('C:/Users/MDrozd/Downloads/dashboardApp/r_shiny_dashboard/nyc_shape_data.csv')
nyc <- nyc %>% select(-X)
nyc_shape@data <- nyc

tseries_df <- read.csv('C:/Users/MDrozd/Downloads/dashboardApp/r_shiny_dashboard/tseries.csv')
tseries_df <- tseries_df %>% select(-X) %>% mutate(Date = as.Date(as.character(Date)))

# 
# 
# 
# 
#
# ARMA MODEL
# 
# tseries_df <- traffic_nyc %>% mutate(Mon = match(.$Monthly,month.abb)) %>%
# tidyr::unite(.,col='Date',Year,Mon,sep='-') %>% mutate(Date = as.Date(paste0(Date,'-01'))) %>%
# group_by(Date) %>% summarise(Count=n())
# 
# datevec <- paste0('2018-',01:12,'-01')
# countvec <- c(rep(NA,12))
# 
# tseries_df <- data.frame(Date = append(tseries_df$Date,as.Date(datevec)), Count = append(tseries_df$Count,countvec))
#write.csv(tseries_df, 'tseries.csv')
#
# par(mfrow=c(2,1))
# count_df <- tseries_df$Count
# acf(count_df)
# pacf(count_df)
# 
# acf(diff(log(count_df)))
# pacf(diff(log(count_df)))
# 
#arima(tseries_df$Count,c(1,1,0),xreg = 1:length(count_df))
#arima.fit <- arima(count_df,c(1,1,0),include.mean = T)
#arima.cast <- forecast(arima.fit,h = 12)
# arima.cast <- arima.cast$mean
# arima.trace <- as.data.frame(arima.cast)
# 
# 
# matrix(c(rep(NA,48),arima.trace$x),ncol = 1)
# 
# datevec <- paste0('2018-',01:12,'-01')
# new_df <- matrix(c(c(as.character(tseries_df$Date), datevec),c(tseries_df$Count,rep(NA,12)),c(rep(NA,48),arima.trace$x)),nrow=60)
# 
# new_df <- as.data.frame(new_df)
# colnames(new_df) <- c('Date','Count','Forecast')
# 
# new_df$Count <- new_df$Count %>% as.character() %>% as.numeric()
# new_df$Forecast <- new_df$Forecast %>% as.character() %>% as.numeric()
# new_df$Date <- new_df$Date %>% as.character() %>% as.Date()
#
# 
# write.csv(new_df, 'arma_forecast.csv')

