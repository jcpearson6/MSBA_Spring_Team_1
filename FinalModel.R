## Load Necessary Packages
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(xgboost)
library(ranger)
library(randomForest)

## Read in Dataframe
fbdata <- read.csv('train.csv') ## takes forever to read

## Filter Data
## Take subset of a 1 km x 1 km section of the Artificial World
fbdata %>% filter(x>1.5, x<2, y>1.5, y<2) -> fbdata
head(fbdata)



##Time is given in Minutes, so we can calculate different time values
## and append those time values into the dataframe
fbdata$hour <- (fbdata$time/60) %% 24
fbdata$weekday <- (fbdata$time/(60/24))%%7
fbdata$month <- (fbdata$time/(60*24*30))%%12
fbdata$year <- fbdata$time/(60*24*365)
fbdata$day <- fbdata$time/(60*24) %% 365

## Create New Train/Test Sets
train.rows <- sample(nrow(fbdata), nrow(fbdata) * .8)
fbtrain <- fbdata[train.rows, ] 
fbtest <- fbdata[-train.rows, ]

## View a Plot of Training Data
ggplot(fbtrain, aes(x,y))+geom_point(aes(color=place_id))+theme_minimal()+
  theme(legend.position='none')+ggtitle("Check-ins colored by place_id")

## Add Hour time dimension to Plot
fbtrain %>% count(place_id) %>% filter(n > 500) -> ids
fbtrainz = fbtrain[fbtrain$place_id %in% ids$place_id,]

plot_ly(data = fbtrainz, x = ~x , y = ~y, z = ~hour, color = ~place_id,  
        type = "scatter3d", mode = "markers", 
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Time of Day")

## Add Day of Week dimension
plot_ly(data = fbtrainz, x = ~x , y = ~y, z = ~weekday, color = ~place_id,  
        type = "scatter3d", mode = "markers", 
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Day of Week")
## this plot does not show any siginficant trends based on day of the week
## each place_id seems to have consistent representation based on each day of the week

##Count number of unique place_id
length(unique(fbtrain$place_id))

## Remove all place_ids with fewer than 10 occurrences
fbtrain %>% count(place_id) %>% filter(n > 10) -> ids
fbtrain = fbtrain[fbtrain$place_id %in% ids$place_id,]
