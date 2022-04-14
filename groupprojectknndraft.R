### Team 1 Machine Learning Group Project
### Inspiration for KNN from "KNN experiment for facebook"

setwd('C:/Users/ashle/Documents/WM/Machine-Learning-II/GroupProject/facebook-v-predicting-check-ins')

## Load Necessary Packages
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(xgboost)
library(ranger)
library(randomForest)
library(FNN) #for knn model

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
#train.rows <- sample(nrow(fbdata), nrow(fbdata) * .8)
#fbtrain <- fbdata[train.rows, ] 
#fbtest <- fbdata[-train.rows, ]


## Split dataset into training and validation set
## Chose the validation set to be recent check-ins so that our validation structure is 
## similar to the one kaggle does behind the scenes on the test set
fbtrain = fbdata[fbdata$time < 7.3e5,] #using 7 days for recent check-ins
fbval = fbdata[fbdata$time >= 7.3e5,]


## View a Plot of Training Data
ggplot(fbtrain, aes(x,y))+geom_point(aes(color=place_id))+theme_minimal()+
  theme(legend.position='none')+ggtitle("Check-ins colored by place_id")

## Add Hour time dimension to Plot
## as our third variable
fbtrain %>% count(place_id) %>% filter(n > 500) -> ids
fbval = fbtrain[fbtrain$place_id %in% ids$place_id,]

plot_ly(data = fbval, x = ~x , y = ~y, z = ~hour, color = ~place_id,  
        type = "scatter3d", mode = "markers", 
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Time of Day")

## Add Day of Week dimension
plot_ly(data = fbval, x = ~x , y = ~y, z = ~weekday, color = ~place_id,  
        type = "scatter3d", mode = "markers", 
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Day of Week")
## this plot does not show any significant trends based on day of the week
## each place_id seems to have consistent representation based on each day of the week

##Count number of unique place_id
length(unique(fbtrain$place_id))

## Remove all place_ids with fewer than 10 occurrences
fbtrain %>% count(place_id) %>% filter(n > 10) -> ids
fbtrain = fbtrain[fbtrain$place_id %in% ids$place_id,]

## Figuring out optimal weights for scaling the variables is tricky due to knn's sensitivity, 
## took inspiration from the competition in choosing the weight below; A combination of using 
## a validation set (not this one) and eye-balling
s = 2
l = 125
w = 500

#creating a function that called create_matrix, that will create
#a matrix for us. Using the argument train
create_matrix = function(train) { 
  cbind(s*train$y, #training y multipied by s
        train$x, #training x
        train$hour/l, #training hour divided by 1
        train$weekday/w, #training weekday divided by w
        train$year/w, #training year divided by w
        train$month/w, #training month divided by w
        train$time/(w*60*24*7)) #dividing the training by the time portion with the different time values multiplied by w
}

#creating a matrix from the split fbtrain data
x = create_matrix(fbtrain)
#creating a matrix from the split fbval data
x_val = create_matrix(fbval)

#creating a knn model using the two matrices that we just created
library(FNN)
model_knn = knn(train = x, test = x_val, cl = fbtrain$place_id, k = 15)

#changing our knn model to a character, called predictions
preds <- as.character(model_knn)
#changing our fbval place id to a character, called actual
actual <- as.character(fbval$place_id)
mean(actual == preds)

