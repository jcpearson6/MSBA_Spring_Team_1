### Team 1 Machine Learning Group Project
### Inspiration for KNN from "KNN experiment for facebook"

#setwd('C:/Users/ashle/Documents/WM/Machine-Learning-II/GroupProject/facebook-v-predicting-check-ins')

## Load Necessary Packages
#install.packages('ranger')
#install.packages('plotly')
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(ranger)
library(randomForest)
library(FNN) #for knn model
#library(ranger)
library(randomForest)

## Read in Dataframe
fbdata <- read.csv('train.csv') ## takes forever to read


##### CLEAN DATA ####

## Filter Data
## Take subset of a 1 km x 1 km section of the Artificial World
fbdata %>% filter(x>1.5, x<2, y>1.5, y<2) -> fbdata
head(fbdata)


## Time is given in Minutes, so we can calculate different time values
## and append those time values into the dataframe
fbdata$hour <- (fbdata$time/60) %% 24
fbdata$weekday <- (fbdata$time/(60/24))%%7
fbdata$month <- (fbdata$time/(60*24*30))%%12
fbdata$year <- fbdata$time/(60*24*365)
fbdata$day <- fbdata$time/(60*24) %% 365


## Split dataset into training and validation set
## Chose the validation set to be recent check-ins so that our validation structure is 
## similar to the one kaggle does behind the scenes on the test set
fbtrain = fbdata[fbdata$time < 7.3e5,] #using 7 days for recent check-ins
fbval = fbdata[fbdata$time >= 7.3e5,]



##### DATA EXPLORATION #####
## View a Plot of Training Data
ggplot(fbtrain, aes(x,y))+geom_point(aes(color=place_id))+theme_minimal()+
  theme(legend.position='none')+ggtitle("Check-ins colored by place_id")

## Explore density of check-ins by hour
fbtrain %>%
  #sample_frac(.01) %>%
  ggplot(aes(x = hour)) +
  geom_density()

## Explore density of check-ins by day
fbtrain %>%
  #sample_frac(.01) %>%
  ggplot(aes(x = day)) +
  geom_density()

## Explore density of check-ins by weekday
fbtrain %>%
  #sample_frac(.01) %>%
  ggplot(aes(x = weekday)) +
  geom_density()

## Explore density of check-ins by year
fbtrain %>%
  #sample_frac(.01) %>%
  ggplot(aes(x = year)) +
  geom_density()

## Explore distribution of place ids 
place_ids <-
  fbtrain %>%
  #sample_frac(0.05) %>%
  group_by(place_id) %>%
  summarise(freq = n())

place_ids %>%
  ggplot(aes(x = freq)) +
  geom_density()

## Plot place ID by time of day in 3D chart
plot_ly(data = fbval, x = ~x, y = ~y, z = ~hour, color = ~place_id,  
        type = "scatter3d", mode = "markers", 
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Time of Day")

## Plot place ID by day of week in 3D chart 
plot_ly(data = fbval, x = ~x , y = ~y, z = ~weekday, color = ~place_id,  
        type = "scatter3d", mode = "markers", 
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Day of Week")
## this plot does not show any significant trends based on day of the week
## each place_id seems to have consistent representation based on each day of the week

## Remove all place_ids with fewer than 500 occurrences
## We tested a number of different ns, and found that the higher the n the higher the accuracy
fbtrain %>% count(place_id) %>% filter(n > 400) -> ids
fbval = fbtrain[fbtrain$place_id %in% ids$place_id,]

##Count number of unique place_id
length(unique(fbtrain$place_id))

##### KNN MODULE #####
## Figuring out optimal weights for scaling the variables is tricky due to knn's sensitivity, 
## took inspiration from the competition in choosing the weight below; A combination of using 
## a validation set (not this one) and eye-balling
weight1 = 2
weight2 = 125
weight3 = 500

#creating a function that called create_matrix, that will create
#a matrix for us. Using the argument train
create_matrix = function(train) { 
  cbind(weight1*train$y, #training y multipied by s
        train$x, #training x
        train$hour/weight2, #training hour divided by 1
        train$weekday/weight3, #training weekday divided by w
        train$year/weight3, #training year divided by w
        train$month/weight3, #training month divided by w
        train$time/(weight3*60*24*7)) #dividing the training by the time portion with the different time values multiplied by w
}

# We decided to create two more matrices to determine (using a knn model) what predictors make a 
# difference in the predictions

# Second Matrix
create_matrix_1 = function(train) {
  cbind(weight1*train$y,
        train$x,
        train$hour/weight2)}

# Third Matrix
create_matrix_2 = function(train) {
  cbind(weight1*train$y,
        train$x)}



#Iterate through predictors and different cluster amounts
accuracy = c(0, 3*6)
counter = 0
pred_lists = c(create_matrix,create_matrix_1,create_matrix_2)
klusters = c(5,10,15,20,25,30)
for (i in klusters){
  for (j in pred_lists){
    counter = counter +1
    j_train = j(fbtrain)
    #print('j_train')
    j_val = j(fbval)
    #print('j_val')
    model = knn(train = j_train, test = j_val, cl = fbtrain$place_id, k = i)
    #print('modle worked')
    preds <- as.character(model)
    actual <- as.character(fbval$place_id)
    accuracy[counter] = mean(actual == preds)
  }
}

best_acc = max(accuracy)
## Best accuracy of  0.8597212
## But this is only predicting about 37 classes 
## The key to high accuracy is breaking world down into small blocks, and targeting high frequency place ids 
best_model = which.max(accuracy)# model 1 - create_matrix_1 & 5 klusters


##### RANDOM FOREST (WITH RANGER PACKAGE) #####

## RandomForest

## Set Seed 
set.seed(1)

## Create random forest model using ranger library 
fbtrain$place_id <- as.factor(fbtrain$place_id) # factor for classification

## Initialize empty accuracy list 
accuracy <- c()

## Define start time and iterate through list of tree amounts 
Sys.time()
for (i in c(100, 200, 300, 400, 500)){
  ra.fb <- ranger(place_id~.-row_id -time,
                  fbtrain,
                  num.trees = i,
                  write.forest = TRUE,
                  importance = 'impurity')
  pred <- predict(ra.fb, fbval)
  accuracy <- append(accuracy, mean(pred$predictions==fbval$place_id))
}
accuracy
Sys.time() 
which.max(accuracy)
# The best model is num.trees = 500, with the accuracy rate of 0.5791473

