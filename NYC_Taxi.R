#Load libraries and helper functions
#install.packages("alluvial")
#install.packages("leaflet.extras")
library(ggplot2) # visualisation
library(scales) # visualisation
library(RColorBrewer) # visualisation
library(alluvial) # visualisation
library(dplyr) # data manipulation
library(readr) # input/output
library(data.table) # data manipulation
library(tibble) # data wrangling
library(tidyr) # data wrangling
library(stringr) # string manipulation
library(forcats) # factor manipulation
library(lubridate) # date and time
library(geosphere) # geospatial locations
library(leaflet) # maps
library(leaflet.extras) # maps
library(maps) # maps
library(xgboost) # modelling
library(caret) # modelling
library(TTR)
library(png)
library(jpeg)
library(bmp)
library(IM)
library(plotly)
library(Matrix)
library(dygraphs)
library(xts)

# 1. EDA
#Load data
train <- as.tibble(fread('train.csv'))
test <- as.tibble(fread('test.csv'))
sample_submit <- as.tibble(fread('sample_submission.csv'))
#File structure and content
#FOR TRAIN DATA
summary(train)
glimpse(train)
#FOR TEST DATA
summary(test)
glimpse(test)

# Missing values
sum(is.na(train))

sum(is.na(test))
#Reformating features
#For our following analysis, we will turn the data and time from characters into date objects. We also recode vendor_id as a factor. This makes it easier to visualise relationships that involve these features.
train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))
# Consistency check
#It is worth checking whether the trip_durations are consistent with the intervals between the pickup_datetime and dropoff_datetime. Presumably the former were directly computed from the latter, but you never know. Below, the check variable shows “TRUE” if the two intervals are not consistent:
train %>%
  mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime)) + trip_duration) > 0) %>%
  select(check, pickup_datetime, dropoff_datetime, trip_duration) %>%
  group_by(check) %>%
  count()
#And we find that everything fits perfectly.

#Individual feature visualisations
# investigating some variations together with the distributions of passenger_count and vendor_id.
#For number of passenger in a ride
train %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")
#The vast majority of rides had only a single passenger, with two passengers being the (distant) second most popular option.

#Whice vendor_id has more number of trips
train %>%
  ggplot(aes(vendor_id, fill = vendor_id)) +
  geom_bar() +
  theme(legend.position = "none")
#Vendor 2 has significantly more trips in this data set than vendor 1 (note the logarithmic y-axis). This is true for every day of the week.

#Total number of pickups versus day of the week
train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")
#We find an interesting pattern with Monday being the quietest day and Friday very busy. This is the same for the two different vendors, with vendor_id == 2 showing significantly higher trip numbers.

#Total number of pickups versus Hour of the day.
train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")
#As one would intuitively expect, there is a strong dip during the early morning hours. There we also see not much difference between the two vendors. We find another dip around 4pm and then the numbers increase towards the evening.

#The trip volume per hour of the day depends somewhat on the month and strongly on the day of the week:
train %>%
  mutate(hpick = hour(pickup_datetime),
         Month = factor(month(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, Month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = Month)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")

train %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color = wday)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")
#We find: January and June have fewer trips, whereas March and April are busier months. This tendency is observed for both vendor_ids. The weekend (Sat and Sun, plus Fri to an extend) have higher trip numbers during the early morning ours but lower ones in the morning between 5 and 10, which can most likely be attributed to the contrast between NYC business days and weekend night life. In addition, trip numbers drop on a Sunday evening/night.


test_kaggle = fread("test.csv", verbose = F, showProgress = F)
train_kaggle = fread("train.csv", verbose = F, showProgress = F)
#Examining the top rows of the data:
head(train_kaggle)

#Add Features
#For each trip the distance (km) is calculated and added to train and test dataframe. The average speed (km/h) is then calculated and added to the dataframe. The hour of pick up and month the trip took place are taken from the pick up time/date data and added.
# function to get distance between points
distance = function(lat1, long1, lat2, long2) {
  deglen = 110.25
  x = lat1 - lat2
  y = (long1 - long2) * cos(lat1)
  return(deglen * sqrt(x * x + y * y))
}

# Add distance to train_kaggle data
train_kaggle$distance = distance(train_kaggle$pickup_latitude, train_kaggle$pickup_longitude, 
                                 train_kaggle$dropoff_latitude, train_kaggle$dropoff_longitude)
test_kaggle$distance = distance(test_kaggle$pickup_latitude, test_kaggle$pickup_longitude, 
                                test_kaggle$dropoff_latitude, test_kaggle$dropoff_longitude)

# Add speed
train_kaggle$speed = train_kaggle$distance/(train_kaggle$trip_duration/(60 * 
                                                                          60))
test_kaggle$speed = test_kaggle$distance/(test_kaggle$trip_duration/(60 * 60))

# Add hour of pickup

# To train data
t.lub.train <- ymd_hms(train_kaggle$pickup_datetime)

train_kaggle$pickup_hour <- as.numeric(format(t.lub.train, "%H")) + as.numeric(format(t.lub.train, 
                                                                                      "%M"))/60

# To test data
t.lub.test <- ymd_hms(test_kaggle$pickup_datetime)

test_kaggle$pickup_hour <- as.numeric(format(t.lub.test, "%H")) + as.numeric(format(t.lub.test, 
                                                                                    "%M"))/60

# Add month
train_kaggle$month <- as.factor(format(t.lub.train, "%m"))
test_kaggle$month <- as.factor(format(t.lub.test, "%m"))
#Looking at the top rows of the data now:
head(train_kaggle)
#Examining data for outliers
par(mfrow = c(1,2))
hist(train_kaggle$trip_duration/60,main='Duration (Minutes)',xlim = c(0,80),breaks = c(0,10,20,30,40,50,60,70,80,500000))
boxplot(train_kaggle$trip_duration/60,main='Duration (Minutes)',ylim=c(0,80))

hist(train_kaggle$distance,main = 'Distance (km)',breaks=c(0,5,10,15,20,25,30,35,500000),xlim=c(0,40))
boxplot(train_kaggle$distance,main = 'Distance (km)',ylim=c(0,20))

#The above graphs show the presence of a number of outliers. We can remove any items with distance > 20 km and duration > 1 hour, as well as items showing a distance of 0 km or 0 minutes.
train_kaggle = train_kaggle[train_kaggle$distance != 0, ]
train_kaggle = train_kaggle[train_kaggle$trip_duration != 0, ]
train_kaggle = train_kaggle[train_kaggle$distance < 20, ]
train_kaggle = train_kaggle[train_kaggle$trip_duration < 3600, ]

set.seed(0)
train <- fread("train.csv", showProgress = FALSE)

#Pickup and Dropoff Heatmap
#' To create an accurate heatmap we need to make sure that time is treated cyclically;
#' that is, Monday's density estimation should include some of Sunday's and Tuesday's 
#' data.  
densityCalc <- function(dt) {
  #' First we need to create our time variables.  The `lubridate` pacakage makes this 
  #' simple. 
  dow <- wday(dt)
  tod <- hour(dt) + minute(dt)/60 + second(dt)/3600
  time <- tod + 24*(dow-1)
  
  #' We will calculate a density of the number of trips for each day.  Each density
  #' will be calculated over a window from 0 to 24.  To do this we subtract time off
  #' of the linear time variable that we created above. The results are stored in a
  #' matrix and then scaled to lie in a range from 0 to 100.
  density_matrix <- matrix(0, 7, 1441)
  for(k in 1:7)  {
    # shift time down k days
    t <- train[, time - 24*(k-1)]
    
    # center current day (subtract half a week)
    t <- (t+3.5*24) %% (24*7) - 3.5*24
    
    # calculate density over [0, 24]
    ind <- 7-((k+5)%%7) # reverse position in array
    density_matrix[ind, ] <- density(x=t, from=0, to=24, width=2, n=1441)$y  
  }
  
  # map from 0 to 100
  density_matrix <- round(density_matrix/max(density_matrix), 5)
  density_matrix
}


#' `plotly` offers high quality interactive graphics.  
heat_ly <- function(mat, ...) {
  # make order factor labels for the x axis
  mins <- sprintf(":%02d", 0:59)
  x_labels <- c(
    paste0(12, mins, " am"), sapply(1:11, paste0, mins, " am"),
    paste0(12, mins, " pm"), sapply(1:11, paste0, mins, " pm"),
    "12:00 am "
  )
  
  plot_ly(
    x = factor(x_labels, levels=x_labels[-1441]),
    z = mat,
    width = 900, height = 450,
    colors = colorRampPalette(brewer.pal(9,"YlOrRd"))(100),
    type = "heatmap") %>% 
    layout(
      margin = list(l=90, b=90),
      yaxis = list(
        showticklabels = TRUE,
        ticks = "",
        tickmode = "array",
        showticklabels = TRUE, 
        tickvals = 0:6,
        ticktext = rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      ),
      xaxis = list(
        showticklabels = TRUE,
        tickangle = 45,
        tickmode = "array",
        tickvals = x_labels[seq(1, 1441, 60)],
        ticktext = gsub(":00", "", x_labels[seq(1, 1441, 60)])
      ),
      ...
    ) %>% 
    colorbar(
      ticks = "",
      title = "Demand",
      titlefont = list(size=14),
      tickmode = "array",
      tickvals = seq(min(mat), 1, length=3),
      ticktext = c(" low", " med", " high")
    )
}

#PICKUPS
d <- densityCalc(train$pickup_datetime)
heat_ly(d, title = "Pickup Demand")
#DROPOFFS
d <- densityCalc(train$dropoff_datetime)
heat_ly(d, title = "Dropoff Demand")


#Exploratory Plots
#1. Trip Duration by Time of Day
# Take sample data set
sample_data = train_kaggle[sample(1:dim(train_kaggle)[1], 20000), ]

# Plot
ggplot(mapping = aes(x = sample_data$pickup_hour, y = sample_data$trip_duration/60)) + 
  geom_point(cex = 0.2, alpha = 0.2) + geom_smooth() + scale_x_continuous(breaks = 0:24) + 
  xlab(label = "Pickup time") + ylab(label = "Trip average duration") + theme(legend.position = "none") + 
  ggtitle("Trip duration by time of Day") + scale_y_continuous(limits = c(0, 
                                                                          30))
#Leaflet Sample Display
#A map view of 1000 random Pickup locations using leaflet is shown below:
sample_data = train_kaggle[sample(1:dim(train_kaggle)[1],1000),]

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(
    lng=sample_data$pickup_longitude, 
    lat=sample_data$pickup_latitude,
    clusterOptions = markerClusterOptions()
    
  )

#Map view of corresponding drop off locations using leaflet
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(
    lng=sample_data$dropoff_longitude, 
    lat=sample_data$dropoff_latitude,
    clusterOptions = markerClusterOptions()
  )
#Dummy Variable Creation
#As there is not a linear relationship between the time of day and trip duration dummy variables for hour of pickup are created for regression.
# Training Data
hour.dummies = model.matrix(~cut(train_kaggle$pickup_hour,breaks = c(0,3,6,7,8,9,19,21,24),include.lowest = T))
hour.dummies = hour.dummies[,-1]
colnames(hour.dummies)=paste('hr',c(3,6,7,8,9,19,21))
hour.dummies = hour.dummies==1
train_kaggle = cbind(train_kaggle,hour.dummies)


#Test Data
hour.dummies.test = model.matrix(~cut(test_kaggle$pickup_hour,breaks = c(0,3,6,7,8,9,19,21,24),include.lowest = T))
hour.dummies.test = hour.dummies.test[,-1]
colnames(hour.dummies.test)=paste('hr',c(3,6,7,8,9,19,21))
hour.dummies.test = hour.dummies.test==1
test_kaggle = cbind(test_kaggle,hour.dummies.test)

#Similarly for the month:
# Train data
month.dummies = model.matrix(~train_kaggle$month)
month.dummies = month.dummies[,-1]
colnames(month.dummies)=paste('mnth',2:6)
month.dummies = month.dummies==1
train_kaggle = cbind(train_kaggle,month.dummies)

# test data
month.dummies = model.matrix(~test_kaggle$month)
month.dummies = month.dummies[,-1]
colnames(month.dummies)=paste('mnth',2:6)
month.dummies = month.dummies==1
test_kaggle = cbind(test_kaggle,month.dummies)

#The data now looks like this:
head(train_kaggle)

#Clustering of pickup and drop off Locations
#So far features collected for regression include distance, time of day and month of year. In order to improve prediction accuracy the route should be taken into account. This can be done by clustering drop off and pick up locations and including combinations as dummy variables.
#First k-means clustering of pickup and drop off locations, with 50 clusters for each. Test data is included with training data for clustering to ensure test data has the cluster information available for prediction.
# Define number of clusters
cen = 10

pickup = rbind(cbind(train_kaggle$pickup_longitude,train_kaggle$pickup_latitude),cbind(test_kaggle$pickup_longitude,test_kaggle$pickup_latitude))
clus_pickup = kmeans(pickup, centers = cen, nstart = 1)

train_kaggle$pickupclus = as.factor(clus_pickup$cluster[1:dim(train_kaggle)[1]])
test_kaggle$pickupclus = as.factor(clus_pickup$cluster[(dim(train_kaggle)[1]+1):length(clus_pickup$cluster)])

# cluster locations for dropoff

dropoff = rbind(cbind(train_kaggle$dropoff_longitude,train_kaggle$dropoff_latitude),cbind(test_kaggle$dropoff_longitude,test_kaggle$dropoff_latitude))
clus_dropoff = kmeans(dropoff, centers = cen, nstart = 1)

train_kaggle$dropoffclus = as.factor(clus_dropoff$cluster[1:dim(train_kaggle)[1]])
test_kaggle$dropoffclus = as.factor(clus_dropoff$cluster[(dim(train_kaggle)[1]+1):length(clus_dropoff$cluster)])

#The pick up and drop off locations are plotted below coloured by cluster:
sample_data = train_kaggle[sample(1:dim(train_kaggle)[1],100000),]

ggplot(mapping=aes(x=sample_data$pickup_longitude,y=sample_data$pickup_latitude))+
  geom_point(alpha=.2,cex=0.0001,aes(colour=sample_data$pickupclus))+
  scale_x_continuous(limits = c(-74.02,-73.85))+
  scale_y_continuous(limits = c(40.7,40.85))+
  ggtitle('Pick up locations clustered')+
  guides(fill=F)+
  xlab('Longitude')+ylab('Latitude')+ggtitle('Plot of pick up locations with clustering')+
  theme(legend.position = 'none')

# dropoff location by cluster----
ggplot(mapping=aes(x=sample_data$dropoff_longitude,y=sample_data$dropoff_latitude))+
  geom_point(alpha=.2,cex=0.0001,aes(colour=sample_data$dropoffclus))+
  scale_x_continuous(limits = c(-74.02,-73.85))+
  scale_y_continuous(limits = c(40.7,40.85))+
  ggtitle('Drop off locations clustered')+
  theme(legend.position = 'none')+
  xlab('Longitude')+ylab('Latitude')+ggtitle('Plot of drop off locations with clustering')

#We can now create dummy variables for routes, eg. for cluster 2 to cluster 11 the variable will be 2/11 and any route going between these clusters will have the boolean for this feature set as true.
# build dummys for one cluster to another
comb=as.factor(paste(as.character(clus_pickup$cluster),as.character(clus_dropoff$cluster),sep='/'))
route.dummies = (model.matrix(~comb))

route.dummies=route.dummies==1



# Add to train and test data

train_kaggle = cbind(train_kaggle,route.dummies[1:dim(train_kaggle)[1],])
test_kaggle = cbind(test_kaggle,route.dummies[(dim(train_kaggle)[1]+1):length(clus_pickup$cluster),])

#Now a number of features are removed to allow model training only using dummy variables and distance (km)
model.data = train_kaggle[,-c(1:10,13,14,15,28,29,30)]

test.model.data = test_kaggle[,-c(1:9,11,12,13,27,28,29)]

#Fit Model
#Linear regression is used on a training set of 90% of training set provided by kaggle. Firstly a model is produced using only the distance variable to get a baseline idea of performance.
n=0.9
N = dim(model.data)[1]
train_ind = sample(1:N,n*N)
test_ind = setdiff(1:N,train_ind)

train = model.data[train_ind,]
test = model.data[test_ind,]

#Basic Model
fitlm = lm(trip_duration~distance, data=train)
fitlm
pred=predict.lm(fitlm,newdata = test)
pred


pred[pred<0] = 5
pred[is.na(pred)]=0

#Have a look at results:
plot(test$trip_duration[1:50]/60,type='h',ylab = 'duration (minutes)')
points(pred[1:50]/60,col='red',pch=15)

#Generally the predictions do not appear to be very good. The model seems to have low variance, high bias (as expected for a simple linear model).

#Linear regression is now used on the full set of features, for the same set (90%).
fitlm = lm(trip_duration~., data=train)
summary(fitlm)
pred=predict.lm(fitlm,newdata = test)
pred
pred[pred<0] = 5
pred[is.na(pred)]=0

#Have a look at results:
plot(test$trip_duration[1:50]/60,type='h',ylab = 'duration (minutes)')
points(pred[1:50]/60,col='red',pch=15)

#The plot shows that predictions in red generally agree with the actual duration in black, certainly outperforming the basic distance model.
#Now using the model on the kaggle test set:
pred = predict.lm(fitlm,newdata = test_kaggle)

pred[pred<0] = 2
pred[is.na(pred)]=0
test_kaggle$trip_duration=pred
out = test_kaggle[,c(1,128)]


write.csv(out,file='results.csv',row.names = F)

#time Series

#importing the data from source
train_timeseries <- fread("train.csv")

#extracting the date,hour and day 
train_timeseries$date1 <- as.Date(train_timeseries$pickup_datetime)
train_timeseries$hour<-format(as.POSIXct(train_timeseries$pickup_datetime, format = "%Y-%m-%d %H:%M:%S"),  "%H")
train_timeseries$day<-format(as.POSIXct(train_timeseries$pickup_datetime, format = "%Y-%m-%d %H:%M:%S"),  "%A")

#identifying records
print(length(unique(train_timeseries$date1)))

#get grouped data based on weekdays
train_raw <- train_timeseries %>%
  group_by(train_timeseries$day)%>%
  summarise(number_of_rides = n())

#removing rows with null values
train_raw<-na.omit(train_raw)

#arranging the data through week sequence - where starting day is "Monday" and last day is "Sunday".
names(train_raw)<-c("weekday","Number_of_rides")
train_raw$weekday <- factor(train_raw$weekday, levels= c( "Monday", 
                                                          "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
train_raw<-train_raw[order(train_raw$weekday), ]


#get grouped data based on hours of ride
train_hour <- train_timeseries %>%
  group_by(train_timeseries$hour)%>%
  summarise(number_of_rides = n())

#removing rows with null values
train_hour<-na.omit(train_hour)

#training the hour data to create the time series with frequency 24
#this will porovide hourly data for timeseries
tsData_hour <- ts(train_hour[,2], start=c(1), frequency=24)
tsData_hour
#plotting the time series
plot.ts(tsData_hour)

#decomposing the timeline using SMA function
hourlySMA <- SMA(tsData_hour, n=1)

#plotting the decomposed data
plot.ts(hourlySMA)


#training the weekday data to create the time series with frequency 7
#this will porovide hdata for each day in a week
tsData_weekday <- ts(train_raw[,2], start=c(1), frequency=7)
tsData_weekday

#plotting the time series
plot.ts(tsData_weekday)

#decomposing the timeline using SMA function
weekdaySMA <- SMA(tsData_weekday,n=1)
#plotting the decomposed data
plot.ts(weekdaySMA)

