#Ruochen Zhong 912888970

library(ggplot2)
library(ggmap)
library(geosphere)
library(lubridate)
library(ggrepel)
hw4data <- read.csv("/Users/apple/Desktop/the_hawks.csv")

#--------Q1----------
# find the center of the map
location <- cbind(hw4data$long, hw4data$lat)
central_location <- apply(location,2,mean)
# draw the map 
google_map <- get_map((central_location - 0.1), maptype = "terrain", zoom = 10)
graph_1 <- ggmap(google_map,extent = "panel")
print(graph_1)
# add all observations in the map
graph_1_revise <- graph_1 + geom_point(aes(x = long,y = lat,color = factor(tag)), alpha = 0.3, data = hw4data) +
                  ggtitle("Distribution of five hawks") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_color_discrete(name="tag") +
                  labs(x = "Longitude", y = "Latitude")
print(graph_1_revise)                 

#---------Q2---------
# find out which two hawks have arrival sequences
which_sequence <- subset(hw4data, hw4data$stage == "arrival")
unique(which_sequence$tag)
hawk1 <- subset(which_sequence, which_sequence$tag == "105928")
hawk2 <- subset(which_sequence, which_sequence$tag == "105936")
# fing the center of the map for each hawk
hawk1_central <- apply(cbind(hawk1$long, hawk1$lat), 2, median)
hawk2_central <- apply(cbind(hawk2$long, hawk2$lat), 2, median)
# draw the arriving sequence for tag 2
center1 <- get_map(hawk1_central, maptype = "terrain", zoom = 15)
graph_2a <- ggmap(center1,extent = "panel")
print(graph_2a)
L<-cbind(hawk1$long,hawk1$lat)
L2<-cbind(L[-nrow(L),],L[-1,])
L2 <- as.data.frame(L2)
names(L2)<-c("long1","lat1","long2","lat2")
graph_2a_revise <- graph_2a + geom_path(aes(x=long,y=lat),color = 'red',data = hawk1) +
                   geom_segment(aes(x=long1,y=lat1,xend=long2,yend=lat2),data=L2, color = 'red',arrow = arrow(length = unit(0.3, "cm"))) +
                   geom_point(aes(x=long,y=lat,color=speed, size=height), data = hawk1, alpha = 0.9) +
                   ggtitle("Arrival Sequences of Hawk tag '105928' ") +
                   theme(plot.title = element_text(hjust = 0.5)) +
                   labs(x = "Longitude", y = "Latitude")
print(graph_2a_revise)
# draw the arriving sequence for tag 4
center2 <- get_map(hawk2_central, maptype = "terrain", zoom = 15)
graph_2b <- ggmap(center2,extent = "panel")
print(graph_2b)
L3<-cbind(hawk2$long,hawk2$lat)
L4<-cbind(L3[-nrow(L3),],L3[-1,])
L4 <- as.data.frame(L4)
names(L4)<-c("long1","lat1","long2","lat2")
graph_2b_revise <- graph_2b + geom_path(aes(x=long,y=lat),data = hawk2, color = 'red') +
                   geom_segment(aes(x=long1,y=lat1,xend=long2,yend=lat2),data=L4, color = 'red',arrow = arrow(length = unit(0.3, "cm"))) +
                   geom_point(aes(x=long,y=lat,color=speed, size=height), data = hawk2, alpha = 0.9) +
                   ggtitle("Arrival Sequences of Hawk tag '105936' ") +
                   theme(plot.title = element_text(hjust = 0.5)) +
                   labs(x = "Longitude", y = "Latitude")
print(graph_2b_revise)

#--------Q3----------
# change the class of the time and then subset each hawks
hw4data$time <- as.POSIXct(hw4data$time)
tag1 <- subset(hw4data,hw4data$tag == '105923')
tag2 <- subset(hw4data,hw4data$tag == '105928')
tag3 <- subset(hw4data,hw4data$tag == '105930')
tag4 <- subset(hw4data,hw4data$tag == '105936')
tag5 <- subset(hw4data,hw4data$tag == '117527')

#use the median of those observation to be the nest
nest1 <- apply(cbind(tag1$long,tag1$lat),2,median)
nest2 <- apply(cbind(tag2$long,tag2$lat),2,median)
nest3 <- apply(cbind(tag3$long,tag3$lat),2,median)
nest4 <- apply(cbind(tag4$long,tag4$lat),2,median)
nest5 <- apply(cbind(tag5$long,tag5$lat),2,median)

# write a function to calculate the distance from the nest of each hawks
calculate_distance <- function(n,d,long,lat){
  distance <- numeric(d)
  for (i in 1:d) {
  distance[i] = distGeo(n,(cbind(long,lat)[i,]))/1609
  }
  M <- as.matrix(distance)
return(M)
}

# For tag 1, draw the time series graph of their distance to the nest 
distance1 <- calculate_distance(nest1,578,long = tag1$long, lat = tag1$lat)
tag1$distance <- distance1
leave_check1 <- ggplot(aes(x = time,y = distance),data = tag1) +
                geom_line( color = 'blue', size = 0.5) +
                geom_point() +
                scale_x_datetime(date_breaks = "5 days") +
                ggtitle(" Distance from Nest for tag '105923' ") +
                theme(plot.title = element_text(hjust = 0.5)) +
                labs(x = "Time", y = "Distance(miles)")
print(leave_check1)

# For tag 2, draw the time series graph of their distance to the nest 
distance2 <- calculate_distance(nest2,1706,long = tag2$long, lat = tag2$lat)
tag2$distance <- distance2
leave_check2 <- ggplot(aes(x = time,y = distance),data = tag2) +
                geom_line( color = 'blue', size = 0.5) +
                geom_point() +
                scale_x_datetime(date_breaks = "15 days") +
                ggtitle(" Distance from Nest for tag '105928' ") +
                theme(plot.title = element_text(hjust = 0.5)) +
                labs(x = "Time", y = "Distance(miles)")
print(leave_check2)
# find the departure time interval of tag 2
date_one <- as.POSIXct("2012-9-13 19:00:00")
date_two <- as.POSIXct("2012-9-14 03:00:00")
difftime(date_two, date_one, units = 'days')

# For tag 3, draw the time series graph of their distance to the nest 
distance3 <- calculate_distance(nest3,1747,long = tag3$long, lat = tag3$lat)
tag3$distance <- distance3
leave_check3 <- ggplot(aes(x = time,y = distance),data = tag3) +
                geom_line( color = 'blue', size = 0.5) +
                geom_point() +
                scale_x_datetime(date_breaks = "15 days") +
                ggtitle(" Distance from Nest for tag '105930' ") +
                theme(plot.title = element_text(hjust = 0.5)) +
                labs(x = "Time", y = "Distance(miles)")
print(leave_check3)

#For tag 4, draw the time series graph of their distance to the nest 
distance4 <- calculate_distance(nest4,785,long = tag4$long, lat = tag4$lat)
tag4$distance <- distance4
leave_check4 <- ggplot(aes(x = time,y = distance),data = tag4) +
                geom_line( color = 'blue', size = 0.5) +
                geom_point() +
                scale_x_datetime(date_breaks = "15 days") +
                ggtitle(" Distance from Nest for tag '105936' ") +
                theme(plot.title = element_text(hjust = 0.5)) +
                labs(x = "Time", y = "Distance(miles)")
print(leave_check4)

# find the departure time interval for tag 4
date_three <- as.POSIXct("2012-08-05 16:00:00")
date_four <- as.POSIXct("2012-08-06 02:00:00")
difftime(date_four, date_three, units = 'days')

#For tag 5, draw the time series graph of their distance to the nest 
distance5 <- calculate_distance(nest5,324,long = tag5$long, lat = tag5$lat)
tag5$distance <- distance5
leave_check5 <- ggplot(aes(x = time,y = distance),data = tag5) +
                geom_line( color = 'blue', size = 0.5) +
                geom_point() +
                #scale_x_datetime(date_breaks = "15 days") +
                ggtitle(" Distance from Nest for tag '117527' ") +
                theme(plot.title = element_text(hjust = 0.5)) +
                labs(x = "Time", y = "Distance(miles)")
print(leave_check5)

#--------Q4----------
# For tag 2 leave, subset those departure sequences
date_one <- as.POSIXct("2012-9-13 19:00:00")
date_two <- as.POSIXct("2012-9-14 03:00:00")
as.numeric(difftime(date_two, date_one , units = 'hours'))
int <- interval(date_one,date_two)
tag2_leave <- tag2[tag2$time %within% int,]
# create a custom variable, hour 
tag2_leave$hour <- as.numeric(difftime(tag2_leave$time, date_one , units = 'hours'))
# create the center of the map
tag2_center <- apply(cbind(tag2_leave$long,tag2_leave$lat),2,median)
tag2_map <- get_map(tag2_center + 0.1, maptype = "terrain", zoom = 10)
# draw the departure sequences 
p <- ggmap(tag2_map, base_layer = ggplot(tag2_leave,aes(long,lat))) +
     geom_point(aes(x = long,y = lat, color = speed, size = height),data = tag2_leave) +
     geom_path(aes(x = long,y = lat),data = tag2_leave, color = 'purple') +
     geom_label_repel(aes(label = hour)) + 
     ggtitle("Depature Sequences of hawk '105928'") +
     theme(plot.title = element_text(hjust = 0.5)) +
     labs(x = "Longitude", y = "Latitude")
print(p)                  

# For tag 4 leave, subset those departure sequences
date_three <- as.POSIXct("2012-08-05 16:00:00")
date_four <- as.POSIXct("2012-08-06 02:00:00")
int_2 <- interval(date_three,date_four)
tag4_leave <- tag4[tag4$time %within% int_2,]
#create a custom variable, hour
tag4_leave$hour <- as.numeric(difftime(tag4_leave$time, date_three, units = 'hours'))
# create the center of the map
tag4_center <- apply(cbind(tag4_leave$long,tag4_leave$lat),2,median)
tag4_map <- get_map( c(-121.6525,38.7686) , maptype = "terrain", zoom = 11)
# draw the departure sequences 
p2 <- ggmap(tag4_map, base_layer = ggplot(tag4_leave,aes(long,lat)))+ 
      geom_point(aes(x = long,y = lat, color = speed, size = height),data = tag4_leave) +
      geom_path(aes(x = long,y = lat),color = 'purple', data = tag4_leave) +
      geom_label_repel(aes(label = hour)) + 
      ggtitle("Depature Sequences of hawk '105936'") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Longitude", y = "Latitude")
print(p2)                  
