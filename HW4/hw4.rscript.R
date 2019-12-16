###### HW 4 Liya Li
###### exploratory spatial data analysis
library(lubridate) # for dates and times
library(ggplot2)
library(ggmap)
library(gridExtra) # use grid.arrange() 
library(geosphere)

Hawk <- read.csv("~/Desktop/Spring Quarter 2018/STA 141A/hw4/the_hawks.csv")
View(Hawk)
str(Hawk)

# clean the data
# tag: five different tags.
unique(Hawk$tag) # 5 tags in total
Hawk$cleaned.tag <- as.factor(Hawk$tag) # convert int to factor and create new column in dataset
# time: timestamp of the event.
Hawk$cleaned.time <- as_date(Hawk$time)
str(Hawk) # now we have cleaned.time and cleaned.tag in appropriate type

############################### Q1
###### Create a spatial map that properly visualizes the location of all five of the hawks. 
###### You will need to represent each point with the longitude and latitude variables. 
###### Make sure that it is possible to distinguish the points between each hawk. 
###### Also make sure you areas with denser concentrations of points by setting the alpha parameters.

## tips for how to use make_bbox(), get_map() and ggmap() are from:
## https://medium.com/fastah-project/a-quick-start-to-maps-in-r-b9f221f44ff3
lat <- Hawk$lat
long <- Hawk$long
tag <- Hawk$cleaned.tag

bbox <- make_bbox(long, lat, f = 0.05)
b <- get_map(bbox, maptype = "roadmap", zoom = 10) # plot the map according to long and lat in our dataset
ggmap(b) # first plot the map
# note: maptype can be "toner-lite", "terrain", "terrain-background", "satellite", "roadmap", and "hybrid" (google maps), "terrain", etc
# the source parameter in get_map() is default to be shown like in Google map

# then add data points on the map
ggmap(b) + geom_point(data = Hawk, aes(long, lat, color = factor(tag)), size=1, alpha=0.9) +
      labs(x = "Longitude", y = "Latitude", title = "Hawk Locations", color = "Tag")

# Error shown if use:
# ggmap(b) + geom_point(data=data, aes(long, lat, group = factor(tag)), size=2, alpha=0.5) +
# labs(x = "Longitude", y = "Latitude", title = "Hawk Locations", color = "Tag")
# Error: ggplot2 doesn't know how to deal with data of class uneval
# This is because ggplot2 doesn't recognize my dataset which is named "data".
# Solution is to change my dataset name. Here is changed to "Hawk".



############################### Q2
###### In this dataset, there are two hawks who have documented arrival sequences. (stage)
###### Create spatial maps that documents these two sequences. 
###### You can examine the arrival sequences by subsetting by stage. 
###### In this plot, find appropriate ways to use height and speed in your plot. 
###### Once again, make sure to draw separate lines by tag. 
###### It is recommended that you write the two sequences as separate plots.

# first subset the sequences of these 2 hawks and obtain which tags are for them
arrival <- subset(Hawk, stage == "arrival")
arrival
unique(arrival$tag)  # bird tags 105936 and 105928 are "arrival"

# this block of coding ideas are from Prof Gupta in class
# separate 2 sequences and plot each
bird1 <- subset(arrival, tag == "105928")
bird2 <- subset(arrival, tag == "105936")
bird1  # view bird 1 infor
bird2  # view bird 2 infor
# since median of each nest by tag is approximately where their nest is, we obtain the median long and lat:
nests.med <- aggregate(cbind(long, lat) ~ tag, Hawk, median)
med1 <- apply(bird1[, 3:4], 2, median)
med2 <- apply(bird2[, 3:4], 2, median)
med1  
med2   

# plot for bird1
# this block of coding ideas are from Prof Gupta in class
map1 <- get_map(location = med1, maptype = "roadmap", zoom = 15)
plot1 <- ggmap(map1, extent = "device") 
# plot1 <- plot1 + geom_line(aes(x=long, y=lat, group=tag), data=bird1)                      
plot1 <- plot1 + geom_point(aes(x=long, y=lat, color=speed, size=height), data=bird1)     
plot1 <- plot1 + geom_point(aes(x=long, y=lat), color="green", size=5, data=nests.med[2,]) # highlight median bird1 location
plot1 <- plot1 + ggtitle("Here come the hawks 105928") # add title

# make path plot for bird1: codes credited to Patrick on Piazza
L1 <- cbind(bird1$long, bird1$lat)
L12 <- cbind(L1[-nrow(L1),], L1[-1,])
L12 <- as.data.frame(L12)
plot1 <- plot1 + geom_segment(aes(x=V1, y=V2, xend=V3, yend=V4), data=L12, arrow = arrow(length=unit(0.3,"cm")), alpha=0.3)
plot1 
# arrow parameter code idea credits to post on piazza

# plot for bird2
# this block of coding ideas are from Prof Gupta in class
map2 <- get_map(location = med2, maptype = "roadmap", zoom = 15)
plot2 <- ggmap(map2, extent = "device") 
# plot2 <- plot2 + geom_line(aes(x=long, y=lat, group=tag), data=bird2)                     
plot2 <- plot2 + geom_point(aes(x=long, y=lat, color=speed, size=height), data=bird2)     
plot2 <- plot2 + geom_point(aes(x=long, y=lat), color="yellow", size=5, data=nests.med[4,]) # highlight median bird2 location
plot2 <- plot2 + ggtitle("Here come the hawks 105936") # add title

# make path plot for bird2: codes credited to Patrick on Piazza
L2 <- cbind(bird2$long, bird2$lat)
L22 <- cbind(L2[-nrow(L2),], L2[-1,])
L22 <- as.data.frame(L22)
plot2 <- plot2 + geom_segment(aes(x=V1, y=V2, xend=V3, yend=V4), data=L22, arrow = arrow(length=unit(0.3,"cm")), alpha=0.3)
plot2 
# arrow parameter code idea credits to post on piazza



############################### Q3
###### Develop a strategy to determine whether each hawk leaves their nest.
###### Report the time period (in days) of which each hawk leaves, if they do. 
###### You are encouraged to use non-spatial visuals to justify your answer. 
###### Explain how you achieved your results and whether you had a consistent strategy for each hawk. 
###### If you believe a hawk did not leave their nest, you do not need to report their time interval. 
###### It is recommended that you take a look at how to use the distGeo() function from the geosphere package to calculate distances between sets of points.

## first, we determine the nest using median location since median is not affected by outliers(locations that far away from nest)
nests.med # there are the median location for 5 birds

## then, we calculate distance of each birds from each location compared to the median location
num <- dim(Hawk)[1] # length of observation on 5 birds
Hawk$distance <- c(rep(0, num))
unique(Hawk$tag) # check tag groups

# bird 105936
b1.index <- which(Hawk$tag==105936)
b1.median <- c(nests.med[4,2], nests.med[4,3])
b1 <- cbind(Hawk[b1.index,]$long, Hawk[b1.index,]$lat)
options(scipen=999) # to avoid scientific notation
Hawk[b1.index, 11] <- distGeo(b1, b1.median)/1609 # distance unit: mile

# bird 105930
b2.index <- which(Hawk$tag==105930)
b2.median <- c(nests.med[3,2], nests.med[3,3])
b2 <- cbind(Hawk[b2.index,]$long, Hawk[b2.index,]$lat)
options(scipen=999) # to avoid scientific notation
Hawk[b2.index, 11] <- distGeo(b2, b2.median)/1609 # distance unit: mile

# bird 105923
b3.index <- which(Hawk$tag==105923)
b3.median <- c(nests.med[1,2], nests.med[1,3])
b3 <- cbind(Hawk[b3.index,]$long, Hawk[b3.index,]$lat)
options(scipen=999) # to avoid scientific notation
Hawk[b3.index, 11] <- distGeo(b3, b3.median)/1609 # distance unit: mile

# bird 105928
b4.index <- which(Hawk$tag==105928)
b4.median <- c(nests.med[2,2], nests.med[2,3])
b4 <- cbind(Hawk[b4.index,]$long, Hawk[b4.index,]$lat)
options(scipen=999) # to avoid scientific notation
Hawk[b4.index, 11] <- distGeo(b4, b4.median)/1609 # distance unit: mile

# bird 117527
b5.index <- which(Hawk$tag==117527)
b5.median <- c(nests.med[5,2], nests.med[5,3])
b5 <- cbind(Hawk[b5.index,]$long, Hawk[b5.index,]$lat)
options(scipen=999) # to avoid scientific notation
Hawk[b5.index, 11] <- distGeo(b5, b5.median)/1609 # distance unit: mile

## Hawk$distance is now updated
## then, we visualize each birds distance from nest and determine whether they leave their nests at which time period

# subset infor for each bird
unique(Hawk$tag) # check tag groups
leave.b1 <- subset(Hawk, tag=="105936")
leave.b2 <- subset(Hawk, tag=="105930")
leave.b3 <- subset(Hawk, tag=="105923")
leave.b4 <- subset(Hawk, tag=="105928")
leave.b5 <- subset(Hawk, tag=="117527")

# plot the distance against time for each bird and find the 
leaveplot1 <- ggplot(leave.b1, aes(x=cleaned.time, y=distance))                     # plot the axises
leaveplot1 <- leaveplot1 + geom_point(aes(color=stage))                             # add data points layer
leaveplot1 <- leaveplot1 + ggtitle("Distance from Estimated Nest for Hawk #105936") # add title
leaveplot1 <- leaveplot1 + xlab("Time") + ylab("Distance(mile)")                    # add labs
leaveplot1    
leave.time1 <- leave.b1[which(leave.b1$distance>5), 2]
max(leave.b1$cleaned.time) # last observated date is 2012/08/06
leave.time1   # times that this bird leaves its nest

leaveplot2 <- ggplot(leave.b2, aes(x=cleaned.time, y=distance))                     # plot the axises
leaveplot2 <- leaveplot2 + geom_point(aes(color=stage))                             # add data points layer
leaveplot2 <- leaveplot2 + ggtitle("Distance from Estimated Nest for Hawk #105930") # add title
leaveplot2 <- leaveplot2 + xlab("Time") + ylab("Distance(mile)")                    # add labs
leaveplot2  
leave.time2 <- leave.b2[which(leave.b2$distance>5), 2]
max(leave.b2$cleaned.time) # last observated date is 2012/08/23
leave.time2   # times that this bird leaves its nest

leaveplot3 <- ggplot(leave.b3, aes(x=cleaned.time, y=distance))                     # plot the axises
leaveplot3 <- leaveplot3 + geom_point(aes(color=stage))                             # add data points layer
leaveplot3 <- leaveplot3 + ggtitle("Distance from Estimated Nest for Hawk #105923") # add title
leaveplot3 <- leaveplot3 + xlab("Time") + ylab("Distance(mile)")                    # add labs
leaveplot3   
leave.time3 <- leave.b3[which(leave.b3$distance>5), 2]
max(leave.b3$cleaned.time) # last observated date is 2012/05/23
leave.time3   # times that this bird leaves its nest

leaveplot4 <- ggplot(leave.b4, aes(x=cleaned.time, y=distance))                     # plot the axises
leaveplot4 <- leaveplot4 + geom_point(aes(color=stage))                             # add data points layer
leaveplot4 <- leaveplot4 + ggtitle("Distance from Estimated Nest for Hawk #105928") # add title
leaveplot4 <- leaveplot4 + xlab("Time") + ylab("Distance(mile)")                    # add labs
leaveplot4    
leave.time4 <- leave.b4[which(leave.b4$distance>5), 2]
max(leave.b4$cleaned.time) # last observated date is 2012/09/14
leave.time4   # times that this bird leaves its nest

leaveplot5 <- ggplot(leave.b5, aes(x=cleaned.time, y=distance))                     # plot the axises
leaveplot5 <- leaveplot5 + geom_point(aes(color=stage))                             # add data points layer
leaveplot5 <- leaveplot5 + ggtitle("Distance from Estimated Nest for Hawk #117527") # add title
leaveplot5 <- leaveplot5 + xlab("Time") + ylab("Distance(mile)")                    # add labs
leaveplot5    
leave.time5 <- leave.b5[which(leave.b5$distance>5), 2]
max(leave.b5$cleaned.time) # last observated date is 2012/08/21
leave.time5   # times that this bird leaves its nest



############################### Q4
###### Using spatial maps, visualize the departure sequences of the hawks that leave their nests. 
###### You are encouraged to use custom variables in your plots. 
###### You will have to decide what variables are useful and how to visualize them. 
###### Make sure to comment on each hawk’s migration patterns in your report.
## from Q3 we notice that Hawk tag ending with 36 and 28 leave their nest forever at the end of their preMigration stage
## therefore here we visualize these two departure sequence
leaveplot1 # 28
leaveplot4 # 36

## in order to find the departure sequences, we subset preMigration on these two sets and analyze those with distance > 10
hawk28 <- subset(Hawk, tag=="105928" & stage=="preMigration" & (cleaned.time=="2012-09-13" | cleaned.time=="2012-09-14"))
hawk28
hawk36 <- subset(Hawk, tag=="105936" & stage=="preMigration" & (cleaned.time=="2012-08-05" | cleaned.time=="2012-08-06"))
hawk36

## plot the paths
nests.med 
med28 <- apply(hawk28[, 3:4], 2, median)
med36 <- apply(hawk36[, 3:4], 2, median)
med28 
med36

## for hawk28
map28 <- get_map(location = med28, maptype = "roadmap", zoom = 9) # if use zoom=10, warning: remove 1 row...
p28 <- ggmap(map28) 
p28 <- p28 + geom_point(aes(x=long, y=lat, color=speed, size=height), data=hawk28) 
p28 <- p28 + geom_point(aes(x=long, y=lat), color="orange", size=5, data=nests.med[2,])
p28 <- p28 + ggtitle("Here leave the hawks 105928") 

# make path plot for hawk28
L28 <- cbind(hawk28$long, hawk28$lat)
L28.2 <- cbind(L28[-nrow(L28),], L28[-1,])
L28.2 <- as.data.frame(L28.2)
p28 <- p28 + geom_segment(aes(x=V1, y=V2, xend=V3, yend=V4), data=L28.2, arrow = arrow(length=unit(0.3,"cm")), alpha=0.3)
p28
# arrow parameter code idea credits to post on piazza

## for hawk36
map36 <- get_map(location = med36, maptype = "roadmap", zoom = 10)
p36 <- ggmap(map36, extent = "device") 
p36 <- p36 + geom_point(aes(x=long, y=lat, color=speed, size=height), data=hawk36) 
p36 <- p36 + geom_point(aes(x=long, y=lat), color="red", size=5, data=nests.med[4,])
p36 <- p36 + ggtitle("Here leave the hawks 105936") 

# make path plot for hawk28
L36 <- cbind(hawk36$long, hawk36$lat)
L36.2 <- cbind(L36[-nrow(L36),], L36[-1,])
L36.2 <- as.data.frame(L36.2)
p36 <- p36 + geom_segment(aes(x=V1, y=V2, xend=V3, yend=V4), data=L36.2, arrow = arrow(length=unit(0.3,"cm")), alpha=0.3)
p36
# arrow parameter code idea credits to post on piazza









