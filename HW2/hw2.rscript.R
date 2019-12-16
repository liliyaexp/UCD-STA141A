### STA 141A Project 2 / Liya Li
### Appendix: R Script

############# Packages needed
library(lubridate) # for dates and times
library(ggplot2) # for fancy plots .('v').
library(lattice)
library(MASS) # for boxcox()
library(treemap)
library(dplyr) # for filter(), tally()
library(maps)

#########################################
######################################### 1. First look at dataset
#########################################
############# Load in data
data <- read.csv("~/Desktop/Spring Quarter 2018/STA 141A/hw2/housing.csv")

# use head() and tail() to see if removal of some description is needed
# View(data)
head(data)
tail(data) 

# dimension
dim(data) # 20000 12

############# Convert the columns to appropriate data types
# check R data types detection
str(data) # levels for county, city, street and date are so many

# convert county, city, street to char since factor levels are a lot in each of them
data$county <- as.character(data$county)
data$city <- as.character(data$city)
data$street <- as.character(data$street)
# convert date to date using lubridate function
data$date <- as_date(data$date)

# get all sale years and months by using year() and month function in date
year(data$date) 
month(data$date)

# store years and months in date to data
data$date.yr <- year(data$date)
data$date.mon <- month(data$date)
# View(data)

str(data) # check R data types detection again

############# Any data irregularities? Yes.
# find missing values numbers in each columns 
colSums(is.na(data)) 

### possible irregularities happening variable
colnames(data)
# zip:
any(data$zip > 99999, na.rm = T) # False
# none zip has more than 5 digits -> none zip is irregular

# price:
any(data$price <= 0, na.rm = T) # True (using na.rm=T ignores NA)
price.irreg <- which(data$price <= 0) # finding the indexes of them
data[price.irreg,] # display information of housing sales that have irregular price: <=0 dollars
# price shouldn't be non positive
# some price are huge and we can consider them as irregular too. later question will take care of this detection.
# in here for now, we just consider those that are non positive irregular.

# br,lsqft, bsqft:
any(data$br <= 0 | data$bsqft <= 0 | data$lsqft <= 0, na.rm = T) # False
# none br/lsqft/bsqft is less or equal to 0 -> none br/lsqft/bsqft is irregular

# year:
any(data$year <= 1500 | data$year > 2018, na.rm = T) # True 
year.irreg <- which(data$year <= 1500 | data$year > 2018) # finding the indexes of them
data[year.irreg,] # display information of housing sales that have irregular year: <=1500 | > 2018
# year shouldn't be too small (smaller than 1500 is considered too small here) or larger than 2018

# long, lat:
# lat: positive value means above the equator 
# long: negative value means west of the prime meridian
any(data$long >= 0 | data$lat <= 0, na.rm = T) # True 
long.irreg <- which(data$long >= 0) # at row index 13962
lat.irreg <- which(data$lat <= 0) # same at row index 13962
data[long.irreg,] # display information of housing sales that have irregular long and lat: <=0
data[lat.irreg,]
# there should not contain a housing with long and lat 0 in this dataset because these housing are from the SF bay area



#########################################
######################################### 2. Find timespans
#########################################
############# Find timespan the houseing sales cover(date)
# there is no NA in date, so no need to use na.rm=T in max() and min()
max(data$date) # timespan ending: "2006-06-04"
min(data$date) # timespan starting: "2003-04-27"
# Therefore, timespan of date is: 2003-04-27 to 2006-06-04
# range(data$date) -> returns the same output 

############# Find timespan of the construction dates of homes(year)
# check the irregular year values and decide how to change or reset them
year.irreg # returns the irregular year indexes
data[year.irreg, "year"] # returns the irregular year values

# fix the irregular year value
which(data$year==20005) # find indexes of those years having 20005 as value
which(data$year<=1500) # find indexes of those years having <=1500 as value
which(data$year>2018 & data$year!=20005) # find index of the year having >2018 but !=20005 as value
data[which(data$year==20005), "year"] <- 2005 # those with year 20005:  probably typo with addition 0 while inputing, so reset 20005 to 2005
data[which(data$year<=1500), "year"] <- NA # those with year <=1500:  can't guess the true values, so reset to NA
data[which(data$year>2018 & data$year!=20005), "year"] <- NA # the one with year >2018 but !=20005:  can't guess the true value, so reset to NA
# double check whether the correction works
any(data$year <= 1500 | data$year > 2018, na.rm = T) # False, correction works


# find max and min of year
max(data$year, na.rm = T) # timespan ending: 2005
min(data$year, na.rm = T) # timespan starting: 1885
# Therefore, timespan of year is: 1885-2005
# range(data$year, na.rm = T) -> returns the same output



#########################################
######################################### 3. Exam the monthly housing sales
#########################################
############# Look at combinations of both year and month using the date variable
############# Make a plot that shows the number of sales over time
period.sales.table <- aggregate(price ~ date.mon + date.yr, data, length)
period.sales.table

# change the name price to sales in this data frame 
colnames(period.sales.table) <- c("date.mon", "date.yr", "sales")
head(period.sales.table) # check whether the names change: yes

is.data.frame(period.sales.table) # True

# The following line of code:
# https://stackoverflow.com/questions/39420136/combine-separate-year-and-month-columns-into-single-date-column
# credited to Rich Scriven
period.sales.table$date.mon.yr <- with(period.sales.table, sprintf("%d-%02d", date.yr, date.mon)) # to combine year and month
head(period.sales.table) # now we have date.mon.yr in this data frame

# change the date format for plotting
period.sales.table$date.mon.yr <- parse_date_time(period.sales.table$date.mon.yr,"y-m")
period.sales.table

##### Plot
p <- ggplot(period.sales.table, aes(date.mon.yr, sales)) + geom_point(color='blue',alpha=0.5) + geom_line() + ggtitle("Plot: Number of Sales Over Time") + xlab("Date(Year-Month)") + ylab("Sales Number")

# The following line of code:
# https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
# credited to Jonathan Chang
p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate the x axis names


############# Make a plot that shows the average house price over time
avg.price.table <- aggregate(price ~ date.mon + date.yr, data, FUN = mean)
avg.price.table

# add date column
avg.price.table$date.mon.yr <- with(avg.price.table, sprintf("%d-%02d", date.yr, date.mon))
head(avg.price.table) # now we have date.mon.yr in this data frame

# change the date format 
avg.price.table$date.mon.yr <- parse_date_time(avg.price.table$date.mon.yr,"y-m")

# plot
p2 <- ggplot(avg.price.table, aes(date.mon.yr, price)) + geom_point(color='purple', alpha=0.5) + geom_line() + ggtitle("Plot: Average House Price Over Time") + xlab("Date(Year-Month)") + ylab("Sales Price")
p2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))



#########################################
######################################### 4. Plot avg price as y. Make line plot to show relationship btw county, bedrooms, sale year.
#########################################
str(data)
############# County variable: use all original levels
# first check how many different counties in total
county.level <- levels(as.factor(data$county)) # county levels
county.level
length(county.level) # there are 22 in total but counties variable needs to be cleaned

# clean up county names: find "county" and replace it with "County"
# gsub("(exact)", "replace", text): find the exact character in the text and replace it with a defined pattern replace
cleaned.county <- gsub("county", "County", data$county) # change all county to County
is.vector(cleaned.county) # True

data$cleaned.county <- cleaned.county # create new column in dataset called cleaned.county

# typo San Franciscoe, change to San Francisco
cleaned.sf <- gsub("Franciscoe", "Francisco", data$cleaned.county) # change Franciscoe to Francisco
is.vector(cleaned.sf) # True
data$cleaned.county <- cleaned.sf # edit cleaned.county again with cleaned.sf

# also we notice that Alpine County is not in the SF bay area, check the recorded city names 
data[which(data$cleaned.county=="Alpine County"), "city"] # all Alpine County housing are in SF
cleaned.alpine <- gsub("Alpine", "San Francisco", data$cleaned.county) # change these counties to SF
is.vector(cleaned.alpine) # True
data$cleaned.county <- cleaned.alpine # edit cleaned.county again with cleaned.alpine

# check whether the corrections work
county.new.level <- levels(as.factor(data$cleaned.county))
county.new.level # no more "county",all "County", correction works
length(county.new.level) 
# now county is cleaned: column cleaned.county

############# Bedrooms variable: define levels as 1,2,3,4+
levels(as.factor(data$br))
# data[which(data$br=="28"),] -> interesting house with 28 br

data$cleaned.br <- cut(data$br, c(0,1,2,3,Inf), c("1","2","3","4+")) # redefine the br levels
# now we get clean cleaned.br 

############# Sale year variable: use levels 2003, 2004, 2005
# return all year levels in date
levels(as.factor(data$date.yr))# "2003","2004","2005","2006"
### subset the sale yrs in later subset()

############# Plot. Requirement: each line has 3 pts corresponding to yr, separate lines for each combination of county and br
avg.price <- aggregate(price ~ date.yr + cleaned.br + cleaned.county, data, mean)
avg.price 

sub4 <- subset(avg.price, date.yr<2006 ) # extract only 2003,2004,2005
sub4
# now sale year is also extracted

# use ggplot2: facet_wrap()
# or use lattice: xyplot()
colnames(sub4)
# draw the xyplot() where each box represent a county, x is yr in date, y is avg price, lines are br infor
xyplot(price ~ date.yr | cleaned.county, data = sub4, groups=cleaned.br, type='l', xlab = "Sale Year", ylab = "Average Housing Price (dollars)", 
       main = "Line Plots: Relationship Between County, Bedrooms, and Sale Years",
       key = list(space="right", lines = list(col=1:4,lty=2), text = list(c("1","2","3","4+"))))
# key usage part credited to Patrick



#########################################
######################################### 5. Observe and get information
#########################################
############# Do all housing sales within a given city only occur in one county? Why/Why not?
# table of city and county numbers
cctable <- table(data$city, data$cleaned.county)
cctable
city.number <- dim(cctable)[1]
city.number

# if row sum = the max entry in a row: other entries are 0 -> unique city and county matching
for(i in 1:city.number){
  if( sum(cctable[i,]) != max(cctable[i,]) ) {
    print(i) # returns 157
    i = i+1
  }
}
 

# if all housing sales within a given city only occur in one county, print(i) should return integer(0)
# here print(i) returns 157, so not all housing in given city only occur in one county
rownames(cctable)[157] # find this city: Vallejo
cctable[157,] # show the infor of this city

# only one such city and this housing sales within Vallejo occur in both Napa County and Solano County
# check from Wikipedia under california counties map, the city of Vallejo is in Solano county but borders Napa county.
# so a house sold in the outskirts of Vallejo may report 'Vallejo' as the city and 'Solano' as the county. 
# This would be why Vallejo has sales in both Napa and Solano counties.



#########################################
######################################### 6. Simple Linear regression model
#########################################
############# Fit linear regression model using bsqft to predict price
# fit linear regression model
model1 <- lm(price ~ bsqft, data)
model1

# original data plot
plot(data$bsqft, data$price, main = "Relationship Between bsqft and price", xlab = "bsqft", ylab = "price (dollars)")
# we can see from this plot that the data are not spreading linearly and there are some outliers.

############# Diagnostics plots
par(mfrow = c(2,2))
plot(model1, main = "Diagnostics Plots for model 1") # interpret
# which = 
# 1 - a plot of residuals against fitted values 
# 2 - a Scale-Location plot of sqrt(| residuals |) against fitted values 
# 3 - a Normal Q-Q plot 
# 4 - a plot of Cook's distances versus row labels 
# 5 - a plot of residuals against leverages 
# 6 - a plot of Cook's distances against leverage/(1-leverage). 
# By default, the first three and 5 are provided.

plot(model1, which = 4)
# directly obtain from this graph: 3 extreme outliers at index: 987, 7419, 19332, we may want to remove them

# by using function to detect outliers we find:
# boxplot.stats(data$price, coef = 1.5)$out # coef determines how far the plot ‘whiskers’ extend out from the box
# length(boxplot.stats(data$price, coef = 1.5)$out) # in this method we will find 1129 price outliers 
# may be remove too much outliers

############# ok to remove extreme outliers
# remove outliers and store the rest data into data2
# price.irreg gives the indexes of price 0's, we want to remove these because they are considered extrame outliers
# we found extrame outliers at index: 987, 7419, 19332 from model1
# we found extrame outliers of those whose bsqft values are larger than 10000 from the original data plot
# first remove these extreme outliers first
model1.outliers <- c(987, 7419, 19332)
bsqft.irreg <- which(data$bsqft>=10000)
data2 <- data[-c(model1.outliers, price.irreg, bsqft.irreg),]
View(data2)
dim(data2) # we removed 20000-19985=15 extrame outliers

plot(data2$bsqft, data2$price, main = "Relationship Between bsqft and price (Without Outliers)", xlab = "bsqft", ylab = "price (dollars)")
# data2 plot looks more linear now

# now build the model again with data2 and check diagnostic
model2 <- lm(price ~ bsqft, data2)
model2
par(mfrow = c(2,2))
plot(model2, main = "Diagnostics Plots for model 2") # interpret
# approximately linear but equal variance and normality don't hold well

############# try data transformations and see whether it works
par(mfrow = c(1,1))
boxcox(model2) # lambda is approximately equal to 0 meaning that we should do log transformation
plot(log(data2$bsqft), log(data2$price), main = "Relationship Between log(bsqft) and log(price) (No Outlier)", xlab = "log(bsqft)", ylab = "log(price) (dollars)")
# this data plot with transform data looks linear 
# fit the final model
model3 <- lm(log(price) ~ log(bsqft), data2) # try log on both price and bsqft
model3

# draw the final diagnostic plot
par(mfrow = c(2,2))
plot(model3, main = "Diagnostics Plots for model 3")
plot(model3, which = 4) # data indexed at 5753, 8625, 16054 may be outliers 
# but all diagnostic assumptions had already be met, so model3 is our final choice



#########################################
######################################### 7. Multiple Linear regression model
#########################################
############# Fit linear regression model using bsqft and lsqft to predict price
model4 <- lm(price ~ bsqft + lsqft, data)
model4

############# Without transformations/diagnostics, conduct hypothesis test
model4.sum <- summary(model4)
model4.sum
est.beta.bsqft <- model4.sum$coefficients[2,1] # estimated beta bsqft
est.beta.bsqft
est.beta.lsqft <- model4.sum$coefficients[3,1] # estimated beta lsqft
est.beta.lsqft
std.beta.bsqft <- model4.sum$coefficients[2,2] # standard deviation of beta bsqft
std.beta.bsqft
std.beta.lsqft <- model4.sum$coefficients[3,2] # standard deviation of beta lsqft
std.beta.lsqft
variance.diff <- std.beta.bsqft^2 + std.beta.lsqft^2
variance.diff
# str(model4.sum) # see details in object model4.sum

############# Report conclusion and test statistic
# find test statistics
ts <- (est.beta.bsqft - est.beta.lsqft) / sqrt(variance.diff)
ts
# under the null hypothesis the ts should follow the t distribution with df = n-p 
# where p=2 is the num of coefficients in the model, and n=20000
# the limiting nature of t for large n=20000 is approximately leading to a z distribution
# therefore, we don't need to be exact in choosing a critical value here
pnorm(ts) # and the p value for this ts is 1, meaning that no matter what alpha value is, the pval is larger than it
# fail to reject H0



#########################################
######################################### 8. Individual regression line using bsqft and price for each separate county
#########################################
############# Use apply fcn. Fit regression models.
# first split the data in terms of county
splited.data <- split(data, data$cleaned.county) 
View(splited.data)

# fit regression models with each county
models.county <- lapply(splited.data, function(x) lm(price~bsqft, data = x))
models.county # a list of regression model infor for each county

############# Draw each individual regression line in a single plot. Distinguish lines.
# first draw an empty plot with x,y range predefined
par(mfrow = c(1,1))
plot(1, type="n", xlab = "bsqft", ylab = "price (dollars)", xlim = range(data$bsqft, na.rm = T), ylim = range(data$price, na.rm = T), main = "Regression Lines for Models in Terms of County")

# then draw regression lines on this empty plot by county, there are 9 counties in total
sapply(1:9, function(x) abline(coef(models.county[[x]]), col = x, lty = 1))
legend("topleft", names(table(data$cleaned.county)), col = 1:9, lty = 1) # add legend

############# Can we conclude that county is a confounding variable? (lines parallel?)
# Yes, since the regression lines are not parallel.
# Confounding variable: variables which may be affecting the relationship between two other variables that you are studying. 



#########################################
######################################### 9. Develop a treemap
######################################### Visualize the avg prices for the three cities with the most sales in each county
############# Use appropriate subsetting methods to find the "top three" for each county
colnames(data)
price.agg <- aggregate(price ~ city + cleaned.county, data, mean) # aggregates price by city and county
price.agg
count.agg <- aggregate(price ~ city + cleaned.county, data, length) # aggregates house sales by city and county
count.agg

# combine them
sales.agg <- price.agg
sales.agg$counts <- count.agg[["price"]] # add the counts into the data frame sales.agg
sales.agg

splited.sales <- split(sales.agg, sales.agg$cleaned.county) # split the sales by county
splited.sales

# find the top three in each county
# try on one county: 
# sales on alameda: 
sales.ala <- splited.sales$`Alameda County`[["counts"]]
# sort them: 
sort(sales.ala)
# take the last three: 
last3.ala <- tail(sort(sales.ala), 3)
# find the positions of them: 
pos.ala <- match(last3.ala, sales.ala)
# find the city names based on these positions: 
splited.sales$`Alameda County`[["city"]][pos.ala]
splited.sales[[1]][["city"]][pos.ala] # same output


# now try on lapply fcn
top3.city <- lapply(c(1:9), function(x){                # for each county
    sales.county <- splited.sales[[x]][["counts"]]      # extract counts
    sort(sales.county)                                  # sort price 
    last3.county <- tail(sort(sales.county), 3)         # find top 3
    pos.county <- match(last3.county, sales.county)     # find indexes of top 3
    splited.sales[[x]][["city"]][pos.county]            # find city names of top 3
} )
top3.city
is.list(top3.city) # True
# return lists of top 3 cities for each county

unlist.top3.city <- unlist(top3.city) # unlist the output
unlist.top3.city
is.vector(unlist.top3.city) # True

# extract the sales of unlist.top3.city
sales.index.top3 <- which(sales.agg[["city"]] %in% unlist.top3.city) # returns the row sales indexes of unlist.top3.city in sales.agg 
sales.index.top3

# obtain the infor for all top3 cities and assign it as a new data frame
top3.data <- sales.agg[sales.index.top3,] 
top3.data
is.data.frame(top3.data) # True

# we found that Vallejo has only 2 sales, which means it is not in the top 3 on Napa county, remove it
remove <- which(top3.data[["counts"]]==2) 
remove # 13
top3.data <- top3.data[-remove,]
top3.data

############# Plot treemap. 
# Make sure the order of the indexing has the city variable "nested" in county by specifying index=c(county, city)
colnames(top3.data)

??treemap
# treemap color cheatsheet: 
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
treemap(dtf = top3.data, height = 200, index = c("cleaned.county","city"), 
        vSize = "counts", vColor = "price", palette = "Paired", type = "value", 
        fontsize.labels = 12, fontsize.legend = 9, 
        align.labels = list(c("center", "center"), c("right", "top")),
        border.lwds = c(5,2), 
        title = "Avg Price in Top Three Sales Cities")



#########################################
######################################### 10. Develop heatmap
######################################### Examine how price and frequency of SF housing sales depend on location
# codes in this question use all hints from Piazza HW2Q&A post by Patrick

############# Subset SF data, create appropriate factor variables for latitude and longitude 
SF.index <- which(data$city == "San Francisco")    # obtain row indexes that has SF as city
SFdata <- data[SF.index,]  # subset SF data from the origianl dataset
View(SFdata)

# implement cell square using transformation to the location variable
SFdata$long2 <- round(SFdata$long, 2) # round to two decimal places 
SFdata$lat2 <- round(SFdata$lat, 2)   # round to two decimal places

# create appropriate factor variables for lat2 and long2 that include levels that are not present in the dataset
long.range <- range(SFdata$long2, na.rm = T)            # obtain range of long2
long.seq <- seq(long.range[1], long.range[2], 0.01)     # create sequence of long2 based on range
SFdata$longF <- factor(SFdata$long2, levels = long.seq) # create factor variable for long2

lat.range <- range(SFdata$lat2, na.rm = T)              # obtain range of lat2
lat.seq <- seq(lat.range[1], lat.range[2], 0.01)       # create sequence of lat2 based on range
SFdata$latF <- factor(SFdata$lat2, levels = lat.seq)    # create factor variable for lat2
head(SFdata)

# then aggregate the sfdata
SF.agg <- aggregate(price ~ latF + longF, SFdata, function(x) c(mean(x),length(x)), drop=FALSE)
head(SF.agg) # price.1 is price, price.2 is counts

############# Draw heatmap that is colorized based on the num of houses in a cell
house.counts <- matrix(SF.agg$price[,2], nlevels(SFdata$longF), nlevels(SFdata$latF), byrow = T)
house.counts[house.counts == 0] <- NA 
# Setting values in a matrix to NA will remove their colorization on a heatmap

# draw heatmap
image(x = long.seq + 0.03, y = lat.seq, z = house.counts, main = "Heatmap1 (based on num of houses in a cell)", xlab = "Longitude", ylab = "latitude", col = heat.colors(256))
# points(x = data$long, y = data$lat) # plot the original long and lat to vertify the heatmap

# this line of code credited to an Annonymous post name Q10 on Piazza
sf.map <- map('county', 'california,san francisco', plot = F)

# draw the border
lines(sf.map$x, sf.map$y)


############# Draw heatmap that is colorized by the average price of the houses within a cell
# create matrix object that contains the corresponding values for each cell
house.price <- matrix(SF.agg$price[,1], nlevels(SFdata$longF), nlevels(SFdata$latF), byrow = T)
house.price[house.price == 0] <- NA 
# Setting values in a matrix to NA will remove their colorization on a heatmap

# draw heatmap
image(x = long.seq + 0.03, y = lat.seq, z = house.price, main = "Heatmap2 (based on avg price of the houses within a cell)", xlab = "Longitude", ylab = "latitude", col = heat.colors(256))
# points(x = data$long, y = data$lat) # plot the original long and lat to vertify the heatmap

# this line of code credited to an Annonymous post name Q10 on Piazza
sf.map <- map('county', 'california,san francisco', plot = F)

# draw the border
lines(sf.map$x, sf.map$y)




