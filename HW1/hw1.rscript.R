### STA141A / HW1 / Liya Li
### read in file
data <- readRDS("~/Desktop/Spring Quarter 2018/STA 141A/hw1/college_scorecard_2013.rds")
# View(data)
head(data)
tail(data) # use head() and tail() to see if removal of some description is needed
# always read the data description first and figure out the data types

########### 1. observation number, college number
obs.n <- dim(data)[1]
college.n <- sum(data$main_campus) # sum the T and F which are actually 1 and 0
rbind(c("observation", obs.n), c("college", college.n)) # clean up the outputs

########### 2. featrue number, categorical feature number, discrete feature number
feature <- dim(data)[2]
feature
# View(data)
str(data) # firstly glance at the data types 

# citation: this block of codes follow Prof Gupta's in-class R code document named "April_12.R"
class.tab1 < sapply(data, class) # assign class to each column
table(class.tab1)
# discrete r.v. take on integer values
# check the variables names and see if they involves factor
type.integer <- 'integer'
type.categorical <- 'factor'
# class.tab1 == type.integer: get the integer variables ("True")
# class.tab1 == type.categorical: get the categorical variables ("True")
integer.names <- names((class.tab1 == type.integer)[class.tab1 == type.integer]) # extract only the names of integer variables, which are T in 'integer'
integer.names
categorical.names <- names((class.tab1 == type.categorical)[class.tab1 == type.categorical]) # extract only the names of categorical variables, which are T in 'factor'
categorical.names
# do not use which() on logical vector


########### 3. missing value number, most missing value feature
mat1 <- is.na(data) # find the missing ones
missing <- length(which(mat1)) # same as using: missing <- sum(mat1) 
missing
# !is.na(data): find the not missing ones

# citation: this block of codes follow Prof Gupta's in-class R code document named "April_12.R"
NA.counts <- colSums(mat1) # find how many missing in each column
# t(t(NA.counts)) # nicely show the outputs, same as using: as.matrix(NA.counts)
sort(NA.counts) # find the most missing value feature
names(which.max(NA.counts)) # find the feature name

# citation: this block of codes follow Prof Gupta's in-class R code document named "April_12.R"
plot(NA.counts, main = "Number of Missing Data Per Feature", xlab = "Feature index", ylab = "Number NA's", cex = 1, pch = 15)
abline(h = c(0,500), col = c("red", "blue"))

# summary(data) # summaries on each column # way 2
# names(which.max(NA.counts))

# hist(NA.counts) # way 3
# table(NA.counts)
# which(NA.counts == 1923)

image(mat1, main = "Missing Values Scale", xlab = "Propotional Missing Value Numbers in Total 23197", ylab = "Propotional Variable in Total 51")
# pattern: missing yellow parts... 0.8 column seems missing. column not row bc it's 90 degree.
# mat1 is a matrix of 0 and 1. image() scales the mat1: y to 51, x to 20000+. 
# more examples: image(matrix(c(1,1,1,0), nrow=2))

########### 4. more public or private, proportions of highest degree awarded.
# citation: these blocks of codes for Q4 follow mostly Prof Gupta's in-class R code document named "April_12.R"
# Display this information in one graph
table(data$ownership)
table(data$ownership == 'Public')
# same outputs: table(factor(data$ownership == "Public", labels=c("Not Public", "Public")))
# create a new column showing the school is public or not
data$IsPublic <- factor(data$ownership == "Public", labels=c("Not Public", "Public"))

table(data$ownership, data$highest_degree) # degree awarded situation for public, nonprofit, profit

# prop degree awarded situation for public, nonprofit, profit
prop.1 <- prop.table(table(data$ownership, data$highest_degree), margin = 1)
prop.1 # be sure to use the right margin value: margin=1 means row prop sum to 1, margin=2 means column prop sum to 1

# prop degree awarded situation for public and private
prop.2 <- prop.table(table(data$IsPublic, data$highest_degree), margin = 1)
prop.2

library(ggplot2)
# convert the dataset into a data frame 
prop.df <- data.frame(percent=c(prop.2[1,], prop.2[2,]), type = c(rep("Private", 5), rep("Public", 5)), degree = rep(c("Other", "Certificate", "Associate", "Bachelor", "Graduate"), 2))
prop.df
# Citation: (for adding data labels)
# https://stackoverflow.com/questions/24198896/how-to-get-data-labels-for-a-histogram-in-ggplot2
degree.proportion.plot <- ggplot(data=prop.df, aes(x=degree, y=percent, fill=type)) + geom_bar(stat="identity", position=position_dodge()) + geom_text(aes(label=round(percent,4), vjust=1.5))
# Citation: (for adding titles and labels)
# http://environmentalcomputing.net/plotting-with-ggplot-adding-titles-and-axis-names/
print(degree.proportion.plot + ggtitle("Degree Proportions in Public and Private Colleges") + labs(y="Degree Proportion in College Type", x = "Degree Offered in College Type"))

# another way to graph and compare
# mosaicplot(prop.2, color = T, shade = T, main = "Proportions of High Degree Per College Type", ylab = "High Degree Awarded", xlab = "Ownership", las = 2)


########### 5. average undergraduate population, median, deciles. 
# Display these statistics and the distribution graphically.
summary(data$undergrad_pop)
# fivenum(data$undergrad_pop, na.rm = T) # similar output
ave.undergrad <- summary(data$undergrad_pop)[["Mean"]]
ave.undergrad
median.undergrad <- summary(data$undergrad_pop)[["Median"]]
median.undergrad
# Citation: (similar way to obtain deciles)
# https://alstatr.blogspot.com/2013/06/quartiles-deciles-and-percentiles.html
# quartiles sort data into 4 quarters, while deciles sort data into 10 quarters
deciles <- quantile(data$undergrad_pop, probs = seq(0,1, length=11), na.rm = T)
deciles

############ in class
# analysis: check chap 6.8 
# mean(data$undergrad_pop, na.rm=TRUE) # same 
# median(data$undergrad_pop, na.rm=TRUE) # same
# quantile(data$undergrad_pop, probs = seq(0,1,0.1), na.rm = T) # same

# the undergrad population is a continuous variable, becasue in Q2 it's not in the discrete group
# better to use histogram

# Citation: this block of codes follow Prof Gupta's in-class R code document named "April_17.R"
hist(data$undergrad_pop, main = "Histogram of Undergraduate Population", ylab = "Frequency", xlab = "Undergraduate Population") # there is outlier 
abline(v = ave.undergrad, col = "blue", lty = "dashed")
abline(v = median.undergrad, col = "red", lty = "solid")
abline(v = deciles, col = "dark grey", lty = "dotted")
# why is it an outlier? maybe not an outlier but just an obs bc it's a small dataset. Explain.
# add legend
legend(x=105000, y=2250, c("mean", "median", "deciles"), col=c("blue","red","dark grey"), lty=c("dashed","solid","dotted") )


########### 6. compare tuition graphically in the 5 most populous states: California, Texas, New York, Illinois, Florida.
# Citation: this block of codes follow Prof Gupta's in-class R code document named "April_17.R"
target <- data[data$state %in% c("CA", "TX", "NY", "IL", "FL"),] # filter the five states
# sort(table(data$state), decreasing = T): sorting the state frequencies
# droplevel(): drop the unused levels from the original vector of states
boxplot(target$tuition ~ droplevels(target$state), main="Boxplot of 5 Most Populous States", xlab="State", ylab="Tuition (dollars)")
# choose boxplot because it's informative in terms of mean, median, quantiles

# we can also draw density plot statewise (need to add legend)
# plot(density(target$tuition[target$state == "CA"], na.rm = T), lty = 5, col="pink")
# lines(density(target$tuition[target$state == "TX"], na.rm = T), lty = 1, col="grey")
# lines(density(target$tuition[target$state == "FL"], na.rm = T), lty = 2, col="red")
# lines(density(target$tuition[target$state == "NY"], na.rm = T), lty = 3, col="green")
# lines(density(target$tuition[target$state == "IL"], na.rm = T), lty = 4, col="blue")
# or draw histgrams statewise
# par(mfrow = c(3,2))
# hist(target$tuition[target$state == "FL"], col = "red", main = "Histogram of Tuition in FL", xlab = "Tuition (dollars)", ylab = "Frenquency")
# hist(target$tuition[target$state == "NY"], col = "green", main = "Histogram of Tuition in NY", xlab = "Tuition (dollars)", ylab = "Frenquency")
# hist(target$tuition[target$state == "CA"], col = "pink", main = "Histogram of Tuition in CA", xlab = "Tuition (dollars)", ylab = "Frenquency")
# hist(target$tuition[target$state == "IL"], col = "blue", main = "Histogram of Tuition in IL", xlab = "Tuition (dollars)", ylab = "Frenquency")
# hist(target$tuition[target$state == "TX"], col = "grey", main = "Histogram of Tuition in TX", xlab = "Tuition (dollars)", ylab = "Frenquency")
# dev.off(): clear the setting if needed.


########### 7. 
# a.what's the name of the university with the largest value of avg_sat
data[which.max(data$avg_sat), "name"]

# b.Does the university with the largest amount of undergrad_pop have open admissions
data[which.max(data$undergrad_pop), "open_admissions"]

# c.List the zip code of the public university with the smallest value of avg_family_inc 
data.filtered <- data[data$ownership=="Public",]
min.index <- which.min(data.filtered$avg_family_inc)
data.filtered$zip[min.index]

# d.Does the university you found in part b. also have the largest amount of grad_pop
data[which.max(data$avg_sat), "grad_pop"]==max(data$grad_pop, na.rm = TRUE)


########### 8. for profit & bachelor
# book "R tips: 16 howto's..."  has hints for 5-8
# chap 4 and 10
# chap 8: tally
# learn ggplot in datacamp: very important
# data visualization: very important

# two num: scatter plot
# one num, one cate: boxplot
# two cate: mosaic, two bar in a barplots side by side
# three mosaic: tree plot??

# some class notes for exploring data more:
# colnames(data)
# boxplot(data$avg_sat, col="red") 

# hist(data$avg_sat, col="green") # specially, the modes of your data
# hist(data$avg_sat, col="green", breaks=10) # with default option might be the best when theres no more other requirments on it
# binside normally is sqrt(#ofbins)

# rug(data$avg_sat) # where to see more frequencies
# rug(data$avg_sat, ticksize=0.03, side = 3) # side=1 is bottom, =3 is top; ticksize is the rug height

profit.bachelor <- subset(data, ownership=="For Profit" & primary_degree=="Bachelor") # subset the schools with requirements profit&bachelor 
profit.bachelor

# a.visualize revenue&spending, describe relationship
revenue.spending <- profit.bachelor[, c('revenue_per_student', 'spend_per_student')] # subset the revenue&spending from the previous subset
par(mfrow = c(1,2)) 
# normal distribution plot
plot(revenue.spending, main = "Revenue vs Spending Per Student", xlab = "Revenue (per student)", ylab = "Spending (per student)") 
# smoothScatter(): produces a smoothed color density representation of a scatterplot, obtained through a (2D) kernel density estimate.
smoothScatter(revenue.spending, main = "Revenue vs Spending Per Student", xlab = "Revenue (per student)", ylab = "Spending (per student)")
dev.off()

# b.create new variable called total_net_income. visualize the top 5 earning schools.
# Citation: calculation idea is from Piazza an answer by Jared Yu
# total_net_income can be calculated as: net per * student total
any(is.na(revenue.spending))==TRUE # first, check if there is any NA in the subset revenue.spending # returns false, so all valid

# then check if there is any NA in the populations
any(is.na(profit.bachelor$undergrad_pop))==TRUE  # false
any(is.na(profit.bachelor$grad_pop))==TRUE # true, set these NAs to 0 # this idea is from Piazza by an answer from Jared Yu
profit.bachelor$grad_pop[is.na(profit.bachelor$grad_pop)] <- 0
any(is.na(profit.bachelor$grad_pop))==TRUE # now it returns false

net.income.per.student <- revenue.spending$revenue_per_student - revenue.spending$spend_per_student
total.student.number <- profit.bachelor$undergrad_pop + profit.bachelor$grad_pop
total_net_income <- net.income.per.student *  total.student.number
total_net_income

# sort the total_net_income and choose the top 5
indexes.totalnet <- order(total_net_income, decreasing = TRUE) # returns the indexes by decreasing value order
profit.bachelor.top <- profit.bachelor[indexes.totalnet,] # returns the value by decreasing order (don't forget the comma)
profit.bachelor.top5 <- profit.bachelor.top[1:5,] # returns the top 5 schools infor (don't forget the comma)
profit.bachelor.top5["name"] # returns the top 5 schools names

# plot the net.income.per.student and total.student.number
# first add these two columns into profit.bachelor.top5 subset for convienience
profit.bachelor.top5$student_pop <- profit.bachelor.top5$undergrad_pop + profit.bachelor.top5$grad_pop
profit.bachelor.top5$student_pop
profit.bachelor.top5$college_net_income <- profit.bachelor.top5$revenue_per_student - profit.bachelor.top5$spend_per_student
profit.bachelor.top5$college_net_income
profit.bachelor.top5$total_college_net_income <-  profit.bachelor.top5$student_pop * profit.bachelor.top5$college_net_income
profit.bachelor.top5$total_college_net_income
par(mfrow = c(1,1)) 
plot(profit.bachelor.top5$student_pop, profit.bachelor.top5$total_college_net_income, main = "Total Student Population vs Total College Net Income in Top 5 Colleges", xlab = 
     "Total Student Population", ylab = "Total College Net Income (dollars)", pch = 1:5)
legend("bottomright", profit.bachelor.top5$name, pch = 1:5)


######### 9.avg sat & admission
# a.visualize the relationship with a plot. split data into two groups.
plot(data$avg_sat, data$admission, main = "Average SAT vs Admission", xlab = "Average SAT", ylab = "Admission rate")
# we can also use smoothScatter plot

# split data into two groups based on our observation in plot
# group1: high SAT: >= 1200 & admission <= 0.4 since that's like the cutoff point
# group2: low SAT: others + the NAs
group <- ifelse(data$avg_sat >= 1200 & data$admission <= 0.4, "Group1.High", "Group2.Low")
groupupdate <- ifelse(is.na(group), "Group2.Low", group) # this line of code follows Piazza response from Patrick Vacek
# add new column group to original data for convenience
data$group <- groupupdate
head(data)

# b.(a) med_10yr_salary
boxplot(data$med_10yr_salary ~ data$group, main = "Median 10 yrs Salary Depending on SAT Groups", ylab = "Median 10 yrs salary (dollars)")

# b.(b) race_white and rave_asian combined
data$race_combined <- data$race_white + data$race_asian # first combine two races
boxplot(data$race_combined ~ data$group, main = "White and Asian Population Combination Proportion Depending on SAT Groups", ylab = "White and Asian Population Combination Proportion")

# b.(c) grad students enrolled at a university
data$grad_proportion <- data$grad_pop / (data$grad_pop + data$undergrad_pop)
boxplot(data$grad_proportion ~ data$group, main = "Graduate Student Population Proportion Depending on SAT Groups", ylab = "Graduate Student Population Proportion")

# c.better to use mosaic plot for categorical variables
par(mfrow = c(2,2))
# (a) open_admission
plota <- mosaicplot(~ group + open_admissions, data = data, main = "Open Admission vs Group", xlab = "Group", ylab = "Open Admission")

# c.(b) main_campus
plotb <- mosaicplot(~ group + main_campus, data = data, main = "Main Campus vs Group", xlab = "Group", ylab = "Main Campus")

# c.(c) ownership
plotc <- mosaicplot(~ group + ownership, data = data, main = "Ownership vs Group", xlab = "Group", ylab = "Ownership")

# c.(d) whether the university has more than 1 branch
# first we need to group the branches
branches <- ifelse(data$branches == 1, "one", "more than one")
plotd <- mosaicplot(~ group + branches, data = data, main = "Branches vs Group", xlab = "Group", ylab = "Branches")


# 10. avg_10yr_salary and avg_family_inc for all schools
# (a). plot them. fit linear regression model. add prediction line. investigate groups of points that may be affecting the regression line.
plot(data$avg_family_inc, data$avg_10yr_salary, main = "Avg Family Income vs Avg 10 yrs Salary", xlab = "Avg Family Income", ylab = "Avg 10 yrs Salary")
model10a <- lm(avg_10yr_salary ~ avg_family_inc, data = data) # fit the linear regression model
abline(model10a, col = "red") # add regression line

# find the outliers: avg 10 yrs salary >= 150000 and not NA in both variables
outliers <- data[data$avg_10yr_salary >= 150000 & !is.na(data$avg_family_inc) & !is.na(data$avg_10yr_salary), ] # don't forget the comma
# find the outliers infor
outliers
dim(outliers)[1] # outlier counts = 9

# (b). 
par(mfrow = c(2,2)) 
plot(model10a, main = "Model: avg_10yr_salary ~ avg_family_inc") # old model
model10b <- lm(avg_10yr_salary ~ avg_family_inc + highest_degree, data = data) # fit the linear regression model
plot(model10b, main = "Model: avg_10yr_salary ~ avg_family_inc + highest_degree") # new model 

par(mfrow = c(1,1))
plot(data$avg_family_inc, data$avg_10yr_salary, main = "Avg Family Income and Highest Degree vs Avg 10 yrs Salary", xlab = "Avg Family Income + Highest Degree", ylab = "Avg 10 yrs Salary")
abline(model10b, col = "blue")






