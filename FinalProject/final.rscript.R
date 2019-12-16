#### STA 141a Final Project
#### Liya Li / Chengchen Luo / Sihui Li
library(grid)      # for plotting the image
library(ggplot2)
library(reshape2)

################### Q1
#### function load_training_images(): 
#### loads the training images and the corresponding labels from the 5 provided training binary files (not the testing bin file）
#### binds them into one data type (a list or a data frame or a matrix)  
#### saves this data type to an RDS file
#### function should have arguments to set the path for the input directory and the output RDS file

?file

# set the working directory
setwd("~/Desktop/Spring Quarter 2018/STA 141A/Final Project")

# create a list of files we take from the working directory
# coding idea from:
# https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
myFiles <- list.files(pattern="data_batch_.*bin")
myFiles

# example: combining two bin files
# file1 <- file(myFiles[1], "rb")
# file2 <- file(myFiles[2], "rb")
# id_1 <- readBin(file1, what = "raw", n = 3073*10000, endian = "little")
# id_2 <- readBin(file2, what = "raw", n = 3073*10000, endian = "little")
# id <- cbind(id_1, id_2)
# head(id)

# function
load_training_images <- function(files.vec)
{
    training_set <- {}
    
    for (i in 1:5)
    {
        id <- readBin(con = files.vec[i], what = "integer", signed = F, n = 3073*10000, endian = "big", size = 1)  # read in each bin.file
        training_set <- cbind(training_set, id)                                             # combine the bin.files into file: training_set_pre
    }
  
    saveRDS(training_set, file = "training_set.rds" )         # save training_set_pre as a RDS file
}

# run the function with myFiles and output the RDS file
load_training_images(myFiles)                                 
training_set <- readRDS("training_set.rds")

# change the column names and view the first 6 rows
colnames(training_set) <- c("bin1", "bin2", "bin3", "bin4", "bin5")
head(training_set)

#### function load_testing_images(): loads testing images and labels
#### binds them into one data type (list/data frame/matrix) 
#### saves this data type to another RDS file

# function
load_testing_images <- function(file)
{
    id <- readBin(con = file, what = "integer", size = 1, signed = F, n = 3073*10000, endian = "big")              # read in the bin.file
    saveRDS(id, file = "testing_set.rds" )                    # save testing_set_pre as a RDS file
}

# run the function with testfile and output the RDS file
testfile <- "test_batch.bin"
load_testing_images(testfile)
testing_set <- readRDS("testing_set.rds")
head(testing_set)

# rescale the dataset in Q4 section

################### Q2
#### function view_images(): displays one observation (one image) from the data set as a color image and its corresponding label 
#### function should allow users to specify which observation they want to display

# get all possible label names
labels <- read.table("~/Desktop/Spring Quarter 2018/STA 141A/Final Project/batches.meta.txt", head=F)
labels <- as.matrix(labels)
labels
# labels[10, 1][["V1"]]               # "trunk"
# is.character(labels[10, 1][["V1"]]) # True

# find the row numbers that indicate the label locations
label_index <- data.frame()

# convert the original rds file data to a data frame
original <- as.data.frame(training_set)

# update the entries in the data frame named label_index 
for(i in 0:9999){ 
    index_row = i * 3073 + 1
  
    for(j in 1:5)
    {
        index_col = j
        label_index[i+1,j] <- original[index_row, index_col]
    }
}
label_index  # each entry in this data frame represents the label of each images in our dataset


#### try example: 
# user <- 2 # here we input the index of label
# find all the possible user-wanted label locations
# user_choices <- which(label_index == user)
# output <- sample(user_choices, 1)
# output_col <- ceiling(output / 10000)
# output_col
# output_row <- output %% 10000
# if(output_row == 0){output_row <- 10000}
# output_row
# user_col <- output_col
# user_row_start <- (output_row - 1) * 3073 +1
# user_row_end <- output_row * 3073
# user_row_vec <- c(user_row_start:user_row_end)
# user_row_vec

# red <- as.matrix(training_set[user_row_vec[1:1024], user_col])
# green <- as.matrix(training_set[user_row_vec[1025:2048], user_col])
# blue <- as.matrix(training_set[user_row_vec[2049:3072], user_col])

# set.seed(1)   # make sure that every time we get the same random stuff
# raster_obj <- rgb(red, green, blue, maxColorValue = 255)  # create colors in color space

# grid.raster(raster.obj) # plot the image
# grid.raster(matrix(raster_obj, 32, 32, byrow=TRUE))
# table(raster_obj)


# function to convert user_input_char to the labels index
convert_label <- function(user_input_label){
    user_input <- 0
  
    for(i in 1:10){
        if(labels[i,][["V1"]] == user_input_label) # if the user input a label in our labels list, we obtain its label index 
        {   
            user_input <- i
        }
    }
    user_input
}

# function
view_image <- function(user_input_label, label_index, tranining_set){
    # first obtain label index by using convert user input label function
    user_input <- convert_label(user_input_label)
  
    # find all the possible user-wanted label locations
    user_input <- user_input - 1                              # in labels, label indexs(1-10), but in our dataset, label indexes(0-9) 
    user_choices <- which(label_index == user_input)          # find those label indexes in our dataset that are the same as our target label index
    output <- sample(user_choices, 1)                         # sample one from the pool
    
    # find the target image label and pixels locations
    output_col <- ceiling(output / 10000)
    output_row <- output %% 10000
    if(output_row == 0){ output_row <- 10000 }
    
    user_col <- output_col
    user_row_start <- (output_row - 1) * 3073 +1
    user_row_end <- output_row * 3073
    user_row_vec <- c(user_row_start:user_row_end)
    user_row_vec
    
    # evaluate the color blocks and plot the image
    red <- as.matrix(training_set[user_row_vec[1:1024], user_col])
    green <- as.matrix(training_set[user_row_vec[1025:2048], user_col])
    blue <- as.matrix(training_set[user_row_vec[2049:3072], user_col])
    
    raster_obj <- rgb(red, green, blue, maxColorValue = 255)  # create colors in color space
    grid.raster(matrix(raster_obj, 32, 32, byrow=TRUE), interpolate = F)       # plot the image
    # interpolate = F: smooth the image
    
    # add title to the image
    grid.text(user_input_label, gp = gpar(col="green", fontsize=20))
}

# example:
view_image("cat", label_index, tranining_set)  # a cat image
view_image("deer", label_index, tranining_set) # a deer image


################### Q3
#### Explore the image data. In addition to your own explorations:
#### Display graphically what each class (airplane, bird, cat...) looks like. You can randomly choose one image per class.
#### Which pixels at which color channels seem the most likely to be useful for classification? 
#### Which pixels at which color channels seem the least likely to be useful for classification? Why?

# display 10 images from 10 classes
for (i in 1:10){
    png(paste(i, ".png", sep=''))                                  # create files and set file names
    view_image(labels[i,][["V1"]], label_index, tranining_set)     # plot image to files(images saved in directory file)
    dev.off()
}

# briefly subset the training data
trainc = do.call("rbind", lapply(1:5, function(x) matrix(training_set[,x], nrow = 10000, byrow=TRUE)))
trainc = data.frame(trainc)

# change column names to display the pixel with its color belonging
names(trainc) = c("label", paste0("red", 1:1024), paste0("green", 1:1024), paste0("blue", 1:1024))

# obtain sd and find the max and min
trainc_sd = apply(trainc[,-1], 2, sd)
max_sd <- which.max(trainc_sd)            # max
min_sd <- which.min(trainc_sd)            # min
sort(trainc_sd, decreasing=TRUE)[1:10]
sort(trainc_sd, decreasing=FALSE)[1:10]


###################################################################################################################
########## rescale the dataset #########
#### first, rearrange the values in training_set into the form in each row: 1 label + 3072 pixels
dim(training_set)
data_training <- matrix(as.matrix(training_set), nrow = 50000, ncol = 3073, byrow = T)     
#### data_training[,1] # the first col is the label col

#### then, rescale our dataset
row_indexes <- {}   # push the row indexes that we want after rescaling

get_row_indexes <- function(label, data_training){
    row_indexes <- which(data_training[,1] == label)      # find all indexes whose label num is label
    row_indexes <- sample(row_indexes, 500)               # return random 500 indexes
}

#### for each label value, after finding the subset 500 indexes, put them all into row_indexes
for(i in 0:9){
    new_add_row_indexes <- get_row_indexes(i, data_training)
    row_indexes <- c(row_indexes, new_add_row_indexes)
}
row_indexes  # this variable contains all the row indexes of data we want in data_training 

#### obtain the training data after rescaling
data_training <- data_training[row_indexes,]
dim(data_training)

#### shuffle by rows and update the data_training
set.seed(2018)
shuffle_training_index <- sample(1:5000)
data_training <- data_training[shuffle_training_index, ]

##########################
#### with the same method, rescale the testing_set into the form in each row: 1 lable + 3072 pixels
data_testing <- matrix(as.matrix(testing_set), nrow = 10000, ncol = 3073, byrow = T) 
row_indexes2 <- {}

for(i in 0:9){
    new_add_row_indexes2 <- get_row_indexes(i, data_testing)
    row_indexes2 <- c(row_indexes2, new_add_row_indexes2)
}
row_indexes2
data_testing <- data_testing[row_indexes2,]
dim(data_testing)

# shuffle by rows and update the data_testing
set.seed(2018)
shuffle_testing_index <- sample(1:1000)
data_testing <- data_testing[shuffle_testing_index, ]

# after rescaling, we obtain: data_training, data_testing, each has dimension 5000*3073 indicating 5000 images.
###################################################################################################################


################### Q4
#### function predict_knn(): uses k-nearest neighbors to predict the label for a point or collection of points. 
#### At the least, your function should take the prediction point(s), the training points, a distance metric, and k as input.
dist_func <- function(predict_point, training_point, dist_point, measure){
    the_matrix <- rbind(predict_point, training_point)
    dist_mat <- as.matrix(dist(the_matrix[,2:3073], method = measure, diag = T, p = dist_point ))
    dist_mat
}

predict_knn <- function(predict_point, training_point, dist_mat, measure, k){
    n_pre <- dim(predict_point)[1]    # how many prediction point
    the_matrix <- rbind(predict_point, training_point)
    n_mat <- dim(the_matrix)[1]
    point<- n_pre + 1                 # where the training point start in the matrix
    
    predict_lable <- {}
  
    for (i in 1:n_pre)
    {
        row_number <- order(dist_mat[i , point:n_mat]) + n_pre    # order the distance and get their row number
        lable <- the_matrix[row_number,1] [1:k]                   # get the knn lable
        
        k_smallest_df <- as.data.frame(table(lable))              # use table to find freqency
        guessing_index <- which.max(k_smallest_df$Freq)           # find the index of the guessing label num which has most freq
        guessing <- as.numeric(as.vector(k_smallest_df$lable[guessing_index]))   #find the guessing label num
        predict_lable <- c(predict_lable, guessing)
    }
  
    predict_lable    # list the predicted label nums
}

# testing example:
haha_pre <- data_training[1:100,]
dim(haha_pre)[1]
haha_train <- data_training[101:1000,]
haha_dist_mat <- dist_func(haha_pre, haha_train, 2, "euclidean")  # p=2
predict_knn(haha_pre, haha_train, haha_dist_mat, "euclidean", 3) # k=3

################### Q5
#### function cv_error_knn(): uses 10-fold cross-validation to estimate the error rate for k-nearest neighbors. 
#### Briefly discuss the strategies you used to make your function run efficiently.

#### first, find the distance_matrix
# distance_matrix: 5000*5000, each cell represents the distance between two corresponding images, smaller distance = image more similar
distance_matrix <- as.matrix(dist(data_training[, 2:3073], method = "euclidean", diag = T, upper = T))
dim(distance_matrix)       # 5000*5000
# distance_matrix[1:10, 2]

#### detailed strategy
# then, find the 10 error rates and the average on them
# visually, we divide the 5000 rows into 10 folds, meaning each fold with 500 image distances        (step A)
# the visual matrix: 10*10 blocks, each block represents 500*500 image distances
# within this visual matrix, at each row, we obtain the training images block                        (step B)
# diagnals are testing images block, and the rest are training images block
# compare distance of each image in a block with 500 images to its 4500 training images              (step C)
# obtain the error number in guessing the true label                                                 (step D)
# in each block, there will be 500 guessings; then, find the error rate                              (step E)
# in total, we will get 10 error rates after the 10-fold cross-validation, and we take the average   (step F)

# how to do (step C):
# in order to calculate whether each image among 500 matching the other 4500, we use the distance_matrix to compare and 
# at any given k, we pick the k smallest differences among 4500, and obtain the guessing label by finding the most label occurancy in the k differences

# for example, in a block of 500 images, among them, there are 300 cases where: 
# k=5, then we find 5 smallest differences among 4500 where 3 indicate the guessing should be "cat", so we guess the real label of this image is "cat"
# the 300 cases are for real "dog", but their guessings are "cat", so these are error
# assuming that the other 200 label(real vs guessing) are matching
# the error rate of this cross-validation is 300/500=0.6
# we can find the other 9 error rates and take the average on 10 to obtain our final answer

# cv_error_knn(): return the average error rate after 10-fold cross-validation
cv_error_knn <- function(distance_mat, k, data_train)
{
    error_array <- {}
  
    # (step A)
    for(i in 1:10)      # 10-fold validation
    {
        block_start <- (i-1)*500+1
        block_end <- i*500
    
        # (step B)
        sub_training <- distance_mat[block_start:block_end, -c(block_start:block_end)] 
    
        lalala <- c() # create the guessing output arr called lalala
    
        # (step C)
        for(j in 1:500)
        {
            sub_training_dis <- as.vector(sub_training[j,])   
            sub_training_dis_index <- c()
      
            if(block_start==1)
            {
                sub_training_dis_index <- c(501:5000)
            }
            if(block_start==4501)
            {
                sub_training_dis_index <- c(1:4500)
            }
            if(block_start!=1 && block_start!=4501)
            {
                sub_training_dis_index <- c(1:(block_start-1), (block_end+1):5000)
            }
      
            find_smallest_k_dis <- as.data.frame( cbind(sub_training_dis_index, sub_training_dis) )
            the_order <- order(find_smallest_k_dis[,2]) [1:k]      # obtain the first k smallest distance 
            
            training_labels <- find_smallest_k_dis[,1][the_order]  # the labels according to choosing k dis
            learning_lables <- data_train[training_labels,1]       # the labels that are the true ones
            
            # find the most frequent label we guess and mark it a guessing
            k_smallest_df <- as.data.frame(table(learning_lables))              
            guessing_index <- which.max(k_smallest_df$Freq)           
            guessing <- as.numeric(as.vector(k_smallest_df$learning_lables[guessing_index]))
            lalala <- c(lalala, guessing)                          
        }
    
        # (step D)
        unmatched <- 0
        itr <- 1
    
        for(w in block_start : block_end)
        {
            if(data_train[w,1] != lalala[itr]) {unmatched <- unmatched+1}
            itr = itr + 1
        }
    
        # (step E)
        error_rate <- unmatched/500
        error_array <- c(error_array, error_rate) 
    }
    
    # (step F)
    error_avg <- sum(error_array)/10
    error_avg
}

cv_error_knn(distance_matrix, 3, data_training)
cv_error_knn(distance_matrix, 10, data_training)


################### Q6
#### Display 10-fold CV error rates for k = 1, ..., 15 and at least 2 different distance metrics in one plot. 
#### Discuss your results. 
#### Which combination of k and distance metric is the best? 
#### Would it be useful to consider additional values of k?

# find error rates for euclidean and maximum
# euclidean
# distance_matrix: created under Q5
q6_10fold_cv_error1 <- c()
for (i in 1:15)
{
    a <- cv_error_knn(distance_matrix, i, data_training)
    q6_10fold_cv_error1 <- c(q6_10fold_cv_error1, a)
}

# maximum
distance_matrix2 <- as.matrix(dist(data_training[,2:3073], method = "maximum", diag = T))
q6_10fold_cv_error2 <- c()
for (i in 1:15)
{
    a = cv_error_knn(distance_matrix2, i, data_training)
    q6_10fold_cv_error2 <- c(q6_10fold_cv_error2, a)
}

# create error rate date frame
q6_cv_error_rate <- as.data.frame(cbind(c(1:15), q6_10fold_cv_error1, q6_10fold_cv_error2))

# plot the error rates for different k and metric using ggplot
error_rate_plot <- ggplot(q6_cv_error_rate, aes(V1)) +                          
                  geom_line(aes(y = q6_10fold_cv_error1), colour="red") +  
                  geom_line(aes(y = q6_10fold_cv_error2), colour="green")+
                  ylab("Error Rate") + xlab("k-Value") + 
                  labs(title = "Error Rate For Different Combination of k and Matrix")
error_rate_plot

order(q6_cv_error_rate[2])[1:3] # best three k in euclidean
order(q6_cv_error_rate[3])[1:3] # best three k in maximum

################### Q7
#### For each of the 3 best k and distance metric combinations, use 10-fold cross-validation to estimate the confusion matrix. 
#### Discuss your results. Does this change which combination you would choose as the best?
cv_error_knn2 <- function(distance_matrix, k, data_training)
{
    error_array <- {}
    predict_label = {}
    true_label = {}
  
    for(i in 1:10)
    {
        block_start <- (i-1)*500+1
        block_end <- i*500
        
        sub_training <- distance_matrix[block_start:block_end, -c(block_start:block_end)] 
        
        lalala <- c()
      
        for(j in 1:500)
        {
            sub_training_dis <- as.vector(sub_training[j,])
            sub_training_dis_index <- c()
            if(block_start==1)
            {
                sub_training_dis_index <- c(501:5000)
            }
            if(block_start==4501)
            {
                sub_training_dis_index <- c(1:4500)
            }
            if(block_start!=1 && block_start!=4501)
            {
                sub_training_dis_index <- c(1:(block_start-1), (block_end+1):5000)
            }
            
            find_smallest_k_dis <- as.data.frame( cbind(sub_training_dis_index, sub_training_dis) )
            the_order <- order(find_smallest_k_dis[,2]) [1:k] 
            
            training_labels <- find_smallest_k_dis[,1][the_order]
            
            learning_lables <- data_training[training_labels,1]
            
            guessing = as.numeric(names(sort(table(learning_lables), decreasing=TRUE)[1]))
            lalala <- c(lalala, guessing)
        }
      
        error_array <- c(error_array, mean(data_training[block_start:block_end, 1] != lalala))     
        predict_label = c(predict_label, lalala)
        true_label = c(true_label, data_training[block_start:block_end, 1])
    }
  
      error_avg <- sum(error_array)/10
      return(list(true = true_label, predict = predict_label))
}


euclidean_labels = lapply(1:15, function(x) cv_error_knn2(distance_matrix, x, data_training))
maximum_labels = lapply(1:15, function(x) cv_error_knn2(distance_matrix2, x, data_training))


one_k <- melt(table(euclidean_labels[[1]]$true, euclidean_labels[[1]]$predict))
names(one_k)[3] <- c("Frequency")
ggplot(one_k, aes(Var1, Var2 )) +
      geom_tile(aes(fill = Frequency), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("Predict value") +
      xlab("Ture value") +ggtitle("k = 1 and eucliedan")

four_k <- melt(table(euclidean_labels[[4]]$true, euclidean_labels[[4]]$predict))
names(four_k)[3] <- c("Frequency")
ggplot(four_k, aes(Var1, Var2 )) +
      geom_tile(aes(fill = Frequency), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("Predict value") +
      xlab("Ture value") + ggtitle("k = 4 and eucliedan")

fift_k <- melt(table(euclidean_labels[[15]]$true, euclidean_labels[[15]]$predict))
names(fift_k)[3] <- c("Frequency")
ggplot(fift_k, aes(Var1, Var2 )) +
      geom_tile(aes(fill = Frequency), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("Predict value") +
      xlab("Ture value") + ggtitle("k = 15 and eucliedan")

ten_k <- melt(table(maximum_labels[[10]]$true, maximum_labels[[10]]$predict))
names(ten_k)[3] <- c("Frequency")
ggplot(ten_k, aes(Var1, Var2 )) +
      geom_tile(aes(fill = Frequency), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("Predict value") +
      xlab("Ture value") + ggtitle("k = 10 and maximum")

nine_k <- melt(table(maximum_labels[[9]]$true, maximum_labels[[9]]$predict))
names(nine_k)[3] <- c("Frequency")
ggplot(nine_k, aes(Var1, Var2 )) +
      geom_tile(aes(fill = Frequency), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("Predict value") +
      xlab("Ture value") + ggtitle("k = 9 and maximum")

six_k <- melt(table(maximum_labels[[6]]$true, maximum_labels[[6]]$predict))
names(six_k)[3] <- c("Frequency")
ggplot(six_k, aes(Var1, Var2 )) +
      geom_tile(aes(fill = Frequency), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("Predict value") +
      xlab("Ture value") + ggtitle("k = 6 and maximum")


################### Q8
#### For the best k and distance metric combination, explore the training data that were misclassified during cross-validation by displaying a confusion matrix. 
#### Discuss what you can conclude about the classifier.
################### Q8
#### For the best k and distance metric combination, explore the training data that were misclassified during cross-validation by displaying a confusion matrix. 
#### Discuss what you can conclude about the classifier.

cv_error_knn_q8 <- function(distance_matrix, k, data_training)
{
    predict_index <- c()
  
    for (i in 1:5000)
    {
        the_order <- order(distance_matrix[i,-i])[1:k]
        learning_lables <- data_training[the_order,1]
        k_smallest_df <- as.data.frame(table(learning_lables))  
    
        guessing_index <- which.max(k_smallest_df$Freq)           
        guessing <- as.numeric(as.vector(k_smallest_df$learning_lables[guessing_index]))
        predict_index <- c(predict_index, guessing)
    }
  
    cv_table <- as.data.frame(cbind(data_training[,1], predict_index))
    table(cv_table$V1, cv_table$predict_index)
}

df_q8 = melt(cv_error_knn_q8(distance_matrix,1,data_training))

ggplot(df_q8, aes(Var1, Var2 )) +
      geom_tile(aes(fill = value), color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      ylab("Predict value") +
      xlab("Ture value") + ggtitle("k = 1 and euclidean")



################### Q9
#### Display test set error rates for k = 1, ..., 15 and at least 2 different distance metrics in one plot. 
#### Compare your results to the 10-fold CV error rates.

# first, obtain two different distance metrics with 6000*6000
# measure in dist() can be: "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
dist1 <- dist_func(data_testing, data_training, 2, "euclidean")
dist2 <- dist_func(data_testing, data_training, 2, "maximum")

# then, find the test set error rates with two different metric
list_euclidean <- lapply(1:15, function(x) predict_knn(data_testing, data_training, dist1, "euclidean", x))
list_maximum <- lapply(1:15, function(x) predict_knn(data_testing, data_training, dist2, "maximum", x))
  
# pred_knn_output: 1000*1, dt_testing:1000*3073
find_test_error_rate <- function(pred_knn_output, dt_testing){
    TF <- (pred_knn_output == dt_testing[,1])      # compare two vectors' values
    count_F <- length(which(TF==FALSE))            # count the number of different pairs
    test_error_rate <- count_F / 1000              # compute the error rate
    test_error_rate                                
}

list_euc_rate <- lapply(1:15, function(x) find_test_error_rate(list_euclidean[[x]], data_testing))
vec_euc_rate <- unlist(list_euc_rate)
vec_euc_rate

list_max_rate <- lapply(1:15, function(x) find_test_error_rate(list_maximum[[x]], data_testing))
vec_max_rate <- unlist(list_max_rate)  
vec_max_rate

# display test set error rates in a plot
vec_rate <- as.data.frame(cbind(c(1:15), vec_euc_rate, vec_max_rate))
vec_rate
ter_plot <- ggplot(vec_rate, aes(V1)) + 
            geom_line(aes(y=vec_euc_rate), colour = "red") + 
            geom_line(aes(y=vec_max_rate), colour = "blue") +
            ggtitle("Test Set Error Rates Plot (red line for euc, blue line for max)") +
            xlab("Value of k") + ylab("Error Rate") 
ter_plot


