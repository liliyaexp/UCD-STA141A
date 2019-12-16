## HW3 Liya Li
## Monopoly

library(ggplot2)

############################################
################# Part One #################
############################################

## 1.1 Write a function simulate_monopoly() that simulates n turns by a player in a game of Monopoly using two d-sided dice. 
## The inputs to your function should be n and d. 
## The output of your function should be a length n+1 vector of positions, encoded as numbers from 0 to 39.

##### some basics and functions we need for the simulate_monopoly() function
# piles for CC and CH for shuffling if I land on any CC or CH 
pile.CC <- c("CC1", "CC2", rep(NA,14)) 
pile.CH <- c("GO", "JAIL", "C1", "E3", "H2", "R1", rep("nextrr",2), "nextut", "back3", rep(NA,6) )

# next.RR: a function returns the index for the next RR
next.RR <- function(current.pos){
      if(current.pos == 7){ current.pos <- 15 }
      else if(current.pos == 22){ current.pos <- 25}
      else if(current.pos == 36){ current.pos <- 5}
      current.pos
}

# next.UT: a function returns the index for the next utility
next.UT <- function(current.pos){
      if(current.pos == 7){ current.pos <- 12 }
      else if(current.pos == 22){ current.pos <- 28}
      else if(current.pos == 36){ current.pos <- 12}
      current.pos
}

# back.3: a function returns the index for going back 3 steps
back.3 <- function(current.pos){
      if(current.pos >= 3){current.pos <- current.pos - 3}
      else if(current.pos == 2){current.pos <- 39}
      else if(current.pos == 1){current.pos <- 38}
      else if(current.pos == 0){current.pos <- 37}
      current.pos
}

# double.check: a function return the indexes of the third in third doubles 
doubles.check <- function(doublesTF){
      count <- 0
      thirds <- c()
      
      for(i in 1:length(doublesTF))
      {
        if(doublesTF[i]==T)
        {
          if(count==2)
          {
            thirds <- c(thirds,i) # at this i, three consecutive doubles occur
            count <- 0            # reset count
          }
          else
          {
            count <- count +1
          }
        }
        else  # when F, reset count
        {
          count <- 0 
        }
      }
      
      cat(thirds) # return the index of three consecutive doubles occurring, so at these indexes, proceed directly to jail
}

#####
# n: turn numbers, d: dice sides
simulate_monopoly <- function(n, d)
{
      # roll two d-sided dice for n times
      die1 <- sample(d, n, replace = T)
      die2 <- sample(d, n, replace = T)
      
      # create a vector of doubles and rolls
      doubles <- (die1 == die2)
      doubles
      rolls <- die1 + die2
      rolls   # rolls: a vector of steps for each turn
      
      # find thirds 
      consecutive.index <- doubles.check(doubles)
      consecutive.index
      
      #### begin the game
      # create current.pos <- 0
      current.pos <- 0
      
      # shuffle the piles
      cards.CC <- sample(pile.CC)
      cards.CH <- sample(pile.CH)
      
      # create path which will be updated and finally becomes the output for simulate_monopoly(), put 0 in it
      path <- 0      
      
      # for each turn, do the following
      for(i in 1:n){
            if(i %in% consecutive.index)         ##### if three doubles
            {
                  current.pos <- 10              # directly go to jail
                  path <- c(path, current.pos)
            }
            else                        
            {
                  current.pos <- current.pos + rolls[i] # update the current.pos by adding the dice roll sum at index i
                  
                  # use for step-by-step analysis
                  # cat(current.pos)    # print out the current.pos 
                  # cat("\n")           # print out a new line after
                  # cat("cards.cc\n")   
                  # cat(cards.CC)       # print out the current CC ordered cards
                  # cat("\ncards.ch\n")
                  # cat(cards.CH)       # print out the current CH ordered cards
                  # cat("\n")
                  
                  # check whether we exceed the map max num 39
                  if(current.pos > 39)
                  {
                        current.pos <- current.pos%%40  ##### if current.pos exceeds 39, then it starts from 0 again
                  }
          
                  # check where we land at based on the updated current.pos
                  if(current.pos == 30)                 ##### if we land at 30 "G2J", we move to 10 "JAIL"
                  {   
                        current.pos <- 10
                        path <- c(path, current.pos) # update the path for each movement
                  }                 
                  else if(current.pos %in% c(2,17,33))   ##### if we land at 2 or 17 or 33 "CC"
                  {
                        chosen <- cards.CC[1]                     # choose the first card in "CC"
                        cards.CC <- c(cards.CC[-1], cards.CC[1])  # put this card back to the tail in "CC"
                        
                        # check what card it is and update the current.pos
                        if(is.na(chosen))
                        {
                          path <- c(path, current.pos)
                        }
                        else if(chosen == "CC1")
                        {
                          current.pos <- 0
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "CC2")
                        {
                          current.pos <- 10
                          path <- c(path, current.pos) # update the path for each movement
                        }
                  }
                  else if(current.pos %in% c(7,22,36))    ##### if we land at 7 or 22 or 36 "CH"
                  {
                        chosen <- cards.CH[1]                     # choose the first card in "CH"
                        cards.CH <- c(cards.CH[-1], cards.CH[1])  # put this card back to the tail in "CH"
                        
                        # check what card it is and update the current.pos
                        if(is.na(chosen))
                        {
                          path <- c(path, current.pos)
                        }
                        else if(chosen == "GO")
                        {
                          current.pos <- 0
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "JAIL")
                        {
                          current.pos <- 10
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "C1")
                        {
                          current.pos <- 11
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "E3")
                        {
                          current.pos <- 24
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "H2")
                        {
                          current.pos <- 39
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "R1")
                        {
                          current.pos <- 5
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "nextrr")
                        {
                          current.pos <- next.RR(current.pos)
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "nextut")
                        {
                          current.pos <- next.UT(current.pos)
                          path <- c(path, current.pos) # update the path for each movement
                        }
                        else if(chosen == "back3")
                        {
                          current.pos <- back.3(current.pos)
                          path <- c(path, current.pos) # update the path for each movement
                        }
                 }
                 else
                 {
                        path <- c(path, current.pos)   # if land on normal place, just update the path by adding the new current.pos in it
                 }
            }
        
            # use for step-by-step analysis
            # cat(rolls) # print out the rolls for comparing
            # cat("\n")  # print out a new line after
            # cat(path)  # print out the path after each movement                    
            # cat("\n")  # print out a new line after
      }
      
      path  # return the path, which is a numeric factor
}

# simulate_monopoly(10,6) # example: 10 turns with two 6-sided dice
# simulate_monopoly(20,4) # example: 20 turns with two 4-sided dice


## 1.2 Write a function estimate_monopoly() that uses your simulation to estimate the long-term probabilities of ending a turn on each Monopoly square.
## What are the 3 most likely squares to end a turn on if you play Monopoly with 6-sided dice? What if you play with 4-sided dice?
## Display graphically the long-term probabilities for 3, 4, 5, and 6-sided dice.
## Use number of turns n=10000 (Patrick's suggestion on Piazza)

## single example: find most highest squares when n=100, d=6, k=1
# output <- simulate_monopoly(100,6)
# is.vector(output)                                   # True
# tab <- table(factor(output, levels = 0:39))         # table the output vector and find the frequencies
# prop.table(tab)                                     # probability table
# vec.sorted <- sort(prop.table(tab)) 
# vec.sorted
# top <- tail(vec.sorted,3)                           # top 3
# top
# names(top)                                          # top 3's associated positions

estimate_monopoly <- function(n, d){
  output <- simulate_monopoly(n, d)
  tab <- table(factor(output, levels = 0:39))         # use factor(): display the 0 frequencies
  prop <- prop.table(tab)
  top <- tail(sort(prop), 3)
  prop   # this function returns the prop table for plotting
}

# apply the estimate_monopoly() to each number of dice sides
dice.prop <- lapply(c(3,4,5,6), function(x) estimate_monopoly(10000, x))
dice.prop
is.list(dice.prop)   # True

# change the list names
names(dice.prop) <- c("3-sided", "4-sided", "5-sided", "6-sided")
dice.prop

# find top3 and plot long term prop for different dice
par(mfrow = c(1,1))
mat <- do.call(cbind, dice.prop)
top3.3 <- tail(sort(mat[,1]), 3) # returns top 3 with 3-sided dice, position index & its prop value
top3.4 <- tail(sort(mat[,2]), 3) # returns top 3 with 4-sided dice, position index & its prop value
top3.5 <- tail(sort(mat[,3]), 3) # returns top 3 with 5-sided dice, position index & its prop value
top3.6 <- tail(sort(mat[,4]), 3) # returns top 3 with 6-sided dice, position index & its prop value
top3.3
top3.4
top3.5
top3.6
matplot(x = rownames(mat), y = mat[,1:4], type='l', lty = 1:4, main = "Matplot: Long Term Probability For Different Sided Dice", xlab = "Position Index", ylab = "Occurrence Probability")
legend("topright", legend = colnames(mat)[1:4], col = 1:4, lty = 1:4) # add legend
# if use:
# matplot(mat, type='l', lty = 1:4, main = "Matplot: Long Term Probability For Different Sided Dice", xlab = "Position Index", ylab = "Occurrence Probability")
# will have a plot which shifts x values to the left. Why?

## 1.3 Use k = 1000 simulations with n = 10000 rolls each to estimate the standard error for the long-term probability of ending a turn in jail.
## Use the standard amount of dice rolls, d = 6.
k <- 1000
n <- 10000
d <- 6
sample.est <- replicate(k, estimate_monopoly(n, d))
sd(sample.est[11,])  # position 11 in sample represents jail
# use keyboard command esc to stop the process in console if needed


############################################
################# Part Two #################
############################################

## 2.1 Write an updated version of the simulate_monopoly() function called simulate_monopoly2(). 
## The function should now take in numeric vectors property and rent as additional inputs. 
## The property represents the indices at which a player loses money if they land on them. 
## The rent represents the associated rent that a player plays for landing on each corresponding property. 
## Furthermore, the function should return the n + 1 board positions (coded 0-39) and the money gained or lost after each dice roll.

simulate_monopoly2 <- function(n, d, property, rent)
{
    moneychange <- c(rep(0,n+1))            # the amount of change of money 

    # roll two d-sided dice for n times, create a vector of doubles and rolls, check consecutive
    die1 <- sample(d, n, replace = T)
    die2 <- sample(d, n, replace = T)
    doubles <- (die1 == die2)
    doubles
    rolls <- die1 + die2
    rolls
    LP <- numeric(n+1) # for creating indicator for 2.3: credited to Patrick during Office Hour
    consecutive.index <- doubles.check(doubles)  # find thirds 
    consecutive.index
    
    # create current.pos <- 0, shuffle the piles, create path
    current.pos <- 0
    cards.CC <- sample(pile.CC)
    cards.CH <- sample(pile.CH)
    path <- 0      
  
    # for each turn, do the following
    for(i in 1:n){
        if(i %in% consecutive.index)         ### if three doubles
        {
              current.pos <- 10                 # directly go to jail
              path <- c(path, current.pos)
        }
        else                        
        {
              current.pos <- current.pos + rolls[i] # update the current.pos by adding the dice roll sum at index i
              
              # use for step-by-step analysis
              # cat(current.pos)    # print out the current.pos 
              # cat("\n")           # print out a new line after
              # cat("cards.cc\n")   
              # cat(cards.CC)       # print out the current CC ordered cards
              # cat("\ncards.ch\n")
              # cat(cards.CH)       # print out the current CH ordered cards
              # cat("\n")
          
              # check whether we exceed the map max num 39
              if(current.pos > 39)
              {
                    moneychange[i+1] <- 200  
                    current.pos <- current.pos%%40   ##### if current.pos exceeds 39, then it starts from 0 again
              }
              # passing GO or land on GO earn 200, adjustment later 
          
              #check where we land at, update current.pos
              if(current.pos == 30)                  ##### if we land at 30 "G2J", we move to 10 "JAIL"
              {   
                    current.pos <- 10
                    path <- c(path, current.pos) # update the path for each movement
              }                 
              else if(current.pos %in% c(2,17,33))   ##### if we land at 2 or 17 or 33 "CC"
              {
                    chosen <- cards.CC[1]                     # choose the first card in "CC"
                    cards.CC <- c(cards.CC[-1], cards.CC[1])  # put this card back to the tail in "CC"
                    
                    # check what card it is and update the current.pos
                    if(is.na(chosen))
                    {
                          path <- c(path, current.pos)
                    }
                    else if(chosen == "CC1")
                    {
                          current.pos <- 0
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "CC2")
                    {
                          current.pos <- 10
                          path <- c(path, current.pos) # update the path for each movement
                          
                          if(current.pos == 2)
                          {
                                moneychange[i+1] <- 0  ### go to jail, so lose the Go money/remain no gain no loss
                          }
                    }
              }
              else if(current.pos %in% c(7,22,36))   ##### if we land at 7 or 22 or 36 "CH"
              {
                    chosen <- cards.CH[1]                     # choose the first card in "CH"
                    cards.CH <- c(cards.CH[-1], cards.CH[1])  # put this card back to the tail in "CH"
                    
                    # check what card it is and update the current.pos
                    if(is.na(chosen))
                    {
                          path <- c(path, current.pos)
                    }
                    else if(chosen == "GO")
                    {
                          current.pos <- 0
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "JAIL")
                    {
                          current.pos <- 10
                          path <- c(path, current.pos) # update the path for each movement
                          
                          if(current.pos == 7)
                          {
                                moneychange[i+1] <- 0  ### go to jail, so lose the Go money/remain no gain no loss
                          }
                    }
                    else if(chosen == "C1")
                    {
                          current.pos <- 11
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "E3")
                    {
                          current.pos <- 24
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "H2")
                    {
                          current.pos <- 39
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "R1")
                    {
                          current.pos <- 5
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "nextrr")
                    {
                          current.pos <- next.RR(current.pos)
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "nextut")
                    {
                          current.pos <- next.UT(current.pos)
                          path <- c(path, current.pos) # update the path for each movement
                    }
                    else if(chosen == "back3")
                    {
                          current.pos <- back.3(current.pos)
                          path <- c(path, current.pos) # update the path for each movement
                          
                          if(current.pos == 7)
                          {
                                moneychange[i+1] <- moneychange[i+1]-200  ### go back 3 steps and land on T1, so lose 200
                          }
                    }
              }
              else   # if land on normal squares
              {
                    path <- c(path, current.pos)   
                    
                    ## check the final position and see whether we earn or lose and how much
                    if(current.pos %in% property) ### if we land on any opponent's property, we lose the corresponding rent
                    {
                          LP[i+1] <- 1  ############ if we land on any opponent's property, the indicator changes to 1
                          property.index <- which(property == current.pos)      # find index of the property
                          moneychange[i+1] <- moneychange[i+1] - rent[property.index]  
                    }
                    else if(current.pos == 4)     ### if land on "T1", we lose $200
                    {
                          moneychange[i+1] <- moneychange[i+1] - 200
                    }
                    else if(current.pos == 38)    ### if we land on "T2", we lose $100
                    { 
                          moneychange[i+1] <- moneychange[i+1] - 100 
                    } # otherwise, no gain no loss
              }
        }
    }
      
    # rolls        # for testing purpose
    # property
    # path
    # moneychange    
    
    output2.1 <- cbind(path, moneychange, LP) 
    output2.1
}
        
# testing example:
# n <- 10  # 10 turns
# d <- 6   # with two 6-sided dice 
# property <- c(1, 3, 18, 23, 37, 39)      # indexes of opponent's properties
# rent <- c(100, 100, 100, 100, 100, 100)  # rents corresponding to property
# simulate_monopoly2(n ,d, property, rent)

## 2.2 Run simulate_monopoly2() with n = 100 dice rolls and k = 1000 simulations for all eight colors. 
## After each simulation is done, sum the money change at each turn to get the total money gained/lost per simulation. 
## Which color seems to be the most effective when it has hotels on it? Which color is the least effective? 
## Provide at least one informative visual showing in your report. You may add more graphics if you desire. 
## To save time for this question, use the properties.csv dataset from Canvas. 
## For more information on the features of this data, see the Requirements section.
color <- read.csv("~/Desktop/Spring Quarter 2018/STA 141A/hw3/color_combos (1).csv")
View(color)
properties <- read.csv("~/Desktop/Spring Quarter 2018/STA 141A/hw3/properties (1).csv")
View(properties)

colors <- c("Purple", "Light Blue", "Red", "Orange", "Pink", "Yellow", "Green", "Blue")
subsets <- lapply(colors, function(x) subset(properties, Color == x))
subsets  # obtain the subsets in terms of each color
names(subsets) <- colors  # change the list names
subsets
# is.data.frame(subsets$Purple) # True

n2 <- 100
k2 <- 1000
d2 <- 6

# obtain samples for each color
purple.pos <- subsets$Purple[["Index"]]
purple.rent <- subsets$Purple[["Rent"]]
purple.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, purple.pos, purple.rent)[,2]))
purple.sample

red.pos <- subsets$Red[["Index"]]
red.rent <- subsets$Red[["Rent"]]
red.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, red.pos, red.rent)[,2]))
red.sample

green.pos <- subsets$Green[["Index"]]
green.rent <- subsets$Green[["Rent"]]
green.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, green.pos, green.rent)[,2]))
green.sample

blue.pos <- subsets$Blue[["Index"]]
blue.rent <- subsets$Blue[["Rent"]]
blue.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, blue.pos, blue.rent)[,2]))
blue.sample

lblue.pos <- subsets$`Light Blue`[["Index"]]
lblue.rent <- subsets$`Light Blue`[["Rent"]]
lblue.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, lblue.pos, lblue.rent)[,2]))
lblue.sample

pink.pos <- subsets$Pink[["Index"]]
pink.rent <- subsets$Pink[["Rent"]]
pink.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, pink.pos, pink.rent)[,2]))
pink.sample

orange.pos <- subsets$Orange[["Index"]]
orange.rent <- subsets$Orange[["Rent"]]
orange.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, orange.pos, orange.rent)[,2]))
orange.sample

yellow.pos <- subsets$Yellow[["Index"]]
yellow.rent <- subsets$Yellow[["Rent"]]
yellow.sample <- replicate(k2, sum(simulate_monopoly2(n2, d2, yellow.pos, yellow.rent)[,2]))
yellow.sample

samples <- cbind(purple.sample, lblue.sample, pink.sample, orange.sample, red.sample, yellow.sample, green.sample, blue.sample)
sample.data <- as.data.frame(samples)
View(sample.data) # total earn/lost on each color

# plot the sample.data
boxplot(samples, col = c("purple", "light blue", "pink", "orange", "red", "yellow", "green", "blue"), main = "Boxplot: Differences of Total Money Changes Among Different Color Opponents", xlab = "Opponent Colors", ylab = "Total Money Earned/Lost")


## 2.3 Simulate a basic version of a two player game of Monopoly.
## In this game, each player will be equipped with hotels on a specific color of properties. 
## There will be no bankruptcy conditions (if a player goes below $0 the game does not halt), 
## instead, the player with the most money at the end of n dice rolls will be deemed the winner. 
## Whenever a player lands on the opposing player’s hotel, they will now transfer the money to the other player at that turn.
## Let w_i be the change in money at turn i, then the final calculation of a player’s wealth is:
## Total Money = Initial Money − Hotel Costs + sum of w_i
## For each simulation, set the initial money to be $5000. Use the hotel cost for a set of properties from properties.csv.

## For all 28 pairwise combinations of colors, run k = 100 head-to-head matchups. 
## Do this for n = {25, 50, 100} dice rolls. 
## Display the ordered standings with wins, losses, and win percentage for each value of n. 
## No graphics are required here. The pairs are in color_combos.csv.


##### suggestion from Patrick:
##### in order to simplify this problem, we just transfer all the money at turn i to opponent, don't worry about the Go money, tax money, etc

# try on single simulation. red vs green. n=25. k=1.
# n <- 25
# d <- 6
# player 1: red
# player 2: green

# players cost for buying hotels 
# player1.buy <- sum(subsets$Red[["Cost"]]) # total money player1 red needs to buy all its hotels
# player1.buy
# player2.buy <- sum(subsets$Green[["Cost"]]) # total money player2 green needs to buy all its hotels
# player2.buy

# pay: returns the indexes along the runs, the earn/loss, indicator(0 means not in opponent's hotel, 1 means in)
# player1.pay <- simulate_monopoly2(n, d, green.pos, green.rent)
# player1.pay
# player2.pay <- simulate_monopoly2(n, d, red.pos, red.rent)
# player2.pay

# transfer money based on each player's LP
# indicator1 <- player1.pay[,3]
# indicator2 <- player2.pay[,3]
# when indicator2 == 1, meaning that player2 lands on player1's property, so player1 gets money from player2
# player1.pay[indicator2 == 1, 2] <- player1.pay[indicator2 == 1, 2] - player2.pay[indicator2 == 1, 2]
# player1.pay # check the updated player1.pay
# when indicator1 == 1, meaning that player1 lands on player2's property, so player2 gets money from player1
# player2.pay[indicator1 == 1, 2] <- player2.pay[indicator1 == 1, 2] - player1.pay[indicator1 == 1, 2]
# player2.pay # check the updated player1.pay 

# final total for each player
# initial <- 5000 # each player has $5000 at the beginning for each game
# total1 <- initial - player1.buy + sum(player1.pay[,2])
# total2 <- initial - player2.buy + sum(player2.pay[,2])

# see who wins in this game
# winner <- NA
# ifelse(total1 > total2, winner <- "Red", winner <- "Green")


## winner: a function return the color of the winner
winner <- function(n, d, player1.color, player2.color){
        # players cost for buying hotels 
        player1.buy <- sum(subsets$player1.color[["Cost"]]) # total money player1 needs to buy all its hotels
        player1.buy
        player2.buy <- sum(subsets$player2.color[["Cost"]]) # total money player2 needs to buy all its hotels
        player2.buy
        
        # pos and rent for each color
        pos1 <- subsets$player1.color[["Index"]]
        rent1 <- subsets$player1.color[["Rent"]]
        pos2 <- subsets$player2.color[["Index"]]
        rent2 <- subsets$player2.color[["Rent"]]
        
        # pay: returns the indexes along the runs, the earn/loss, indicator(0 means not in opponent's hotel, 1 means in)
        player1.pay <- simulate_monopoly2(n, d, pos1, rent1)
        player1.pay
        player2.pay <- simulate_monopoly2(n, d, pos2, rent2)
        player2.pay
        
        # transfer money based on each player's LP
        indicator1 <- player1.pay[,3]
        indicator2 <- player2.pay[,3]
        # when indicator2 == 1, meaning that player2 lands on player1's property, so player1 gets money from player2
        player1.pay[indicator2 == 1, 2] <- player1.pay[indicator2 == 1, 2] - player2.pay[indicator2 == 1, 2]
        player1.pay # check the updated player1.pay
        # when indicator1 == 1, meaning that player1 lands on player2's property, so player2 gets money from player1
        player2.pay[indicator1 == 1, 2] <- player2.pay[indicator1 == 1, 2] - player1.pay[indicator1 == 1, 2]
        player2.pay # check the updated player2.pay 
        
        # final total for each player
        initial <- 5000 # each player has $5000 at the beginning for each game
        total1 <- initial - player1.buy + sum(player1.pay[,2])
        total2 <- initial - player2.buy + sum(player2.pay[,2])
        
        # see who wins in this game
        winner <- NA
        ifelse(total1 > total2, winner <- player1.color, winner <- player2.color)
        
        # return winner
        winner
}

################ each pair battles k=100 times
##### first try one pair with n=25
# p1 <- color[1, 1] # obtain two player colors
# p2 <- color[1, 2]
# find.winner25 <- replicate(100, winner(25, 6, p1, p2)) # find winner
# is.factor(find.winner25) # TRUE
# winner25 <- as.character(find.winner25)
# p1.winning <- which(winner25 == p1)
# p1.winning.num <- length(p1.winning)      # how many wins on p1
# p1.winning.prop <- p1.winning.num / 100
# p2.winning.prop <- 1 - p1.winning.prop
# winning.prop <- data.frame( p1, p1.winning.prop, p2, p2.winning.prop)
# winning.prop

##### for n=25
winnings25 <- c()
for(i in 1:28){
        p1 <- color[i, 1]
        p2 <- color[i, 2]
        find.winner25 <- replicate(100, winner(25, 6, p1, p2)) # find winner
        winner25 <- as.character(find.winner25)
        p1.winning <- which(winner25 == p1)
        p1.winning.num <- length(p1.winning)      # how many wins on p1
        p2.winning.num <- 100 - p1.winning.num    # how many wins on p2
        p1.winning.prop <- p1.winning.num / 100
        p2.winning.prop <- 1 - p1.winning.prop
        winning.prop <- data.frame(p1, p1.winning.num, p1.winning.prop, p2, p2.winning.num, p2.winning.prop)
        winnings25 <- rbind(winnings25, winning.prop)
}
winnings25

##### for n=50
winnings50 <- c()
for(i in 1:28){
        p1 <- color[i, 1]
        p2 <- color[i, 2]
        find.winner50 <- replicate(100, winner(50, 6, p1, p2)) # find winner
        winner50 <- as.character(find.winner50)
        p1.winning <- which(winner50 == p1)
        p1.winning.num <- length(p1.winning)      # how many wins on p1
        p2.winning.num <- 100 - p1.winning.num    # how many wins on p2
        p1.winning.prop <- p1.winning.num / 100
        p2.winning.prop <- 1 - p1.winning.prop
        winning.prop <- data.frame(p1, p1.winning.num, p1.winning.prop, p2, p2.winning.num, p2.winning.prop)
        winnings50 <- rbind(winnings50, winning.prop)
}
winnings50

##### for n=100
winnings100 <- c()
for(i in 1:28){
        p1 <- color[i, 1]
        p2 <- color[i, 2]
        find.winner100 <- replicate(100, winner(100, 6, p1, p2)) # find winner
        winner100 <- as.character(find.winner100)
        p1.winning <- which(winner100 == p1)
        p1.winning.num <- length(p1.winning)      # how many wins on p1
        p2.winning.num <- 100 - p1.winning.num    # how many wins on p2
        p1.winning.prop <- p1.winning.num / 100
        p2.winning.prop <- 1 - p1.winning.prop
        winning.prop <- data.frame(p1, p1.winning.num, p1.winning.prop, p2, p2.winning.num, p2.winning.prop)
        winnings100 <- rbind(winnings100, winning.prop)
}
winnings100

# view all the tables in data frame
View(winnings25)
View(winnings50)
View(winnings100)
