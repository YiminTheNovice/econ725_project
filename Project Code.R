###################################################################
##########################  725 Project ###########################
###################################################################


rm(list = ls())
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(ggplot2))
suppressMessages(library(stargazer))
suppressMessages(library(glmnet))
suppressMessages(library(data.table))
setwd("/Users/fuglc/725 Work/Project")


############ Outline ############

##### Step 1 #####

# a. First we need 1 year before and after the merger we are considering from Db1b
# data we downloaded at the beginning of the semester. deal closed near the 
# end of q4 2014, so take  data from 2013 as pre period and 2014 as post.

#read in all data subsetted for American and US Airways
data_2013_q1 <- read_csv("data/2013_q1.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2013_q2 <- read_csv("data/2013_q2.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2013_q3 <- read_csv("data/2013_q3.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2013_q4 <- read_csv("data/2013_q4.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2014_q1 <- read_csv("data/2014_q1.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2014_q2 <- read_csv("data/2014_q2.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2014_q3 <- read_csv("data/2014_q3.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2014_q4 <- read_csv("data/2014_q4.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")

data_pre <- rbind.fill(data_2013_q1,
                       data_2013_q2,
                       data_2013_q3,
                       data_2013_q4)
data_post <- rbind.fill(data_2014_q1,
                        data_2014_q2,
                        data_2014_q3,
                        data_2014_q4)

# b. Clean this up so we get the variables we have used in our problem sets

# Start by removing any observations with a ticket carrier change
data_pre1 <- subset(data_pre, data_pre$TK_CARRIER_CHANGE==0)
data_post1 <- subset(data_post, data_post$TK_CARRIER_CHANGE==0)

# remove outlier prices 
data_pre2 <- subset(data_pre1, data_pre1$MARKET_FARE >= 25.0 & data_pre1$MARKET_FARE<=2500.0)
data_post2 <- subset(data_post1, data_post1$MARKET_FARE >= 25.0 & data_post1$MARKET_FARE<=2500.0)

# concatenate market IDs to get a market indicator
data_pre3 <- data_pre2 %>% 
  mutate(Market_Ind = paste(data_pre2$ORIGIN_AIRPORT_ID,data_pre2$DEST_AIRPORT_ID,sep=""))
data_post3 <- data_post2 %>% 
  mutate(Market_Ind = paste(data_post2$ORIGIN_AIRPORT_ID,data_post2$DEST_AIRPORT_ID,sep=""))

# Join the list of markets with averages over 20 passengers per day back to 
# the ticket level data using an inner join to keep only the ticket level 
# observations where the market averages over 20 passengers a day
data_pre4 <- data_pre3 %>% 
  dplyr::group_by(Market_Ind) %>% 
  dplyr::summarize(Tot_Passengers=sum(PASSENGERS)) %>% 
  mutate(daily_ave=Tot_Passengers*(4/365)) %>% 
  dplyr::filter(daily_ave > 20)
data_pre5 <- inner_join(data_pre3,data_pre4,by="Market_Ind")
data_post4 <- data_post3 %>% 
  dplyr::group_by(Market_Ind) %>% 
  dplyr::summarize(Tot_Passengers=sum(PASSENGERS)) %>% 
  mutate(daily_ave=Tot_Passengers*(4/365)) %>% 
  dplyr::filter(daily_ave > 20)
data_post5 <- inner_join(data_post3,data_post4,by="Market_Ind")

# Get data onto a market airline level by grouping by market indicator and carrier
Market_airline_pre <- data_pre5 %>% 
  dplyr::group_by(Market_Ind, TICKET_CARRIER) %>% 
  dplyr::summarize(ave_price=mean(MARKET_FARE, na.rm=T),
            Tot_Pass_by_market_airline=sum(PASSENGERS),
            ave_distance=mean(MARKET_DISTANCE, na.rm=T))
Market_airline_post <- data_post5 %>% 
  dplyr::group_by(Market_Ind, TICKET_CARRIER) %>% 
  dplyr::summarize(ave_price=mean(MARKET_FARE, na.rm=T),
                   Tot_Pass_by_market_airline=sum(PASSENGERS),
                   ave_distance=mean(MARKET_DISTANCE, na.rm=T))

# c. Split into a test set (pre period), validation set (pre period), and a test
# set (post period)

# d. Try an xgboost (or random forest) model to predict prices. Train the model on
# the train set and test its performance on the validation set before using the
# test data to generate our actual predictions

# e. Should also try:
# Linear Regression
# Logistic Regression
# Polynomial Regression
# Stepwise Regression
# Ridge Regression
# Lasso Regression
# ElasticNet Regression

# f. Evaluate our best model and use it to predict prices on the test data set

# g. take some time to convince the reader of why our model is strong and why
# we made any decisions we made



##### Step 2
# a. Now that we have our model, we can use it to predict prices a fictional merger
# between American Airlines and United Airlines

# b. To do this, we should first download the data from our 'pre merger' time, 
# which can be two years prior to today. Then we can use our model we found in 
# step 1 to predict the prices going out two years from today, assuming today 
# was when our merger happened

# c. provide a discussion for our findings, present summary tables and charts


############ Code ############

#hi connor + yo nick

