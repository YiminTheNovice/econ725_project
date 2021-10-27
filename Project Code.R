###################################################################
##########################  725 Project ###########################
###################################################################

###### Hey Everybody, I need to re-download the data again because
# we need the city names to merge some of the data on. I will do that tonight


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
suppressMessages(library(matrixStats))
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

# get data on a market level by grouping just by market indicator
Market_data_pre <- data_pre5 %>%
  dplyr::group_by(Market_Ind) %>%
  dplyr::summarize(ave_price=mean(MARKET_FARE, na.rm=T),
            ave_distance=mean(MARKET_DISTANCE, na.rm=T))
Market_data_post <- data_post5 %>%
  dplyr::group_by(Market_Ind) %>%
  dplyr::summarize(ave_price=mean(MARKET_FARE, na.rm=T),
                   ave_distance=mean(MARKET_DISTANCE, na.rm=T))

############## We shouldn't need HHI or number of firms, seeing as we are only
# looking at 2 firms that ultimately become 1 firm.

# In order to get total number of firms, I notice that we can use the market
# airline level data. Since this is on a per market per firm basis, we can add
# a 1 to each of these observations and then sum them up by market to get the
# total number of firms per market. Then I left join that information to the 
# data set above.
# Market_airline_pre2 <- Market_airline_pre %>% 
#   mutate(firm_no=1) %>% 
#   dplyr::group_by(Market_Ind) %>% 
#   dplyr::summarize(tot_firms=sum(firm_no))
# Market_data_pre2 <- left_join(Market_data_pre, Market_airline_pre2, by="Market_Ind")
# 
# Market_airline_post2 <- Market_airline_post %>% 
#   mutate(firm_no=1) %>% 
#   dplyr::group_by(Market_Ind) %>% 
#   dplyr::summarize(tot_firms=sum(firm_no))
# Market_data_post2 <- left_join(Market_data_post, Market_airline_post2, by="Market_Ind")


# Calculating HHI is even more complicated. For this, I first sum up the total
# passengers from the market airline data by market. 
# HHI_merge_pre<-Market_airline_pre %>% 
#   dplyr::group_by(Market_Ind) %>% 
#   dplyr::summarize(Market_Pass_tot=sum(Tot_Pass_by_market_airline))
# HHI_merge_post<-Market_airline_post %>% 
#   dplyr::group_by(Market_Ind) %>% 
#   dplyr::summarize(Market_Pass_tot=sum(Tot_Pass_by_market_airline))

# Then I merge that back onto the market airline level data set
# HHI_merge_pre2<-left_join(Market_airline_pre,HHI_merge_pre,by="Market_Ind")
# HHI_merge_post2<-left_join(Market_airline_post,HHI_merge_post,by="Market_Ind")

# Then I divide the Total passengers by market airline by the total number of
# passengers by market and square that to get market share squared
# HHI_merge_pre3<-HHI_merge_pre2 %>% 
#   mutate(Market_share=(Tot_Pass_by_market_airline/Market_Pass_tot)*100,
#          Market_share_2=Market_share^2)
# HHI_merge_post3<-HHI_merge_post2 %>% 
#   mutate(Market_share=(Tot_Pass_by_market_airline/Market_Pass_tot)*100,
#          Market_share_2=Market_share^2)

# I then sum up the market share squared variable by market to get the HHI
# HHI_merge_pre4<-HHI_merge_pre3 %>% 
#   dplyr::group_by(Market_Ind) %>% 
#   dplyr::summarize(HHI=sum(Market_share_2))
# HHI_merge_post4<-HHI_merge_post3 %>% 
#   dplyr::group_by(Market_Ind) %>% 
#   dplyr::summarize(HHI=sum(Market_share_2))

# Lastly, I merge this onto our market level data which already contains
# average price, average distance, and number of firms
# Market_data_pre3<-left_join(Market_data_pre2, HHI_merge_pre4, by="Market_Ind")
# Market_data_post3<-left_join(Market_data_post2, HHI_merge_post4, by="Market_Ind")



# Next we will bring in the populations data
load("data/populations.R")
populations_merge <- populations %>% 
  mutate(Market_Ind = paste(origin_airport_id,dest_airport_id,sep=""))

Market_data_pre1 <- left_join(Market_data_pre, populations_merge, by="Market_Ind") #%>% 
  #select(-c(origin_airport_id,dest_airport_id))
Market_data_post1 <- left_join(Market_data_post, populations_merge, by="Market_Ind")#%>% 
  #select(-c(origin_airport_id,dest_airport_id))

# then the hub data
load("data/lookup_and_hub_r.R")

lookup_and_hub$hub_flag = rowMaxs(as.matrix(lookup_and_hub[,c(-1,-2,-3)]))
lookup_and_hub2 <- lookup_and_hub %>% 
  dplyr::select(c(1,2,3,135))

lookup_and_hub_Omerge_pre <- lookup_and_hub2 %>% 
  dplyr::rename(O_hub_flag=hub_flag,
                origin_airport_id=Code)
data_a_1_pre <- left_join(Market_data_pre1, lookup_and_hub_Omerge_pre, by="origin_airport_id")
lookup_and_hub_Dmerge_pre <- lookup_and_hub2 %>% 
  dplyr::rename(D_hub_flag=hub_flag,
                dest_airport_id=Code)
data_a_2_pre <- left_join(data_a_1_pre, lookup_and_hub_Dmerge_pre, by="dest_airport_id")
data_a_3_pre <- data_a_2_pre %>% 
  mutate(hub_flag=pmax(D_hub_flag, O_hub_flag)) %>% 
  dplyr::select(c(-7,-8,-9,-10,-11,-12))

lookup_and_hub_Omerge_post <- lookup_and_hub2 %>% 
  dplyr::rename(O_hub_flag=hub_flag,
                origin_airport_id=Code)
data_a_1_post <- left_join(Market_data_post1, lookup_and_hub_Omerge_post, by="origin_airport_id")
lookup_and_hub_Dmerge_post <- lookup_and_hub2 %>% 
  dplyr::rename(D_hub_flag=hub_flag,
                dest_airport_id=Code)
data_a_2_post <- left_join(data_a_1_post, lookup_and_hub_Dmerge_post, by="dest_airport_id")
data_a_3_post <- data_a_2_post %>% 
  mutate(hub_flag=pmax(D_hub_flag, O_hub_flag)) %>% 
  dplyr::select(c(-7,-8,-9,-10,-11,-12))

# then the vacation spot data
# load("data/vacations.R")
# 
# vacations_Omerge_pre <- vacations %>% 
#   dplyr::rename(O_vac_flag=vacation_spot,
#                 origin_city=origin_cities)
# data_b_1_pre <- left_join(data_a_3_pre, vacations_Omerge_pre, by="origin_city")

# then the income data
load("data/data_income.R")
load("data/airline_data_market_level.R")

# then the slot controlled data
# after all of that, we should have our data that we want





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

