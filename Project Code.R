###################################################################
##########################  725 Project ###########################
###################################################################

###################################################################
################### Set memory limit ##############################
###################################################################
memory.limit(size=15000) # try making this slightly bigger if you run into issues
###################################################################
###################################################################
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
suppressMessages(library(matrixStats))
suppressMessages(library(caret))
suppressMessages(library(splitstackshape))
suppressMessages(library(xgboost))
suppressMessages(library(randomForest))
suppressMessages(library(parsnip))
suppressMessages(library(psych))
suppressMessages(library(kableExtra))
setwd("/Users/fuglc/725 Work/Project")


############ Outline ############

##### Step 1 #####

# a. First we need 1 year before and after the merger we are considering from Db1b
# data we downloaded at the beginning of the semester. deal closed near the 
# end of q4 2014, so take  data from 2013 as pre period and 2014 as post.

#read in all data subsetted for American and US Airways
data_2012_q1 <- read_csv("data/2012_q1.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2012_q2 <- read_csv("data/2012_q2.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2012_q3 <- read_csv("data/2012_q3.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
data_2012_q4 <- read_csv("data/2012_q4.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="US")
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
data_2019_q4 <- read_csv("data/2019_q4.csv") %>% 
  subset(TICKET_CARRIER=="AA" | TICKET_CARRIER=="UA")

data_pre <- rbind.fill(data_2012_q1,
                       data_2012_q2,
                       data_2012_q3,
                       data_2012_q4,
                       data_2013_q1,
                       data_2013_q2,
                       data_2013_q3,
                       data_2013_q4)
data_post <- rbind.fill(data_2014_q1,
                        data_2014_q2,
                        data_2014_q3,
                        data_2014_q4,
                        data_2019_q4)

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
  dplyr::group_by(Market_Ind, YEAR, QUARTER) %>%
  dplyr::summarize(ave_price=mean(MARKET_FARE, na.rm=T),
                   ave_distance=mean(MARKET_DISTANCE, na.rm=T),
                   ave_passengers=mean(Tot_Passengers, na.rm=T))
Market_data_post <- data_post5 %>%
  dplyr::group_by(Market_Ind, YEAR, QUARTER) %>%
  dplyr::summarize(ave_price=mean(MARKET_FARE, na.rm=T),
                   ave_distance=mean(MARKET_DISTANCE, na.rm=T),
                   ave_passengers=mean(Tot_Passengers, na.rm=T))

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
  dplyr::select(c(-11,-14))

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
  dplyr::select(c(-11,-14))

data_a_4_pre <- data_a_3_pre %>% 
  dplyr::rename(o_city=Description.x,
                d_city=Description.y)
data_a_4_post <- data_a_3_post %>% 
  dplyr::rename(o_city=Description.x,
                d_city=Description.y)

data_a_5_pre <- data_a_4_pre %>% 
  separate(o_city, c("origin_city",NA), sep = ":") %>% 
  separate(d_city, c("dest_city",NA), sep=":") %>% 
  dplyr::select(c(-11,-13))
data_a_5_post <- data_a_4_post %>% 
  separate(o_city, c("origin_city",NA), sep = ":") %>% 
  separate(d_city, c("dest_city",NA), sep=":") %>% 
  dplyr::select(c(-11,-13))

# then the vacation spot data
load("data/vacations.R")

vacations_Omerge_pre <- vacations %>%
  dplyr::rename(O_vac_flag=vacation_spot,
                origin_city=origin_cities)
data_b_1_pre <- left_join(data_a_5_pre, vacations_Omerge_pre, by="origin_city")
vacations_Dmerge_pre <- vacations %>% 
  dplyr::rename(D_vac_flag=vacation_spot,
                dest_city=origin_cities)
data_b_2_pre <- left_join(data_b_1_pre, vacations_Dmerge_pre, by="dest_city")
data_b_3_pre <- data_b_2_pre %>% 
  mutate(vac_flag=pmax(D_vac_flag, O_vac_flag)) %>% 
  dplyr::select(c(-13,-14))

vacations_Omerge_post <- vacations %>%
  dplyr::rename(O_vac_flag=vacation_spot,
                origin_city=origin_cities)
data_b_1_post <- left_join(data_a_5_post, vacations_Omerge_post, by="origin_city")
vacations_Dmerge_post <- vacations %>% 
  dplyr::rename(D_vac_flag=vacation_spot,
                dest_city=origin_cities)
data_b_2_post <- left_join(data_b_1_post, vacations_Dmerge_post, by="dest_city")
data_b_3_post <- data_b_2_post %>% 
  mutate(vac_flag=pmax(D_vac_flag, O_vac_flag)) %>% 
  dplyr::select(c(-13,-14))

# then the income data
load("data/data_income.R")

income_Omerge_pre <- msa_income %>% 
  dplyr::rename(O_income=median_income,
                origin_city=city)
data_c_1_pre <- left_join(data_b_3_pre, income_Omerge_pre, by="origin_city")
income_Dmerge_pre <- msa_income %>% 
  dplyr::rename(D_income=median_income,
                dest_city=city)
data_c_2_pre <- left_join(data_c_1_pre, income_Dmerge_pre, by="dest_city")
data_c_3_pre <- data_c_2_pre %>% 
  mutate(mean_income=sqrt(O_income*D_income)) %>% 
  dplyr::select(c(-14,-15))

income_Omerge_post <- msa_income %>% 
  dplyr::rename(O_income=median_income,
                origin_city=city)
data_c_1_post <- left_join(data_b_3_post, income_Omerge_post, by="origin_city")
income_Dmerge_post <- msa_income %>% 
  dplyr::rename(D_income=median_income,
                dest_city=city)
data_c_2_post <- left_join(data_c_1_post, income_Dmerge_post, by="dest_city")
data_c_3_post <- data_c_2_post %>% 
  mutate(mean_income=sqrt(O_income*D_income)) %>% 
  dplyr::select(c(-14,-15))

# then the slot controlled data
load("data/slot_controlled.R")

slot_Omerge_pre <- slot_controlled %>% 
  dplyr::rename(O_slot_flag=slot_controlled,
                origin_airport_id=airport)
data_d_1_pre <- left_join(data_c_3_pre, slot_Omerge_pre, by="origin_airport_id")
slot_Dmerge_pre <- slot_controlled %>% 
  dplyr::rename(D_slot_flag=slot_controlled,
                dest_airport_id=airport)
data_d_2_pre <- left_join(data_d_1_pre, slot_Dmerge_pre, by="dest_airport_id")
data_d_3_pre <- data_d_2_pre %>% 
  mutate(slot_controlled_flag=pmax(D_slot_flag, O_slot_flag)) %>% 
  dplyr::select(c(-15,-16))

slot_Omerge_post <- slot_controlled %>% 
  dplyr::rename(O_slot_flag=slot_controlled,
                origin_airport_id=airport)
data_d_1_post <- left_join(data_c_3_post, slot_Omerge_post, by="origin_airport_id")
slot_Dmerge_post <- slot_controlled %>% 
  dplyr::rename(D_slot_flag=slot_controlled,
                dest_airport_id=airport)
data_d_2_post <- left_join(data_d_1_post, slot_Dmerge_post, by="dest_airport_id")
data_d_3_post <- data_d_2_post %>% 
  mutate(slot_controlled_flag=pmax(D_slot_flag, O_slot_flag)) %>% 
  dplyr::select(c(-15,-16))

# sort the data and add indicator for pre/post period
airport_data_pre <- data_d_3_pre %>% 
  arrange(origin_airport_id, dest_airport_id) %>% 
  mutate(period=0)
airport_data_post <- data_d_3_post %>% 
  arrange(origin_airport_id, dest_airport_id) %>% 
  mutate(period=1)

# stack the data (and remove NAs)!
airport_data_full <- rbind.fill(airport_data_pre,
                                airport_data_post) %>% 
  na.omit()



# take the log of prices to make the distribution more 'normal' looking
airport_data_full <- airport_data_full %>% 
  mutate(log_ave_price = log(ave_price))



# split out the full data and the 2019 data
airport_data_counter <- airport_data_full %>% 
  subset(YEAR == 2019)

airport_data_full <- airport_data_full %>% 
  subset(YEAR != 2019)




hist(airport_data_full$ave_price,
     main="Distribution of Average Price",
     xlab="Average Price")
hist(airport_data_full$log_ave_price,
     main="Distribution of Log Average Price",
     xlab="Log Average Price")
descriptive_stats <- data.frame(psych::describe(airport_data_full, fast=TRUE))
descriptive_stats <- descriptive_stats[-c(1, 7, 8, 10, 11, 16), ]
descriptive_stats <- descriptive_stats[ ,-c(1, 7, 8)]
output_table <- knitr::kable(descriptive_stats,
                             "html",
                             caption = "Summary Statistics",
                             format.args = list(scientific = FALSE)) %>%
  kable_styling("striped") %>%
  save_kable("summ_stats.html")
output_table
save_kable(output_table, file="summ_stats.pdf")




# Now we want to split the data in to test and split sets stratified on 
# models and years
set.seed(11022021)

train_data <- stratified(airport_data_full,
                         c('Market_Ind', 'YEAR'),
                         0.85)

train_data_merge <- train_data %>% 
  select('Market_Ind', 'YEAR', 'QUARTER')

test_data <- anti_join(airport_data_full,
                       train_data_merge,
                       by=c('Market_Ind', 'YEAR', 'QUARTER'))




lm1 <- lm(log_ave_price ~ #YEAR +
            QUARTER +
            mean_income +
            slot_controlled_flag +
            vac_flag +
            hub_flag +
            ave_distance +
            market_size +
            period,
          data=train_data)
lm1_summ <- summary(lm1)
stargazer::stargazer(lm1, type="text", header=T)




covars = c("mean_income",
           "slot_controlled_flag",
           "vac_flag",
           "hub_flag",
           "ave_distance",
           "market_size",
           "YEAR",
           "QUARTER",
           "period")

polyvars = data.frame(poly(as.matrix(train_data[, c("mean_income",
                                                    "slot_controlled_flag",
                                                    "vac_flag",
                                                    "hub_flag",
                                                    "ave_distance",
                                                    "market_size",
                                                    "YEAR",
                                                    "QUARTER",
                                                    "period")]),
                           degree = 2,
                           raw = T))

polyvars_test = data.frame(poly(as.matrix(test_data[, c("mean_income",
                                                        "slot_controlled_flag",
                                                        "vac_flag",
                                                        "hub_flag",
                                                        "ave_distance",
                                                        "market_size",
                                                        "YEAR",
                                                        "QUARTER",
                                                        "period")]),
                                degree = 2,
                                raw = T))

polylm <- lm(data.frame(train_data$log_ave_price, polyvars))

cvg_lasso_lambda <- cv.glmnet(as.matrix(polyvars), train_data$log_ave_price,
                              type.measure = "mse", nfolds = 10, alpha = 1)$lambda.min

cvg_lasso <- glmnet(as.matrix(polyvars), train_data$log_ave_price,
                    alpha = 1, lambda = cvg_lasso_lambda)

MSE_lm = mean((test_data$log_ave_price - predict(lm1, test_data))^2)
MSE_poly = mean((predict(polylm, polyvars_test) - test_data$log_ave_price)^2)
MSE_lasso<- mean((predict.glmnet(cvg_lasso, as.matrix(polyvars_test),
                                 s = cvg_lasso_lambda, type='response') - test_data$log_ave_price)^2)

# descale the MSEs to get how many percent off they are from correct
MSE_lm_ds = (10^(MSE_lm)-1)*100
MSE_poly_ds = (10^(MSE_poly)-1)*100
MSE_lasso_ds = (10^(MSE_lasso)-1)*100

# mse_table <-data.frame(cbind(MSE_lm_ds, MSE_poly_ds, MSE_lasso_ds))
# names(mse_table) <-c("linear probability model",
#                      "polynomial linear model",
#                      "lasso")
# knitr::kable(mse_table,
#              "simple")


# these are not overly accurate, being off by roughly 1.3 dollars.
# Try something more fancy like XGBoost, random forests, and Neural Networks

##### XGBoost #####

# train_data_xgb <- train_data[,-c(10,11)]
# test_data_xgb <- test_data[,-c(10,11)]

train_x = data.matrix(train_data[,-c(17, 4)])
train_y = data.matrix(train_data[, 17])

test_x = data.matrix(test_data[,-c(17, 4)])
test_y = data.matrix(test_data[, 17])


xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

xgbc = xgboost(data = xgb_train, max.depth = 8, nrounds = 400)
print(xgbc)

pred_y = predict(xgbc, xgb_test)
mse_xgb = mean((test_y - pred_y)^2)

# this puts the mse in terms of %. So on average, our xgb model is 1.1% off 
# the actual price of the ticket.
mse_xgb_ds = (10^(mse_xgb)-1)*100


x = 1:length(test_y)
plot(x, test_y, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.9), type = "l")
lines(x, pred_y, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.9), type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"),
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))





###### Random Forest #####

rf = randomForest(log_ave_price ~ QUARTER +
                    mean_income +
                    slot_controlled_flag +
                    vac_flag +
                    hub_flag +
                    ave_distance +
                    market_size +
                    period,
                  data=train_data,
                  ntree=2000,
                  mtry=7,
                  importance=TRUE)
MSE_rf = mean((test_data$log_ave_price - predict(rf, test_data))^2)

# this puts the mse in terms of %. So on average, our rf model is 1.3% off 
# the actual price of the ticket.
MSE_rf_ds = (10^(MSE_rf)-1)*100




mse_table <-data.frame(cbind(MSE_lm_ds, MSE_poly_ds, MSE_lasso_ds, mse_xgb_ds, MSE_rf_ds))
names(mse_table) <-c("linear probability model",
                     "polynomial linear model",
                     "lasso",
                     "XGBoost",
                     "Random Forest")
knitr::kable(mse_table,
             "html",
             caption="Mean Squared Error of Different Models") %>% 
  kable_styling("striped",
                full_width = F) %>% 
  save_kable("MSE_table.html")



##### Counter Factual Merger AA and United #####
# 1. Read in and clean the Q4 2019 data to use as the test data from our
#    model from above. We will use the Random Forest and xgboost models.

# try the xgboost model
test_x_counter = data.matrix(airport_data_counter[,-17])
test_y_counter = data.matrix(airport_data_counter[, 17])
xgb_test_counter = xgb.DMatrix(data = test_x_counter, label = test_y_counter)
pred_y_counter = predict(xgbc, xgb_test_counter)
mse_xgb_counter = mean((test_y_counter - pred_y_counter)^2)
mse_xgb_counter_ds = (10^(mse_xgb_counter)-1)*100


# try the rf model
MSE_rf_counter = mean((airport_data_counter$log_ave_price - predict(rf, airport_data_counter))^2)
MSE_rf_counter_ds = (10^(MSE_rf_counter)-1)*100
