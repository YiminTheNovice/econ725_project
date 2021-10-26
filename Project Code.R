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

#read in all data
data_2013_q1 <- read_csv("data/2013_q1.csv")
data_2013_q2 <- read_csv("data/2013_q2.csv")
data_2013_q3 <- read_csv("data/2013_q3.csv")
data_2013_q4 <- read_csv("data/2013_q4.csv")
data_2014_q1 <- read_csv("data/2014_q1.csv")
data_2014_q2 <- read_csv("data/2014_q2.csv")
data_2014_q3 <- read_csv("data/2014_q3.csv")
data_2014_q4 <- read_csv("data/2014_q4.csv")

data_pre <- rbind.fill(data_2013_q1,
                       data_2013_q2,
                       data_2013_q3,
                       data_2013_q4)
data_post <- rbind.fill(data_2014_q1,
                        data_2014_q2,
                        data_2014_q3,
                        data_2014_q4)











#set all data - omt 2000, as it was not a full data set
data_full <- rbind.fill(data_2001_q1_q4, data_2002_q1_q4, data_2003_q1_q4, data_2004_q1_q4, data_2005_q1_q4, data_2006_q1_q4, data_2007_q1_q4, data_2008_q1_q4, data_2009_q1_q4, data_2010_q1_q4, data_2011_q1_q4, data_2012_q1_q4, data_2013_q1_q4, data_2014_q1_q4, data_2015_q1_q4, data_2016_q1_q4, data_2017_q1_q4, data_2018_q1_q4, data_2019_q1_q4, data_2020_q1_q4)

# b. Clean this up so we get the variables we have used in our problem sets

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

