##############################
'5. Forecasting Models Recursively, 12-month Ahead, Using Principal Components'
#Author: Will Winterbotham 
#Date: Sep 2023
# 1/N ~ Forecasting with 12 lags, 12 steps ahead (Y_(t+12) = Y_(t) + Y_(t-1) +...  Y(t-11) 
# All approaches use PCs as predictors
##############################

##############################
'Importing Packages'
##############################

rm(list = ls()) 
setwd("~/Documents/Dissertation")

library(xgboost)
library(stats)
library(tidyverse)
library(caret)
library(randomForest)
library(dfms)
library(data.table)
library(DiagrammeR)

df <- readRDS("2.df_clean")

##############################
'Creating Dataframe with 12 lags'
##############################

#set structure of forecast
lags <- 12 
month_ahead <- 12

#set y, will bind df later
df_lag <- data.frame(y = df$CPI)

#create df with 12 lags, forecasting 12-ahead
for(ii in month_ahead:(month_ahead + lags - 1)){ 
  df_lag <- cbind(df_lag, lag(df,ii))
  }

#drop first 23 months (NAs)
df_lag <- df_lag[-c(1:23),]

#set lagged colnames
lag_prefix <- paste0(colnames(df), "_L")
lag_numbers <- rep(12:23, each = ncol(df))
colnames(df_lag) <- c("y",paste0(rep(lag_prefix, 12), lag_numbers))
saveRDS(df_lag, "12-month_df")

#extract df with only inflation for later
y_lag_prefix <- paste0("CPI", "_L")
y_lag_numbers <- c(12:23)
y_lag_names <- paste0(rep(y_lag_prefix, 12), y_lag_numbers)

#subset df for use later
df_lag_inf <- subset(df_lag, select = y_lag_names)

##############################
'Set Training / Test Params'
##############################

test_length <- 12*10  #final 10 years are for testing
train_length <- nrow(df_lag) - test_length
t_length <- nrow(df_lag) #total periods

#set y variable
y_test <- df_lag$y[-(1:train_length)]

##############################
'XGBoost with PCs'
##############################

#taking 5 principal components of df_lag AND lagged y 

xgb_forecast <- vector(length = test_length)
no_pc <- 5

for(ii in (train_length+1):t_length){ 
  #extract PCs
  pca_extract <- prcomp(df_lag[1:(ii-1),-1], scale. = TRUE) #remove y for PCs
  pcs <- data.frame(pca_extract$x[,1:(no_pc)])
  
  #retrieve lags to add in
  lag_y <- df_lag_inf[1:(ii-1),]
  
  #merge lags and PCs
  pca_df <- cbind(lag_y, pcs)
  
  #set training
  x_train <- as.matrix(pca_df)
  y_train <- as.vector(df_lag$y[1:(ii-1)])

  #set test - predict PCs for next period
  pcs_test <- as.matrix(predict(pca_extract, df_lag[(ii:t_length),-1])[,1:no_pc]) #remove y also 
  
  #bind new PCs to old, if-loop neccessary for very last observation (cbind doesn't work)
  ifelse(ii == t_length, 
         x_test <- cbind(df_lag_inf[(ii:t_length),],t(pcs_test)) ,
         x_test <- cbind(df_lag_inf[(ii:t_length),],pcs_test))
  
  #make xgb matrix
  x_test <- xgb.DMatrix(as.matrix(x_test))
  
  #run model 
  set.seed(123)
  xgb_model <- xgboost(x_train,y_train, nrounds= 100)
  
  xgb_forecast[(ii-train_length)] <- predict(xgb_model, x_test)[1]
  
}

RMSE(y_test, xgb_forecast)

##############################
'XGBoost without PCs'
##############################

xgb_forecast_only_lags <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    y_train_boost <- as.vector(df_lag[1:(ii-1),1]) #extracts y value
    x_train_boost <- as.matrix(df_lag[1:(ii-1),-1]) #removes y value
    x_test_df <- as.matrix(df_lag[(ii):t_length,-1]) #remove y value 
    x_test_df <- xgb.DMatrix(x_test_df) #xgboost format
    
    #run model 
    set.seed(123)
    boost_model <- xgboost(data = x_train_boost, label = y_train_boost, nrounds = 100) 
    
    #predict one step ahead
    boost_pred <- predict(boost_model, x_test_df)
    xgb_forecast_only_lags[(ii-train_length)] <- boost_pred[1]
  }
)

RMSE(y_test,xgb_forecast_only_lags)

##############################
'XGBoost with PCs #2'
##############################

#xgboost with lagged PCs

#empty vector 
xgb_forecast_2 <- vector(length = test_length)
no_pc <- 5

for(ii in (train_length+1):t_length){

#extract pcs
pca_extract <- prcomp(df_lag[1:(ii-1),-1], scale. = TRUE) #remove y for PCs
pcs <- data.frame(pca_extract$x[,1:no_pc])

df_pcs_2 <- df_lag[1:(ii-1),1] #set df to populate
df_pcs_2 <- cbind(df_pcs_2, pcs)

#create df with 12 lags, forecasting 12-ahead
for(jj in 1:(lags-1)){ 
  df_pcs_2 <- cbind(df_pcs_2, lag(pcs,jj))
}

#clear NA rows from lags
df_pcs_2 <- df_pcs_2[-c(1:12),]

#set lagged colnames
lag_prefix_pc <- paste0(colnames(pcs), "_L")
lag_numbers_pc <- rep(12:23, each = ncol(pcs))
colnames(df_pcs_2) <- c("y",paste0(rep(lag_prefix_pc, 12), lag_numbers_pc))

#retrieve lags to add in
lag_y <- df_lag_inf[13:(ii-1),] #select from 13 since 12 rows have been removed already

#merge lags and PCs
df_pcs_2 <- cbind(df_pcs_2, lag_y)

#set x/y train
y_train_boost <- as.vector(df_pcs_2[1:(ii-lags-1),1]) #extracts y value
x_train_boost <- as.matrix(df_pcs_2[1:(ii-lags-1),-1]) #removes y value

#predict next PC for test x
pcs_test <- as.matrix(predict(pca_extract, df_lag[(ii),-1])[,1:no_pc])

#merge with other lagged PCs
x_test_df <- df_pcs_2[(nrow(df_pcs_2)),-1]
x_test_df[,(no_pc+1):(lags*no_pc)] <- x_test_df[,1:(lags*no_pc - no_pc)]
x_test_df[1:no_pc] <- t(pcs_test)

#add new lagged y 
x_test_df[(lags*no_pc+1):ncol(x_test_df)] <- df_lag_inf[ii,]

#add empty row so xgb.Dmatrix works (only extract 1-ahead prediction so row is redundant)
x_test_df[2,] <- 1

#make xgb matrix
x_test_df <- xgb.DMatrix(as.matrix(x_test_df))

#run xgboost
boost_model_2 <- xgboost(data = x_train_boost, label = y_train_boost,nrounds = 100) 
xgb_forecast_2[(ii-train_length)] <- predict(boost_model_2, x_test_df)[1]
}

RMSE(xgb_forecast_2,y_test)

##############################
'AR(p)'
##############################

#univariate forecast for GDP 

ar_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    train_df <- df_lag$y[1:(ii - month_ahead)] #retrieve y
    
    #run model 
    ar_model <- ar(train_df, order.max = 12)
    
    #predict 12 step ahead
    ar_pred <- predict(ar_model, n.ahead = 12)$pred[12]
    ar_forecast[(ii-train_length)] <- ar_pred
  }
)

RMSE(y_test, ar_forecast)

##############################
'XGBoost without PCs + Tuning (CV)'
##############################

#empty vector for forecasts
xgb_tune <- vector(length = test_length) #to populate with forecasts later

#set cores 
registerDoParallel(cores=3)

#tuningcross  validation params
myTimeControl <- trainControl(
  method = "cv",
  number = 5,
  allowParallel = TRUE)



system.time(
  for(ii in (train_length+1):t_length){
    
    #set test and train
    train_df <- df_lag[1:(ii-1),]
    test_df <- df_lag[(ii):t_length,-1]
    
    #run model 
    set.seed(123)
      gbm.mod <- caret::train(y ~. ,
                              data = train_df,
                              method = "xgbTree",
                              trControl = myTimeControl,
                              tuneLength=tuneLength.num,
                              verbose=FALSE,
                              metric='RMSE')

    #predict one step ahead
    xgb_tune[(ii-train_length)] <- predict(gbm.mod,test_df)[1]
  }
)

RMSE(y_test,xgb_tune)

##############################
'XGBoost without PCs + Tuning (Sliding Window)'
##############################

#set empty vector
xgb_tune_sliding <- vector(length = test_length)

#set cores 
registerDoParallel(cores=3)

#run model
system.time(
  for(ii in (train_length+1):t_length){
    
    #set sliding window - skip means only one window used
    myTimeControl_sliding <- trainControl(method = "timeslice",
                                          initialWindow = 360,
                                          horizon = 1,
                                          skip = ii,
                                          fixedWindow = TRUE,
                                          allowParallel = TRUE)
    
    
    #set test and train
    train_df <- df_lag[1:(ii-1),]
    test_df <- df_lag[(ii):t_length,-1]
    
    #run model 
    set.seed(123)
    gbm.mod <- caret::train(y ~. ,
                            data = train_df,
                            method = "xgbTree",
                            trControl = myTimeControl_sliding,
                            verbose=FALSE,
                            metric='RMSE')
    
    #predict one step ahead
    xgb_tune_sliding[(ii-train_length)] <- predict(gbm.mod,test_df)[1]
  }
)

RMSE(xgb_tune_sliding,y_test)


xgb.importance(gbm.mod)


##############################
'Store Forecasts'
##############################


models <- c("AR(p)", "XGBoost", "XGBoost with PCs", "XGBoost with lagged PCs", 
            "XGBoost Tune CV", "XGBoost Tune Sliding Window")

#create data frame with results
results <- data.frame(rbind(y_test, ar_forecast,
                            xgb_forecast_only_lags, xgb_forecast, xgb_forecast_2
))

rownames(results) <- c("y",models)

#round forecasts to nearest 0.1 for summary stats (as CPI inflation is)
results <- round(results,1)

#save forecasts
saveRDS(results, "12month_pca_forecasts")

#define R-Squared
rsquared <- function(y,yhat){
  1 - mean((y - yhat)^2)/var(y)
}

#define sum stats
Rsquared <- vector(length = nrow(results))
rmse <- vector(length = nrow(results))
mae <- vector(length = nrow(results))
min <- vector(length = nrow(results))
max <- vector(length = nrow(results))
sd <- vector(length = nrow(results))

#populate summary stats for each model
for (ii in 1:nrow(results)){
  Rsquared[ii] <- rsquared(y_test, as.numeric(results[ii,]))
  rmse[ii] <- RMSE(y_test, as.numeric(results[ii,]))
  mae[ii] <- MAE(y_test, as.numeric(results[ii,]))
  min[ii] <- min(as.numeric(results[ii,]))
  max[ii] <- max(as.numeric(results[ii,]))
  sd[ii] <- sqrt(var(as.numeric(results[ii,])))
}

#merge summary stats into new df
results_summary <- data.frame(cbind(rmse,mae,Rsquared,min,max,sd))
rownames(results_summary) <- rownames(results)
results_summary <- results_summary[-1,] #remove stats for actuals

saveRDS(results_summary,"12month_pca_results")

##############################
'Extension : Exploring Principal Components'
##############################

#evaluating error rate at differing levels of PCs

#set no of principal components to loop over
pc_numbers <- c(5:20)

#empty vectors to store results by pc
rmse_pcs <- vector(length = length(pc_numbers))
mae_pcs <- vector(length = length(pc_numbers))

#start loop over pc_numbers
for(kk in pc_numbers){
#empty vector 
xgb_forecast_2_pc_varies <- vector(length = test_length)
no_pc <- kk

for(ii in (train_length+1):t_length){
  
  #extract pcs
  pca_extract <- prcomp(df_lag[1:(ii-1),-1], scale. = TRUE) #remove y for PCs
  pcs <- data.frame(pca_extract$x[,1:no_pc])
  
  df_pcs_2 <- df_lag[1:(ii-1),1] #set df to populate
  df_pcs_2 <- cbind(df_pcs_2, pcs)
  
  #create df with 12 lags, forecasting 12-ahead
  for(jj in 1:(lags-1)){ 
    df_pcs_2 <- cbind(df_pcs_2, lag(pcs,jj))
  }
  
  #clear NA rows from lags
  df_pcs_2 <- df_pcs_2[-c(1:12),]
  
  #set lagged colnames
  lag_prefix_pc <- paste0(colnames(pcs), "_L")
  lag_numbers_pc <- rep(12:23, each = ncol(pcs))
  colnames(df_pcs_2) <- c("y",paste0(rep(lag_prefix_pc, 12), lag_numbers_pc))
  
  #retrieve lags to add in
  lag_y <- df_lag_inf[13:(ii-1),] #select from 13 since 12 rows have been removed already
  
  #merge lags and PCs
  df_pcs_2 <- cbind(df_pcs_2, lag_y)
  
  #set x/y train
  y_train_boost <- as.vector(df_pcs_2[1:(ii-lags-1),1]) #extracts y value
  x_train_boost <- as.matrix(df_pcs_2[1:(ii-lags-1),-1]) #removes y value
  
  #predict next PC for test x
  pcs_test <- as.matrix(predict(pca_extract, df_lag[(ii),-1])[,1:no_pc])
  
  #merge with other lagged PCs
  x_test_df <- df_pcs_2[(nrow(df_pcs_2)),-1]
  x_test_df[,(no_pc+1):(lags*no_pc)] <- x_test_df[,1:(lags*no_pc - no_pc)]
  x_test_df[1:no_pc] <- t(pcs_test)
  
  #add new lagged y 
  x_test_df[(lags*no_pc+1):ncol(x_test_df)] <- df_lag_inf[ii,]
  
  #add empty row so xgb.Dmatrix works (only extract 1-ahead prediction so row is redundant)
  x_test_df[2,] <- 1
  
  #make xgb matrix
  x_test_df <- xgb.DMatrix(as.matrix(x_test_df))
  
  #run xgboost
  boost_model_2 <- xgboost(data = x_train_boost, label = y_train_boost,nrounds = 100) 
  xgb_forecast_2_pc_varies[(ii-train_length)] <- predict(boost_model_2, x_test_df)[1]
}

#store the results in vector (minus 4 since we start at 5)
rmse_pcs[(kk-pc_numbers[1]-1)] <- RMSE(xgb_forecast_2_pc_varies,y_test)
mae_pcs[(kk-pc_numbers[1]-1)] <- MAE(xgb_forecast_2_pc_varies,y_test)
}

#bind rmse and mae
summary_pcs_table <- data.frame(pcs = 5:20,cbind(rmse_pcs,mae_pcs))

#number of pcs 
cbind(rmse_pcs,mae_pcs)

saveRDS(summary_pcs_table,"summary_pcs_table")



