##############################
'4. Forecasting Models Recursively, 1-Month Ahead'
#Author: Will Winterbotham 
#Date: June 26th 2023
# 1/N ~ Forecasting with three lags, one step ahead (Y_(t+1) = Y_(t) + Y_(t1) + Y(t-2))
##############################

#Models in the following order: 
#   1. Bagged Trees 
#   3. Random Forest 
#   4. Boosted Model 
#   5. AR(1) model 
#   6. VAR(3) model
#   7. FAVAR model
#   8. LASSO 
#   9. Ridge 
#   10. Univariate Neural Network 

##############################
'Importing Packages'
##############################

rm(list = ls()) 
setwd("~/Documents/Dissertation")

library(randomForest)
library(glmnet) 
library(tidyverse)
library(caret)
library(tree)
library(xgboost)
library(ipred)
library(vars)
library(elasticnet)
library(class)
library(keras)
library(neuralnet)
library(BVAR)
library(doParallel)
library(forecast)
library(stats)

#load data and names
df <- readRDS("1.SA.final.dataset") 
names <- readRDS("1.name-list") 

#remove date and make numeric
df_no_date <- df[,-1]
df_no_date <- as.data.frame(sapply(df_no_date,as.numeric))

# Using a VAR(1) to find colinear variables
var_model <- VAR(df_no_date, p = 1)
find_colinear <- as.data.frame(var_model$varresult$CPI$coefficients) 
find_colinear$name <- rownames(find_colinear)
colinear_vars <- rownames(find_colinear[is.na(find_colinear),])
colinear_vars <- substr(colinear_vars, 1, nchar(colinear_vars) - 3) #extracts original column name

#remove colinear variables
df_clean <- df_no_date[,!colnames(df_no_date) %in% colinear_vars]
saveRDS(df_clean, "2.df_clean")

##############################
'Creating Dataframe with 3 lags'
##############################

#set y and bind lagged datsets
df_lag <- cbind(y = df_clean$CPI, lag(df_clean,1), lag(df_clean,2),lag(df_clean,3))
df_lag <- df_lag[-c(1:3),]

#set colnames
colnames(df_lag) <- c("y",
                      paste0(colnames(df_clean),"_L1"),
                      paste0(colnames(df_clean),"_L2"),  
                      paste0(colnames(df_clean),"_L3"))

##############################
'Set Training / Test Params'
##############################

test_length <- 12*10  #final 10 years are for testing
train_length <- nrow(df_lag) - test_length
t_length <- nrow(df_lag) #total periods

#set y variable
y_test <- df_lag$y[-(1:train_length)]

##############################
'AR(3)'
##############################

#univariate forecast for GDP 

ar_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    train_df <- df_lag[1:(ii-1),] #retrieve y
    test_df <- df_lag[(ii):nrow(df_lag),]
    
    #run model 
    ar_model <- lm(y ~ CPI_L1 + CPI_L2 + CPI_L3 , data = train_df)
    
    #predict one step ahead
    ar_pred <- predict(ar_model, newdata = test_df[1,])
    ar_forecast[(ii-train_length)] <- ar_pred
  }
)

RMSE(ar_forecast,y_test)

##############################
'VAR(3)'
##############################

#VAR with inflation, interest rates, and ouput

var_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    train_df <- df_lag[1:(ii-1),] #retrieve y
    test_df <- df_lag[(ii):nrow(df_lag),]
    
    #run model with 3 lags
    var_model <- lm(y ~ 
                      CPI_L1 + CPI_L2 + CPI_L3 + 
                      INTGSBGBM193N_L1 + INTGSBGBM193N_L2 + INTGSBGBM193N_L3 +
                      GBRPROINDMISMEI_L1 + GBRPROINDMISMEI_L2 + GBRPROINDMISMEI_L3 , 
                    data = train_df)
    
    #predict one step ahead
    var_pred <- predict(var_model, newdata = test_df[1,])
    var_forecast[(ii-train_length)] <- var_pred
  }
)

RMSE(var_forecast,y_test)

##############################
'Tree Forecast'
##############################

#tree forecast

tree_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    train_df <- df_lag[1:(ii-1),] 
    test_df <- df_lag[(ii):nrow(df_lag),]
    
    #run model 
    set.seed(123)
    tree_model <- tree(y ~., data = train_df)
    
    #predict one step ahead
    tree_pred <- predict(tree_model, newdata = test_df[1,])
    tree_forecast[(ii-train_length)] <- tree_pred
  }
)

RMSE(tree_forecast,y_test)


##############################
'Bagged trees'
##############################

# bagged trees

bag_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    train_df <- df_lag[1:(ii-1),] 
    test_df <- df_lag[(ii):nrow(df_lag),]
    
    #run model with 3 lags
    set.seed(123)
    bag_model <- bagging(y ~., 
                         data = train_df)
    
    #predict one step ahead
    bag_pred <- predict(bag_model, newdata = test_df[1,])
    bag_forecast[(ii-train_length)] <- bag_pred
  }
)

RMSE(bag_forecast,y_test)

##############################
'Random_Forest'
##############################

#rf takes ~1000 seconds (6 minutes)

rf_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    train_df <- df_lag[1:(ii-1),] 
    test_df <- df_lag[(ii):nrow(df_lag),]
    
    #run model 
    set.seed(123)
    rf_model <- randomForest(y ~., data = train_df)
    
    #predict one step ahead
    rf_pred <- predict(rf_model, newdata = test_df[1,])
    rf_forecast[(ii-train_length)] <- rf_pred
  }
)

RMSE(rf_forecast,y_test)

##############################
'LASSO'
##############################

#begin loop
lasso_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    y_train_lasso <- as.vector(df_lag[1:(ii-1),1]) #extracts y value
    x_train_lasso <- as.matrix(df_lag[1:(ii-1),-1]) #removes y value
    x_test_lasso <- as.matrix(df_lag[(ii):t_length,-1]) #remove y value 
    
    set.seed(123)
    lasso_cv <- cv.glmnet(x_train_lasso, y_train_lasso, nfolds = 5, type.measure = "mse", alpha = 1)
    best_lambda_lasso <- lasso_cv$lambda.min
    
    #run model 
    set.seed(123)
    lasso_model <- glmnet(x_train_lasso,y_train_lasso, alpha = 1, lambda = best_lambda_lasso,
                          parallel = "TRUE")
    
    #predict 3 step ahead
    lasso_pred <- predict(lasso_model, x_test_lasso[1,])
    lasso_forecast[(ii-train_length)] <- lasso_pred
  }
)

RMSE(lasso_forecast, y_test)

##############################
'Ridge'
##############################

#begin loop
ridge_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){
    y_train_ridge <- as.vector(df_lag[1:(ii-1),1]) #extracts y value
    x_train_ridge <- as.matrix(df_lag[1:(ii-1),-1]) #removes y value
    x_test_ridge <- as.matrix(df_lag[(ii):t_length,-1]) #remove y value 
    
    set.seed(123)
    ridge_cv <- cv.glmnet(x_train_ridge, y_train_ridge, nfolds = 5, alpha = 0, type.measure = "mse")
    best_lambda_ridge <- ridge_cv$lambda.min
    
    #run model 
    set.seed(123)
    ridge_model <- glmnet(x_train_ridge,y_train_ridge, alpha = 0, lambda = best_lambda_ridge,
                          parallel = "TRUE")
    
    #predict 3 step ahead
    ridge_pred <- predict(ridge_model, x_test_ridge[1,])
    ridge_forecast[(ii-train_length)] <- ridge_pred
  }
)

RMSE(ridge_forecast,y_test)

##############################
'Boosted Trees'
##############################

#xgboost algorithm - tree depth of 6

boost_forecast <- vector(length = test_length) #to populate with forecasts later

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
    boost_forecast[(ii-train_length)] <- boost_pred[1]
  }
)

RMSE(y_test,boost_forecast)

##############################
'FAVAR'
##############################

#take top 5 principal components and use VAR (using lags of PCs)

favar_forecast <- vector(length = test_length) #to populate with forecasts later

system.time(
  for(ii in (train_length+1):t_length){ 
    #setup data for PCA
    pca_df <- scale(df_lag[1:(ii-1),-1])
    
    #set number of pc's used for favar
    no_pc <- 5
    
    #run pca 
    set.seed(123)
    extract_pcs <- prcomp(pca_df)
    #retrieve pcs
    pcs <- data.frame(extract_pcs$x)
    #setup df for favar
    favar_model_df <- cbind(y = df_lag[1:(ii-1),1],pcs[,1:(no_pc)],
                            "CPI_L1" = df_lag[1:(ii-1),]$CPI_L1, 
                            "CPI_L2" = df_lag[1:(ii-1),]$CPI_L2,
                            "CPI_L3" = df_lag[1:(ii-1),]$CPI_L3)
    #run model 
    favar_model <- lm(y ~., data = favar_model_df)
    
    #predict PCs in next period for testing
    pca_test_x <- data.frame(predict(extract_pcs,newdata = df_lag[(ii),-1]))
    pca_test_x <- cbind(pca_test_x[,1:(no_pc)],
                        "CPI_L1" = df_lag$CPI_L1[ii],
                        "CPI_L2" = df_lag$CPI_L2[ii],
                        "CPI_L3" = df_lag$CPI_L3[ii]) #include all 3 lags
    
    #predict using new PCs
    favar_pred <- predict(favar_model, pca_test_x)
    #populate vector with forecast
    favar_forecast[(ii-train_length)] <- favar_pred
  }
)

RMSE(y_test,favar_forecast)

##############################
'Univariate FF Neural Network'
##############################

#univariate neural network (averaged over 20 networks) using 6-4-1 layout with 33 weights

nn_forecast <- vector(length = test_length)

#determine hidden units
system.time(
  for(ii in (train_length+1):t_length){
    train_df <- df_lag$y[1:(ii- 1)] #extract y
    
    #run model 
    set.seed(123)
    nnet_model <- nnetar(train_df, p = 3)
    fcast <- forecast(nnet_model)
    
    #predict one step ahead
    nn_forecast[(ii-train_length)] <- fcast$mean[1]  #extract 1-ahead forecast 
  }
)

plot(nn_forecast, type = "l")
RMSE(y_test,nn_forecast)

#for post-period forecast
plot(fcast)

##############################
'Collect Results'
##############################

models <- c("Tree", "Bagged Trees", "Random Forest", "XGBoost","AR","VAR", 
            "FAVAR","LASSO","Ridge", "Univariate NN")

#create data frame with results
results <- data.frame(rbind(y_test,
                            tree_forecast, bag_forecast, rf_forecast, boost_forecast, ar_forecast, var_forecast, favar_forecast, 
                            lasso_forecast, ridge_forecast , nn_forecast
))

#save forecasts
saveRDS(results, "one_month_ahead_forecasts")

#round forecasts to nearest 0.1 for summary stats (as CPI inflation is)
results <- round(results,1)

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

saveRDS(results_summary,"one_month_ahead_summary_results")




