##############################
'5. Plots + Tables '
#Author: Will Winterbotham 
#Date: Sep 2023
# Codebook with plots used, labelled by figures
##############################

rm(list = ls()) 
setwd("~/Documents/Dissertation")

#load packages
library(xtable)
library(tidyverse)
library(ggarchery)
library(caret)

#read datasets
names <- readRDS("1.name-list")
date <- readRDS("1.SA.final.dataset") 
date <- as.Date(date$date)
df <- readRDS("2.df_clean")

#read forecasts
month_1 <- readRDS("one_month_ahead_forecasts")
month_3 <- readRDS("three_month_ahead_forecasts")
month_12 <- readRDS("12_month_ahead_forecasts")
month_12_pca <- readRDS("12month_pca_forecasts")

#read summary results
month_1_summary <- readRDS("one_month_ahead_summary_results")
month_3_summary <- readRDS("three_month_ahead_summary_results")
month_12_summary <- readRDS("12_month_ahead_summary_results")
month_12_pca_summary <- readRDS("12month_pca_results")

#############
'Diebold-Mariano Test'
#############
y_test <- month_1[1,]
y_matrix <- y_test
#loop to create matrix of y-s
for(ii in 1:10){y_matrix <- rbind(y_test,y_matrix)}

#calculate error
error_1month <- month_1 - y_matrix 
error_1month <- error_1month[-1,] #remove y_test row

error_3month <- month_3 - y_matrix 
error_3month <- error_3month[-1,]

error_12month <- month_12 - y_matrix 
error_12month <- error_12month[-1,]

#############
'1-month DM test'
#############
dm_1_month <- vector(length = 9)
ar_error <- as.numeric(error_1month[5,])
error_1month <- error_1month[-5,] #remove ar forecast

for(ii in 1:9){
dm <- dm.test(as.numeric(error_1month[ii,]),ar_error, alternative = "less")
dm_1_month[ii] <- dm$p.value }
dm_1_month

#############
'3-month DM test'
#############
dm_3_month <- vector(length = 9)
ar_error <- as.numeric(error_3month[5,])
error_3month <- error_3month[-5,] #remove ar forecast

for(ii in 1:9){
  dm <- dm.test(as.numeric(error_3month[ii,]),ar_error, alternative = "less")
  dm_3_month[ii] <- dm$p.value }
dm_3_month

#############
'12-month DM test'
#############
dm_12_month <- vector(length = 9)
ar_error <- as.numeric(error_12month[5,])
error_12month <- error_12month[-5,] #remove ar forecast

for(ii in 1:9){
  dm <- dm.test(as.numeric(error_12month[ii,]),ar_error, alternative = "less")
  dm_12_month[ii] <- dm$p.value }
dm_12_month

#function to return significance 

pvalue_to_asterisk <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return(" ")
  }
}

#asterisks (update latex tables with these)
sapply(dm_1_month,pvalue_to_asterisk)
sapply(dm_3_month,pvalue_to_asterisk)
sapply(dm_12_month,pvalue_to_asterisk)

#############
'Table 1/2/3'
#############

#use xtable for latex code
print(xtable(month_1_summary))
print(xtable(month_3_summary))
print(xtable(month_12_summary))

#############
'Plot 1'
#############

#set date for vline
cutoff <- date[nrow(df) - 120]

plot_1 <- ggplot(data = df, aes(x = date, y = CPI)) + 
  geom_line() + 
  theme_classic() + 
  geom_vline(xintercept = cutoff, linetype = "dashed", col = "darkred") + 
  annotate("text", x=as.Date("1995/01/01"), y=12, label= "Training Set", col = "darkblue", size = 3) + 
  annotate("text", x=as.Date("2019/01/01"), y=12, label= "Test Set", col = "darkred", size = 3) +
  geom_segment(aes(x = as.Date("2014/01/01"), xend = as.Date("2022/03/01"), y = 10.5, yend = 10.5), 
               arrow = arrow(length=unit(0.30,"cm"),type = "closed"), 
               col = "darkred", linewidth = 0.1) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank())

ggsave(plot_1, filename="plot_1.png",device = "png", path = "plots", width = 6, height = 2)
  


#############
'Plot 2'
#############

#take transpose for plot, set dates for vlines

month_12_t <- as.data.frame(t(month_12))
test_date <- date[(length(date)-119):length(date)]
covid_date <- date[length(date) - 35]

plot_2 <- ggplot(data = month_12_t) + 
  geom_line( aes(y = y_test, x = test_date, color = 'Actual')) + 
  geom_line( aes(y = boost_forecast, x = test_date, color = 'XGBoost')) + 
  geom_line( aes(y = ar_forecast, x = test_date, color = 'AR')) + 
  geom_vline(xintercept = covid_date) +
  annotate("text", x=covid_date-100, y=7, label="First UK Lockdown", angle=90, size = 3) +
  theme_classic() + theme(legend.position="top") + 
  scale_color_manual(name = '',
    breaks=c('Actual', 'XGBoost', 'AR'),
                     values=c('Actual'='darkgrey', 'XGBoost'='darkred', 'AR'='red')) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position = "bottom")
  
ggsave(plot_2, filename="plot_2.png",device = "png", path = "plots", width = 6, height = 2.5)

#############
'Table 2'
#############


#############
'pre-covid'
#############

covid_cutoff <- 85

#split data
forecast_pre_covid <- month_12[,1:(covid_cutoff )]
forecast_pre_covid <- round(forecast_pre_covid,1)
y_test_pre_covid <- as.numeric(forecast_pre_covid[1,])

#define sum stats
rmse <- vector(length = nrow(forecast_pre_covid))
mae <- vector(length = nrow(forecast_pre_covid))

#populate summary stats for each model
for (ii in 1:nrow(forecast_pre_covid)){
  rmse[ii] <- RMSE(y_test_pre_covid, as.numeric(forecast_pre_covid[ii,]))
  mae[ii] <- MAE(y_test_pre_covid, as.numeric(forecast_pre_covid[ii,]))
}

#merge summary stats into new df
results_summary <- data.frame(cbind(rmse,mae))
rownames(results_summary) <- rownames(forecast_pre_covid)
results_summary <- results_summary[-1,] 

#dm-test 
error_pre_covid <- error_12month[,1:covid_cutoff]
ar_error_pre_covid <- ar_error[1:covid_cutoff]

dm_pre <- vector(length = 9)

for(ii in 1:9){
  dm <- dm.test(as.numeric(error_pre_covid[ii,]), ar_error_pre_covid, alternative = "less")
  dm_pre[ii] <- dm$p.value }

sapply(dm_pre,pvalue_to_asterisk)

#############
'post-covid'
#############

forecast_post_covid <- month_12[,(covid_cutoff + 1):120]
forecast_post_covid <- round(forecast_post_covid,1)
y_test_post_covid <- as.numeric(forecast_post_covid[1,])

#define sum stats
rmse_2 <- vector(length = nrow(forecast_post_covid))
mae_2 <- vector(length = nrow(forecast_post_covid))

#populate summary stats for each model
for (ii in 1:nrow(forecast_post_covid)){
  rmse_2[ii] <- RMSE(y_test_post_covid, as.numeric(forecast_post_covid[ii,]))
  mae_2[ii] <- MAE(y_test_post_covid, as.numeric(forecast_post_covid[ii,]))
}

#merge summary stats into new df
results_summary_2 <- data.frame(cbind(rmse_2,mae_2))
rownames(results_summary_2) <- rownames(forecast_post_covid)
results_summary_2 <- results_summary_2[-1,] 

#bind both tables
results_merged <- cbind(results_summary,results_summary_2)

#print latex table
print(xtable(results_merged))

#dm test post covid
error_post_covid <- error_12month[,(covid_cutoff+1):120]
ar_error_post_covid <- ar_error[(covid_cutoff+1):120]

dm_post <- vector(length = 9)

for(ii in 1:9){
  dm <- dm.test(as.numeric(error_post_covid[ii,]), ar_error_post_covid, alternative = "less")
  dm_post[ii] <- dm$p.value }

sapply(dm_post,pvalue_to_asterisk)

#############
'Plot 3'
#############

#plot errors over time

error_month_12_t <- as.data.frame(t(rbind(error_12month,ar_forecast = ar_error)))
test_date <- date[(length(date)-119):length(date)]

plot3 <- ggplot(data = error_month_12_t) + 
  geom_line( aes(y = boost_forecast, x = test_date, color = 'XGBoost')) + 
  geom_line( aes(y = ar_forecast, x = test_date, color = 'AR')) + 
  geom_line( aes(y = rf_forecast, x = test_date, color = 'Random Forest')) +
  geom_vline(xintercept = covid_date) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Forecast Error (% Point)") +
  annotate("text", x=covid_date+800, y=0.5, label="No Error Line", angle=0, size = 3) +
  theme_classic() + theme(legend.position="top") + 
  scale_color_manual(name = '',
                     breaks=c('XGBoost', 'AR', 'Random Forest'),
                     values=c('XGBoost'='darkred', 'AR'='red', 'Random Forest' = 'cadetblue')) +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(size = 8),
        legend.position = "bottom")

ggsave(plot3, filename="plot_3.png",device = "png", path = "plots", width = 6, height = 2.5)

#############
'Table 3'
#############

print(xtable(month_12_pca_summary))

#dm test statistics
y_matrix_pca <- y_test
#loop to create matrix of y-s
for(ii in 1:5){y_matrix_pca <- rbind(y_test,y_matrix_pca)}

#calc forecast errors
dm_pca <- month_12_pca[-1,] - y_matrix_pca

#set for loop
dm_vector <- vector(length = 3)
ar_error_pca <- as.numeric(dm_pca[1,])
error_12month_pca <- dm_pca[-1,] #remove ar forecast

for(ii in 1:3){
  dm <- dm.test(as.numeric(error_12month_pca[ii,]),ar_error_pca, alternative = "less")
  dm_vector[ii] <- dm$p.value }
sapply(dm_vector,pvalue_to_asterisk)

#############
'Plot 4'
#############

plot4_df <- as.data.frame(t(rbind(dm_pca)))
colnames(plot4_df) <- c("ar_forecast","boost_forecast","boost_forecast_2","boost_forecast_3")

plot4 <- ggplot(data = plot4_df) + 
  geom_line( aes(y = ar_forecast, x = test_date, color = 'AR(p)')) +
  geom_line( aes(y = boost_forecast, x = test_date, color = 'XGBoost')) +
  geom_line( aes(y = boost_forecast_3, x = test_date, color = 'XGBoost with Lagged PCs')) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Forecast Error (% Point)") +
  annotate("text", x=covid_date+800, y=0.8, label="No Error Line", angle=0, size = 3) +
  theme_classic() + theme(legend.position="top") + 
  scale_color_manual(name = '',
                     breaks=c('AR(p)','XGBoost','XGBoost with Lagged PCs'),
                     values=c('XGBoost'='darkred', 'AR(p)'='red','XGBoost with Lagged PCs' = 'cadetblue')) +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(size = 8),
        legend.position = "bottom")

ggsave(plot4, filename="plot_4.png",device = "png", path = "plots", width = 6, height = 2.5)


#############
'Plot 5'
#############

plot5_df <- readRDS("summary_pcs_table")

#plot 5
plot5 <- ggplot(plot5_df, aes(x = pcs)) + 
  geom_line(aes(y = rmse_pcs)) + 
  theme_classic() + 
  ylab("Root Mean Squared Error") + 
  xlab("Number of Principal Components")

ggsave(plot5, filename="plot5.png",device = "png", path = "plots", width = 6, height = 2.5)


#############
'name list table'
#############

name_table <- names %>% 
  dplyr::select(ID = id,Title = title)

name_table <- name_table[,-1]

print(xtable(name_table))





