##############################
'7. Professional Forecasts Comparison + Further Plots '
#Author: Will Winterbotham 
#Date: Sep 2023
# Codebook with plots used, labelled by figures
##############################

rm(list = ls()) 
setwd("~/Documents/Dissertation")

library(readxl)
library(zoo)
library(tidyverse)

#############
'Read forecasts and prepare'
#############

boe_forecasts <- read_excel("boe_forecasts_may.xlsx", 
                            sheet = "CPI Forecast", skip = 4)

#filter based off median forecast
fcast_df <- boe_forecasts %>% 
  filter(...4 == "Market Median")

#df with only fcasts
fcast_only <- fcast_df[,5:ncol(fcast_df)]

# Function to get the first non-NA value in a row
get_fifth_non_na <- function(row) {
  fifth_non_na <- na.omit(row)[5]
  return(fifth_non_na)
}

# Apply the function across rows
fifth_non_na_values <- apply(fcast_only, 1, get_fifth_non_na)

# Print the first non-NA values
print(fifth_non_na_values)

df <- cbind(fcast_df[,1:4],fifth_non_na_values)
df <- df[-c(1:24),] #keep only needed months
df <- df[,c(1,2,5)] #keep required vars
colnames(df) <- c("year","month","inflation")

#remove NAs in date row 
df$year <- na.locf(df$year)

#make month numeric
month_numeric <- match(df$month, month.name)

#create the date variable (and quarterly)
df$date <- as.Date(paste(df$year, month_numeric, "01", sep = "-"))
df$qtr_date <- as.yearqtr(df$date)

#create var with forecast date (as that is 4 quarters ahead)
df$forecast_qtr_date <- df$qtr_date + 1
df$forecast_date <- as.Date(df$forecast_qtr_date)

#############
'Compare to xgboost/ar forecasts'
#############
original_vars <- readRDS("1.SA.final.dataset") 
pca_fcast <- data.frame(t(readRDS("12month_pca_forecasts"))) #transpose results

#create date var and make quarterly
pca_fcast$forecast_date <- original_vars$date[(nrow(original_vars)-119):nrow(original_vars)]
pca_fcast$qtr_date <- as.yearqtr(pca_fcast$forecast_date)

#############
'xgb forecast'
#############

#find quarterly forecast for xgb
xgb_qtr <- pca_fcast %>% 
  group_by(qtr_date) %>% 
  summarise(xgb_fcast = mean(XGBoost.with.lagged.PCs))

#merge xgb back to dataset
pca_fcast <- pca_fcast %>%
  left_join(xgb_qtr, by = "qtr_date")

#############
'actual inf'
#############

#find actual quarterly inflation rate 
y_qtr <- pca_fcast %>% 
  group_by(qtr_date) %>% 
  summarise(y_fcast = mean(y))

#merge inflation back to dataset
pca_fcast <- pca_fcast %>%
  left_join(y_qtr, by = "qtr_date")

#############
'ar fcast'
#############

#find ar(p) quarterly fcast
ar_qtr <- pca_fcast %>% 
  group_by(qtr_date) %>% 
  summarise(ar_fcast = mean(AR.p.))

#merge inflation back to dataset
pca_fcast <- pca_fcast %>%
  left_join(ar_qtr, by = "qtr_date")


#keep neccessary vars 
pca_fcast <- pca_fcast[,-c(1:5)]

#############
'compare with prof. fcast'
#############

df_merged <- left_join(pca_fcast, df, by = "forecast_date") %>% 
  drop_na() %>% 
  dplyr::select(date = qtr_date.x,xgb_fcast, y = y_fcast, ar_fcast, boe_fcast = inflation) %>% 
  mutate_if(is.numeric, round, digits = 2)

plot1_prof_forecast <- ggplot(data = df_merged) + 
  geom_line( aes(y = xgb_fcast, x = date, color = 'XGBoost')) + 
  geom_line( aes(y = boe_fcast, x = date, color = 'Bank of England')) + 
  geom_line( aes(y = y, x = date, color = 'Actual Inflation'),linetype = 'dashed') +
  ylab("Quarterly Inflation Forecast") +
  theme_classic() + theme(legend.position="top") + 
  scale_y_continuous(limits = c(0, 12), breaks = c(0, 2, 4, 6, 8, 10,12)) + 
  scale_x_yearqtr(format = '%Y Q%q') +
  scale_color_manual(name = '',
                     breaks=c('XGBoost', 'Bank of England', 'Actual Inflation'),
                     values=c('XGBoost'='darkred', 'Bank of England'='red', 'Actual Inflation' = 'black')) +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(size = 8),
        legend.position = "bottom")

ggsave(plot1_prof_forecast, filename="plot1_prof_forecast.png",
       device = "png", path = "plots", width = 6, height = 2.5)

#############
'barplot'
#############

df_merged$pre2021 <- df_merged$date < 2021

calculate_error <- function(actual, forecast) {
  mae <- mean(abs(actual - forecast))
  return(MAE = mae)
}

#create tibble with summary stats
bar_chart_df_boe <- df_merged %>% 
  group_by(pre2021) %>% 
  reframe("BOE" = calculate_error(y,boe_fcast), 
          "AR(p)" = calculate_error(y,ar_fcast),
          "XGBoost" = calculate_error(y,xgb_fcast)) %>% 
  dplyr::select(!(pre2021))

bar_chart_df_boe <- data.frame(name = colnames(bar_chart_df_boe), 
                        pre_2021 = t(bar_chart_df_boe[2,]), 
                        post_2021 = t(bar_chart_df_boe[1,]))


#pivot longer
bar_chart_df_boe <- bar_chart_df_boe %>%
  pivot_longer(cols = c(pre_2021,post_2021), names_to = "year", values_to = "values")

bar_chart_df_boe$year <- factor(bar_chart_df_boe$year, levels = c("pre_2021", "post_2021"))

#bar chart
plot4_prof_forecast <- ggplot(bar_chart_df_boe, aes(x = name, y = values, fill = year)) + 
  geom_col(position = "dodge") + 
  geom_text(
    aes(label = round(values,2)),
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.9)
  ) + 
  theme_classic() + 
  ylab("Mean Absolute Error (% point)") +
  scale_fill_manual(values=c("cadetblue","darkred"),name = "", labels = c("Pre-2021","2021 Onwards")) + 
  theme(axis.title.x=element_blank(),
        axis.text = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(size = 8),
        legend.position = c(0.8, 0.8))

#save chart
ggsave(plot4_prof_forecast, filename="plot4_prof_forecast.png",
       device = "png", path = "plots", width = 6, height = 4)



#############
'check values align with quarterly index'
#############

cpi_index <- read_csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7bt/mm23",
         skip = 7)

cpi_index <- cpi_index[95:177,] #keep quarterly values
colnames(cpi_index) <- c("date", "cpi")

#calculate growth rate
cpi_index <- cpi_index %>%
  mutate(growth = 100*(cpi - lag(cpi,4))/lag(cpi,4))

#make date format
cpi_index$date <- as.yearqtr(cpi_index$date)

#merge
cpi_comparison <- df_merged %>% 
  dplyr::select(date, y) %>% 
  left_join(cpi_index)

mean(abs(cpi_comparison$growth - cpi_comparison$y)) #finds mean difference between two series

#############
'treasury forecasts'
#############

#import file - transpose so that forecast date is on left

hmt_forecasts <- data.frame(t(read_csv("hmt_forecasts.csv")))

colnames(hmt_forecasts) <- hmt_forecasts[1,]
hmt_forecasts <- hmt_forecasts[-c(1:10),]

get_non_na <- function(row) {
  get_non_na <- na.omit(row)[11]
  return(get_non_na)
}

# Apply the function across rows
hmt_forecast_clean <- data.frame(forecast = apply(hmt_forecasts, 1, get_non_na))

#make into yr-qtr date
hmt_forecast_clean$date <-   as.yearqtr(rownames(hmt_forecast_clean))
hmt_forecast_clean$forecast <- as.numeric(hmt_forecast_clean$forecast)

hmt_forecast_clean <- hmt_forecast_clean %>% 
  left_join(df_merged, by = "date") %>% 
  drop_na() 

#line plot of forecasts with HMT

plot2_prof_forecast <-  ggplot(data = hmt_forecast_clean) + 
  geom_line( aes(y = xgb_fcast, x = date, color = 'XGBoost')) + 
  geom_line( aes(y = boe_fcast, x = date, color = 'Bank of England')) + 
  geom_line( aes(y = y, x = date, color = 'Actual Inflation'),linetype = 'dashed') +
  geom_line( aes(y = forecast, x = date, color = 'Professional Consensus')) +
  ylab("Quarterly Inflation Forecast") +
  theme_classic() + theme(legend.position="top") + 
  scale_y_continuous(limits = c(0, 12), breaks = c(0, 2, 4, 6, 8, 10,12)) + 
  scale_x_yearqtr(format = '%Y', n = 10) +
  scale_color_manual(name = '',
                     breaks=c('XGBoost', 'Bank of England', 'Actual Inflation','Professional Consensus'),
                     values=c('XGBoost'='darkred', 'Bank of England'='red', 
                              'Actual Inflation' = 'black', 'Professional Consensus' = 'blue')) +
  theme(axis.title.x=element_blank(), 
        axis.title.y = element_text(size = 8),
        legend.position = "bottom")

ggsave(plot2_prof_forecast, filename="plot2_prof_forecast.png",
       device = "png", path = "plots", width = 6, height = 2.5)

#############
'bar chart of performance'
#############

hmt_forecast_clean$pre2021 <- hmt_forecast_clean$date < 2021

calculate_error <- function(actual, forecast) {
  mae <- mean(abs(actual - forecast))
  return(MAE = mae)
}

#create tibble with summary stats
bar_chart_df <- hmt_forecast_clean %>% 
  group_by(pre2021) %>% 
  reframe("Prof. Consensus" = calculate_error(y,forecast),
          "BOE" = calculate_error(y,boe_fcast), 
          "AR(p)" = calculate_error(y,ar_fcast),
          "XGBoost" = calculate_error(y,xgb_fcast)) %>% 
  dplyr::select(!(pre2021))

bar_chart <- data.frame(name = colnames(bar_chart_df), 
           pre_2021 = t(bar_chart_df[2,]), 
           post_2021 = t(bar_chart_df[1,]))

#pivot longer
bar_chart <- bar_chart %>%
  pivot_longer(cols = c(pre_2021,post_2021), names_to = "year", values_to = "values")

bar_chart$year <- factor(bar_chart$year, levels = c("pre_2021", "post_2021"))

#bar chart
plot3_prof_forecast <- ggplot(bar_chart, aes(x = name, y = values, fill = year)) + 
  geom_col(position = "dodge") + 
  geom_text(
    aes(label = round(values,2)),
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.9)
  ) + 
  theme_classic() + 
  ylab("Mean Absolute Error (% point)") +
  scale_fill_manual(values=c("cadetblue","darkred"),name = "", labels = c("Pre-2021", "2021 Onwards")) + 
  theme(axis.title.x=element_blank(),
        axis.text = element_text(face="bold", colour = "black"), 
        axis.title.y = element_text(size = 8),
        legend.position = c(0.85, 0.8))

ggsave(plot3_prof_forecast, filename="plot3_prof_forecast.png",
       device = "png", path = "plots", width = 6, height = 4)






