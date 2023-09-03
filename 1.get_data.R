########################################
'1. Using FRED to get monthly time series'
#Author: Will Winterbotham 
#Date: June 26th 2023
#note: due to FRED API limits this code needs to be run in 1 minute intervals between sections
########################################

##############################
'Importing Packages'
##############################

setwd("~/Documents/Dissertation")
rm(list = ls())

#load packages
library(fredr)
library(tidyverse)

#enter unique API key
fredr_set_key("7d87f40427096d79c714cd7b68b6ab1e")

##############################
'Retrieve CPI Inflation from ONS'
##############################

#import historical CPI data (1950 - 1988)
cpi_hist <- read_csv("https://www.ons.gov.uk/generator?uri=/economy/inflationandpriceindices/methodologies/consumerpriceinflationhistoricalestimatesuk1950to1988methodology/250546fa&format=csv",
                     skip = 6)

#import recent CPI data (1989 - 2023)
cpi_current <- read_csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7g7/mm23", 
                        skip = 180, col_names = c("Date","CPI"))

#align date format
cpi_current$Date <- as.Date(paste0(cpi_current$Date, " 01"), format = "%Y %b %d")
cpi_hist$Date <- as.Date(paste0("01-", cpi_hist$Date), format = "%d-%b-%y")

#remove pre-1980 and previous data for historical cpi
cpi_hist <- cpi_hist[-c(1:360),-3]
colnames(cpi_hist) <- colnames(cpi_current) #merge colnames for merge later

#merge old and new
cpi <- rbind(cpi_hist,cpi_current)


#############
'UK indicators - Retrieve Other Variables'
#############

#find monthly series for UK data
query1 <- fredr_series_search_text(
  search_text = "United Kingdom",
  order_by = "popularity",
  sort_order = "desc", 
  filter_variable = "frequency",
  filter_value = "Monthly", 
  limit = 1000
)

#set dates to filter
query1$observation_start <- as.Date(query1$observation_start)
query1$observation_end <- as.Date(query1$observation_end)

#plot date distribution
hist(query1$observation_start, breaks = 100)

#restrict to 1980-2020
query1_clean <- query1 %>% 
  filter(observation_start <= "1980/01/01", observation_end >= "2023/03/01")

#set ID list for loop
ID_list <- query1_clean$id
start_date <- as.Date("1980/01/01")
end_date <- as.Date("2023/03/01") 

#import first df to know dimensions
df_test <- fredr(series_id = ID_list[1], observation_start = start_date, observation_end = end_date) 

#set df for loop to fill
df1 <- data.frame(matrix(nrow = nrow(df_test), ncol = length(ID_list)))

#first loop
for(ii in 1:length(ID_list)){
  df <- fredr(series_id = ID_list[ii], 
              observation_start = start_date, observation_end = end_date)
  df1[,ii] <- df$value
}

#make colnames, drop NAs
colnames(df1) <- ID_list
df1 <- df1[, I(colSums(is.na(df1))==0)]


#############
'global indicators'
#############

#find monthly series for global indicators
query2 <- fredr_series_search_text(
  search_text = "global",
  order_by = "popularity",
  sort_order = "desc", 
  filter_variable = "frequency",
  filter_value = "Monthly", 
  limit = 1000
)

#set dates to filter
query2$observation_start <- as.Date(query2$observation_start)
query2$observation_end <- as.Date(query2$observation_end)

#plot date distribution
hist(query2$observation_start, breaks = 100)

#restrict to 1980-2020
query2_clean <- query2 %>% 
  filter(observation_start <= "1980/01/01", observation_end >= "2023/03/01")

ID_list2 <- query2_clean$id

#set df for loop to fill
df2 <- data.frame(matrix(nrow = nrow(df_test), ncol = length(ID_list2)))

#second loop
for(ii in 1:length(ID_list2)){
  df <- fredr(series_id = ID_list2[ii], 
              observation_start = start_date, observation_end = end_date)
  df2[,ii] <- df$value
}

#make colnames, drop NAs
colnames(df2) <- ID_list2
df2 <- df2[, I(colSums(is.na(df2))==0)]

#############
'commodity prices'
#############

query3 <- fredr_series_search_text(
  search_text = "commodity",
  order_by = "popularity",
  sort_order = "desc", 
  filter_variable = "frequency",
  filter_value = "Monthly", 
  limit = 1000
)

#set dates to filter
query3$observation_start <- as.Date(query3$observation_start)
query3$observation_end <- as.Date(query3$observation_end)

#restrict to 1980-2020
query3_clean <- query3 %>% 
  filter(observation_start <= "1980/01/01", observation_end >= "2023/03/01", 
         seasonal_adjustment_short == "SA")

ID_list3 <- query3_clean$id

#set df for loop to fill
df3 <- data.frame(matrix(nrow = nrow(df_test), ncol = length(ID_list3)))

#second loop
for(ii in 1:length(ID_list3)){
  df <- fredr(series_id = ID_list3[ii], 
              observation_start = start_date, observation_end = end_date)
  df3[,ii] <- df$value
}

#make colnames, drop NAs
colnames(df3) <- ID_list3
df3 <- df3[, I(colSums(is.na(df3))==0)]

#############
'merge datasets'
#############

#bind datasets (and date variable)
df4 <- cbind(date = df_test$date,df1,df2,df3)

colnames(df4) <- c("date",colnames(df1), colnames(df2),colnames(df3))

#remove identical series (NSA/SA)
all_series <- rbind(query1_clean,query2_clean,query3_clean) 
all_series$name1 <- substr(all_series$id,start = 1,stop = 8)

#filter to remove duplicates
all_series <- all_series %>% 
  group_by(name1) %>% 
  filter(row_number() == 1)

#vector with final id names
vec <- all_series$id 
  
#apply this vector to retrieve final dataset
df5 <- df4[,colnames(df4) %in% vec]

#retrieve final names dataset 
names <- all_series[all_series$id %in% colnames(df5),]

saveRDS(df5,"1.final-dataset")
saveRDS(names,"1.name-list")

#############
'prepare data'
#############

#series are log-differenced, any non-seasonally adjusted datasets are adjusted

#seasonally adjust for variables that require using loop
df5_t <- df5 #set dataset to populate
NSA_names <- names$id[names$seasonal_adjustment_short == "NSA"]

#loop through NSA names and seasonally adjust
for (i in NSA_names) {
  ts_data <- ts(df5_t[, i], frequency = 12)
  seasonal_adjusted <- decompose(ts_data)
  adjusted_data <- seasonal_adjusted$x - seasonal_adjusted$seasonal
  df5_t[, i] <- adjusted_data
}

#create function which adds (min value + 1) if there are negative values (to avoid log(<0))

add_min_if_negative_dataset <- function(data) {
  for (column_name in colnames(data)) {
    if (is.numeric(data[[column_name]]) && any(data[[column_name]] < 0)) {
      min_value <- min(data[[column_name]])
      data[[column_name]] <- data[[column_name]] + abs(min_value) + 1
    }
  }
  return(data)
}

#apply function, create transormed df
df5_t <- add_min_if_negative_dataset(df5_t)

min(df5_t) #confirms no negative values

#set df6 to populate
df6 <- df5_t

#now take log differences of variables
for(ii in colnames(df6)){
  df6[,ii] <- c("NA",100*diff(log(df5_t[,ii])))
}

#add date
df6 <- cbind(date = df_test$date,df6)
df6 <- df6[-c(1:12),] #drop NA row from differencing

#merge cpi inflation
colnames(cpi)[1] <- "date"
df6 <- merge(cpi,df6, by = "date")

#save final file
saveRDS(df6,"1.SA.final.dataset")













