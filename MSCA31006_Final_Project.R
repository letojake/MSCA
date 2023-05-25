### Final Project ###
# Dataset - MSU game attendance
# Objective - forecast game attendance

library(forecast)
library(readr)
library(dplyr)
library(lubridate)
library(tseries)
library(ggplot2)
library(zoo)

###### Create Dataset ######


project_folder = 'C:/Users/JakeLeto/OneDrive - Hexagon/Documents/MSCA_31006_Final_Project/'
gs_2001_2017 <- read.csv(file.path(project_folder,'msu_game_summary_2001_2017.csv'))
gs_2013_2018 <- read.csv(file.path(project_folder,'msu_game_summary_2013_2018.csv'))


## Convert time column to date 
gs_2001_2017$scheduled_date <- as.Date(gs_2001_2017$scheduled_date, format = "%d-%b-%y %I:%M %p")
gs_2013_2018$scheduled_date <- as.Date(gs_2013_2018$scheduled_date)




## Initial EDA ## 
ggplot(
  gs_2001_2017, aes(x = scheduled_date, y = attendance)) +
  geom_line() +
  labs(x = "Date", y = "Attendance", title = 'MSU Basketball Attendance (2001-2017 Season)') +
  scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
  gs_2013_2018, aes(x = scheduled_date, y = attendance)) +
  geom_line() +
  labs(x = "Date", y = "Attendance", title =  'MSU Basketball Attendance (2013-2018 Season)') +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Calculate percent capacity - attendance/venue capacity
gs_2013_2018$percent_capacity <- gs_2013_2018$attendance/gs_2013_2018$venue_capacity

params <- c('percent_capacity','field_goals_pct','opp_field_goals_pct','turnovers','rebounds','assists','steals','blocks')
## Check Correlations ## 
correlations <- cor(na.omit(gs_2013_2018)[params]) 


## For 2001-2012 we don't know where the game was played - assume 50% of the games are home and randomly calculate capacity
## Union datasets rowwise (only take needed columns)

pcap_func <- function(df, index_array, numerator_col, denominator_col, new_denominator_col) {
  df_copy <- df
  df_copy[, "percent_capacity"] <- df[, numerator_col] / df[, denominator_col]
  df_copy[index_array, "percent_capacity"] <- df[index_array, numerator_col] / df[index_array, new_denominator_col]
  return(df_copy)
}

season_dfs <- list()

seasons <- unique(gs_2001_2017$season)

for (szn in seasons) {
  season_df <- gs_2001_2017 %>% filter(season == szn) 
  away_games <- sample(seq_len(nrow(season_df)), floor(nrow(season_df) / 2), replace = FALSE)
  result <- pcap_func(season_df, away_games, "attendance", "venue_capacity", "opp_venue_capacity")
  season_dfs[[length(season_dfs) + 1]] <- result
}

gs_2001_2017 <- bind_rows(season_dfs)

#gs_2001_2017 <- gs_2001_2017 %>%
#  mutate(percent_capacity = ifelse(percent_capacity <= 1.0, percent_capacity, 1.0))


ggplot(
  gs_2001_2017, aes(x = scheduled_date, y = percent_capacity)) +
  geom_line() +
  labs(x = "Date", y = "Percent Capacity", title = 'MSU Basketball Percent Capacity (2001-2017 Season)') +
  scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(
  gs_2013_2018, aes(x = scheduled_date, y = percent_capacity)) +
  geom_line() +
  labs(x = "Date", y = "Percent Capacity", title =  'MSU Basketball Percent Capacity (2013-2018 Season)') +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#gs_2001_2013 <- gs_2001_2017 %>% filter(season < min(gs_2013_2018$season)) 


## Function to make ts uniform (linear interpolation)
make_uniform <- function(df,params,min_date,max_date) {
  uniform_dates <- seq(min_date, max_date, by = "day")
  uniform_df <- merge(df, data.frame(scheduled_date = uniform_dates), by = "scheduled_date", all = TRUE)
  uniform_df$percent_capacity <- na.approx(uniform_df$percent_capacity)
  uniform_df[[params[3]]] <- na.approx(uniform_df[[params[3]]])
  uniform_df[[params[4]]] <- na.approx(uniform_df[[params[4]]])
  return(uniform_df)
}




## Concatenate attendance data w/ desired variables - train data will be from 2001-2017

params <- c('scheduled_date','percent_capacity','points_game','opp_points_game')
train_2001_2017 <- make_uniform(gs_2001_2017,params,min(gs_2001_2017$scheduled_date),max(gs_2001_2017$scheduled_date))[, params]
names(train_2001_2017)[names(train_2001_2017) == 'points_game'] <- "points"
names(train_2001_2017)[names(train_2001_2017) == 'opp_points_game'] <- 'opp_points'

params <- c('scheduled_date','percent_capacity','points','opp_points')
max(train_2001_2017$scheduled_date)
max(gs_2013_2018$scheduled_date)
train_2017_2018 <- make_uniform(na.omit(gs_2013_2018),params,max(train_2001_2017$scheduled_date),max(gs_2013_2018$scheduled_date))[, params]
train_2017_2018 <- train_2017_2018 %>% filter(scheduled_date > max(train_2001_2017$scheduled_date))

train_data <- bind_rows(train_2001_2017,train_2017_2018)

ggplot(
  train_data, aes(x = scheduled_date, y = percent_capacity)) +
  geom_line() +
  labs(x = "Date", y = "Percent Capacity", title = 'MSU Basketball Percent Capacity (2001-2018 Season)') +
  scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Aggregate Data ### 
train_data_monthly <- train_data %>%
  group_by(year = lubridate::year(scheduled_date), month = lubridate::month(scheduled_date)) %>%
  summarise(avg_pct_capacity = mean(percent_capacity))

plot(acf(train_data_monthly$avg_pct_capacity))

ggplot(
train_data_monthly, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = avg_pct_capacity)) +
  geom_line() +
  labs(x = "Date", y = "Average Percent Capacity",title='Average Monthly Percent Capacity (2001-2018)') +
  scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Check Stationarity using ADF and KPSS ## 

# Avg Pct Capacity
adf.test(train_data_monthly$avg_pct_capacity) # Stationary
kpss.test(train_data_monthly$avg_pct_capacity)  # Stationary

# Function to calculate accuracy metrics
calculate_accuracy <- function(observed, predicted) {
  accuracy <- data.frame(
    MAE = mean(abs(observed - predicted)),
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAPE = mean(abs((observed - predicted) / observed) * 100)
  )
  return(accuracy)
}

split_row <- nrow(train_data_monthly) - 3

####### ARIMA  ########

arima_train <- train_data_monthly[1:split_row,]
arima_test <- train_data_monthly[(split_row + 1):nrow(train_data_monthly), 'avg_pct_capacity']

baseline_mod <- auto.arima(arima_train$avg_pct_capacity)
baseline_forecast <- forecast(baseline_mod,h=3)
plot(baseline_forecast,xlab = "Index", ylab = "Average Percent Capacity")
basemod_acc <- calculate_accuracy(baseline_forecast$mean,arima_test$avg_pct_capacity)
summary(baseline_mod)

adjusted_model <- Arima(arima_train$avg_pct_capacity, order = c(2,0,2))
adj_forecast <- forecast(adjusted_model,h=3)
plot(adj_forecast,xlab = "Index", ylab = "Average Percent Capacity")
adjmod_acc <- calculate_accuracy(adj_forecast$mean,arima_test$avg_pct_capacity)
summary(adjusted_model)