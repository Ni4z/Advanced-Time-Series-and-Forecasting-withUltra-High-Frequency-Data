library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ACDm)

df <- read_csv("tick_data.csv")

## UHF Return Calculations

# Handling Timestapms,
df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, origin="1970-01-01", tz="UTC")) %>%
  arrange(timestamp)

df <- df %>% arrange(timestamp)

# log returns for each Asset_ID
df <- df %>%
  group_by(Asset_ID) %>%
  mutate(Log_Return = log(Close / lag(Close)))


df_asset6 <- df %>% filter(Asset_ID == 6)

ggplot(df_asset6, aes(x=timestamp, y=Log_Return)) +
  geom_line() +
  labs(title="UHF Log Returns for Ethereum", x="Time", y="Log Returns") +
  theme_minimal()


# Estimation and Plot of Realized Volatility

df <- df %>% mutate(Squared_Log_Return = Log_Return^2)

# Aggregating squared-log returns by day and Asset_ID
rv_daily <- df %>%
  group_by(Date = date(timestamp), Asset_ID) %>%
  summarize(Daily_RV = sqrt(sum(Squared_Log_Return, na.rm = TRUE)))

# Plotting RV for Asset_ID 2/ or I changed the data variable according to my need.
rv_asset6 <- rv_daily %>% filter(Asset_ID == 6)

ggplot(rv_asset6, aes(x=Date, y=Daily_RV)) +
  geom_line(color="blue") +
  labs(title="Daily Realized Volatility for Cardano", x="Date", y="Realized Volatility") +
  theme_minimal()


# Example: Aggregating 'Close' prices by hour for Asset_ID 0-4, 6
hourly_data <- df %>% 
  filter(Asset_ID == 6) %>%
  mutate(Hour = floor_date(timestamp, "hour")) %>%
  group_by(Hour) %>%
  summarize(Avg_Close = mean(Close, na.rm = TRUE))

# Plotting
ggplot(hourly_data, aes(x=Hour, y=Avg_Close)) +
  geom_line() +
  labs(title="Hourly Average Close Price for Ethereum", x="Time", y="Average Close Price") +
  theme_minimal()



df <- df %>%
  mutate(timestamp = as.POSIXct(timestamp, origin="1970-01-01", tz="UTC")) %>%
  arrange(timestamp)

two_days <- df %>%
  mutate(Date = as.Date(timestamp)) %>%
  distinct(Date) %>%
  slice(1:2) %>%
  pull(Date)

df_filtered <- df %>%
  filter(Asset_ID %in% c(1, 6), as.Date(timestamp) %in% two_days)

df_filtered <- df_filtered %>%
  group_by(Asset_ID, Date = as.Date(timestamp)) %>%
  mutate(Log_Return = log(Close / lag(Close)))

ggplot(df_filtered, aes(x=timestamp, y=Log_Return, color=factor(Asset_ID))) +
  geom_line() +
  labs(title="UHF Log Returns for Asset ID 1 and 6 Over Two Days", x="Time", y="Log Returns") +
  theme_minimal() +
  facet_wrap(~Asset_ID, scales = "free_y")


first_day <- min(df_filtered$Date)
first_hour_start <- min(df_filtered$timestamp[df_filtered$Date == first_day])
first_hour_end <- first_hour_start + hours(1) - seconds(1) 

df_first_hour <- df_filtered %>%
  filter(timestamp >= first_hour_start & timestamp <= first_hour_end)

df_first_hour <- df_first_hour %>%
  group_by(Asset_ID, Date = as.Date(timestamp)) %>%
  mutate(Log_Return = log(Close / lag(Close)))

ggplot(df_first_hour, aes(x=timestamp, y=Log_Return, color=factor(Asset_ID))) +
  geom_line() +
  labs(title="UHF Log Returns for Bitcoin (1) and Ethereum (6) for a Specific Hour", x="Time", y="Log Returns") +
  theme_minimal() +
  facet_wrap(~Asset_ID, scales = "free_y") +
  scale_x_datetime(date_labels = "%H:%M", 
                   breaks = seq(from = min(df_first_hour$timestamp), 
                                to = max(df_first_hour$timestamp), 
                                by = "10 min")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_first_hour, aes(x=timestamp, y=Close, color=factor(Asset_ID))) +
  geom_line() +
  labs(title="Stock Prices for Bitcoin (1) and Ethereum (6) for a Specific Hour", x="Time", y="Stock Price") +
  theme_minimal() +
  facet_wrap(~Asset_ID, scales = "free_y") +
  scale_x_datetime(date_labels = "%H:%M", 
                   breaks = seq(from = min(df_first_hour$timestamp), 
                                to = max(df_first_hour$timestamp), 
                                by = "10 min")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ........................RV for two assets......................


df <- df %>%
  group_by(Asset_ID) %>%
  mutate(Log_Return = log(Close / lag(Close))) %>%
  ungroup()

# Filter for Asset ID 1 and 6 only
df_filtered <- df %>%
  filter(Asset_ID %in% c(1, 6))

# Calculate squared log returns (necessary for RV calculation)
df_filtered <- df_filtered %>%
  mutate(Squared_Log_Return = Log_Return^2)

# Aggregate these squared returns by hour and by Asset_ID to compute hourly RV
hourly_rv <- df_filtered %>%
  mutate(Hour = floor_date(timestamp, "hour")) %>%
  group_by(Asset_ID, Hour) %>%
  summarize(Hourly_RV = sum(Squared_Log_Return, na.rm = TRUE))

# Plotting RV for Asset_ID 1 and 6
ggplot(hourly_rv, aes(x=Hour, y=sqrt(Hourly_RV), color=factor(Asset_ID))) + # Using sqrt to get the standard deviation as RV
  geom_line() +
  labs(title="Hourly Realized Volatility for Bitcoin(1) and Ethereum(6)", x="Hour", y="Realized Volatility (RV)") +
  theme_minimal() +
  facet_wrap(~Asset_ID, scales = "free_y") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   date_breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#--------------stock price-----------------
# Aggregating Close Prices by hour for Asset_ID 1 and 6
hourly_prices <- df_filtered %>%
  mutate(Hour = floor_date(timestamp, "hour")) %>%
  group_by(Asset_ID, Hour) %>%
  summarize(Avg_Close = mean(Close, na.rm = TRUE))


# Plotting Hourly Average Stock Prices for Asset_ID 1 and 6
ggplot(hourly_prices, aes(x=Hour, y=Avg_Close, color=factor(Asset_ID))) +
  geom_line() +
  labs(title="Hourly Average Stock Prices for Bitcoin(1) and Ethereum(6)", x="Hour", y="Average Stock Price") +
  theme_minimal() +
  facet_wrap(~Asset_ID, scales = "free_y") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   date_breaks = "1 day") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#.............................Comparison.................................


df <- df %>%
  mutate(Log_Return = log(Close / lag(Close)))

# Step 2: Calculate Hourly Realized Volatility
rv_hourly <- df %>%
  filter(Asset_ID %in% c(1, 6)) %>%
  group_by(Asset_ID, Hour = floor_date(timestamp, "hour")) %>%
  summarize(RV = sqrt(sum(Log_Return^2, na.rm = TRUE)))

# Step 3: Plot Hourly Realized Volatility
#ggplot(rv_hourly, aes(x=Hour, y=RV, color=factor(Asset_ID))) +
  #geom_line() +
  #labs(title="Hourly Realized Volatility for Asset ID 1 and 6", x="Hour", y="Realized Volatility") +
  #theme_minimal() +
  #facet_wrap(~Asset_ID, scales = "free_y")"""


# Step 4: Aggregate Close Prices by Hour for Plotting
'hourly_prices <- df %>%
  filter(Asset_ID %in% c(1, 6)) %>%
  group_by(Asset_ID, Hour = floor_date(timestamp, "hour")) %>%
  summarize(Avg_Close = mean(Close, na.rm = TRUE))'

#Hourly Average Stock Prices
ggplot(hourly_prices, aes(x=Hour, y=Avg_Close, color=factor(Asset_ID))) +
  geom_line() +
  labs(title="Hourly Average Stock Prices for Bitcoin(1) and Ethereum(6)", x="Hour", y="Average Stock Price") +
  theme_minimal() +
  facet_wrap(~Asset_ID, scales = "free_y")
