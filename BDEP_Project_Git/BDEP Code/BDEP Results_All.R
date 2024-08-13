#download all necessary libraries 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyr)
library(weathercan)
library(dygraphs)
library(xts)
library(htmltools)

#import and retrieve raw data logger data. 
soil_data <- read.csv("Data_Loggers.csv", header = TRUE)

date_columns <- paste0("Date_SM", 1:6)

# Convert date columns to POSIXct format y-m-d H:M
soil_data[date_columns] <- lapply(soil_data[date_columns], as.POSIXct, format = "%Y-%m-%d %H:%M")

# Convert depth columns to numeric
depth_columns <- paste0("SM", 1:6, "_Depth15")
soil_data[depth_columns] <- sapply(soil_data[depth_columns], as.numeric)

#create individual data frames for each data logger, because the csv file contains data for all 6 loggers in 1 tab.
#whenever new data is collected from the data loggers, this csv file will need to be updated, and the code will need to be re-run
#the rows for each data were selected, so there are no blank cells selected for the data frame 
SM1 <- select(soil_data,1:15)
SM1 <- SM1[1:89977,]
SM2 <- select(soil_data,17:31)
SM2 <- SM2[1:8561,]
SM3 <- select(soil_data, 33:47)
SM3 <- SM3[1:12703,]
SM4 <- select(soil_data, 49:63) 
SM4 <- SM4[1:25555,]
SM5 <- select(soil_data, 65:76)
SM5 <- SM5[1:16688,]
SM6 <- select(soil_data, 78:88)
SM6 <- SM6[1:14616,]

#collecting historical percipitation data for calgary- Station name = Calgary INT'L CS
yyc <- weather_dl(station_ids = "27211", start = "2020-07-01", end = "2024-08-09", interval = "hour") %>%
  select(time, precip_amt)
print("the data has been compiled")

#aggregate data in a weekly timsteap instead of daily 
yyc_weekly <- yyc %>%
  mutate(week_start = floor_date(time, "week")) %>%
  group_by(week_start) %>%
  summarize(precip_amt_mean = sum(precip_amt, na.rm = TRUE))

dyHide <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Hide",
    path = system.file("plugins/hide.js", package = "dygraphs")
  )
}

calculate_weekly_mean <- function(df, sensor_name) {
  df %>%
    mutate(week_start = floor_date(get(paste0("Date_", sensor_name)), "week")) %>%
    group_by(week_start) %>%
    summarize(across(starts_with(paste0(sensor_name, "_Depth")), mean, na.rm = TRUE)) %>%
    rename(Date = week_start)
}

SM1_weekly <- calculate_weekly_mean(SM1, "SM1")
SM2_weekly <- calculate_weekly_mean(SM2, "SM2")
SM3_weekly <- calculate_weekly_mean(SM3, "SM3")
SM4_weekly <- calculate_weekly_mean(SM4, "SM4")
SM5_weekly <- calculate_weekly_mean(SM5, "SM5")
SM6_weekly <- calculate_weekly_mean(SM6, "SM6")



true_depth <- read.csv("Files/BDEP_TrueDepth.csv", header = TRUE) %>%
  slice(1:8) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))%>%
  mutate(across(everything(), ~ round(., 0)))

# #import precipitation data downloaded from the channel (gauges) 11 and 38 city of Calgary website: https://data.calgary.ca/Environment/Historical-Rainfall/d9kv-swk3/about_data
# precip_data <- read.csv("Files/Historical_Rainfall_20240528.csv", header = TRUE)
# precip_data$TIMESTAMP <- as.POSIXct(precip_data$TIMESTAMP, format = "%Y-%m-%d %H:%M")
# 
# #create individual data frames for each gauges 11 (west dover) and 38 (water center) 
# west_dover <-subset(precip_data, CHANNEL ==11)
# west_dover$TIMESTAMP <- as.POSIXct(west_dover$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
# west_dover <- west_dover %>%
#   mutate(TIMESTAMP = ymd_hms(TIMESTAMP), 
#          HourlyTime = floor_date(TIMESTAMP, "hour"))
# # Aggregate the rainfall data to hourly intervals, taking the largest value for each hour
# west_dover_hourly <- west_dover %>%
#   group_by(HourlyTime) %>%
#   summarize(RAINFALL = max(RAINFALL, na.rm = TRUE)) %>%
#   ungroup()
# west_dover_hourly <- west_dover_hourly %>%
#   rename(TIMESTAMP = HourlyTime)
# 
# 
# water_center<-subset(precip_data, CHANNEL ==38)
# water_center$TIMESTAMP <- as.POSIXct(water_center$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
# water_center <- water_center %>%
#   mutate(TIMESTAMP = ymd_hms(TIMESTAMP), 
#          HourlyTime = floor_date(TIMESTAMP, "hour"))
# # Aggregate the rainfall data to hourly intervals, taking the largest value for each hour
# water_center_hourly <- water_center %>%
#   group_by(HourlyTime) %>%
#   summarize(RAINFALL = max(RAINFALL, na.rm = TRUE)) %>%
#   ungroup()
# water_center_hourly <- water_center_hourly %>%
#   rename(TIMESTAMP = HourlyTime)

#PLOTTING
# 
# p1 <- ggplot(water_center_hourly, aes(x = TIMESTAMP, y = RAINFALL)) +
#   geom_line() +
#   labs(title = "Rainfall at Gauge Water Center", x = "Date", y = "Rainfall (mm)")+
#   coord_cartesian(ylim = c(0, 20))   # Set y-axis limits
# 
# # Plot for Water Center
# p2 <- ggplot(west_dover_hourly, aes(x = TIMESTAMP, y = RAINFALL)) +
#   geom_line() +
#   labs(title = "Rainfall at Gauge West Dover", x = "Date", y = "Rainfall (mm)")+
#   coord_cartesian(ylim = c(0, 20))   # Set y-axis limits
# 
# # Find the start and end dates
# precip_start_date <- min(water_center_hourly$TIMESTAMP, na.rm = TRUE)
# precip_end_date <- max(water_center_hourly$TIMESTAMP, na.rm = TRUE)
# # Subset the dataframe to include only rows between start_date and end_date
# 
# p_yyc <- ggplot(yyc, aes(x = time, y = precip_amt)) +
#   geom_line() +
#   labs(title = "Rainfall at YYC INT A", x = "Date", y = "precip (mm)")+
#   coord_cartesian(ylim = c(0, 20))   # Set y-axis limits
# 
# yyc_subset <- yyc %>%
#   filter(time >= precip_start_date & time <= precip_end_date)

# #plot rainfall with each other to compare the results. 
# p0 <- ggplot(yyc_subset, aes(x = time, y = precip_amt)) +
#   geom_line() +
#   labs(title = "Rainfall at YYC INT A", x = "Date", y = "precip (mm)")+
#   coord_cartesian(ylim = c(0, 20))   # Set y-axis limits
# 
# grid.arrange(p1, p2, p0,ncol=1)
# 
# combined_plot <- ggplot() +
#   geom_line(data = yyc_subset, aes(x = time, y = precip_amt, color = "INT'l Airport"), alpha = 0.25) +
#   geom_line(data = water_center_hourly, aes(x = TIMESTAMP, y = RAINFALL, color = "Water Center")) +
#   geom_line(data = west_dover_hourly, aes(x = TIMESTAMP, y = RAINFALL, color = "West Dover")) +
#   labs(x = "Year", y = "Precipitation (mm)", title = "Combined Plot of Water Gauges") +
#   scale_color_manual(name = "Series", values = c("Water Center" = "blue", "West Dover" = "red", "INT'l Airport" = "purple")) +
#   theme_minimal() +
#   theme(legend.position = "bottom")+
#   coord_cartesian(ylim = c(0, 20))   # Set y-axis limits
# 
# 
# # Print the combined plot
# print(combined_plot)







# p3 <- ggplot() +
#   # Plot for SM1 soil moisture at Depth 15
#   geom_line(data = SM1, aes(x = Date_SM1, y = SM1_Depth15, color = "SM1_Depth15")) +
#   # Plot for SM1 soil moisture at Depth 30
#   geom_line(data = SM1, aes(x = Date_SM1, y = SM1_Depth30, color = "SM1_Depth30"))+
#   # Plot for SM1 soil moisture at Depth 45
#   geom_line(data = SM1, aes(x = Date_SM1, y = SM1_Depth45, color = "SM1_Depth45")) +
#   # Add axis labels and title
#   labs(x = "Date", y = "Volumetric Soil Moisture (%)", color = "Legend") +
#   ggtitle("Soil Moisture for SM1") +
#   # Set colors manually
#   scale_color_manual(values = c("SM1_Depth15" = "red", "SM1_Depth30" = "green", "SM1_Depth45" = "blue")) +
#   theme_minimal()
# 
# # Find the minimum and maximum timestamps from both datasets
# min_timestamp <- min(c(min(water_center$TIMESTAMP), min(SM1$Date_SM1)))
# max_timestamp <- max(c(max(water_center$TIMESTAMP), max(SM1$Date_SM1)))
# 
# # Set the limits for the X-axis in both plots
# p2 <- p2 + xlim(min_timestamp, max_timestamp)
# p3 <- p3 + xlim(min_timestamp, max_timestamp)
# 
# # Arrange plots side by side
# grid.arrange(p2, p3, ncol = 1)

# # Create the plot
# p4 <- ggplot() +
# 
#   # Plot for SM1 soil moisture
#   geom_line(data = SM1, aes(x = Date, y = SM1_Depth15, color = "SM1 Soil Moisture"), size = 1) +
#   # Add axis labels and title
#   labs(x = "Date", y = "Volumetric Soil Moisture (%)", color = "Legend") +
#   ggtitle("Soil Moisture for SM1") +
#   # Set colors manually
#   scale_color_manual(values = c("West Dover Rainfall" = "blue", "SM1 Soil Moisture" = "red")) +
#   # Set x-axis limits and breaks
#   scale_x_datetime(limits = as.POSIXct(c("2021-09-10", "2021-09-11")), date_labels = "%H",
#                    date_breaks = "1 hour") +
#   # Set primary and secondary y-axes
#   scale_y_continuous(name = "SM1 Soil Moisture", 
#                      sec.axis = sec_axis(~ ., name = "West Dover Rainfall", 
#                                          breaks = seq(0, 10, by = 1), labels = seq(0, 10, by = 1))) +
#   # Plot for West Dover rainfall
#   geom_line(data = west_dover, aes(x = TIMESTAMP, y = RAINFALL, color = "West Dover Rainfall"), size = 1) +
#   theme_minimal()

