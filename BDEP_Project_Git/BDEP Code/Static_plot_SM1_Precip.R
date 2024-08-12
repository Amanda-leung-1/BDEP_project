library(ggplot2)
library(gridExtra)

# Define custom color palette for depths
depth_colors <- c("Depth 15" = "red", "Depth 30" = "blue", "Depth 45" = "green",
                  "Depth 60" = "orange", "Depth 75" = "purple", "Depth 90" = "pink",
                  "Depth 105" = "brown", "Depth 120" = "black")

# Filter data based on a start and end time
start_date <- as.POSIXct('2021-07-28')
end_date <- as.POSIXct('2022-01-01')
filtered_data <- subset(SM1, Date_SM1 >= start_date & Date_SM1 <= end_date)
filtered_precip <- subset(yyc, time >= start_date & time <= end_date)

# Get y-axis limits for both plots
depth_columns <- c("SM1_Depth15", "SM1_Depth30", "SM1_Depth45", "SM1_Depth60",
                   "SM1_Depth75", "SM1_Depth90", "SM1_Depth105", "SM1_Depth120")

y_limits <- range(unlist(filtered_data[depth_columns]), na.rm = TRUE)

y_breaks <- seq(0, ceiling(y_limits[2]), by = 10)

# Plotting SM1 Soil Moisture vs. Date
p_SM1 <- ggplot(filtered_data, aes(x = Date_SM1)) +
  geom_line(aes(y = SM1_Depth15, color = "Depth 15")) +
  geom_line(aes(y = SM1_Depth30, color = "Depth 30")) +
  geom_line(aes(y = SM1_Depth45, color = "Depth 45")) +
  geom_line(aes(y = SM1_Depth60, color = "Depth 60")) +
  geom_line(aes(y = SM1_Depth75, color = "Depth 75")) +
  geom_line(aes(y = SM1_Depth90, color = "Depth 90")) +
  geom_line(aes(y = SM1_Depth105, color = "Depth 105")) +
  geom_line(aes(y = SM1_Depth120, color = "Depth 120")) +
  labs(x = "Date", y = "Volumetric Soil Moisture (%)", color = NULL) +  # Removed x-axis title and legend
  scale_color_manual(values = depth_colors) +  # Apply custom color palette
  ggtitle("SM1 Depth 15 to 120") +
  coord_cartesian(ylim = y_limits) +  # Set y-axis limits
  scale_y_continuous(breaks = y_breaks, labels = y_breaks)+   
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m-%d") +  # Format x-axis to include year
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

p8 <- ggplot(filtered_precip, aes(x = time)) +
  geom_line(aes(y = precip_amt, colour = "RAINFALL")) +
  labs(x = "Date", y = "Precipitation (mm)", color = NULL) +  # Removed x-axis title, y-axis title, and legend
  scale_y_reverse() +  # Reverse the y-axis
  #scale_x_datetime(date_breaks = "1 month") +  # Format x-axis to include year
  theme_minimal() +
  theme(legend.position = "right")

# Arrange the plots and add legend_table
grid.arrange(p8 + theme(axis.title.x = element_blank()),
             p_SM1 + theme(axis.title.x = element_blank()),
             nrow = 2,
             heights = c(2, 4))  # Adjust widths as needed for the main plots and legend table
