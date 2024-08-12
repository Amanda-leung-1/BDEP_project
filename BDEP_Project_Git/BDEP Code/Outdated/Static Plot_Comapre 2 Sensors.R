library(ggplot2)
library(gridExtra)

# Define custom color palette for depths
depth_colors <- c("Depth 15" = "red", "Depth 30" = "blue", "Depth 45" = "green",
                  "Depth 60" = "orange", "Depth 75" = "purple", "Depth 90" = "pink",
                  "Depth 105" = "brown", "Depth 120" = "black")

# Filter data based on a start and end time
start_date <- as.POSIXct('2020-10-15')
end_date <- as.POSIXct('2022-06-28')
filtered_data <- subset(soil_data, Date_SM3 >= start_date & Date_SM3 <= end_date)

# Get y-axis limits for both plots
depth_columns <- c("SM3_Depth15", "SM3_Depth30", "SM3_Depth45", "SM3_Depth60",
                   "SM3_Depth75", "SM3_Depth90", "SM3_Depth105", "SM3_Depth120",
                   "SM4_Depth15", "SM4_Depth30", "SM4_Depth45", "SM4_Depth60",
                   "SM4_Depth75", "SM4_Depth90", "SM4_Depth105", "SM4_Depth120")
y_limits <- range(unlist(filtered_data[depth_columns]), na.rm = TRUE)

y_breaks <- seq(0, ceiling(y_limits[2]), by = 5)

# Plotting SM3 Soil Moisture vs. Date
plot_SM3 <- ggplot(filtered_data, aes(x = Date_SM3)) +
  geom_line(aes(y = SM3_Depth15, color = "Depth 15")) +
  geom_line(aes(y = SM3_Depth30, color = "Depth 30")) +
  geom_line(aes(y = SM3_Depth45, color = "Depth 45")) +
  geom_line(aes(y = SM3_Depth60, color = "Depth 60")) +
  geom_line(aes(y = SM3_Depth75, color = "Depth 75")) +
  geom_line(aes(y = SM3_Depth90, color = "Depth 90")) +
  geom_line(aes(y = SM3_Depth105, color = "Depth 105")) +
  geom_line(aes(y = SM3_Depth120, color = "Depth 120")) +
  labs(x = NULL, y = "Volumetric Soil Moisture (%)", color = NULL) +  # Removed x-axis title and legend
  scale_color_manual(values = depth_colors) +  # Apply custom color palette
  ggtitle("SM3") +
  coord_cartesian(ylim = y_limits) +  # Set y-axis limits
  scale_y_continuous(breaks = y_breaks, labels = y_breaks)+   
  scale_x_datetime(date_breaks = "6 months", date_labels = "%Y-%m-%d") +  # Format x-axis to include year
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Plotting SM4 Soil Moisture vs. Date
plot_SM4 <- ggplot(filtered_data, aes(x = Date_SM4)) +
  geom_line(aes(y = SM4_Depth15, color = "Depth 15")) +
  geom_line(aes(y = SM4_Depth30, color = "Depth 30")) +
  geom_line(aes(y = SM4_Depth45, color = "Depth 45")) +
  geom_line(aes(y = SM4_Depth60, color = "Depth 60")) +
  geom_line(aes(y = SM4_Depth75, color = "Depth 75")) +
  geom_line(aes(y = SM4_Depth90, color = "Depth 90")) +
  geom_line(aes(y = SM4_Depth105, color = "Depth 105")) +
  geom_line(aes(y = SM4_Depth120, color = "Depth 120")) +
  labs(x = NULL, y = NULL, color = NULL) +  # Removed x-axis title, y-axis title, and legend
  scale_color_manual(values = depth_colors) +  # Apply the same custom color palette
  ggtitle("SM4") +
  coord_cartesian(ylim = y_limits) +  # Set y-axis limits
  scale_y_continuous(breaks = y_breaks, labels = y_breaks)+   
  scale_x_datetime(date_breaks = "6 months", date_labels = "%Y-%m-%d") +  # Format x-axis to include year
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Create legend table with depths and colors
legend_data <- data.frame(Depth = names(depth_colors), Color = depth_colors)
legend_table <- tableGrob(
  legend_data$Depth,  # Display depth names
  rows = NULL, cols = NULL,
  theme = ttheme_default(
    core = list(
      bg_params = list(fill = legend_data$Color),  # Use colors as background fill
      fg_params = list(col = "white", fontsize = 8)  # Set text color to white and adjust font size
    ),
    colhead = list(
      fg_params = list(fontsize = 10, fontface = "bold", col = "white")  # Header style with white text
    )
  )
)
print(plot_SM4)
# Arrange the plots and add legend_table
grid.arrange(plot_SM3 + theme(axis.title.x = element_blank()),
             plot_SM4 + theme(axis.title.x = element_blank()),
             legend_table,
             ncol = 3,
             heights = c(3, 3, 0.5))  # Adjust widths as needed for the main plots and legend table
