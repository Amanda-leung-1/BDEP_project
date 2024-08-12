library(purrr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(dygraphs)
library(htmltools)
library(xts)
library(RColorBrewer)

# Function to calculate means and normalize data
normalize_and_store_means <- function(df) {
  means <- df %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  normalized_df <- df %>%
    mutate(across(where(is.numeric), ~ .x / means[[cur_column()]]))
  
  list(means = means, normalized_df = normalized_df)
}

# List of data frames
dfs <- list(SM1, SM2, SM3, SM4, SM5, SM6)

# Apply the function to each dataframe
results <- map(dfs, normalize_and_store_means)

# Extract means and normalized dataframes
means_list <- map(results, "means")
normalized_dfs <- map(results, "normalized_df")

normalize_mean <- function(sensor_data, precipitation_data, sensor, start_date, end_date, means_list, number) {
  title_string <- sprintf("Soil moisture for %s from %s to %s", sensor, start_date, end_date)
  
  # Clean NA values from the sensor data and assign to a new variable
  sensor_cleaned <- na.omit(sensor_data)
  
  # Convert to xts object for plotting
  sensor_xts <- xts(
    x = sensor_cleaned[, paste0(sensor, "_Depth", c(15, 30, 45, 60, 75, 90, 105, 120))],
    order.by = sensor_cleaned[, paste0("Date_", sensor)]
  )
  
  # Convert to xts object for precipitation
  Precipitation_xts <- xts(
    x = precipitation_data[, "precip_amt"],
    order.by = precipitation_data$time
  )
  
  # Subset Precipitation_xts and sensor_xts to the common date range
  Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date, ]
  sensor_xts_subset <- sensor_xts[index(sensor_xts) >= start_date & index(sensor_xts) <= end_date, ]
  
  # Define colors for the series
  colors <- brewer.pal(9, "Set1")[c(1, 2, 3, 4, 5, 7, 8, 9)]
  
  # Get the limits for the current sensor from means_list
  sensor_index <- as.numeric(number)
  limits <- means_list[[sensor_index]]
  
  # Assuming the limits are stored in the same columns across all dataframes
  limit_values <- limits[c(2:9)]  # Adjust the columns as needed
  
  # Create dygraph objects
  plot_precip <- dygraph(Precipitation_xts_subset, group=sensor, main="Precipitation", height=200) %>%
    dySeries("precip_amt", label = "Precipitation (mm)") %>%
    dyOptions(drawAxes = FALSE) %>%
    dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0))
  
  plot_sensor <- dygraph(sensor_xts_subset, group=sensor, main=title_string) %>%
    dySeries(paste0(sensor, "_Depth15"), label = "Depth 15") %>%
    dySeries(paste0(sensor, "_Depth30"), label = "Depth 30") %>%
    dySeries(paste0(sensor, "_Depth45"), label = "Depth 45") %>%
    dySeries(paste0(sensor, "_Depth60"), label = "Depth 60") %>%
    dySeries(paste0(sensor, "_Depth75"), label = "Depth 75") %>%
    dySeries(paste0(sensor, "_Depth90"), label = "Depth 90") %>%
    dySeries(paste0(sensor, "_Depth105"), label = "Depth 105") %>%
    dySeries(paste0(sensor, "_Depth120"), label = "Depth 120") %>%
    dyOptions(drawGrid = FALSE, stackedGraph = FALSE, colors = colors, digitsAfterDecimal = 2, axisLabelFontSize = 14) %>%
    dyAxis("y", label = "Soil Moisture Index", valueRange = c(0, 20)) %>%
    dyLegend(show = "always") %>%
    dyRangeSelector() %>%
    dyHide() %>%
    dyLimit(limit_values[1], color = "red", strokePattern = "dashed") %>%
    dyLimit(limit_values[2], color = "blue", strokePattern = "dashed") %>%
    dyLimit(limit_values[3], color = "green", strokePattern = "dashed") %>%
    dyLimit(limit_values[4], color = "purple", strokePattern = "dashed") %>%
    dyLimit(limit_values[5], color = "orange", strokePattern = "dashed") %>%
    dyLimit(limit_values[6], color = "brown", strokePattern = "dashed") %>%
    dyLimit(limit_values[7], color = "pink", strokePattern = "dashed") %>%
    dyLimit(limit_values[8], color = "grey", strokePattern = "dashed")
  
  # Render the dygraphs objects using htmltools
  htmltools::browsable(htmltools::tagList(plot_precip, plot_sensor))
}

normalize_mean(SM3, yyc, "SM3", as.POSIXct('2020-01-01'), as.POSIXct('2023-01-01'), means_list, "3")












# Create a base dataframe with the combined dates
# Define start and end dates
start_date <- ymd_hms("2020-01-01 00:00:00")
end_date <- ymd_hms("2023-01-01 00:00:00")
# Generate sequence of dates by hour
dates <- seq(from = start_date, to = end_date, by = "hour")
# Create a base dataframe with the generated dates
depth_30_normal <- data.frame(Date_depth_30 = dates, stringsAsFactors = FALSE)

# Process each dataframe and combine results
for (i in seq_along(dfs)) {
  col_date <- paste0("Date_SM", i)
  col_depth <- paste0("SM", i, "_Depth30")
  col_name <- paste0("SM", i, "_depth_30")
  
  processed_df <- dfs[[i]] %>%
    select(Date = all_of(col_date), Depth = all_of(col_depth)) %>%
    mutate(Date = round_date(Date, unit = "hour"))
  
  depth_30_normal <- depth_30_normal %>%
    left_join(processed_df, by = c("Date_depth_30" = "Date")) %>%
    rename(!!col_name := Depth)
}


# Convert to xts object for plotting
depth_30_normal_xts <- xts(
  x = depth_30_normal[, c("SM1_depth_30", "SM2_depth_30", "SM3_depth_30", "SM4_depth_30", "SM5_depth_30", "SM6_depth_30")],
  order.by = depth_30_normal$Date_depth_30
)

# Create a list of dygraphs objects
plotobj_30_normal <- list(
  dygraphs::dygraph(Precipitation_xts, group="Depth_30_normal", main="Deviation from the mean depth = 30cm", height = 200 ) %>%
    dySeries("precip_amt", label = "Precipitation (mm)") %>%
    dyOptions(
      drawAxes = FALSE,  # Hide both x and y axes
      axisLineColor = "transparent"  # Hide axis line for cleaner appearance
    ) %>%
    dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0)),  # Adjust y-axis options as needed
  dygraphs::dygraph(depth_30_normal_xts, group="Depth_30_normal", main="Deviation from the mean depth = 30cm") %>%
    dyLimit(0.213, color = "red", strokePattern = "dashed") %>%
    dyLimit(5.07, color = "blue", strokePattern = "dashed") %>%
    dyLimit(21.3, color = "green", strokePattern = "dashed") %>%
    dyLimit(8.34, color = "purple", strokePattern = "dashed") %>%
    dyLimit(0.1, color = "orange", strokePattern = "dashed") %>%
    dyLimit(0.136, color = "brown", strokePattern = "dashed") %>%
    
    dyOptions(
      drawGrid = FALSE,
      colors = RColorBrewer::brewer.pal(7, "Set1")[c(1, 2, 3, 4, 5, 7)]
    ) %>%
    dyAxis("y", label = "Soil Moisture Index") %>%
    dyLegend(show = "always") %>%
    dyRangeSelector() %>%
    dyOptions(digitsAfterDecimal = 2, axisLabelFontSize = 14)
)  # end list
# Render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(plotobj_30_normal))


# Convert dataframe directly to xts with specified columns
SM1_xts_normal <- xts(
  x =normalized_dfs[[1]][, c("SM1_Depth15", "SM1_Depth30", "SM1_Depth45", "SM1_Depth60", "SM1_Depth75", "SM1_Depth90", "SM1_Depth105", "SM1_Depth120")],
  order.by = normalized_dfs[[1]]$Date_SM1
)
# Create a list of dygraphs objects
plotobj_SM1_normal <- list(
  dygraphs::dygraph(SM1_xts_normal, group="SM4", main="Deviation from the mean SM1")%>%
    dygraphs::dySeries("SM1_Depth15", label = "Depth 15") %>%
    dygraphs:: dySeries("SM1_Depth30", label = "Depth 30") %>%
    dygraphs::dySeries("SM1_Depth45", label = "Depth 45") %>%
    dygraphs:: dySeries("SM1_Depth60", label = "Depth 60") %>%
    dygraphs:: dySeries("SM1_Depth75", label = "Depth 75") %>%
    dygraphs::dySeries("SM1_Depth90", label = "Depth 90") %>%
    dygraphs::dySeries("SM1_Depth105", label = "Depth 105") %>%
    dygraphs::dySeries("SM1_Depth120", label = "Depth 120") %>%dygraphs::dyOptions(drawGrid = FALSE, stackedGraph = FALSE, colors = RColorBrewer::brewer.pal(9, "Set1")[c(1, 2, 3, 4, 5, 7,8,9)])%>%
    dygraphs::dyAxis("y", label = "Soil Moisture Index")%>%
    dygraphs::dyLegend(show = "always") %>%
    dygraphs::dyRangeSelector() %>%
    dyHide() %>%
    dygraphs::dyOptions(digitsAfterDecimal = 2, axisLabelFontSize = 14)%>%
  dyLimit(0.907, color = "red", strokePattern = "dashed") %>%
    dyLimit(0.213, color = "blue", strokePattern = "dashed") %>%
    dyLimit(0.1, color = "green", strokePattern = "dashed") %>%
    dyLimit(0.529, color = "purple", strokePattern = "dashed") %>%
    dyLimit(1.56, color = "orange", strokePattern = "dashed") %>%
    dyLimit(0.154, color = "brown", strokePattern = "dashed") %>%
    dyLimit(0.0229, color = "pink", strokePattern = "dashed") %>%
    dyLimit(0.0451, color = "grey", strokePattern = "dashed") 
)  # end list

# Render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(plotobj_SM1_normal))

