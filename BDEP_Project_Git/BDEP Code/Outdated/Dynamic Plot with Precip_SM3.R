library(dygraphs)
library(xts)
library(htmltools)

start_date <- as.POSIXct('2020-10-15')
end_date <- as.POSIXct('2022-01-22')
sensor <- "SM3"
title_string <- sprintf("Soil moisture for %s from %s to %s", sensor, start_date, end_date)

# Clean NA values from SM3 and assign to a new variable
SM3_cleaned <- na.omit(SM3)

# Convert to xts object for plotting
SM3_xts <- xts(
  x = SM3_cleaned[, c("SM3_Depth15", "SM3_Depth30", "SM3_Depth45", "SM3_Depth60", "SM3_Depth75", "SM3_Depth90", "SM3_Depth105", "SM3_Depth120")],
  order.by = SM3_cleaned$Date_SM3
)

# Convert to xts object for precipitation
Precipitation_xts <- xts(
  x = water_center_hourly[, "RAINFALL"],
  order.by = water_center_hourly$TIMESTAMP
)

# Subset Precipitation_xts and SM3_xts to the common date range
Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date,]
SM3_xts_subset <- SM3_xts[index(SM3_xts) >= start_date & index(SM3_xts) <= end_date,]

# Create the dygraph for soil moisture depths
dygraph_soil_moisture <- dygraph(SM3_xts_subset, xlab = "date") %>%
  dySeries("SM3_Depth15", label = "Depth 15") %>%
  dySeries("SM3_Depth30", label = "Depth 30") %>%
  dySeries("SM3_Depth45", label = "Depth 45") %>%
  dySeries("SM3_Depth60", label = "Depth 60") %>%
  dySeries("SM3_Depth75", label = "Depth 75") %>%
  dySeries("SM3_Depth90", label = "Depth 90") %>%
  dySeries("SM3_Depth105", label = "Depth 105") %>%
  dySeries("SM3_Depth120", label = "Depth 120") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyAxis("y", label = "Volumetric Soil Moisture Content (%)")  # Adjust y-axis options as needed
dyRangeSelector()

# Create the dygraph for precipitation with hidden x-axis
dygraph_precipitation <- dygraph(Precipitation_xts_subset, height = 200, main = title_string) %>%
  dySeries("RAINFALL", label = "Precipitation (mm)") %>%
  dyOptions(
    drawAxes = FALSE ) %>%  # Hide both x and y axes
  dyAxis("y", label = "Precipitation (mm)", valueRange = c(2, 0))  # Adjust y-axis options as needed


# Combine the two dygraphs vertically
combined_dygraphs <- tagList(dygraph_precipitation, dygraph_soil_moisture)

# Display the combined dygraphs
browsable(combined_dygraphs)


