library(dygraphs)
library(xts)
library(htmltools)

start_date <- as.POSIXct('2021-07-28')
end_date <- as.POSIXct('2022-05-25')
sensor <- "SM2"
title_string <- sprintf("Soil moisture for %s from %s to %s", sensor, start_date, end_date)

# Clean NA values from SM2 and assign to a new variable
SM2_cleaned <- na.omit(SM2)

# Convert to xts object for plotting
SM2_xts <- xts(
  x = SM2_cleaned[, c("SM2_Depth15", "SM2_Depth30", "SM2_Depth45", "SM2_Depth60", "SM2_Depth75", "SM2_Depth90", "SM2_Depth105", "SM2_Depth120")],
  order.by = SM2_cleaned$Date_SM2
)

# Convert to xts object for precipitation
Precipitation_xts <- xts(
  x = yyc[, "precip_amt"],
  order.by = yyc$time
)

# Subset Precipitation_xts and SM2_xts to the common date range
Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date,]
SM2_xts_subset <- SM2_xts[index(SM2_xts) >= start_date & index(SM2_xts) <= end_date,]

# Create the dygraph for soil moisture depths
dygraph_soil_moisture <- dygraph(SM2_xts_subset, xlab = "date") %>%
  dySeries("SM2_Depth15", label = "Depth 15") %>%
  dySeries("SM2_Depth30", label = "Depth 30") %>%
  dySeries("SM2_Depth45", label = "Depth 45") %>%
  dySeries("SM2_Depth60", label = "Depth 60") %>%
  dySeries("SM2_Depth75", label = "Depth 75") %>%
  dySeries("SM2_Depth90", label = "Depth 90") %>%
  dySeries("SM2_Depth105", label = "Depth 105") %>%
  dySeries("SM2_Depth120", label = "Depth 120") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyAxis("y", label = "Volumetric Soil Moisture Content (%)")  # Adjust y-axis options as needed
  dyRangeSelector()

# Create the dygraph for precipitation with hidden x-axis
dygraph_precipitation <- dygraph(Precipitation_xts_subset, height = 200, main = title_string) %>%
  dySeries("precip_amt", label = "Precipitation (mm)") %>%
  dyOptions(
    drawAxes = FALSE ) %>%  # Hide both x and y axes
  dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0))  # Adjust y-axis options as needed


# Combine the two dygraphs vertically
combined_dygraphs <- tagList(dygraph_precipitation, dygraph_soil_moisture)

# Display the combined dygraphs
browsable(combined_dygraphs)


