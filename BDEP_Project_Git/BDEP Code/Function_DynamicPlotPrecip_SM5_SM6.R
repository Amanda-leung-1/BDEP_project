
plot_soil_moisture_2 <- function(sensor_data, precipitation_data, sensor, start_date, end_date) {
  title_string <- sprintf("Soil moisture for %s from %s to %s", sensor, start_date, end_date)
  
  # Clean NA values from the sensor data and assign to a new variable
  sensor_cleaned <- na.omit(sensor_data)
  
  # Convert to xts object for plotting
  sensor_xts <- xts(
    x = sensor_cleaned[, paste0(sensor, "_Depth", c(15, 30, 45, 60, 75, 90))],
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
  
  # Create a list of dygraphs objects
  plotobj <- list(
    dygraphs::dygraph(Precipitation_xts_subset, group=sensor, main="Precipitation", height=200) %>%
      dySeries("precip_amt", label = "Precipitation (mm)") %>%
      dyOptions(drawAxes = FALSE) %>%  # Hide both x and y axes
      dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0)),  # Adjust y-axis options as needed
    dygraphs::dygraph(sensor_xts_subset, group=sensor, main=title_string) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth15"), label = "Depth 15") %>%
      dygraphs::dySeries(paste0(sensor, "_Depth30"), label = "Depth 30") %>%
      dygraphs::dySeries(paste0(sensor, "_Depth45"), label = "Depth 45") %>%
      dygraphs::dySeries(paste0(sensor, "_Depth60"), label = "Depth 60") %>%
      dygraphs::dySeries(paste0(sensor, "_Depth75"), label = "Depth 75") %>%
      dygraphs::dySeries(paste0(sensor, "_Depth90"), label = "Depth 90") %>%
      dygraphs::dyOptions(drawGrid = FALSE, stackedGraph = FALSE, colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
      dygraphs::dyAxis("y", label = "Soil Moisture Index") %>%
      dygraphs::dyLegend(show = "always") %>%
      dygraphs::dyRangeSelector() %>%
      dyHide() %>%
      dygraphs::dyOptions(digitsAfterDecimal = 2, axisLabelFontSize = 14)
  )  # end list
  
  # Render the dygraphs objects using htmltools
  htmltools::browsable(htmltools::tagList(plotobj))
}

# Example usage:
# plot_soil_moisture(SM4, yyc, "SM4", as.POSIXct('2020-01-01'), as.POSIXct('2023-01-01'))

plot_soil_moisture_2(SM5, yyc, "SM5", as.POSIXct('2020-01-01'), as.POSIXct('2025-01-01'))
plot_soil_moisture_2(SM6, yyc, "SM6", as.POSIXct('2020-01-01'), as.POSIXct('2025-01-01'))

