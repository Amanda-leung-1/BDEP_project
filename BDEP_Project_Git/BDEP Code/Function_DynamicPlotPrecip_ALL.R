library(dygraphs)
library(xts)
library(htmltools)


highlight_periods <- list(
  list(from = as.Date("2020-04-01"), to = as.Date("2020-10-31")),
  list(from = as.Date("2021-04-01"), to = as.Date("2021-10-31")),
  list(from = as.Date("2022-04-01"), to = as.Date("2022-10-31")),
  list(from = as.Date("2023-04-01"), to = as.Date("2023-10-31")),
  list(from = as.Date("2024-04-01"), to = as.Date("2024-10-31"))
)

#function to plot the HOURLY timestep for soil moisture 
plot_soil_moisture <- function(sensor_data, precipitation_data, sensor, start_date, end_date, label) {
  title_string <- sprintf("Soil moisture for %s", sensor)
  depth_label <- label[[sensor]]
  # Clean NA values from the data and assign to a new variable
  sensor_cleaned <- na.omit(sensor_data)
  # Convert data to an xts object for plotting with dygraph
  sensor_xts <- xts(
    x = sensor_cleaned[, paste0(sensor, "_Depth", c(15, 30, 45, 60, 75, 90, 105, 120))],
    order.by = sensor_cleaned$Date
  )
  # Convert percipitation to an xts object for plotting with dygraph 
  Precipitation_xts <- xts(
    x = precipitation_data[, "precip_amt"],
    order.by = precipitation_data$time
  )
  # Create a list of dygraphs objects
  plotobj <- list(
    #dygraph for percipitation with the y-axis label, reversed y-axis, and set height 
    dygraphs::dygraph(Precipitation_xts, group=sensor, main="Precipitation (mm)", height=200) %>%
      dySeries("precip_amt", label = "Precipitation (mm)") %>%
      dyAxis("y", label = "Precipitation (mm)", valueRange = c(20, 0)),
    #dygraph for the soil moisture data at the corrected depth 
    dygraphs::dygraph(sensor_xts, group=sensor, main = title_string) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth15"), label = paste0("Depth_",depth_label[1])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth30"), label = paste0("Depth_",depth_label[2])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth45"), label = paste0("Depth_",depth_label[3])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth60"), label = paste0("Depth_",depth_label[4])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth75"), label = paste0("Depth_",depth_label[5])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth90"), label = paste0("Depth_",depth_label[6])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth105"), label = paste0("Depth_",depth_label[7])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth120"), label = paste0("Depth_",depth_label[8])) %>%
      dygraphs::dyOptions(drawGrid = TRUE, stackedGraph = FALSE, 
                          digitsAfterDecimal = 2, axisLabelFontSize = 14,
                          colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
      dygraphs::dyAxis("y", label = "Soil Moisture Index", valueRange=c(0,NULL)) %>%
      dygraphs::dyLegend(show = "always") %>%
      #add shading for the irrigation period as listed under list highlighted_periods
      dygraphs:: dyShading(from = highlight_periods[[1]]$from, to = highlight_periods[[1]]$to, color = "lightgray") %>%
      dygraphs:: dyShading(from = highlight_periods[[2]]$from, to = highlight_periods[[2]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[3]]$from, to = highlight_periods[[3]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[4]]$from, to = highlight_periods[[4]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[5]]$from, to = highlight_periods[[5]]$to, color = "lightgray")%>%
      dygraphs::dyRangeSelector()%>%  # Set initial date window using JavaScript Date format
      dyHide()
  )  # end list
  
  # Render the dygraphs objects using htmltools
  htmltools::browsable(htmltools::tagList(plotobj))
}

#call the plot_soil_moisture function for the 4 soil moisture sensors 
plot_soil_moisture(SM1, yyc, "SM1", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)
plot_soil_moisture(SM2, yyc, "SM2", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)
plot_soil_moisture(SM3, yyc, "SM3", as.POSIXct('2020-01-01'), as.POSIXct('2025-01-01'), true_depth)
plot_soil_moisture(SM4, yyc, "SM4", as.POSIXct('2020-01-01'), as.POSIXct('2025-01-01'), true_depth)



plot_soil_moisture_weekly_SM1to4 <- function(sensor_data, precipitation_data, sensor, start_date, end_date, label) {
  title_string <- sprintf("Soil moisture for %s", sensor)
  # Clean NA values from the sensor data and assign to a new variable
  sensor_cleaned <- na.omit(sensor_data)
  sensor<-sensor
  # Convert sensor data to an xts object for plotting with dygraph
  sensor_xts <- xts(
    x = sensor_cleaned[, paste0(sensor, "_Depth", c(15, 30, 45, 60, 75, 90, 105, 120))],
    order.by = sensor_cleaned$Date
  )
  # Convert percipitation to an xts object for plotting with dygraph 
  Precipitation_xts <- xts(
    x = precipitation_data[, "precip_amt_mean"],
    order.by = precipitation_data$week_start
  )
  # Subset Precipitation_xts and sensor_xts to the common date range
  Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date, ]
  sensor_xts_subset <- sensor_xts[index(sensor_xts) >= start_date & index(sensor_xts) <= end_date, ]

  depth_label <- label[[sensor]]
  # Create a list of dygraphs objects
  plotobj <- list(
    htmltools::tags$div(style = "margin-left: 10px;",  # Adjust the value as needed
    dygraphs::dygraph(Precipitation_xts_subset, group=sensor, main=title_string, height=200) %>%
      dySeries("precip_amt_mean", fillGraph = TRUE, label = "Precipitation (mm)") %>%
      dyAxis("y", label = "Precipitation (mm)", valueRange = c(50, 0), pixelsPerLabel = 40)  # Adjust y-axis options as needed
    ),
    dygraphs::dygraph(sensor_xts_subset, group=sensor) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth15"), label = paste0("Depth_",depth_label[1])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth30"), label = paste0("Depth_",depth_label[2])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth45"), label = paste0("Depth_",depth_label[3])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth60"), label = paste0("Depth_",depth_label[4])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth75"), label = paste0("Depth_",depth_label[5])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth90"), label = paste0("Depth_",depth_label[6])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth105"), label = paste0("Depth_",depth_label[7])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth120"), label = paste0("Depth_",depth_label[8])) %>%
      dygraphs::dyOptions(drawGrid = TRUE, stackedGraph = FALSE, 
                          digitsAfterDecimal = 2, axisLabelFontSize = 14,
                          pointSize = 2, drawPoints = TRUE,
                          colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
      dygraphs::dyAxis("y", label = "Soil Moisture Index", pixelsPerLabel = 40) %>%
      dygraphs::dyLegend(show = "always") %>%
      dygraphs:: dyShading(from = highlight_periods[[1]]$from, to = highlight_periods[[1]]$to, color = "lightgray") %>%
      dygraphs:: dyShading(from = highlight_periods[[2]]$from, to = highlight_periods[[2]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[3]]$from, to = highlight_periods[[3]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[4]]$from, to = highlight_periods[[4]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[5]]$from, to = highlight_periods[[5]]$to, color = "lightgray")%>%
      dygraphs::dyRangeSelector(dateWindow = c(start_date, end_date))%>%  # Set initial date window using JavaScript Date format
      dyHide()
  )  # end list
  
  # Render the dygraphs objects using htmltools
  htmltools::browsable(htmltools::tagList(plotobj))
}
plot_soil_moisture_weekly_SM5to6 <- function(sensor_data, precipitation_data, sensor, start_date, end_date, label) {
  title_string <- sprintf("Soil moisture for %s", sensor)
  # Clean NA values from the sensor data and assign to a new variable
  sensor_cleaned <- na.omit(sensor_data)
  sensor<-sensor
  # Convert sensor data to an xts object for plotting with dygraph
  sensor_xts <- xts(
    x = sensor_cleaned[, paste0(sensor, "_Depth", c(15, 30, 45, 60, 75, 90))],
    order.by = sensor_cleaned$Date
  )
  # Convert percipitation to an xts object for plotting with dygraph 
  Precipitation_xts <- xts(
    x = precipitation_data[, "precip_amt_mean"],
    order.by = precipitation_data$week_start
  )
  # Subset Precipitation_xts and sensor_xts to the common date range
  Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date, ]
  sensor_xts_subset <- sensor_xts[index(sensor_xts) >= start_date & index(sensor_xts) <= end_date, ]
  
  depth_label <- label[[sensor]]
  # Create a list of dygraphs objects
  plotobj <- list(
    htmltools::tags$div(style = "margin-left: 10px;",  # Adjust the value as needed
                        dygraphs::dygraph(Precipitation_xts_subset, group=sensor, main=title_string, height=200) %>%
                          dySeries("precip_amt_mean", fillGraph = TRUE, label = "Precipitation (mm)") %>%
                          dyAxis("y", label = "Precipitation (mm)", valueRange = c(50, 0), pixelsPerLabel = 40)  # Adjust y-axis options as needed
    ),
    dygraphs::dygraph(sensor_xts_subset, group=sensor) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth15"), label = paste0("Depth_",depth_label[1])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth30"), label = paste0("Depth_",depth_label[2])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth45"), label = paste0("Depth_",depth_label[3])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth60"), label = paste0("Depth_",depth_label[4])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth75"), label = paste0("Depth_",depth_label[5])) %>%
      dygraphs::dySeries(paste0(sensor, "_Depth90"), label = paste0("Depth_",depth_label[6])) %>%
      dygraphs::dyOptions(drawGrid = TRUE, stackedGraph = FALSE, 
                          digitsAfterDecimal = 2, axisLabelFontSize = 14,
                          pointSize = 2, drawPoints = TRUE,
                          colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
      dygraphs::dyAxis("y", label = "Soil Moisture Index", pixelsPerLabel = 40) %>%
      dygraphs::dyLegend(show = "always") %>%
      dygraphs:: dyShading(from = highlight_periods[[1]]$from, to = highlight_periods[[1]]$to, color = "lightgray") %>%
      dygraphs:: dyShading(from = highlight_periods[[2]]$from, to = highlight_periods[[2]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[3]]$from, to = highlight_periods[[3]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[4]]$from, to = highlight_periods[[4]]$to, color = "lightgray")%>%
      dygraphs:: dyShading(from = highlight_periods[[5]]$from, to = highlight_periods[[5]]$to, color = "lightgray")%>%
      dygraphs::dyRangeSelector(dateWindow = c(start_date, end_date))%>%  # Set initial date window using JavaScript Date format
      dyHide()
  )  # end list
  
  # Render the dygraphs objects using htmltools
  htmltools::browsable(htmltools::tagList(plotobj))
}


plot_soil_moisture_weekly_SM1to4(SM1_weekly, yyc_weekly, "SM1", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)
plot_soil_moisture_weekly_SM1to4(SM2_weekly, yyc_weekly, "SM2", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)
plot_soil_moisture_weekly_SM1to4(SM3_weekly, yyc_weekly, "SM3", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)
plot_soil_moisture_weekly_SM1to4(SM4_weekly, yyc_weekly, "SM4", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)

plot_soil_moisture_weekly_SM5to6(SM5_weekly, yyc_weekly, "SM5", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)
plot_soil_moisture_weekly_SM5to6(SM6_weekly, yyc_weekly, "SM6", as.POSIXct('2020-07-01'), as.POSIXct('2024-08-01'), true_depth)
