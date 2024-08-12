library(dygraphs)
library(xts)
library(htmltools)

start_date <- as.POSIXct('2021-01-01')
end_date <- as.POSIXct('2023-01-01')
sensor <- "SM6"
title_string <- sprintf("Soil moisture for %s from %s to %s", sensor, start_date, end_date)

# Clean NA values from SM6 and assign to a new variable
SM6_cleaned <- na.omit(SM6)

# Convert to xts object for plotting
SM6_xts <- xts(
  x = SM6_cleaned[, c("SM6_Depth15", "SM6_Depth30", "SM6_Depth45", "SM6_Depth60", "SM6_Depth75", "SM6_Depth90")],
  order.by = SM6_cleaned$Date_SM6
)

# Convert to xts object for precipitation
Precipitation_xts <- xts(
  x = yyc[, "precip_amt"],
  order.by = yyc$time
)

# Subset Precipitation_xts and SM6_xts to the common date range
Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date,]
SM6_xts_subset <- SM6_xts[index(SM6_xts) >= start_date & index(SM6_xts) <= end_date,]

# Create a list of dygraphs objects
plotobj_SM6 <- list(
  dygraphs::dygraph(Precipitation_xts_subset, group="SM6", main="Soil Moisture Sensor SM6", height = 200 )%>%
    dySeries("precip_amt", label = "Precipitation (mm)") %>%
    dyOptions(
      drawAxes = FALSE ) %>%  # Hide both x and y axes
    dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0)),  # Adjust y-axis options as needed
  dygraphs::dygraph(SM6_xts_subset, group="SM6", main=NULL)%>%
    dygraphs::dySeries("SM6_Depth15", label = "Depth 15") %>%
    dygraphs:: dySeries("SM6_Depth30", label = "Depth 30") %>%
    dygraphs::dySeries("SM6_Depth45", label = "Depth 45") %>%
    dygraphs:: dySeries("SM6_Depth60", label = "Depth 60") %>%
    dygraphs:: dySeries("SM6_Depth75", label = "Depth 75") %>%
    dygraphs::dySeries("SM6_Depth90", label = "Depth 90") %>%
    dygraphs::dyOptions(drawGrid = FALSE, stackedGraph = FALSE, colors = RColorBrewer::brewer.pal(7, "Set1")[c(1, 2, 3, 4, 5, 7)])%>%
    dygraphs::dyAxis("y", label = "Soil Moisture Index")%>%
    dygraphs::dyLegend(show = "always") %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyOptions(digitsAfterDecimal = 2, axisLabelFontSize = 14)
)  # end list

# Render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(plotobj_SM6))
