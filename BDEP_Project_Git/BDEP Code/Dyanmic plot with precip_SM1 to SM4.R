library(dygraphs)
library(xts)
library(htmltools)

start_date <- as.POSIXct('2020-01-01')
end_date <- as.POSIXct('2025-01-01')
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

# Create a list of dygraphs objects
plotobj_SM2 <- list(
  dygraphs::dygraph(Precipitation_xts_subset, group="SM2", main="Soil Moisture Sensor SM2", height = 200 )%>%
    dySeries("precip_amt", label = "Precipitation (mm)") %>%
    dyOptions(
      drawAxes = FALSE ) %>%  # Hide both x and y axes
    dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0)),  # Adjust y-axis options as needed
   dygraphs::dygraph(SM2_xts_subset, group="SM2", main=NULL)%>%
    dygraphs::dySeries("SM2_Depth15", label = "Depth 15") %>%
    dygraphs:: dySeries("SM2_Depth30", label = "Depth 30") %>%
    dygraphs::dySeries("SM2_Depth45", label = "Depth 45") %>%
    dygraphs:: dySeries("SM2_Depth60", label = "Depth 60") %>%
    dygraphs:: dySeries("SM2_Depth75", label = "Depth 75") %>%
    dygraphs::dySeries("SM2_Depth90", label = "Depth 90") %>%
    dygraphs::dySeries("SM2_Depth105", label = "Depth 105") %>%
    dygraphs::dySeries("SM2_Depth120", label = "Depth 120") %>%
    dygraphs::dyOptions(drawGrid = FALSE, stackedGraph = FALSE, colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
    dygraphs::dyAxis("y", label = "Soil Moisture Index")%>%
    dygraphs::dyLegend(show = "always") %>%
    dygraphs::dyRangeSelector() %>%
    dyHide() %>%
    dygraphs::dyOptions(digitsAfterDecimal = 2, axisLabelFontSize = 13)
)  # end list

# Render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(plotobj_SM2))
