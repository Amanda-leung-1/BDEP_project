# Function to divide each numeric column by its mean
divide_by_mean <- function(df) {
  # Calculate means for all numeric columns
  means <- colMeans(df[, sapply(df, is.numeric)], na.rm = TRUE)
  # Divide each numeric column by its mean
  df[, sapply(df, is.numeric)] <- sweep(df[, sapply(df, is.numeric)], 2, means, FUN = "/")
  return(df)
}

# List of dataframes
sensors_all <- list(SM1, SM2, SM3, SM4, SM5, SM6)
# Apply the function to each dataframe
NormalizeMean <- lapply(sensors_all, divide_by_mean)

library(dygraphs)
library(xts)
library(htmltools)

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
  x = yyc[, "precip_amt"],
  order.by = yyc$time
)

# Subset Precipitation_xts and SM3_xts to the common date range
Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date,]
SM3_xts_subset <- SM3_xts[index(SM3_xts) >= start_date & index(SM3_xts) <= end_date,]

# Create a list of dygraphs objects
plotobj_SM3 <- list(
  dygraphs::dygraph(Precipitation_xts_subset, group="SM3", main="Soil Moisture Sensor SM3", height = 200 )%>%
    dySeries("precip_amt", label = "Precipitation (mm)") %>%
    dyOptions(
      drawAxes = FALSE ) %>%  # Hide both x and y axes
    dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0)),  # Adjust y-axis options as needed
  dygraphs::dygraph(SM3_xts_subset, group="SM3", main=NULL)%>%
    dygraphs::dySeries("SM3_Depth15", label = "Depth 15") %>%
    dygraphs:: dySeries("SM3_Depth30", label = "Depth 30") %>%
    dygraphs::dySeries("SM3_Depth45", label = "Depth 45") %>%
    dygraphs:: dySeries("SM3_Depth60", label = "Depth 60") %>%
    dygraphs:: dySeries("SM3_Depth75", label = "Depth 75") %>%
    dygraphs::dySeries("SM3_Depth90", label = "Depth 90") %>%
    dygraphs::dySeries("SM3_Depth105", label = "Depth 105") %>%
    dygraphs::dySeries("SM3_Depth120", label = "Depth 120") %>%
    dygraphs::dyOptions(drawGrid = FALSE, stackedGraph = FALSE, colors = RColorBrewer::brewer.pal(7, "Set1")[c(1, 2, 3, 4, 5, 7, 8, 9)])%>%
    dygraphs::dyAxis("y", label = "Soil Moisture Index")%>%
    dygraphs::dyLegend(show = "always") %>%
    dygraphs::dyRangeSelector() %>%
    dyHide() %>%
    dygraphs::dyOptions(digitsAfterDecimal = 2, axisLabelFontSize = 13)
)  # end list

# Render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(plotobj_SM3))
