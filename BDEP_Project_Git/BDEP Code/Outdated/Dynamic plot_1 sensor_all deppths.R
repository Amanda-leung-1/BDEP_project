library(dygraphs)
library(xts)
library(htmltools)

start_date <- as.POSIXct('2020-10-15')
end_date <- as.POSIXct('2022-06-28')
# Clean NA values from SM4 and assign to a new variable
SM4_cleaned <- na.omit(SM4)

# Convert to xts object for plotting
SM4_xts <- xts(
  x = SM4_cleaned[, c("SM4_Depth15", "SM4_Depth30", "SM4_Depth45", "SM4_Depth60")],
  order.by = SM4_cleaned$Date_SM4
)

SM4_xts_subset <- SM4_xts[index(SM4_xts) >= start_date & index(SM4_xts) <= end_date,]

# Create the dygraph for soil moisture depths
dygraph_soil_moisture <- dygraph(SM4_xts_subset) %>%
  dySeries("SM4_Depth15", label = "Depth 15") %>%
  dySeries("SM4_Depth30", label = "Depth 30") %>%
  dySeries("SM4_Depth45", label = "Depth 45") %>%
  dySeries("SM4_Depth60", label = "Depth 60") %>%
  dySeries("SM4_Depth75", label = "Depth 75") %>%
  dySeries("SM4_Depth90", label = "Depth 90") %>%
  dySeries("SM4_Depth105", label = "Depth 105") %>%
  dySeries("SM4_Depth120", label = "Depth 120") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyRangeSelector()

browsable(dygraph_soil_moisture)

