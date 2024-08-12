library(dygraphs)
library(xts)
library(htmltools)

start_date <- as.POSIXct('2020-01-01')
end_date <- as.POSIXct('2023-01-01')
sensor <- "SM6"
depth <- "15"
title_string <- sprintf("Soil moisture for %s at Depth %s", sensor, depth)

# Clean NA values from SM6 and assign to a new variable
SM6_cleaned <- na.omit(SM6)

# Convert to xts object for plotting
SM6_xts <- xts(
  x = SM6_cleaned[,"SM6_Depth90"],
  order.by = SM6_cleaned$Date_SM6
)

SM6_xts_subset <- SM6_xts[index(SM6_xts) >= start_date & index(SM6_xts) <= end_date,]

# Create the dygraph for soil moisture depths
dygraph_soil_moisture <- dygraph(SM6_xts_subset, main = title_string) %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyAxis("y", label = "Volumetric Soil Moisture", valueRange = c(0,10))%>%
  dyAxis("x", axisLabelFormatter = "function(d) { return d.getFullYear() + '-' + ('0' + (d.getMonth() + 1)).slice(-2); }")%>%
  dyRangeSelector()%>%

browsable(dygraph_soil_moisture)



# Specify the columns you want to summarize
columns_to_summarize <- c("SM6_Depth15", "SM6_Depth30", "SM6_Depth45", "SM6_Depth60", "SM6_Depth75", "SM6_Depth90")

# Create a summary function
summary_function <- function(column) {
  return(data.frame(
    Min = min(column, na.rm = TRUE),
    Q1 = quantile(column, 0.25, na.rm = TRUE),
    Median = median(column, na.rm = TRUE),
    Mean = mean(column, na.rm = TRUE),
    Q3 = quantile(column, 0.75, na.rm = TRUE),
    Max = max(column, na.rm = TRUE)
  ))
}

# Apply the summary function to each specified column and combine into a single dataframe
summary_df <- bind_rows(lapply(SM6[columns_to_summarize], summary_function), .id = "Column")

# Print the summary dataframe
print(summary_df)

write.csv(summary_df, "summary_statistics.csv", row.names = FALSE)

 