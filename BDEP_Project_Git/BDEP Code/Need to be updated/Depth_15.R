library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(dygraphs)
library(htmltools)


dates <- seq(from = as.POSIXct("2020-07-09 00:00:00"), 
             to = as.POSIXct("2022-06-28 23:00:00"), 
             by = "hour")

start_date <- as.POSIXct('2020-07-09')
end_date <- as.POSIXct('2022-06-28')

filtered_precip <- subset(yyc, time >= start_date & time <= end_date)
# Convert to xts object for precipitation
Precipitation_xts <- xts(
  x = filtered_precip[, "precip_amt"],
  order.by = filtered_precip$time
)
# Subset Precipitation_xts and SM1_xts to the common date range
Precipitation_xts_subset <- Precipitation_xts[index(Precipitation_xts) >= start_date & index(Precipitation_xts) <= end_date,]


SM1_depth_15 <- SM1 %>%
  select(Date_SM1, SM1_Depth15) %>%
  mutate(Date_SM1 = round_date(Date_SM1, unit = "hour"))

SM2_depth_15 <- SM2 %>%
  select(Date_SM2, SM2_Depth15) %>%
  mutate(Date_SM2 = round_date(Date_SM2, unit = "hour"))

SM3_depth_15 <- SM3 %>%
  select(Date_SM3, SM3_Depth15) %>%
  mutate(Date_SM3 = round_date(Date_SM3, unit = "hour"))

SM4_depth_15 <- SM4 %>%
  select(Date_SM4, SM4_Depth15) %>%
  mutate(Date_SM4 = round_date(Date_SM4, unit = "hour"))

SM5_depth_15 <- SM5 %>%
  select(Date_SM5, SM5_Depth15) %>%
  mutate(Date_SM5 = round_date(Date_SM5, unit = "hour"))

SM6_depth_15 <- SM6 %>%
  select(Date_SM6, SM6_Depth15) %>%
  mutate(Date_SM6 = round_date(Date_SM6, unit = "hour"))


depth_15 <- data.frame(
  Date_depth_15 = dates,
  stringsAsFactors = FALSE  # Optional: Ensure Date column is not converted to factors
)

depth_15 <- depth_15 %>%
  mutate(SM1_depth_15 = if_else(Date_depth_15 %in% SM1_depth_15$Date_SM1, SM1_depth_15$SM1_Depth15[match(Date_depth_15, SM1_depth_15$Date_SM1)], NA_real_))
depth_15 <- depth_15 %>%
  mutate(SM2_depth_15 = if_else(Date_depth_15 %in% SM2_depth_15$Date_SM2, SM2_depth_15$SM2_Depth15[match(Date_depth_15, SM2_depth_15$Date_SM2)], NA_real_))
depth_15 <- depth_15 %>%
  mutate(SM3_depth_15 = if_else(Date_depth_15 %in% SM3_depth_15$Date_SM3, SM3_depth_15$SM3_Depth15[match(Date_depth_15, SM3_depth_15$Date_SM3)], NA_real_))
depth_15 <- depth_15 %>%
  mutate(SM4_depth_15 = if_else(Date_depth_15 %in% SM4_depth_15$Date_SM4, SM4_depth_15$SM4_Depth15[match(Date_depth_15, SM4_depth_15$Date_SM4)], NA_real_))
depth_15 <- depth_15 %>%
  mutate(SM5_depth_15 = if_else(Date_depth_15 %in% SM5_depth_15$Date_SM5, SM5_depth_15$SM5_Depth15[match(Date_depth_15, SM5_depth_15$Date_SM5)], NA_real_))
depth_15 <- depth_15 %>%
  mutate(SM6_depth_15 = if_else(Date_depth_15 %in% SM6_depth_15$Date_SM6, SM6_depth_15$SM6_Depth15[match(Date_depth_15, SM6_depth_15$Date_SM6)], NA_real_))


# Convert to xts object for plotting
depth_15_xts <- xts(
  x = depth_15[, c("SM1_depth_15", "SM2_depth_15", "SM3_depth_15", "SM4_depth_15", "SM5_depth_15", "SM6_depth_15")],
  order.by = depth_15$Date_depth_15
)

# Create a list of dygraphs objects
plotobj <- list(
  dygraphs::dygraph(Precipitation_xts, group="Depth_15", main="Soil Moisture sensors at depth = 15cm", height = 200 )%>%
    dySeries("precip_amt", label = "Precipitation (mm)") %>%
    dyOptions(
      drawAxes = FALSE ) %>%  # Hide both x and y axes
    dyAxis("y", label = "Precipitation (mm)", valueRange = c(10, 0)),  # Adjust y-axis options as needed
   dygraphs::dygraph(depth_15_xts, group="Depth_15", main=NULL)%>%
   dygraphs::dyOptions(drawGrid = FALSE, colors = RColorBrewer::brewer.pal(7, "Set1")[c(1, 2, 3, 4, 5, 7)])%>%
     dygraphs::dyAxis("y", label = "Soil Moisture Index")%>%
     dygraphs::dyLegend(show = "always") %>%
     dygraphs::dyRangeSelector() %>%
    dyHide() %>%
     dygraphs::dyOptions(digitsAfterDecimal = 2, axisLabelFontSize = 14)
)  # end list

# Render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(plotobj))




depth_15_long <- tidyr::pivot_longer(depth_15, 
                                     cols = starts_with("SM"), 
                                     names_to = "Sensor", 
                                     values_to = "Moisture_Content")
# Plot using ggplot2
depth_15_plot <- ggplot(depth_15_long, aes(x = Date_depth_15, y = Moisture_Content, color = Sensor)) +
  geom_line() +
  labs(x = "Date", y = "Soil Moisture Index", title = "Soil Moisture Index for all sensors at a depth of 15 cm") +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "6 months") +
  theme_minimal()

plot_precip <- ggplot(filtered_precip, aes(x = time)) +
  geom_line(aes(y = precip_amt, colour = "RAINFALL")) +
  labs(x = "Date", y = "Precipitation (mm)", color = NULL) +  # Removed x-axis title, y-axis title, and legend
  scale_y_reverse() +  # Reverse the y-axis
  #scale_x_datetime(date_breaks = "1 month") +  # Format x-axis to include year
  theme_minimal() +
  theme(legend.position = "right")
