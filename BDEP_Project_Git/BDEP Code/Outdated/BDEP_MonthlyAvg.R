library(lubridate)
library(ggplot2)
##GET MONTHLY AVERAGEs
SM1_monthavg <- SM1 %>%
  mutate(Month = floor_date(Date_SM1, "month")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

## maybe replace with   summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
# View the result

SM2_monthavg <- SM2 %>%
  mutate(Month = floor_date(Date_SM2, "month")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

SM3_monthavg <- SM3 %>%
  mutate(Month = floor_date(Date_SM3, "month")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

SM4_monthavg <- SM4 %>%
  mutate(Month = floor_date(Date_SM4, "month")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

SM5_monthavg <- SM5 %>%
  mutate(Month = floor_date(Date_SM5, "month")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

SM6_monthavg <- SM6 %>%
  mutate(Month = floor_date(Date_SM6, "month")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

months <- seq(from = as.POSIXct("2020-07-01 00:00:00"), 
             to = as.POSIXct("2022-07-01 00:00:00"), 
             by = "month")
depth_15_month <- data.frame(
  Month = months,
  stringsAsFactors = FALSE)  # Optional: Ensure Date column is not converted to factors

depth_15_month <- depth_15_month %>%
  left_join(select(SM1_monthavg, Month, SM1_Depth15), by = "Month")%>%
  left_join(select(SM2_monthavg, Month, SM2_Depth15), by = "Month")%>%
  left_join(select(SM3_monthavg, Month, SM3_Depth15), by = "Month")%>%
  left_join(select(SM4_monthavg, Month, SM4_Depth15), by = "Month")%>%
  left_join(select(SM5_monthavg, Month, SM5_Depth15), by = "Month")%>%
  left_join(select(SM6_monthavg, Month, SM6_Depth15), by = "Month")
  

sad <- ggplot(filtered_precip, aes(x = time)) +
  geom_line(aes(y = precip_amt, colour = "RAINFALL")) +
  labs(x = "Date", y = "Precipitation (mm)", color = NULL) +  # Removed x-axis title, y-axis title, and legend
  scale_y_reverse() +  # Reverse the y-axis
  #scale_x_datetime(date_breaks = "1 month") +  # Format x-axis to include year
  theme_minimal() +
  theme(legend.position = "right")

