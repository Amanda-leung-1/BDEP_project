library(weathercan)
library(dplyr)
library(ggplot2)

stations_search("CALGARY", interval = "hour")
#Station name = Calgary INT'L CS
yyc <- weather_dl(station_ids = "27211", start = "2019-01-01", end = "2024-01-01", interval = "hour") %>%
  select(time, precip_amt)

print("the data has been compiled")


p_yyc <- ggplot(yyc, aes(x = time, y = precip_amt)) +
  geom_line() +
  labs(title = "Rainfall at YYC INT A", x = "Date", y = "precip (mm)")+
  coord_cartesian(ylim = c(0, 20))   # Set y-axis limits
