# Filter rows where SM3_Depth15 is "(Error)"
filtered_data <- SM3[SM3$SM3_Depth15 == "(Error)", ]

# Convert Date_SM3 to Date format if it's not already in Date format
filtered_data$Date_SM3 <- as.Date(filtered_data$Date_SM3, format = "%Y-%m-%d %H:%M:%S")

# Get unique dates in YYYY-MM-DD format
unique_dates <- unique(filtered_data$Date_SM3)

# Print unique dates
print(unique_dates)



# Assuming df is your dataframe
filtered_SM6 <- SM6 %>%
  filter(SM6_Depth30 == 0)

# Extract unique dates in YYYY-MM-DD format
unique_dates <- substr(filtered_SM6$Date_SM6, 1, 10)
duplicate_counts <- table(unique_dates)
filtered_dates <- names(duplicate_counts[duplicate_counts > 13])
print(duplicate_counts)
print(filtered_dates)
