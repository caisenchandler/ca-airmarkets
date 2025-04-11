# 1. Load libraries
library(dplyr)
library(readr)    # or data.table for big files

# 2. Read in the DB1B dataset
# Replace with your actual file path
db1b_data <- read_csv("db1b_data/2024_q4.csv")

# 3. Inspect the column names
head(db1b_data)
names(db1b_data)

# 4. Define Bay Area and LA Area airport codes
bay_area_airports <- c("SFO", "SJC", "OAK", "STS")
la_area_airports <- c("LAX", "SNA", "BUR", "ONT", "LGB", "SBD")

# 5. Filter for routes between Bay Area and LA Area (in either direction)
bay_la_routes <- db1b_data %>%
  filter((Origin %in% bay_area_airports & Dest %in% la_area_airports) |
           (Origin %in% la_area_airports & Dest %in% bay_area_airports))

# 6. Summarize the number of passengers
total_passengers <- bay_la_routes %>%
  summarise(total_passengers = sum(Passengers))

print(total_passengers)

# Optional: Group by carrier
passengers_by_carrier <- bay_la_routes %>%
  group_by(OpCarrier) %>%
  summarise(passengers = sum(Passengers) * 10) %>%  # Adjusting for DB1B 10% sample
  arrange(desc(passengers))

print(passengers_by_carrier)

# 7. Estimate total flights (optional)
# If each flight averages about 150 passengers
estimated_flights <- total_passengers$total_passengers * 10 / 150
print(estimated_flights)
