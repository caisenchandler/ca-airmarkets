#Repeating analysis for q4 of 2023

#Reading in data 
library(tidyverse)
coupons_q4_2023 <- read_csv("C:\\Users\\eilis\\OneDrive\\Documents\\UCDavis\\HSR\\Data\\Flights\\DB1B\\DB1B_Q4_2023\\q4_2023_coupon.zip")
market_q4_2023 <- read_csv("C:\\Users\\eilis\\OneDrive\\Documents\\UCDavis\\HSR\\Data\\Flights\\DB1B\\DB1B_Q4_2023\\q4_2023_market.zip")

# Define CA airport IDs (14: LAX, SFO, SJC, OAK, STS, LAX, SNA, BUR, ONT, LGB, SBD, FAT, BFL, SMF, SAN)
airport_ids <- c(12892, 14771, 14831, 13796, 15023, 14908, 10800, 13891, 12954, 14691, 11638, 10561, 14893, 14679)

# Generate all possible combinations of airport pairs, excluding where origin and destination airports are the same. 
airport_pairs <- expand.grid(ORIGIN_AIRPORT_ID = airport_ids, DEST_AIRPORT_ID = airport_ids) %>%
  filter(ORIGIN_AIRPORT_ID != DEST_AIRPORT_ID) %>%
  mutate(pair = paste(ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID, sep = "-"))

# Creating the function to process each pair of airports.
process_airport_pair <- function(origin_airport_code, dest_airport_code) {
  
#Filter market data for the current pair and create TRAFFIC_STATUS column.
  pair_market <- market_q4_2023 %>% 
    filter(ORIGIN_AIRPORT_ID == origin_airport_code, DEST_AIRPORT_ID == dest_airport_code) %>% 
    mutate(TRAFFIC_STATUS = case_when(
      MARKET_COUPONS == 1 ~ "direct_passengers", 
      MARKET_COUPONS >= 1 ~ "indirect_passengers", 
      TRUE ~ "other"
    ))
  
#Filtering coupon data for the current pair and creating TRAFFIC_STATUS column. If the itineraries are also in the market df, we know that they are direct passengers. 
  pair_coupon <- coupons_q4_2023 %>% 
    filter(ORIGIN_AIRPORT_ID == origin_airport_code, DEST_AIRPORT_ID == dest_airport_code) %>% 
    mutate(TRAFFIC_STATUS = case_when(
      ITIN_ID %in% pair_market$ITIN_ID ~ "direct_passengers", 
      TRUE ~ "connecting_passengers"
    ))
  
#Filter out "direct_passengers" from pair_market for later processing to prevent duplicates of direct passenger rows. 
  pair_market_filtered <- pair_market %>%
    filter(TRAFFIC_STATUS != "direct_passengers")
  
#Append the filtered market and coupon dfs together.
  pair_coupon_market <- bind_rows(pair_market_filtered, pair_coupon)
  
#Summarize the data, ensuring all TRAFFIC_STATUS types are included.
  pair_coupon_market_summary <- pair_coupon_market %>%
    group_by(ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID, TRAFFIC_STATUS) %>%
    summarize(passengers = sum(PASSENGERS), .groups = 'drop') %>%
    mutate(TRAFFIC_STATUS = factor(TRAFFIC_STATUS, levels = c("direct_passengers", "indirect_passengers", "connecting_passengers")))  # Ensure all statuses are included
  
#Pivot the summary data to wide format, filling missing traffic statuses with 0 if there are no passengers between a pair of airports (likely with smaller airports).
  pair_coupon_market_pivot <- pair_coupon_market_summary %>%
    pivot_wider(names_from = TRAFFIC_STATUS, values_from = passengers, values_fill = list(passengers = 0))
  
#Ensure all traffic status columns exist; if any are missing, create them with 0.
  required_columns <- c("direct_passengers", "indirect_passengers", "connecting_passengers")
  missing_columns <- setdiff(required_columns, colnames(pair_coupon_market_pivot))
  
#If any columns are missing, add them with 0 values.
  if(length(missing_columns) > 0) {
    for(col in missing_columns) {
      pair_coupon_market_pivot[[col]] <- 0
    }
  }
  
#Calculate total_passengers and percentage for each traffic status.
  pair_coupon_market_pivot <- pair_coupon_market_pivot %>%
    mutate(total_passengers = `direct_passengers` + `indirect_passengers` + `connecting_passengers`) %>%
    mutate(
      across(
        c(direct_passengers, indirect_passengers, connecting_passengers), 
        ~ . / total_passengers * 100, 
        .names = "percent_{.col}")
    )
  
#Return the final data with the airport pair info. 
  pair_coupon_market_pivot %>%
    mutate(ORIGIN_AIRPORT_ID = origin_airport_code, DEST_AIRPORT_ID = dest_airport_code)
}

#Apply the process for all airport pairs using purrr::pmap_dfr(). 
CA_airports_pass_flow_q4_2023 <- airport_pairs %>%
  pmap_dfr(~ process_airport_pair(..1, ..2)) %>% 
  arrange(desc(total_passengers))

#Print the results. 
print(CA_airports_pass_flow_q4_2023)
view(CA_airports_pass_flow_q4_2023)
