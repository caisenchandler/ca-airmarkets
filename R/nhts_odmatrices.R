## This is a script that analyzes the NHTS NextGen Annual Passenger data. To
## use this script, first go to https://nhts.ornl.gov/od/downloads and download
## the data. Put the csv file in a folder named nhts_data and name it 
## 2022_Passenger_OD_Annual_Data.csv

# Load required packages
library(dplyr)
library(tidyr)
library(tibble)
library(readr)

# Function to create and return an OD matrix by mode
get_od_matrix_by_mode <- function(od_data, mode_col, use_names = FALSE) {
  # Select and group by appropriate zone identifiers
  if (use_names) {
    matrix <- od_data |>
      select(origin_zone_name, destination_zone_name, !!sym(mode_col)) |>
      group_by(origin_zone_name, destination_zone_name) |>
      summarise(total_trips = sum(!!sym(mode_col), na.rm = TRUE), .groups = 'drop') |>
      pivot_wider(
        names_from = destination_zone_name,
        values_from = total_trips,
        values_fill = list(total_trips = 0)
      ) |>
      column_to_rownames("origin_zone_name") |>
      as.matrix()
  } else {
    matrix <- od_data |>
      select(origin_zone_id, destination_zone_id, !!sym(mode_col)) |>
      group_by(origin_zone_id, destination_zone_id) |>
      summarise(total_trips = sum(!!sym(mode_col), na.rm = TRUE), .groups = 'drop') |>
      pivot_wider(
        names_from = destination_zone_id,
        values_from = total_trips,
        values_fill = list(total_trips = 0)
      ) |>
      column_to_rownames("origin_zone_id") |>
      as.matrix()
  }
  
  return(matrix)
}

# Load your data
od_data <- read_csv("nhts_data/2022_Passenger_OD_Annual_Data.csv")

# Filter only California zones (adjust column names if needed)
od_data_ca <- od_data |>
  filter(origin_state == "CA" & destination_state == "CA")

# Generate OD matrices (unnamed version by ID)
od_air     <- get_od_matrix_by_mode(od_data_ca, "mode_air")
od_rail    <- get_od_matrix_by_mode(od_data_ca, "mode_rail")
od_vehicle <- get_od_matrix_by_mode(od_data_ca, "mode_vehicle")
od_atf     <- get_od_matrix_by_mode(od_data_ca, "mode_atf")

# Optional: Generate named versions using zone names
od_air_named     <- get_od_matrix_by_mode(od_data_ca, "mode_air", use_names = TRUE)
od_rail_named    <- get_od_matrix_by_mode(od_data_ca, "mode_rail", use_names = TRUE)
od_vehicle_named <- get_od_matrix_by_mode(od_data_ca, "mode_vehicle", use_names = TRUE)
od_atf_named     <- get_od_matrix_by_mode(od_data_ca, "mode_atf", use_names = TRUE)
