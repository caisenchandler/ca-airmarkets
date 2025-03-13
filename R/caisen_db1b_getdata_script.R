# Load required libraries
library(skynet)  # For DB1B data processing
library(httr)    # For downloading files
library(readr)   # For reading CSV files

# Set working directory (adjust path as needed)
setwd("/Users/caisenchandler/Desktop/Transpo_Job/db1b_data/2024")

# Define download parameters
url <- "https://transtats.bts.gov/PREZIP/Origin_and_Destination_Survey_DB1BMarket_2024_2.zip"
destfile <- "DB1B_Market_2024_Q2.zip"

# Download the ZIP file from the specified URL
GET(url, 
    write_disk(destfile, overwrite = TRUE), 
    config = config(ssl_verifypeer = FALSE))

# Download and extract DB1B data using skynet function
download_db1b(y = 2024, q = 2)

# Unzip the downloaded file to a specified directory
unzip(destfile, exdir = "unzipped_data_1")

# Define path to extracted CSV file
csv_file <- "unzipped_data_1/Origin_and_Destination_Survey_DB1BMarket_2024_2.csv"

# Read the CSV file into a dataframe (adjust path if necessary)
df <- read_csv(csv_file)

# Display the first few rows of the dataframe
head(df)
