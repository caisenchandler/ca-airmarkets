# this is the code I used to download all the data

download_db1b_zip <- function(year, quarter) {
  url <- paste0("https://transtats.bts.gov/PREZIP/Origin_and_Destination_Survey_DB1BMarket_", year, "_", quarter, ".zip")
  destfile <- paste0("DB1B_Market_", year, "_Q", quarter, ".zip")
  
  download.file(
    url = url,
    destfile = destfile,
    method = "curl",
    extra = "-k"
  )
}

#Setting the working directory
setwd("/Users/caisenchandler/Desktop/Transpo_Job/db1b_data/2024")

# Calling the Function and Downloading the Data, repeat and change 2nd argument
# for other quarters
download_db1b_zip(2024, 1)
download_db1b_zip(2024, 1)
download_db1b_zip(2024, 1)
download_db1b_zip(2024, 1)