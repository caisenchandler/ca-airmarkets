library(curl)

get_db1bfiles <- function(years = 2023, quarters = 1:4) {
    
    # Define the root URL for the DB1B data
    root_url <- "https://transtats.bts.gov/OT_Delay/OT_Delay/"
    # Define the destination file path
    
    # loop through years
    for(year in years){
        for(q in quarters){
            market_url <- paste0(root_url, year, "_Q", q, ".zip")
            market_file <- paste0("data/market_", year, "_Q", q, ".zip")
            coupon_url <- paste0(root_url, year, "_Q", q, ".zip")
            coupon_file <- paste0("data/coupon_", year, "_Q", q, ".zip")

            download_data(market_url, market_file)
            download_data(coupon_url, coupon_file)
        }
    }
    
    message("DB1B data downloaded and unzipped successfully.")
}

download_data <- function(url, destfile) {
    if (missing(url) || missing(destfile)) {
        stop("Both 'url' and 'destfile' must be provided.")
    }
    
    curl_download(url, destfile)
    message("Data downloaded successfully to ", destfile)
}

# Example usage:
# download_data("http://example.com/data.csv", "data.csv")