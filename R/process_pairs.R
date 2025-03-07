#' Process pairs of airport codes to get percent direct and indirect
#' @param origin_airport_code The origin airport code.
#' @param dest_airport_code The destination airport code.
#' @param coupon The coupon data frame.
#' @param market The market data frame.
#' @return A data frame with the processed airport pair information.
#' @importFrom dplyr filter mutate case_when group_by summarize bind_rows across
#' @importFrom tidyr pivot_wider
#' @importFrom purrr pmap_dfr
process_airport_pair <- function(origin_airport_code, dest_airport_code, coupon, market) {
    # Filter market data for the current pair and create TRAFFIC_STATUS column.
    pair_market <- market_q1_2023 %>%
        dplyr::filter(
            ORIGIN_AIRPORT_ID == origin_airport_code,
            DEST_AIRPORT_ID == dest_airport_code
        ) %>%
        dplyr::mutate(
            TRAFFIC_STATUS = dplyr::case_when(
                MARKET_COUPONS == 1 ~ "direct_passengers",
                MARKET_COUPONS >= 1 ~ "indirect_passengers",
                TRUE ~ "other"
            )
        )

    # Filtering coupon data for the current pair and creating TRAFFIC_STATUS column.
    # If the itineraries are also in the market df, we know that they are direct
    # passengers.
    pair_coupon <- coupons_q1_2023 %>%
        filter(
            ORIGIN_AIRPORT_ID == origin_airport_code,
            DEST_AIRPORT_ID == dest_airport_code
        ) %>%
        mutate(TRAFFIC_STATUS = case_when(
            ITIN_ID %in% pair_market$ITIN_ID ~ "direct_passengers",
            TRUE ~ "connecting_passengers"
        ))

    # Filter out "direct_passengers" from pair_market for later processing to
    # prevent duplicates of direct passenger rows.
    pair_market_filtered <- pair_market %>%
        filter(TRAFFIC_STATUS != "direct_passengers")

    # Append the filtered market and coupon dfs together.
    pair_coupon_market <- bind_rows(pair_market_filtered, pair_coupon)

    # Summarize the data, ensuring all TRAFFIC_STATUS types are included.
    pair_coupon_market_summary <- pair_coupon_market %>%
        group_by(ORIGIN_AIRPORT_ID, DEST_AIRPORT_ID, TRAFFIC_STATUS) %>%
        summarize(passengers = sum(PASSENGERS), .groups = "drop") %>%
        mutate(TRAFFIC_STATUS = factor(TRAFFIC_STATUS, levels = c("direct_passengers", "indirect_passengers", "connecting_passengers")))

    # Pivot the summary data to wide format, filling missing traffic statuses with 0 if there are no passengers between a pair of airports.
    pair_coupon_market_pivot <- pair_coupon_market_summary %>%
        pivot_wider(names_from = TRAFFIC_STATUS, values_from = passengers, values_fill = list(passengers = 0))

    # Ensure all traffic status columns exist; if any are missing, create them with
    # 0.
    required_columns <- c("direct_passengers", "indirect_passengers", "connecting_passengers")
    missing_columns <- setdiff(required_columns, colnames(pair_coupon_market_pivot))

    # If any columns are missing, add them with 0 values.
    if (length(missing_columns) > 0) {
        for (col in missing_columns) {
            pair_coupon_market_pivot[[col]] <- 0
        }
    }

    # Calculate total_passengers and percentage for each traffic status.
    pair_coupon_market_pivot <- pair_coupon_market_pivot %>%
        mutate(total_passengers = `direct_passengers` + `indirect_passengers` + `connecting_passengers`) %>%
        mutate(
            across(
                c(direct_passengers, indirect_passengers, connecting_passengers),
                ~ . / total_passengers * 100,
                .names = "percent_{.col}"
            )
        )

    # Return the final data with the airport pair info.
    pair_coupon_market_pivot %>%
        mutate(ORIGIN_AIRPORT_ID = origin_airport_code, DEST_AIRPORT_ID = dest_airport_code)
}