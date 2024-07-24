#' Truncate time window for computation/plot
#'
#' @param df
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
#truncate_window <- function(df, start, end){
truncate_window <- function(df, start, end) {
  # Ensure start and end are in the correct time zone
  start <- as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  end <- as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  # Convert bg_date_time to POSIXct with the same time zone
  df$bg_date_time <- as.POSIXct(df$bg_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  # Initialize indices
  start_index <- NA
  end_index <- NA

  # Loop to find start_index
  i <- 1
  condition1 <- FALSE
  while (!condition1 && i <= nrow(df)) {
    if (!is.na(df$bg_date_time[i])) {
      if (difftime(start, df$bg_date_time[i], units = "secs") <= 0) {
        condition1 <- TRUE
        start_index <- i
      }
    }
    i <- i + 1
  }

  # Check if start_index was found
  if (is.na(start_index)) {
    stop("Start date is out of range")
  }

  # Loop to find end_index
  j <- nrow(df)
  condition2 <- FALSE
  while (!condition2 && j >= 1) {
    if (!is.na(df$bg_date_time[j])) {
      if (difftime(end, df$bg_date_time[j], units = "secs") >= 0) {
        condition2 <- TRUE
        end_index <- j
      }
    }
    j <- j - 1
  }

  # Check if end_index was found
  if (is.na(end_index)) {
    stop("End date is out of range")
  }

  df <- df %>%
    dplyr::filter(dplyr::row_number() >= start_index & dplyr::row_number() <= end_index)

  return(df)
}
