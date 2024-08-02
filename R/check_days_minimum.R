check_days_minimum <- function(df, min_days = 14) {
  number_of_days <- df %>%
    dplyr::mutate(day = lubridate::as_date(bg_date_time)) %>%
    dplyr::summarise(unique_days = dplyr::n_distinct(day)) %>%
    dplyr::pull(unique_days)

  # Check if the number of unique days meets the minimum requirement
  return(number_of_days >= min_days)
}
