#' Compute average glucose in given time periods
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#'
#'
#'
#' @return
#' @export
#'
#' @examples
compute_avg_glucose <- function(df_dex){

  #start and end values
  if(start == "default"){
    start_date <- lubridate::as_date(find_start_date(df_dex))
  } else{start_date <- start}
  if(end == "default"){
    end_date <- lubridate::as_date(find_end_date(df_dex))
  } else{end_date <- end}
  if(start != "default" || end != "default"){
    df_dex <- truncate_window(df_dex, start = start_date, end = end_date)
  }
  df <- df_dex


  #changes NA values to zero
  df$bg_value_num <- replace(df$bg_value_num, is.na(df$bg_value_num), 0)

  #make numeric
  df$bg_value_num <- as.numeric(df$bg_value_num)

  df_avg <- df %>%
    dplyr::group_by(df$inter) %>%
    dplyr::summarise(
      n = dplyr::n(),
      bg_total = sum(bg_value_num),
      bg_mean = sum(bg_value_num)) %>%
    dplyr::mutate(across(bg_mean, ~ ./n))
}
