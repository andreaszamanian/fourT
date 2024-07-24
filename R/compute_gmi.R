#' Compute GMI
#'
#' Computes the glucose management indicator for a given time frame.
#' GMI is computed following the results of:
#' https://diabetesjournals.org/care/article/42/4/e60/36083/Glucose-Management-Indicator-GMI-Insights-and
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#' @param include_bv If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags
#'
#' @return
#' @export
#'
#' @examples
compute_gmi <- function(df_dex, inter, include_bv = T) {

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






  if(inter <= 0){
    stop("Nonpositive timer interval inputted. Please input
         positive number of days")
  }

  df <- set_inter(df_dex, inter)
  if(include_bv == T){
    df <- convert_bv(df)
  }

  df_avg <- compute_avg_glucose(df)

  df_gmi <- df_avg %>%
    dplyr::mutate(gmi = 3.31 + (0.02392* df_avg$bg_mean))
}

