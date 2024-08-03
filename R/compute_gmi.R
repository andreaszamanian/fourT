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
#' @param breaks Vector. Specify range computations manually, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
#' would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5. EDIT, does not describe well.
#' @param include_bv If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags
#'
#' @return
#' @export
#'
#' @examples
compute_gmi <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                        breaks = NULL, include_bv = T) {

  check_lookback(lookback)
  df_dex <- change_window(df_dex = df_dex, start = start, end = end)
  df <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)
  #change boundary values?
  if(include_bv == T){
    df <- convert_bv(df)
  }

    i <- 1
    while(i <= max(df$inter)){
      df_constrained <- dplyr::filter(df, inter == i)
      end_date <- df_constrained$bg_date_time[length(df_constrained$bg_date_time)]
      start_date <- as.POSIXct(end_date - lubridate::seconds(lookback*86400))
      df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))
      df_constrained_a$`bg_value_num` <- as.numeric(df_constrained_a$`bg_value_num`)
      #compute gmi in that window
      df_avg <- compute_avg_glucose(df_dex = df_constrained_a,from_gmi = T)
      df_gmi <- df_avg %>%
        dplyr::mutate(gmi = 3.31 + (0.02392* df_avg$bg_mean))

      #checking enough data in lookback window
      if(check_days_minimum(df_constrained_a) == F){
        df_avg <- compute_avg_glucose(df_dex = df_constrained_a,from_gmi = T)
        df_gmi <- df_avg %>%
          dplyr::mutate(gmi = NA)
        print(paste0("Time interval ", i, " does not have enough data, values set to NA"))
      }
      if(i == 1){
        df_final <- df_gmi
      }
      else{
        df_final <- dplyr::bind_rows(df_final, df_gmi)
      }
      i <- i+1
    }
    df_gmi <- df_final #so there is a single return statement
  return(df_gmi)
}

