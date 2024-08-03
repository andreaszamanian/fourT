#' Compute average glucose in given time periods
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param from_gmi Boolean. For internal use only. TRUE if compute_avg_glucose is being called
#' by the compute_gmi function. Otherwise FALSE.
#'
#'
#' @return
#' @export
#'
#' @examples
compute_avg_glucose <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                                breaks = NULL, from_gmi = F){
  #not from gmi
  if(from_gmi == F){
    check_lookback(lookback)
    df_dex <- change_window(df_dex = df_dex, start = start, end = end)
    df_dex <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)
  }
  #both cases
  df <- df_dex

  #from gmi
  if(from_gmi == T){
    #changes NA values to zero
    df$bg_value_num <- replace(df$bg_value_num, is.na(df$bg_value_num), 0) #this is fine, but then should
    #also keep some sort of NA count to not skew the mean

    #make numeric
    df$bg_value_num <- as.numeric(df$bg_value_num)

    df_avg <- df %>%
      dplyr::summarise(
        n = dplyr::n(),
        bg_total = sum(bg_value_num),
        bg_mean = sum(bg_value_num)) %>%
      dplyr::mutate(across(bg_mean, ~ ./n))

    #checking enough data in lookback window, but not print statement, that is handled by compute_gmi
    #also no iteration, because compute_ag_glucose is inside while loop in compute_gmi
    if(check_days_minimum(df) == F){
      df_avg <- df %>%
        dplyr::summarise(
          n = dplyr::n(),
          bg_total = NA,
          bg_mean = NA)
    }

  #not from gmi
  }
  if(from_gmi == F){
      #changes NA values to zero
      df$bg_value_num <- replace(df$bg_value_num, is.na(df$bg_value_num), 0) #this is fine, but then should
      #also keep some sort of NA count to not skew the mean
      #make numeric
      df$bg_value_num <- as.numeric(df$bg_value_num)
      i <- 1
      while(i <= max(df$inter)){
        df_constrained <- dplyr::filter(df, inter == i)
        end_date <- df_constrained$bg_date_time[length(df_constrained$bg_date_time)]
        start_date <- as.POSIXct(end_date - lubridate::seconds(lookback*86400))
        df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

        #changes NA values to zero
        df_constrained_a$bg_value_num <- replace(df_constrained_a$bg_value_num, is.na(df_constrained_a$bg_value_num), 0) #this is fine, but then should
        #also keep some sort of NA count to not skew the mean
        #make numeric
        df_constrained_a$bg_value_num <- as.numeric(df_constrained_a$bg_value_num)

        df_running <- df_constrained_a %>%
          dplyr::summarise(
            n = dplyr::n(),
            bg_total = sum(bg_value_num),
            bg_mean = sum(bg_value_num)) %>%
          dplyr::mutate(across(bg_mean, ~ ./n))

        #checking enough data in lookback window
        if(check_days_minimum(df_constrained_a) == F){
          df_running <- df_constrained_a %>%
            dplyr::summarise(
              n = dplyr::n(),
              bg_total = NA,
              bg_mean = NA)
          print(paste0("Time interval ", i, " does not have enough data, values set to NA"))
        }

        if(i == 1){
          df_avg <- df_running
        }
        else{
          df_avg <- dplyr::bind_rows(df_avg, df_running)
        }
        i <- i+1
      }
  }
  return(df_avg)
}
