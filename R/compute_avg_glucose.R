#' Compute average glucose in given time periods
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param from_gmi Boolean. For internal use only. TRUE if compute_avg_glucose is being called
#' by the compute_gmi function. Otherwise FALSE.
#'
#' @return Data frame with average glucose metrics
#' @export
#'
#' @examples
compute_avg_glucose <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                                breaks = NULL, from_gmi = F) {

  # Not from gmi
  if(from_gmi == F) {
    check_lookback(lookback)
    df_dex <- change_window(df_dex = df_dex, start = start, end = end)
    if(!is.null(breaks)) {
      df_dex <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)
    }
    if(!is.null(freq)) {
      df_dex <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks, compute = TRUE)
    }
  }


  # Both cases
  df <- df_dex
  df <- convert_bv(df)


  df$`bg_value_num` <- as.numeric(df$`bg_value_num`)

  # From gmi
  if(from_gmi == T) {
    df$bg_value_num <- replace(df$bg_value_num, is.na(df$bg_value_num), 0)
    df$bg_value_num <- as.numeric(df$bg_value_num)

    df_avg <- df %>%
      dplyr::summarise(
        n = dplyr::n(),
        bg_total = sum(bg_value_num),
        bg_mean = sum(bg_value_num) / n
      )

  } else {
      previous_end_date <- as.POSIXct("0001-01-01 01:01:01", tz = "UTC")
      i <- 1
      while(i <= max(df$inter)) {
        df_constrained <- dplyr::filter(df, inter == i)

        if (nrow(df_constrained) > 0) {
          end_date <- as.POSIXct(df_constrained$labs[nrow(df_constrained)], tz = "UTC")
          end_date <- end_date + lubridate::seconds(86400)
          start_date <- as.POSIXct(end_date - lubridate::seconds(lookback * 86400), tz = "UTC")

          if (!is.na(start_date) && !is.na(end_date)) {
            df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

            if(check_days_minimum(df_constrained_a) == F) {
              df_running <- df_constrained_a %>%
                dplyr::summarise(
                  n = dplyr::n(),
                  bg_total = NA,
                  bg_mean = NA
                )
              print(paste0("Time interval ", i, " does not have enough data, values set to NA"))
            } else {
              df_running <- df_constrained_a %>%
                dplyr::summarise(
                  n = dplyr::n(),
                  bg_total = sum(bg_value_num),
                  bg_mean = sum(bg_value_num) / n
                )
            }
          }
        } else {
          end_date <- as.POSIXct(previous_end_date, tz = "UTC")
          start_date <- as.POSIXct(end_date - lubridate::seconds((lookback - freq) * 86400), tz = "UTC")
          df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

          if(check_days_minimum(df_constrained_a) == F) {
            df_running <- df_constrained_a %>%
              dplyr::summarise(
                n = dplyr::n(),
                bg_total = NA,
                bg_mean = NA
              )
          } else {
            df_running <- df_constrained_a %>%
              dplyr::summarise(
                n = dplyr::n(),
                bg_total = sum(bg_value_num),
                bg_mean = sum(bg_value_num) / n
              )
          }
        }

        if(i == 1) {
          df_avg <- df_running
        } else {
          df_avg <- dplyr::bind_rows(df_avg, df_running)
        }
        i <- i + 1
      previous_end_date <- end_date
      }
  }

  return(df_avg)
}
