#' Compute GMI
#'
#' Computes the glucose management indicator for a given time frame.
#' GMI is computed following the results of:
#' https://diabetesjournals.org/care/article/42/4/e60/36083/Glucose-Management-Indicator-GMI-Insights-and
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param lookback Number of days to look back for GMI computations. Defaults to 90 days.
#' @param freq Time interval for computation; e.g. every 14 days, every month, etc.
#' @param breaks Vector. Specify range computations manually, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
#' would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5.
#' @param include_bv If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags.
#'
#' @return Data frame with GMI metrics
#' @export
#'
#' @examples
compute_gmi <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                        breaks = NULL, include_bv = TRUE) {

  check_lookback(lookback)
  df_dex <- change_window(df_dex = df_dex, start = start, end = end)

  if(!is.null(breaks)) {
    df <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)
  }
  if(!is.null(freq)) {
    df <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks, compute = TRUE)
  }

  if(include_bv) {
    df <- convert_bv(df)
  }

  df$`bg_value_num` <- as.numeric(df$`bg_value_num`)

  previous_end_date <- as.POSIXct("0001-01-01 01:01:01", tz = "UTC")
  if(!is.null(freq)) {
    i <- 1
    while(i <= max(df$inter)) {
      df_constrained <- dplyr::filter(df, inter == i)

      if (nrow(df_constrained) > 0) {
        end_date <- as.POSIXct(df_constrained$labs[nrow(df_constrained)], tz = "UTC")
        end_date <- end_date + lubridate::seconds(86400)
        start_date <- as.POSIXct(end_date - lubridate::seconds(lookback * 86400), tz = "UTC")

        if (!is.na(start_date) && !is.na(end_date)) {
          df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

          if(check_days_minimum(df_constrained_a) == FALSE) {
            df_gmi <- data.frame(bg_mean = NA, bg_total = NA, gmi = NA)
            print(paste0("Time interval ", i, " does not have enough data, values set to NA"))
          } else {
            df_avg <- compute_avg_glucose(df_dex = df_constrained_a, from_gmi = TRUE)
            df_gmi <- df_avg %>%
              dplyr::mutate(gmi = 3.31 + (0.02392 * df_avg$bg_mean))
          }
        }
      } else {
        end_date <- as.POSIXct(previous_end_date, tz = "UTC")
        start_date <- as.POSIXct(end_date - lubridate::seconds((lookback - freq) * 86400), tz = "UTC")
        df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

        if(nrow(df_constrained_a) > 0) {
          df_avg <- compute_avg_glucose(df_dex = df_constrained_a, from_gmi = TRUE)
          df_gmi <- df_avg %>%
            dplyr::mutate(gmi = 3.31 + (0.02392 * df_avg$bg_mean))
        } else {
          df_gmi <- data.frame(bg_mean = NA, bg_total = NA, gmi = NA)
          print("Empty lookback window.")
        }
      }

      if(i == 1) {
        df_final <- df_gmi
      } else {
        df_final <- dplyr::bind_rows(df_final, df_gmi)
      }
      i <- i + 1
      previous_end_date <- end_date
    }
  }

  if(!is.null(breaks)) {
    df_final <- df %>%
      dplyr::group_by(inter) %>%
      dplyr::summarise(
        bg_mean = mean(bg_value_num, na.rm = TRUE),
        bg_total = sum(bg_value_num, na.rm = TRUE),
        .groups = 'drop') %>%
      dplyr::mutate(gmi = 3.31 + (0.02392 * bg_mean))
  }

  return(df_final)
}
