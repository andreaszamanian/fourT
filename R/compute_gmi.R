#' Compute glucose management indicator (GMI)
#'
#' GMI is calculated per Bergenstal & Beck (2018) as an approximation of HbA1c (%).
#' The GMI computation uses up to 90 days of prior glucose readings, and no less than 14 days; this
#' is reflected in the limitations on the lookback parameter.
#' The explicit formula is Formula: 3.31 + 0.02392*avg_glucose.
#' Here 'avg_glucose' is the sensor glucose readings averaged over up to 90 days.
#' For days with less than 14 days in the its lookback window, GMI is set to NA
#'
#' For more examples see the 'Getting started' vignette.
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param lookback Integer. Number of days to look back for GMI computations. Must be greater than or equal
#' to 14 and less than or equal to 90. Defaults to 90 days.
#' @param freq Integer. Time interval frequency (in days) for
#' computation; e.g. every 14 days, every 30 days, etc. The default value is 14 days to match
#' convention in 4T studies.
#' @param breaks Numeric vector. Specify range computations manually by month, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
#' would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5. Similarly to the usage with the freq
#' parameter, if there are less than 14 days of data in a given breaks interval then GMI is set to NA
#' @param include_bv If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags.
#'
#' @return Data frame with GMI metrics
#' @export
#'
#' @examples
#' bg_date_time <- as.POSIXct(c("2020-01-01 01:01:01", "2020-03-01 01:01:01",
#' "2020-06-01 01:01:01", "2020-07-01 01:01:01", "2020-09-09 01:01:01", "2020-12-09 01:01:01",
#' "2021-01-01 01:01:01"), tz = "UTC")
#' bg_value_num <- c(67, 100, 150, 123, 124, 80, 300)
#' record_id <- c(010101, 010101, 010101, 010101, 010101, 010101, 010101)
#' bg_value_flag <- c(NA, NA, NA, NA, NA, NA, NA)
#'
#'
#' df <- dplyr::tibble(bg_date_time = bg_date_time, bg_value_num = bg_value_num, record_id = record_id,
#' bg_value_flag = bg_value_flag)
#'
#'
#' compute_gmi(df, freq = 14)
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
