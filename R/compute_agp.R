#' Compute AGP Thermometers
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param lookback Number of days to look back for AGP computations. Defaults to 90 days.
#' @param freq Time interval for computation; e.g. every 14 days, every month, etc.
#' @param breaks Vector. Specify range computations manually in months, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
#' would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5.
#' @param include_bv Boolean. If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags.
#'
#' @return Data frame with AGP metrics
#' @export
#'
#' @examples
compute_agp <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
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
          df_constrained_b <- df_constrained_a %>%
            dplyr::mutate(range = cut(df_constrained_a$bg_value_num, c(0, 54, 70, 181, 251, Inf),
                                      include.lowest = TRUE, labels = FALSE))

          if(check_days_minimum(df_constrained_b) == FALSE) {
            df_constrained_2 <- df_constrained_b %>%
              dplyr::ungroup() %>%
              dplyr::summarise(
                n = dplyr::n(),
                tbr2 = NA,
                tbr = NA,
                tar = NA,
                tar2 = NA,
                tir = NA, .groups = 'drop')
            print(paste0("Time interval ", i, " does not have enough data, values set to NA"))
          } else {
            df_constrained_2 <- df_constrained_b %>%
              dplyr::ungroup() %>%
              dplyr::summarise(
                n = dplyr::n(),
                tbr2 = sum(range == 1),
                tbr = sum(range <= 2),
                tar = sum(range >= 4),
                tar2 = sum(range == 5),
                tir = sum(range == 3), .groups = 'drop') %>%
              dplyr::mutate(across(tbr2:tir, ~ ./n))
          }
        }
      } else {
        end_date <- as.POSIXct(previous_end_date, tz = "UTC")
        start_date <- as.POSIXct(end_date - lubridate::seconds((lookback - freq) * 86400), tz = "UTC")
        df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

        if(nrow(df_constrained_a) > 0) {
          df_constrained_b <- df_constrained_a %>%
            dplyr::mutate(range = cut(df_constrained_a$bg_value_num, c(0, 54, 70, 181, 251, Inf),
                                      include.lowest = TRUE, labels = FALSE))
          df_constrained_2 <- df_constrained_b %>%
            dplyr::ungroup() %>%
            dplyr::summarise(
              n = dplyr::n(),
              tbr2 = sum(range == 1),
              tbr = sum(range <= 2),
              tar = sum(range >= 4),
              tar2 = sum(range == 5),
              tir = sum(range == 3), .groups = 'drop') %>%
            dplyr::mutate(across(tbr2:tir, ~ ./n))
        } else {
          df_constrained_2 <- df_constrained_b %>%
            dplyr::ungroup() %>%
            dplyr::summarise(
              n = dplyr::n(),
              tbr2 = NA,
              tbr = NA,
              tar = NA,
              tar2 = NA,
              tir = NA, .groups = 'drop')
          print("Empty lookback window.")
        }
      }

      if(i == 1) {
        df_tir <- df_constrained_2
      } else {
        df_tir <- dplyr::bind_rows(df_tir, df_constrained_2)
      }
      i <- i + 1
      previous_end_date <- end_date
    }
  }

  if(!is.null(breaks)) {
    df_tir <- df %>%
      dplyr::group_by(inter) %>%
      dplyr::mutate(range = cut(bg_value_num, c(0, 54, 70, 181, 251, Inf),
                                include.lowest = TRUE, labels = FALSE)) %>%
      dplyr::summarise(
        n = dplyr::n(),
        tbr2 = sum(range == 1),
        tbr = sum(range <= 2),
        tar = sum(range >= 4),
        tar2 = sum(range == 5),
        tir = sum(range == 3), .groups = 'drop') %>%
      dplyr::mutate(across(tbr2:tir, ~ ./n))
  }

  return(df_tir)
}
