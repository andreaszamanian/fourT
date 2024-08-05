#' Computes all metrics
#'
#' Computes AGP thermometers, GMI and average glucose with the option to create the AGP thermometers plot
#' by calling each of the respective individual function.
#' Like the other computation only one of 'freq' and 'breaks' may be specified. Start and end dates may
#' also be specified. It is recommended that this is only used when the data is not deidentified.
#' There is also the option
#' to not include boundary values.
#'
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
#' @return Data frame. Will print AGP plot if requested.
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
#' df <- dplyr::tibble(bg_date_time = bg_date_time, bg_value_num = bg_value_num, record_id = record_id,
#' bg_value_flag = bg_value_flag)
#'
#' cgm_summaries(df, breaks = c(0, 1.5, 4.5, 7, 10, 13), plot = F)
#'
cgm_summaries <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                          breaks = NULL, include_bv = T, plot = F){
  check_lookback(lookback)
  df_dex1 <- change_window(df_dex = df_dex, start = start, end = end)
  df <- partition_window(df_dex = df_dex1, freq = freq, breaks = breaks)

  agp <- compute_agp(df_dex = df, lookback = lookback, freq = freq, breaks = breaks, include_bv = include_bv)
  gmi <- compute_gmi(df_dex = df, lookback = lookback, freq = freq, breaks = breaks, include_bv = include_bv)
  gmi <- gmi[ , !names(gmi) %in% "n"] #removing the n column here because it already exists in agp
  #something to do with wear time
  df_summary <- dplyr::bind_cols(agp, gmi)

  if(plot == T){
    if (!is.null(breaks)) {
      x_labels <- paste0(seq_len(nrow(df_dex)), " month", ifelse(seq_len(nrow(df_dex)) > 1, "s", ""))
    } else if (!is.null(freq)) {
      x_labels <- sapply(seq_len(nrow(df_dex)) * freq, function(days) {
        if (days == 1) {
          return("1 day")
        } else {
          return(paste0(days, " days"))
        }
      })
    } else {
      x_labels <- as.character(seq_len(nrow(df_dex)))  # Fallback
    }

    plot_agp(df_dex = df_dex, start = start, end = end, lookback = lookback, freq = freq,
             breaks = breaks) #+
      #ggplot2::scale_x_discrete(labels = x_labels)
  }

  return(df_summary)
}
