#' Cohort cgm_summaries
#'
#' @param cohort_dexcom_output
#' @param indiv
#' @param ordered
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
#' record_id <- c(185637, 908716, 111665, 389966, 488888, 523452, 111110)
#' bg_value_flag <- c(NA, NA, NA, NA, NA, NA, NA)
#'
#' df <- dplyr::tibble(bg_date_time = bg_date_time, bg_value_num = bg_value_num, record_id = record_id,
#' bg_value_flag = bg_value_flag)
#'
#' cgm_summaries(df, breaks = c(0, 1.5, 4.5, 7, 10, 13), plot = F)
cohort_cgm_summaries <- function(cohort_dexcom_output, indiv = F, ordered = T,
                                 start = "default", end = "default",
                                 lookback = 90, freq = NULL,
                                 breaks = NULL, include_bv = T, plot = F
                                 ){
  if(indiv == F){
    if(ordered == T){
      cgm_summaries(cohort_dexcom_output, start = start, end = end, lookback = lookback,
                    freq = freq, breaks = breaks, include_bv = include_bv, plot = plot)
    }
    else{
      stop("Data must be ordered if 'indiv' is set to FALSE")
    }
  } else{
    stop("This method has not been created yet")
    #compute for each individual data frame
    #cohort_dexcom_output <-
  }
}
