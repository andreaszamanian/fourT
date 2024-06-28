#' Compute GMI
#'
#' Computes the glucose management indicator for a given time frame
#' @param x A data frame (in the form of read_dexcom output)
#' @param y Time period length in weeks
#' @param z Time period length in days
#' @param exclude_bv Boolean. Compute GMI with high/low values as 400/___ respectively?
#' @param allow_norm Boolean. Compute GMI with time periods 'rounded' to start of day
#'
#' @return
#' @export
#'
#' @examples
compute_gmi <- function(x,
                        y = 0,
                        z = 0,
                        exclude_bv = FALSE,
                        allow_norm = TRUE
                        ) {
  frame <- x
  period_days <- (7*y)+z

  #chop up frame into certain date-time ranges
  cut_frame <- as.vector.factor(cut(frame$`bg_date_time`, breaks = period_days))

  #for each date-time range compute mean(bg_value_num)
    #count number of observations in resp. time-range
    #sum total bg_value_num in that time range
    #mean formula

  #return df with means for each range

  #compute gmi per time range

  #return df with gmi for each range




  glucose_mean =
  gmi <- 3.31 + (0.02392* glucose_mean)
}


#takes as input the data frame that read_dexcom outputs
#x = df
#y = how many weeks per period
#gmi function comes from:
#https://diabetesjournals.org/care/article/42/4/e60/36083/Glucose-Management-Indicator-GMI-Insights-and



