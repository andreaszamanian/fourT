#' Compute GMI
#'
#' Computes the glucose management indicator for a given time frame.
#' GMI is computed following the results of:
#' https://diabetesjournals.org/care/article/42/4/e60/36083/Glucose-Management-Indicator-GMI-Insights-and
#'
#' @param x A data frame (in the form of read_dexcom output)
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#' @param include_bv If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags
#'
#' @return
#' @export
#'
#' @examples
compute_gmi <- function(x, inter, include_bv = T) {
  if(inter <= 0){
    stop("Nonpositive timer interval inputted. Please input
         positive number of days")
  }

  df <- set_inter(x, inter)
  if(include_bv == T){
    df <- convert_bv(df)
  }

  #setting up df
  #GETTING ERROR HERE
  time_period_totals <- matrix(NA, nrow = max(df$inter, na.rm = T), ncol = 5)
  colnames(time_period_totals)[which(names(time_period_totals) == "V1")] <- "time_period"
  colnames(time_period_totals)[which(names(time_period_totals) == "V2")] <- "bg_total"
  colnames(time_period_totals)[which(names(time_period_totals) == "V3")] <- "obs_count"
  colnames(time_period_totals)[which(names(time_period_totals) == "V4")] <- "bg_mean"
  colnames(time_period_totals)[which(names(time_period_totals) == "V5")] <- "gmi"

  #this logic seems inefficient?
  i <- 1
  #i is which time period

  while(i <= max(df$inter, na.rm = T)){
    time_period_totals[i, time_period] <- df$inter[i]

    running_total = 0
    running_count = 0
    j <- 1
    while(j <= length(df$inter)){
      if(df$inter[j] == i){
        if(is.na(bg_values_num) == FALSE){
          running_total = running_total + df[j, bg_value_num]
        }
        #otherwise do nothing
      }
      j = j+1
      #
      time_period_totals[i , bg_total] <- running_total
      running_count = running_count+1
    }
    time_period_totals[i, obs_count] <- running_count
    i = i+1
  }

  time_period_totals$bg_mean <- (time_period_totals$bg_total)/(time_period_totals$obs_count)
  gmi <- 3.31 + (0.02392* time_period_totals$bg_mean)
}




