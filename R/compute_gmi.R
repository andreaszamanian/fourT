#' Compute GMI
#'
#' Computes the glucose management indicator for a given time frame.
#' GMI is computed following the results of:
#' https://diabetesjournals.org/care/article/42/4/e60/36083/Glucose-Management-Indicator-GMI-Insights-and
#'
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
                        allow_norm = TRUE #maybe remove this, would be a bit
                        #of work to make another method which would do same thing
                        #but with precision of start second
                        ) {
  frame <- x
  period_days <- (7*y)+z
  if(period_days != 1){
    period_days <- as.character(period_days)
    day_or_days <- "days"
  }
  if(period_days == 1){
    period_days <- as.character(period_days)
    day_or_days <- "day"
  }
  breaks_input <- paste(period_days, day_or_days, sep = " ")

  #print(breaks_input)

  #chop up frame into certain date-time ranges
  cut_frame <- frame

  #THIS IS LAST ISSUE TO FIX
  #edit call to period_days here
  cut_frame_ints <- cut(frame$`bg_date_time`, breaks = breaks_input)
  cut_frame <- as.vector.factor(cut(frame$`bg_date_time`, breaks = breaks_input))

  #setting up df
  time_period_totals <- matrix(NA, nrow = max(cut_frame_ints), ncol = 5)
  colnames(time_period_totals)[which(names(time_period_totals) == "V1")] <- "time_period"
  colnames(time_period_totals)[which(names(time_period_totals) == "V2")] <- "bg_total"
  colnames(time_period_totals)[which(names(time_period_totals) == "V3")] <- "obs_count"
  colnames(time_period_totals)[which(names(time_period_totals) == "V4")] <- "bg_mean"
  colnames(time_period_totals)[which(names(time_period_totals) == "V5")] <- "gmi"

  #this logic seems inefficient?
  i <- 1
  #i is which time period

  while(i <= max(cut_frame_ints)){
    #fill in the correct time periods
    time_period_totals[i, time_period] <- cut_frame[levels(cut_frame)[i]]

    #iterating variable
    j <- 1

    #running total bg
    running_total = 0
    running_count = 0

    while(j <= length(cut_frame_ints)){
      if(cut_frame_ints[j] == i){
        if(is.na(bg_values_num) == FALSE){
          running_total = running_total + frame[j, bg_value_nums]
        }
        #check whether the flag is high or low, and whether exclude_bv is TRUE
        #or FALSE
        else{
          if(exlude_bv == FALSE){
            if(bg_value_flag[j] == "High"){
              running_total = running_total + 300
            }
            if(bg_value_flag[j] == "Low"){
              running_total = running_total + 65
            }
          }
          #otherwise do nothing
        }
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




