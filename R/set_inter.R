#' Creates 'inter' column for read_dexcom ouptut data frame
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param interval Integer. Time interval, in days.
#' @param cut_reference Either "end" or "start". Which endpoint should intervals be cut
#' starting from. Default value is "end", since usually computations are made in some
#' 'lookback' period.
#'
#' @return Input data frame with new column 'inter' which is an integer
#' indicating which time interval an observation is from
#'
#' @export
#'
#' @examples
set_inter <- function(df_dex, interval, cut_reference = "end"){
  #creates new 'inter' column which records which interval an observation
  #is part of. Intervals are formed and then assigned a unique integer
  #(ascending, starting from 1), which is then assigned to the observation

  #current limitations: cannot deal with non-integer inputs, e.g. 0.5
  #also, issue when cut_reference = "end" and dealing with if there are NA values
  #otherwise, working

  df <- df_dex
  if(interval <= 0){
    stop("Nonpositive number of days inputted. Please input
         positive number of days")
  }
  if(interval != 1){
    period_days <- as.character(interval)
    day_or_days <- "days"
  }
  if(interval == 1){
    period_days <- as.character(interval)
    day_or_days <- "day"
  }
  interval <- paste(period_days, day_or_days, sep = " ") #interval is now a string "# days"
  if(cut_reference == "end"){
    interval_ints <- cut(rev(df$`bg_date_time`), breaks = interval, labels = FALSE)
    interval_ints <- max(interval_ints, na.rm = T) - (interval_ints-1)
  } else{
      if(cut_reference == "start"){
        interval_ints <- cut(df$`bg_date_time`, breaks = interval, labels = FALSE)
      } else{
        stop("Incorrect cut_reference input")
      }
  }

  df <- df %>% dplyr::mutate(inter = interval_ints)
  return(df)
}
