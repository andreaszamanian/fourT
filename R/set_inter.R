#' Creates 'inter' column for read_dexcom ouptut data frame
#'
#' @param x Input data frame
#' @param interval Integer. Time interval, in days.
#'
#' @return Input data frame with new column 'inter' which is an integer
#' indicating which time interval an observation is from
#'
#' @export
#'
#' @examples
set_inter <- function(x, interval){
  #creates new 'inter' column which records which interval an observation
  #is part of. Intervals are formed and then assigned a unique integer
  #(ascending, starting from 1), which is then assigned to the observation

  #current limitations: cannot deal with non-integer inputs, e.g. 0.5
  #otherwise, working

  df <- x
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
  interval_ints <- cut(df$`bg_date_time`, breaks = interval, labels = FALSE)
  df <- df %>% dplyr::mutate(inter = interval_ints)
  return(df)
}
