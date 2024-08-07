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
set_inter <- function(df_dex, interval = NULL, breaks = NULL, cut_reference = "end"){
  #creates new 'inter' column which records which interval an observation
  #is part of. Intervals are formed and then assigned a unique integer
  #(ascending, starting from 1), which is then assigned to the observation

  #current limitations: cannot deal with non-integer inputs, e.g. 0.5
  #also, issue when cut_reference = "end" and dealing with if there are NA values
  #breaks cannot deal with non_integer values
  #otherwise, working

  df <- df_dex
  if(is.null(interval) == F){
    if(interval <= 0){
      stop("Nonpositive number of days inputted for 'inter' parameter. Please input
         positive number of days for 'inter' parameter")
    }
    if(interval != 1){
      period_days <- as.character(interval)
      day_or_days <- "days"
    }
    if(interval == 1){
      period_days <- as.character(interval)
      day_or_days <- "day"
    }
  }


  #using breaks argument
  if(is.null(breaks) == T){
    breaks_arg <- paste(period_days, day_or_days, sep = " ") #interval is now a string "# days"
  }
  else{
    start = as.POSIXct(df$`bg_date_time`[1])
    breaks_arg <- start + months(breaks)
  }

  if(cut_reference == "end"){
    interval_ints <- cut(rev(df$`bg_date_time`), breaks = breaks_arg, labels = FALSE)
    interval_ints <- max(interval_ints, na.rm = T) - (interval_ints-1)
  } else{
      if(cut_reference == "start"){
        interval_ints <- cut(df$`bg_date_time`, breaks = breaks_arg, labels = FALSE)
      } else{
        stop("Incorrect cut_reference input, see function documentation")
      }
  }

  df <- df %>% dplyr::mutate(inter = interval_ints)
  return(df)
}
