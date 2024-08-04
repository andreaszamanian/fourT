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
set_inter <- function(df_dex, interval = NULL, breaks = NULL, cut_reference = "end", compute = F){
  #creates new 'inter' column which records which interval an observation
  #is part of. Intervals are formed and then assigned a unique integer
  #(ascending, starting from 1), which is then assigned to the observation

  #also, issue when cut_reference = "end" and dealing with if there are NA values
  #otherwise, working
  df <- df_dex

  df$bg_date_time <- as.POSIXct(df$bg_date_time, tz = "UTC")
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

  if(is.null(breaks) == T){
    breaks_arg <- paste(period_days, day_or_days, sep = " ") #interval is now a string "# days"
    #breaks_arg <- seq(max(df$bg_date_time) - lubridate::days(interval), min(df$bg_date_time), by = -interval * 86400)

  }
  else{
    breaks = breaks * 2613600 #2613600 is 30.25 days (i.e. 1 month) in seconds (30.25*24*60*60)
    start = as.POSIXct(df$`bg_date_time`[1], tz = "UTC")
    breaks_arg <- start + lubridate::seconds(breaks)
  }

  if(cut_reference == "end"){
    reverse_times <- rev(as.POSIXct(df$bg_date_time, tz = "UTC"))
    interval_ints <- cut(reverse_times, breaks = breaks_arg, labels = FALSE)
    interval_ints <- rev(interval_ints)

    #interval_ints <- max(interval_ints, na.rm = T) - (interval_ints-1)
  } else{
      if(cut_reference == "start"){
        interval_ints <- cut(df$`bg_date_time`, breaks = breaks_arg, labels = FALSE)
      } else{
        stop("Incorrect cut_reference input, see function documentation")
      }
  }
  df <- df %>% dplyr::mutate(inter = interval_ints)

  if(compute == T){
    interval_labs <- cut(reverse_times, breaks = breaks_arg)
    interval_labs <- rev(interval_labs)
    df <- df %>% dplyr::mutate(labs = interval_labs)
  }

  return(df)
}

