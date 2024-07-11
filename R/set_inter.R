set_inter <- function(x, interval){
  #creates new 'inter' column which records which interval an observation
  #is part of. Intervals are formed and then assigned a unique integer
  #(ascending, starting from ?what integer? (0 or 1)), which is then assigned to the observation

  df <- x
  if(inter <= 0){
    stop("Nonpositive number of days inputted. Please input
         positive number of days")
  }
  if(interval != 1){
    period_days <- as.character(period_days)
    day_or_days <- "days"
  }
  if(interval == 1){
    period_days <- as.character(period_days)
    day_or_days <- "day"
  }
  interval <- paste(period_days, day_or_days, sep = " ") #interval is now a string "# days"
  interval_ints <- cut(df$`bg_date_time`, breaks = interval, labels = FALSE)
  df <- df %>% mutate(inter = interval_ints)
  return(df)
}
