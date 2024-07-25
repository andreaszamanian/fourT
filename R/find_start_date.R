find_start_date <- function(x){
  i = 1
  check <- F
  while(i < length(x$bg_date_time)+1 && check == F){
    if(is.na(x$bg_date_time[i]) == F){
      start_date <- x$bg_date_time[i]
      check = T
    } else{ i <- i+1}
  }
  return(start_date)
}
