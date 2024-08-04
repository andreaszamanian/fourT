#' Find end of inter period
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
find_end_date <- function(x){
  i = length(x$bg_date_time)
  check <- F
  while(i > 0 && check == F){
    if(is.na(x$bg_date_time[i]) == F){
      end_date <- x$bg_date_time[i]
      check = T
    } else{i <- i-1}
  }
  return(end_date)
}
