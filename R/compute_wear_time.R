#' Compute wear time for individual patient
#'
#' @param df Data frame, in format of read_dexcom output
#' @param start Date of CGM initiation
#' @param end End of study
#' @param fumonths Study window of interest (as months since T1D onset)
#'
#' @return Individual wear time
#' @export
#'
#' @examples
compute_wear_time <- function(df, start = "default", end = "default", fumonths){

  #default start and end values
  if(start == "default"){
    start_date <- lubridate::as_date(find_start_date(df))
  } else{start_date <- start}
  if(start == "default"){
    end_date <- lubridate::as_date(find_end_date(df))
  } else{end_date <- end}

  df <- convert_bv(df)
  ncgm <- df %>%
    dplyr::mutate(date = lubridate::as_date(df$bg_date_time), .keep = "none") %>%
    dplyr::count(date)

  df_wear <- tibble::tibble(
    date = seq.Date(start_date + 1, end_date - 1, by = 'days'),
    #day_number indexed starting from 0 (i.e. day 0 is one day
    #after original start date (start_date))
    day_number = difftime(date, min(date), units = 'day') %>% as.numeric()) %>%
    dplyr::left_join(ncgm, by = dplyr::join_by(date)) %>%
    #dplyr::mutate(record_id = df$record_id[x], #here
           #n = ifelse(is.na(n), 0, n))
    dplyr::mutate(wear_prop = pmin(1, n/288))

  x <- sum(df_wear$wear_prop/length(df_wear$wear_prop))
  x <- as.character(x)

  df_wear <- df_wear %>% dplyr::mutate(total_wear_prop = x)

  #maybe remove this
  #print(paste("Total wear proportion:", x, sep = " "))

  return(df_wear)
}

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
