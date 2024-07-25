#' Compute wear time for individual patient
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date of CGM initiation.
#' @param end End of study
#' @param fumonths Study window of interest (as months since T1D onset)
#'
#' @return Individual wear time
#' @export
#'
#' @examples
compute_wear_time <- function(df_dex, start = "default", end = "default", fumonths){

  #default start and end values, this method will be slightly simpler than
  #that for the computation functions
  if(start == "default"){
    start_date <- lubridate::as_date(find_start_date(df_dex))
  } else{start_date <- start}
  if(start == "default"){
    end_date <- lubridate::as_date(find_end_date(df_dex))
  } else{end_date <- end}

  df <- convert_bv(df_dex)
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
