change_window <- function(df_dex, start, end){
  #start and end values
  if(start == "default"){
    start_date <- lubridate::as_date(find_start_date(df_dex)) + lubridate::days(1)
  } else{start_date <- start}
  if(end == "default"){
    end_date <- lubridate::as_date(find_end_date(df_dex)) - lubridate::days(1)
  } else{end_date <- end}
  if(start != "default" || end != "default"){
    df_dex <- truncate_window(df_dex, start = start_date, end = end_date)
  }
}


