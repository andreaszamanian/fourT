change_window <- function(df_dex, start, end){
  #start and end values
  if(as.character(start) == "default"){
    start_date <- lubridate::as_date(find_start_date(df_dex)) + lubridate::days(1)
  } else{start_date <- start}
  if(as.character(end) == "default"){
    end_date <- lubridate::as_date(find_end_date(df_dex)) - lubridate::days(1)
  } else{end_date <- end}
  if(as.character(start) != "default" || as.character(end) != "default"){
    df_dex <- truncate_window(df_dex, start = start_date, end = end_date)
  }
  return(df_dex)
}


