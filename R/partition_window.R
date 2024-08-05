#' Partition the time window into intervals or breaks
#'
#' @param df_dexcom
#' @param freq
#' @param breaks
#'
#' @return
#' @export
#'
#' @examples
partition_window <- function(df_dexcom, freq = NULL, breaks = NULL, compute = F){
  if(is.null(freq) == F && is.null(breaks) == F){
    stop("Either specify 'freq' or 'breaks' parameter, but not both")
  }
  if(is.null(freq) == F){
    if(compute == T){
      df <- set_inter(df_dex = df_dexcom, interval = freq, compute = T)
    } else{
      #set interval from `inter`
      df <- set_inter(df_dex = df_dexcom, interval = freq)
    }

  }
  if(is.null(breaks) == F){
    start <- as.POSIXct(df_dexcom$bg_date_time[1], tz = "UTC")
    end <- as.POSIXct(start + (max(breaks, na.rm = T) * 2613600), tz = "UTC")
    #constrain to given intervals
    df <- change_window(df_dex = df_dexcom, start = start, end = end)
    #set interval from `breaks`
    df <- set_inter(df_dex = df, breaks = breaks, cut_reference = "start")
  }
  return(df)
}
