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
partition_window <- function(df_dexcom, freq = NULL, breaks = NULL){
  if(is.null(freq) == F && is.null(breaks) == F){
    stop("Either specify 'freq' or 'breaks' parameter, but not both")
  }
  if(is.null(freq) == F){
    #set interval from `inter`
    df <- set_inter(df_dex = df_dexcom, interval = freq)
  }
  if(is.null(breaks) == F){
    #set interval from `breaks`
    df <- set_inter(df_dex = df_dexcom, breaks = breaks, cut_reference = "start")
  }
  return(df)
}
