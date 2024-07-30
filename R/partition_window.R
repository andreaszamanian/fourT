partition_window <- function(df_dexcom, freq, breaks){
  if(is.null(freq) == F && is.null(breaks) == F){
    stop("Either specify 'freq' or 'breaks' parameter, but not both")
  }
  if(is.null(freq) == F){
    #set interval from `inter`
    df <- set_inter(df, freq)
  }
  if(is.null(breaks) == F){
    #set interval from `breaks`
    df <- set_inter(df, breaks = breaks, cut_reference = "start")
  }
  return(df)
}
