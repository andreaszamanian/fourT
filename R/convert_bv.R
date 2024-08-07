#' Converts high/low flags to numeric values
#'
#' @param df_dex Data frame outputted by read_dexcom
#'
#' @return Data frame with bg_value_num updated to include numeric values
#' associated with high/low flags
#' @export
#'
#' @examples
convert_bv <- function(df_dex){
  df <- df_dex
  i <- 1
  while(i <= length(df$`bg_value_num`)){
    if(is.na(df$`bg_value_flag`[i]) == T){
      #do nothing
    }
    else{
      if(df$`bg_value_flag`[i] == "High"){
        df$`bg_value_num`[i] = 400
      }
      if(df$`bg_value_flag`[i] == "Low"){
        df$`bg_value_num`[i] = 40
      }
    }
    i = i+1
  }
  return(df)
}
