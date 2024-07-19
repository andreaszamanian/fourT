#' Compute average glucose in given time periods
#'
#' @param df Data frame, read_dexcom output
#' @param human_run Boolean. For internal use, default set to TRUE.
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#'
#'
#' @return
#' @export
#'
#' @examples
compute_avg_glucose <- function(df, inter, human_run = T){
  if(human_run == T){
    df <- set_inter(df, inter)
  }
  #changes NA values to zero; THIS WAS JUST TO TEST THAT include_bv PARAMETER WORKS, REMOVE THIS!!!!
  df$bg_value_num <- replace(df$bg_value_num, is.na(df$bg_value_num), 0)

  #make numeric
  df$bg_value_num <- as.numeric(df$bg_value_num)

  df_avg <- df %>%
    dplyr::group_by(df$inter) %>%
    dplyr::summarise(
      n = dplyr::n(), #is this n counting the NA entries as well?
      bg_total = sum(bg_value_num),
      bg_mean = sum(bg_value_num)) %>%
    dplyr::mutate(across(bg_mean, ~ ./n))
}
