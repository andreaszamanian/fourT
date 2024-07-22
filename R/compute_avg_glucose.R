#' Compute average glucose in given time periods
#'
#' @param df
#' @param start
#' @param end
#'
#'
#'
#' @return
#' @export
#'
#' @examples
compute_avg_glucose <- function(df){
  #changes NA values to zero
  df$bg_value_num <- replace(df$bg_value_num, is.na(df$bg_value_num), 0)

  #make numeric
  df$bg_value_num <- as.numeric(df$bg_value_num)

  df_avg <- df %>%
    dplyr::group_by(df$inter) %>%
    dplyr::summarise(
      n = dplyr::n(),
      bg_total = sum(bg_value_num),
      bg_mean = sum(bg_value_num)) %>%
    dplyr::mutate(across(bg_mean, ~ ./n))
}
