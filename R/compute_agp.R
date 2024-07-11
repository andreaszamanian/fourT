#' Compute AGP Thermometers
#'
#' @param x Data frame with format identical to that outputted by read_dexcom
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#' @param include_bv Boolena. If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags
#'
#'
#' @return Data frame with AGP metrics
#' @export
#'
#' @examples
compute_agp <- function(x, inter, include_bv = T){
  if(include_bv == T){
    df <- convert_bv(x)
  }
  else{
    df <- x
  }
  df <- set_inter(df, inter)

  dt_tir <- df %>%
    dplyr::group_by(df$inter) %>% #i dont understand this grouping
    dplyr::mutate(range = cut(bg_value_num, c(0, 54, 70, 181, 251, Inf),
                       include.lowest = T, labels = F)) %>%
    dplyr::summarise(
      n = n(),
      tbr2 = sum(range == 1),
      tbr = sum(range <= 2),
      tar = sum(range >= 4),
      tar2 = sum(range == 5),
      tir = sum(range == 3), .groups = 'drop') %>%
    dplyr::mutate(across(tbr2:tir, ~ ./n))
}



#how to deal with incomplete time periods? e.g. if there are 3 days in the last
#14 day window




