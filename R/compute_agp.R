#' Compute AGP Thermometers
#'
#' @param x Data frame outputted by read_dexcom
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
compute_agp <- function(x, inter = 14, include_bv = T){
  #limitations/issues: cannot deal with NA values in `bg_value_num` column
  #when NA values are found in some time interval it does not ignore them
  #during computation, so just returns 'NA'

  #the ouput contains the df$inter integer value, as opposed to some data/time
  #range, should probably change this

  #Otherwise, the functions works

  if(include_bv == T){
    df <- convert_bv(x)
  }
  else{
    df <- x
  }
  df <- set_inter(df, inter)

  df$`bg_value_num` <- as.numeric(df$`bg_value_num`)

  dt_tir <- df %>%
    dplyr::group_by(inter) %>%
    dplyr::mutate(range = cut(bg_value_num, c(0, 54, 70, 181, 251, Inf),
                       include.lowest = T, labels = F)) %>%
    dplyr::summarise(
      n = dplyr::n(),
      tbr2 = sum(range == 1),
      tbr = sum(range <= 2),
      tar = sum(range >= 4),
      tar2 = sum(range == 5),
      tir = sum(range == 3), .groups = 'drop') %>%
    dplyr::mutate(across(tbr2:tir, ~ ./n))
}



#how to deal with incomplete time periods? e.g. if there are 3 days in the last
#14 day window

#could write a function 'check_complete_period' which looks if there
#is an observation whose 'inter' value is one greater (limitation to this:
#what if the data ends precisely at end of period, would return as incomplete
#even though the period is complete)




