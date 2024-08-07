#' Compute AGP Thermometers
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#' @param breaks Vector. Specify range computations manually, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
#' would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5. EDIT, does not describe well.
#' @param include_bv Boolean. If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags
#'
#'
#' @return Data frame with AGP metrics
#' @export
#'
#' @examples
compute_agp <- function(df_dex, start = "default", end = "default", inter = NULL, breaks = NULL, include_bv = T){
  #limitations/issues: cannot deal with NA values in `bg_value_num` column
  #when NA values are found in some time interval it does not ignore them
  #during computation, so just returns 'NA'

  #the ouput contains the df$inter integer value, as opposed to some data/time
  #range, should probably change this

  #Otherwise, the functions works


  #start and end values
  if(start == "default"){
    start_date <- lubridate::as_date(find_start_date(df_dex))
  } else{start_date <- start}
  if(end == "default"){
    end_date <- lubridate::as_date(find_end_date(df_dex))
  } else{end_date <- end}
  if(start != "default" || end != "default"){
    df_dex <- truncate_window(df_dex, start = start_date, end = end_date)
  }

  #address include_bv
  if(include_bv == T){
    df <- convert_bv(df_dex)
  }
  else{
    df <- df_dex
  }

  if(is.null(inter) == F && is.null(breaks) == F){
    stop("Either specify 'inter' or 'breaks' parameter, but not both")
  }
  if(is.null(inter) == F){
    #set interval from `inter`
    df <- set_inter(df, inter = inter)
  }
  if(is.null(breaks) == F){
    #set interval from `breaks`
    df <- set_inter(df, breaks = breaks)
  }



  #compute_agp
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




