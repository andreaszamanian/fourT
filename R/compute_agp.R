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
compute_agp <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                        breaks = NULL, include_bv = T){
  #limitations/issues: cannot deal with NA values in `bg_value_num` column
  #when NA values are found in some time interval it does not ignore them
  #during computation, so just returns 'NA'

  #the ouput contains the df$inter integer value, as opposed to some data/time
  #range, should probably change this

  #Otherwise, the functions works


  check_lookback(lookback)
  df_dex <- change_window(df_dex = df_dex, start = start, end = end)
  df <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)

  #address include_bv
  if(include_bv == T){
    df <- convert_bv(df)
  }

  df$`bg_value_num` <- as.numeric(df$`bg_value_num`)

  #compute_agp
  #at this point above functions should have already checked that only one
  #of 'inter' and 'breaks' has been specified by user


  if(is.null(freq) == F){
    i <- 1
    while(i <= max(df$inter)){
      #find last date in that inter
      df_constrained <- dplyr::filter(df, inter == i)

      end_date <- df_constrained$bg_date_time[length(df_constrained$bg_date_time)]
      #<- lubridate::as_date(find_end_date(df_constrained))
      #use to compute start of window
      #start_date <- lubridate::as_date(end_date - lubridate::seconds(lookback*86400))
      start_date <- as.POSIXct(end_date - lubridate::seconds(lookback*86400))
      #find all observations within lookback window before last date

      df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

      df_constrained_b <- df_constrained_a%>%
        dplyr::mutate(range = cut(df_constrained_a$bg_value_num, c(0, 54, 70, 181, 251, Inf),
                                  include.lowest = T, labels = F))

      #compute agp in that window
      df_constrained_2 <- df_constrained_b%>%
        dplyr::ungroup()%>% #probably not necessary
        dplyr::summarise(
          n = dplyr::n(),
          tbr2 = sum(range == 1),
          tbr = sum(range <= 2),
          tar = sum(range >= 4),
          tar2 = sum(range == 5),
          tir = sum(range == 3), .groups = 'drop') %>%
        dplyr::mutate(across(tbr2:tir, ~ ./n))

      if(i == 1){
        df_tir <- df_constrained_2
      }
      else{
        df_tir <- dplyr::bind_rows(df_tir, df_constrained_2)
      }
      i <- i+1
    }
  }
  if(is.null(breaks) == F){
    df_tir <- df %>%
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
  return(df_tir)
}



#how to deal with incomplete time periods? e.g. if there are 3 days in the last
#14 day window

#could write a function 'check_complete_period' which looks if there
#is an observation whose 'inter' value is one greater (limitation to this:
#what if the data ends precisely at end of period, would return as incomplete
#even though the period is complete)





