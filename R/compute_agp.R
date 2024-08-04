#' Compute AGP Thermometers
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter and computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param freq Time interval for computation; e.g. every 14 days, every month, etc.
#' @param breaks Vector. Specify range computations manually in months, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
#' would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5.
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
  check_lookback(lookback)
  df_dex <- change_window(df_dex = df_dex, start = start, end = end)
  df <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)

  #address include_bv
  if(include_bv == T){
    df <- convert_bv(df)
  }

  df$`bg_value_num` <- as.numeric(df$`bg_value_num`)

    previous_end_date <- as.POSIXct("0001-01-01 01:01:01")
    i <- 1
    while(i <= max(df$inter)){
      #find last date in that inter
      df_constrained <- dplyr::filter(df, inter == i)
      print(nrow(df_constrained))
      if (nrow(df_constrained) > 0) {
        end_date <- df_constrained$bg_date_time[length(df_constrained$bg_date_time)]
        start_date <- as.POSIXct(end_date - lubridate::seconds(lookback*86400))
        if (!is.na(start_date) && !is.na(end_date)) { #then actually do something
          df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))
          df_constrained_b <- df_constrained_a%>%
            dplyr::mutate(range = cut(df_constrained_a$bg_value_num, c(0, 54, 70, 181, 251, Inf),
                                      include.lowest = T, labels = F))



          #checking enough data in lookback window
          if(check_days_minimum(df_constrained_b) == F){
            df_constrained_2 <- df_constrained_b%>%
              dplyr::ungroup()%>% #probably not necessary
              dplyr::summarise(
                n = dplyr::n(),
                tbr2 = NA,
                tbr = NA,
                tar = NA,
                tar2 = NA,
                tir = NA, .groups = 'drop')
            print(paste0("Time interval ", i, " does not have enough data, values set to NA"))
          }
          else{
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
          }
        }
        #find all observations within lookback window before last date
        print(paste("Start date:", start_date))
        print(paste("End date:", end_date))
        print(paste("Number of rows in df_constrained:", nrow(df_constrained)))
      }
      else{
        if(nrow(df_constrained) == 0){
            end_date <- as.POSIXct(previous_end_date)
            start_date <- as.POSIXct(end_date - lubridate::seconds(lookback*86400))
            print("no time1")
            print(start_date)
            print(end_date)
            print("no time2")
            df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))
            if(nrow(df_constrained_a) > 0){
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
            }
            else{
              df_constrained_2 <- df_constrained_b%>%
                dplyr::ungroup()%>% #probably not necessary
                dplyr::summarise(
                  n = dplyr::n(),
                  tbr2 = NULL,
                  tbr = NULL,
                  tar = NULL,
                  tar2 = NULL,
                  tir = NULL, .groups = 'drop')
              print("Empty lookback window.")
            }
        }
      }

      if(i == 1){
        df_tir <- df_constrained_2
      }
      else{
        df_tir <- dplyr::bind_rows(df_tir, df_constrained_2)
      }
      i <- i+1
      previous_end_date <- end_date
    }
  return(df_tir)
}





