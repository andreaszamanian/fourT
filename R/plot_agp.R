#' Canonical bar plot for AGP Thermometers
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param inter time interval
#'
#' @return
#' @export
#'
#' @examples
plot_agp <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL, breaks = NULL){
  check_lookback(lookback)
  df_dex <- change_window(df_dex = df_dex, start = start, end = end)
  df <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)
  df <- convert_bv(df)
  df$`bg_value_num` <- as.numeric(df$`bg_value_num`)
  dt_agp <- compute_agp_for_plot(df_dex = df, start = start, end = end, lookback = lookback,
                                 freq = freq, breaks = breaks)

  #transform data for plotting
  dt_agp_long <- dt_agp %>%
    tidyr::pivot_longer(cols = starts_with("range"),
                        names_to = "range",
                        values_to = "bg_level_dist") %>%
    dplyr::mutate(range = as.numeric(gsub("range", "", range)))


  if (!is.null(breaks)) {
    breaks_2 <- breaks[breaks != 0]
    x_labels <-  paste0(breaks_2, " month", ifelse(breaks_2 > 1, "s", ""))
  } else if (!is.null(freq)) {
    x_labels <- sapply(seq_len(nrow(dt_agp)) * freq, function(days) {
      if (days == 1) {
        return("1 day")
      } else {
        return(paste0(days, " days"))
      }
    })
  } else {
    x_labels <- as.character(seq_len(nrow(dt_agp)))  # Fallback
  }

  #Make the plot
  print(make_plot(dt_agp_long) +
    ggplot2::scale_x_discrete(labels = x_labels))
}

compute_agp_for_plot <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                                 breaks = NULL){
  df <- convert_bv(df_dex) #automatically converting boundary values
  df$bg_value_num <- as.numeric(df$bg_value_num)
  if(is.null(freq) == F){
    i <- 1
    while(i <= max(df$inter)){
      #find last date in that inter
      df_constrained <- dplyr::filter(df, inter == i)

      end_date <- as.POSIXct(df_constrained$labs[nrow(df_constrained)], tz = "UTC")
      end_date <- end_date + lubridate::seconds(86400)
      #<- lubridate::as_date(find_end_date(df_constrained))
      #use to compute start of window
      #start_date <- lubridate::as_date(end_date - lubridate::seconds(lookback*86400))
      start_date <- as.POSIXct(end_date - lubridate::seconds(lookback*86400), tz = "UTC")
      #find all observations within lookback window before last date

      df_constrained_a <- dplyr::filter(df, dplyr::between(df$bg_date_time, start_date, end_date))

      df_constrained_b <- df_constrained_a%>%
        dplyr::mutate(range = cut(df_constrained_a$bg_value_num, c(0, 54, 70, 181, 251, Inf),
                                  include.lowest = T, labels = F))

      df_constrained_2 <- df_constrained_b%>%
        dplyr::ungroup()%>% #probably not necessary
        dplyr::summarise(
          n = dplyr::n(),
          range1 = sum(range == 1),
          range2 = sum(range == 2),
          range3 = sum(range == 3),
          range4 = sum(range == 4),
          range5 = sum(range == 5),, .groups = 'drop') %>%
        dplyr::mutate(across(range1:range5, ~ ./n))

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
        range1 = sum(range == 1),
        range2 = sum(range == 2),
        range3 = sum(range == 3),
        range4 = sum(range == 4),
        range5 = sum(range == 5), .groups = 'drop') %>%
      dplyr::mutate(across(range1:range5, ~ ./n))
  }
  #add back the inter column
  df_tir <- df_tir %>%
    dplyr::mutate(inter = c(1:max(df$inter, na.rm = T)))
  return(df_tir)
}
