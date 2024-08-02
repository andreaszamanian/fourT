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
  df_1 <- df #for future use in n_value vector
  df$`bg_value_num` <- as.numeric(df$`bg_value_num`) #write this into read_dexcom function?
  #or will NA values be an issue

  print(df)
  dt_agp <- compute_agp_for_plot(df_dex = df, start = start, end = end, lookback = lookback,
                                 freq = freq, breaks = breaks)

  df <- df %>%
    dplyr::mutate(range = as.character(df$range)) #move this outside of the make_plot() function

  #making the underlying plot graphic
  df %>%
    dplyr::mutate(range = as.factor(df$range),
                  range = forcats::fct_recode(
                    range,
                    '<54 mg/dL' = '1',
                    '54-69 mg/dL' = '2',
                    '70-180 mg/dL' = '3',
                    '181-250 mg/dL' = '4',
                    '>250 mg/dL' = '5'),
                  range = forcats::fct_rev(range),
                  txt = paste0(round(100*bg_level_dist,1), '%')) %>%
    dplyr::mutate(cumpct = cumsum(bg_level_dist)) %>%
    dplyr::ungroup()%>%
    ggplot2::ggplot() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank()) +
    #making the bars ###############################################################





  #can make all of this cleaner?
  i <- 1
  while(i <= max(dt_agp$inter, na.rm = T)){
    #map each inter row to a new df
    j <- as.character(i)
    title <- paste("time period", j, sep = " ")
    dt_agp_a <- dt_agp %>%
      dplyr::filter(inter == i) #need to make sure that typeof(inter) is numeric
    #store n value
    n_value <- vector(mode = "integer", length = length(`df_1`$inter))
    n_value[i] = dt_agp[1, 2]
    #'vectorize' this
    k <- 1
    bg_level_dist_a <- vector(mode = "double", length = 5)
    while(k <= 5){
      bg_level_dist_a[k] <- dt_agp_a[1, k+2]
      k = k+1
    }
    dt_agp_2 <- tibble::tibble(
      .rows = 5,
      range = c(1,2,3,4,5),
      bg_level_dist = as.numeric(bg_level_dist_a)
    )
    #assign the modified dt_agp to the title name; have NOT checked if this works yet
    assign(title, dt_agp_2)
    if(is.na(dt_agp_2[1, 2]) == T || is.na(dt_agp_2[2, 2]) == T ||
       is.na(dt_agp_2[3, 2]) == T || is.na(dt_agp_2[4, 2]) == T ||
       is.na(dt_agp_2[5, 2]) == T){
      stop("Due to missing values plot cannot be produced")
    }
    else{
      #make_plot(dt_agp_2)
      #store that plot somewhere?
    }
    dt_agp_2 <- dt_agp_2 %>%
      dplyr::mutate(inter = i)
    i = i+1
  }

  print(dt_agp_2)

  ##########################################################################################











  #print(dt_agp)
  #make_plot(dt_agp)
}


compute_agp_for_plot <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                                 breaks = NULL){
  df <- convert_bv(df_dex) #automatically converting boundary values
  df$`bg_value_num` <- as.numeric(df$`bg_value_num`)
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

