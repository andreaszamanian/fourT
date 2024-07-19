#' Canonical bar plot for AGP Thermometers
#'
#' @param df read_dexcom output
#' @param inter time interval
#'
#' @return
#' @export
#'
#' @examples
plot_agp <- function(df, inter){
  #recompute the different ranges, in format of compute_agp,
  #but with individual ranges
  df <- convert_bv(df)
  df <- set_inter(df, interval = inter)

  df_1 <- df #for future use in initializing n_value vector
  df$`bg_value_num` <- as.numeric(df$`bg_value_num`) #write this into read_dexcom function

  dt_agp <- df %>%
    dplyr::group_by(inter) %>% #redundant
    dplyr::mutate(range = cut(bg_value_num, c(0, 54, 70, 181, 251, Inf),
                              include.lowest = T, labels = F)) %>%
    dplyr::summarise(
      n = dplyr::n(),
      range1 = sum(range == 1),
      range2 = sum(range == 2),
      range3 = sum(range == 3),
      range4 = sum(range == 4),
      range5 = sum(range == 5),
       .groups = 'drop') %>%
    dplyr::mutate(across(range1:range5, ~ ./n))

  print(dt_agp)

  print("Initial df computed ") ############################
  #can make all of this cleaner?
  i <- 1
  print(max(dt_agp$inter, na.rm = T))
  while(i <= max(dt_agp$inter, na.rm = T)){
    print("inside")
    #map each inter row to a new df
    j <- as.character(i)

    title <- paste("time period", j, sep = " ")
    dt_agp_a <- dt_agp %>%
      dplyr::filter(inter == i) #need to make sure that typeof(inter) is numeric

    print("First part of first while loop completed") ############################

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
      print("Due to missing values plot cannot be produced")
    }
    else{
      make_plot(dt_agp_2)
      #store that plot somewhere?
    }
    print("loop iteration finished") ############################
    i = i+1
  }
}
#bg_level_dist is the proportions for the time in whatever range
#bg_level is which range in, I changed all calls of bg_level to 'range'

make_plot <- function(df){
  print("making a plot") ############################
  df <- df %>%
    dplyr::mutate(range = as.character(df$range)) #move this outside of the make_plot() function

  df <- df %>%
    dplyr::mutate(range = as.factor(df$range),
                  range = forcats::fct_recode(
                    range,
                    '<54 mg/dL' = '1',
                    '54-69 mg/dL' = '2',
                    '70-180 mg/dL' = '3',
                    '181-250 mg/dL' = '4',
                    '>250 mg/dL' = '5'),
                  txt = paste0(round(100*bg_level_dist,1), '%')) %>%
    dplyr::mutate(cumpct = cumsum(bg_level_dist))

    print(df)

    df <- df %>%
    dplyr::ungroup()


    print(df %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = range, y = bg_level_dist, fill = range), color = 'black') +
    ggplot2::geom_text(ggplot2::aes(x = range, y = cumpct, label = txt)) +
    ggplot2::labs(x = 'Time (months)',
                  y = '% Glucose Range',
                  fill = '') +
    ggplot2::scale_fill_manual(values = c('#fd8d3c', '#fecc5c', '#a6d96a', '#d7191c', '#a6611a')) +
    ggplot2::scale_y_continuous(labels = scales::percent))
}

