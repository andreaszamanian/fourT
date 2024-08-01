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
plot_agp <- function(df_dex, start = "default", end = "default", freq = NULL, breaks = NULL){
  df_dex <- change_window(df_dex = df_dex, start = start, end = end)
  df <- partition_window(df_dex = df_dex, freq = freq, breaks = breaks)

  #recompute the different ranges, in format of compute_agp,
  #but with individual ranges
  df <- convert_bv(df)
  df_1 <- df #for future use in n_value vector
  df$`bg_value_num` <- as.numeric(df$`bg_value_num`) #write this into read_dexcom function?
  #or will NA values be an issue
  dt_agp <- df %>%
    dplyr::group_by(inter) %>% #redundant?
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
      make_plot(dt_agp_2)
      #store that plot somewhere?
    }
    i = i+1
  }
}

