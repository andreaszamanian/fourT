#' Compute GMI
#'
#' Computes the glucose management indicator for a given time frame.
#' GMI is computed following the results of:
#' https://diabetesjournals.org/care/article/42/4/e60/36083/Glucose-Management-Indicator-GMI-Insights-and
#'
#' @param df_dex Data frame outputted by read_dexcom
#' @param start Date-time. Computation window start date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param end Date-time. Computation window end date. Value of "default" means
#' function ignores this parameter at computes up to the start of the data.
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#' @param breaks Vector. Specify range computations manually, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
#' would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5. EDIT, does not describe well.
#' @param include_bv If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags
#'
#' @return
#' @export
#'
#' @examples
compute_gmi <- function(df_dex, start = "default", end = "default", inter = NULL,
                        breaks = NULL, include_bv = T) {

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

  #setting interval
  if(is.null(inter) == F && is.null(breaks) == F){
    stop("Either specify 'inter' or 'breaks' parameter, but not both")
  }
  if(is.null(inter) == F){
    #set interval from `inter`
    df <- set_inter(df, inter = inter)
  }
  if(is.null(breaks) == F){
    #set interval from `breaks`
    df <- set_inter(df, breaks = breaks, cut_reference = "start")
  }

  #compute gmi
  df_avg <- compute_avg_glucose(df)
  df_gmi <- df_avg %>%
    dplyr::mutate(gmi = 3.31 + (0.02392* df_avg$bg_mean))
}

