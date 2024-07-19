#' Compute GMI
#'
#' Computes the glucose management indicator for a given time frame.
#' GMI is computed following the results of:
#' https://diabetesjournals.org/care/article/42/4/e60/36083/Glucose-Management-Indicator-GMI-Insights-and
#'
#' @param x Data frame, read_dexcom output
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#' @param include_bv If TRUE, "High" and "Low" flags are converted to value 400 and 40
#' respectively and used in computations. If FALSE, this conversion does not happen;
#' computations ignore the "High" and "Low" flags
#'
#' @return
#' @export
#'
#' @examples
compute_gmi <- function(x, inter, include_bv = T) {
  if(inter <= 0){
    stop("Nonpositive timer interval inputted. Please input
         positive number of days")
  }

  df <- set_inter(x, inter)
  if(include_bv == T){
    df <- convert_bv(df)
  }

  df_avg <- compute_avg_glucose(df, human_run = F)

  df_gmi <- df_avg %>%
    dplyr::mutate(gmi = 3.31 + (0.02392* df_avg$bg_mean))
}

