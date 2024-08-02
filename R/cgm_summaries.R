#' Computes all metrics
#'
#' Wrapper function whcih calls the other computation functions to quickly compute all CMG metrics.
#' @param df_dex
#' @param start
#' @param end
#' @param lookback
#' @param freq
#' @param breaks
#' @param include_bv
#'
#' @return
#' @export
#'
#' @examples
cgm_summaries <- function(df_dex, start = "default", end = "default", lookback = 90, freq = NULL,
                          breaks = NULL, include_bv = T, plot = F){
  check_lookback(lookback)
  df_dex1 <- change_window(df_dex = df_dex, start = start, end = end)
  df <- partition_window(df_dex = df_dex1, freq = freq, breaks = breaks)

  agp <- compute_agp(df_dex = df, lookback = lookback, freq = freq, breaks = breaks, include_bv = include_bv)
  gmi <- compute_gmi(df_dex = df, lookback = lookback, freq = freq, breaks = breaks, include_bv = include_bv)
  gmi <- gmi[ , !names(gmi) %in% "n"] #removing the n column here because it already exists in agp
  #something to do with wear time
  df_summary <- dplyr::bind_cols(agp, gmi)

  if(plot == T){
    if (!is.null(breaks)) {
      x_labels <- paste0(seq_len(nrow(df_dex)), " month", ifelse(seq_len(nrow(df_dex)) > 1, "s", ""))
    } else if (!is.null(freq)) {
      x_labels <- sapply(seq_len(nrow(df_dex)) * freq, function(days) {
        if (days == 1) {
          return("1 day")
        } else {
          return(paste0(days, " days"))
        }
      })
    } else {
      x_labels <- as.character(seq_len(nrow(df_dex)))  # Fallback
    }

    plot_agp(df_dex = df_dex, start = start, end = end, lookback = lookback, freq = freq,
             breaks = breaks) #+
      #ggplot2::scale_x_discrete(labels = x_labels)
  }

  return(df_summary)
}
