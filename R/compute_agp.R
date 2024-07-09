#' Compute AGP Thermometers
#'
#' @param x Data frame with format identical to that outputted by read_dexcom
#' @param inter Time interval for computation; e.g. every 14 days, every month, etc.
#'
#' @return Data frame with AGP metrics
#' @export
#'
#' @examples
compute_agp <- function(x, inter){
  dt_tir <- x %>%
    group_by(pid, inter) %>%
    mutate(range = cut(glucose, c(0, 54, 70, 181, 251, Inf),
                       include.lowest = T, labels = F)) %>%
    summarise(
      n = n(),
      tbr2 = sum(range == 1),
      tbr = sum(range <= 2),
      tar = sum(range >= 4),
      tar2 = sum(range == 5),
      tir = sum(range == 3), .groups = 'drop') %>%
    mutate(across(tbr2:tir, ~ ./n))
}

#how to deal with incomplete time periods? e.g. if there are 3 days in the last
#14 day window




