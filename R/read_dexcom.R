#' Title
#'
#' @param x A file (maybe filepath).
#'
#' @return A data frame.
#' @export
#'
#' @examples
read_dexcom <- function(x) {
  y = remove_phi(readr::read_csv(x))
  #y = shift_time(y)

  #adding columns
  #record id
  #parse_id(x)
  #bg_date_time
  #bg_value_num
  #bg_value_glaf
  y
}


#taking out top three row to remove PHI
remove_phi <- function(x) {
  dplyr::filter(x, x$Index >= 4)
}

shift_time <- function(x) {
  #random choice of given numbers

}

#parse_id <- function(x) {

  #parses file name for record id
#}

