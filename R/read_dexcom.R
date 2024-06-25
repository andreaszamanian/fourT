#' Title
#'
#' @param x A file (maybe filepath).
#'
#' @return A data frame.
#' @export
#'
#' @examples
read_dexcom <- function(x) {
  #if(x != .csv file){
    #throw error "incorrect file type input"
  #}
  y = readr::read_csv(x)
  y = as.data.frame(y)
  z = remove_phi(readr::read_csv(x))

  #super simple randomization, may want to make it more complex in future
  random_num <- sample(c(-4,-3,-2,-1,1,2), 1)
  z = shift_time(z, random_num)

  #adding columns
  #record id
  #parse_id(x)
  #bg_date_time
  #bg_value_num
  #bg_value_glaf
  return(z) #probably change that it returns this
}


#taking out top three row to remove PHI
#takes a df as inputloa
remove_phi <- function(x) {
  frame <- x
  deid_frame <- dplyr::filter(frame, frame$Index >= 4)
  deid_frame$Index <- deid_frame$Index - 3 #shifts index so that it starts at 1 again
  return(deid_frame)
}

#do some randomization AT RUN TIME -> right now I will write that
#randomization into the read_dexcom function
shift_time <- function(x, y){
  frame <- x
  shift_amount <- 28 * y
  datetime <- frame$`Timestamp (YYYY-MM-DDThh:mm:ss)`
  current_year <-  lubridate::year(datetime)
  lubridate::year(datetime) <- current_year + shift_amount
  frame$`Timestamp (YYYY-MM-DDThh:mm:ss)` <- datetime #the issue is probably here
  return(frame)
}

#parse_id <- function(x) {

  #parses file name for record id
#}

