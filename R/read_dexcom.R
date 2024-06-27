#' Reads .csv file, deidentifies the data (removes PHI)
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

  z = reformat(z)
  #adding columns
    #record id
    #parse_id(x)
    #bg_date_time
    #bg_value_num
    #bg_value_glaf
  return(z)
}

#renames columns per guidelines in our doc

reformat <- function(x){
  frame <- x
  #renames time stamp column
  colnames(frame)[which(names(fram) == "Timestamp (YYYY-MM-DDThh:mm:ss")] <- "bg_date_time"

  #renames blood glucose column, then splits into two columns
  #one which holds numeric values
  #other holds "high"/"low" characters if attained
  colnames(frame)[which(names(fram) == "Glucose Value (mg/dL")] <- "bg_value_num"
  #new column for boundary values "high" and "low"
  frame |>
    mutate(
      bg_value_flag = bg_value_num
    )

  while(i <= length(frame$Index)){

    if(as.type(frame$'bg_value_num') == "char"){

    }
    i = i+1
  }


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
  frame$`Timestamp (YYYY-MM-DDThh:mm:ss)` <- datetime
  return(frame)
}


#NOTE: instead of this, just prompt user to setwd() to correct location
#so that can input ONLY file name instead of entire file path
#Could write two methods to deal with either input


#if given file path
#parse_id <- function(x) {
  #filepath <- x
  #filepath <- as.character(filename)

  #ask user if Windows or Mac (or other device)
  #should assume filepath syntax off of that

  #if(Mac) {
    #file_name <- strsplit(filepath, "/")
  #}
  #if(Windows) {
    #file_name <- strsplit(filepath, "\\")
  #}
  #isolate filename in filepath
  #file_name <-  strsplit(file_name, "_")
  #isolate record id
  #record_id <- tail(file_name, n = 1)
  #remove the .csv at the end (i.e. last 4 characters)
  #record_id = substr(record_id, 1, nchar(record_id)-4)
  #return(record_id)
#}

#if given file name
parse_id <- function(x) {
  file_name <- x
  file_name = as.character(file_name)
  record_id = substr(file_name, 1, nchar(file_name)-4)
}




