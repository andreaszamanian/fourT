#' Reads Dexcom data
#'
#' Reads .csv file, deidentifies the data (removes PHI), reformats
#' @param x A file (maybe filepath).
#'
#' @return A data frame.
#' @export
#'
#' @examples
read_dexcom <- function(x, col_spec = FALSE) {
  #if(x != .csv file){
    #stop("incorrect file type input")
  #}
  show_col <-col_spec
  y = readr::read_csv(x, show_col_types = show_col)
  y = as.data.frame(y)
  z = remove_phi(readr::read_csv(x, show_col_types = show_col))
  #super simple randomization, may want to make it more complex in future
  random_num <- sample(c(-4,-3,-2,-1,1,2), 1)
  z = shift_time(z, random_num)
  z = reformat(z, parse_id(x))
  z = clean_data(z)
  return(z)
}


parse_id <- function(x) {
  file_name <- x
  file_name = as.character(file_name)
  record_id = substr(file_name, nchar(file_name)-9, nchar(file_name)-4)
  return(record_id)
}

#renames/restructures columns per guidelines in doc

reformat <- function(x, y){
  frame <- x
  loc_record_id <- y

  colnames(frame)[which(names(frame) == "Timestamp (YYYY-MM-DDThh:mm:ss)")] <- "bg_date_time" #renames time stamp column
  frame[, "record_id"] <- loc_record_id #makes new column with every row saying the record id

  colnames(frame)[which(names(frame) == "Glucose Value (mg/dL)")] <- "bg_value_num"
  frame[, "bg_value_flag"] <- NA #new column for boundary values "high" and "low"
  i <- 1
  while(i <= length(frame$Index)){
    if(is.na(frame[i, "bg_value_num"]) != TRUE &&
       substr(frame[i, "bg_value_num"], 1, 1) != "1" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "2" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "3" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "4" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "5" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "6" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "7" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "8" &&
       substr(frame[i, "bg_value_num"], 1, 1) != "9" ) {
        frame[i, "bg_value_flag"] <- frame[i, "bg_value_num"]
        frame[i, "bg_value_num"] <- NA
    }
    i = i+1
  }

  #if I want to just return the four doc columns
  #COMMENT THIS OUT if I want these columns back in
  frame <- subset(frame, select = -c(`Index`,
                                     `Event Type`,
                                     `Event Subtype`,
                                     `Device Info`,
                                     `Source Device ID`,
                                     `Insulin Value (u)`,
                                     `Carb Value (grams)`,
                                     `Duration (hh:mm:ss)`,
                                     `Glucose Rate of Change (mg/dL/min)`,
                                     `Transmitter Time (Long Integer)`,
                                     `Transmitter ID`)
                  )

  return(frame)
}

#taking out top three row to remove PHI
#takes a df as input
remove_phi <- function(x) {
  frame <- x
  deid_frame <- dplyr::filter(frame, frame$Index >= 4)
  deid_frame$Index <- deid_frame$Index - 3 #shifts index so that it starts at 1 again
  #remove patient info column (unnecessary now)
  deid_frame <- subset(deid_frame, select = -`Patient Info`)
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

clean_data <- function(df_dex){
  na_rows <- stats::complete.cases(df_dex$bg_date_time)
  df_dex <- df_dex %>%
    dplyr::filter(na_rows == T)
  return(df_dex)
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

