#' Reads cohort dexcom data
#'
#' @param folder_path Path to cohort folder.
#' @param indiv Boolean. If (default) FALSE, all patient files are binded into a single data frame.
#' If TRUE, then each patient data frame is stored in an entry in a list of data frames.
#' The order of the list corresponds to the order in which patient .csv files were read.
#' @param ordered Boolean. Only relevant when 'indiv = FALSE'. If 'ordered = TRUE', then the
#' single data frame is rearranged into chronological order (as opposed to leaving the grouping of
#' observations by patient)
#'
#'
#' @return A data frame or a list of data frames.
#' If 'indiv = F', a data frame in the format of read_dexcom output.
#' If 'indiv = T', a list of data frame, each in the format of read_dexcom output.
#' @export
#'
#' @examples
#' read_cohort_dexcom("~/Desktop/R Workspace/4T/external files/cohort-ex")
read_cohort_dexcom <- function(folder_path, indiv = F, ordered = T) {
  file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  dataframes_list <- lapply(file_list, read_dexcom)
  if(indiv == T){
    return(dataframes_list)
  } else{
    combined_df <- do.call(rbind, dataframes_list)
    if (nrow(combined_df) == 0) {
      stop("combined_df is empty after combining.")
    }
    if(ordered == T){
      if (!inherits(combined_df$bg_date_time, "POSIXct")) {
        combined_df$bg_date_time <- as.POSIXct(combined_df$bg_date_time, format = "%Y-%m-%d %H:%M:%S")
      }
      combined_df <- combined_df[order(combined_df$bg_date_time), ]
    } else{
      #do nothing
    }
    return(combined_df)
  }



}
