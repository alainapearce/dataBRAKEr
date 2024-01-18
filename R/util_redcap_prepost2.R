#' util_redcap_prepost2: Organize Pre/Post visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from event visit_2_prepost_arm_1
#'
#' @param data data from REDCap event visit_2_prepost_arm_1
#' @inheritParams util_redcap_prepost1
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw Pre/Post visit 2 datasets
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' prepost_visit2_data <- util_redcap_prepost2(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_prepost2 <- function(data, return_data = TRUE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
    stop("data for REDCap event visit_2_prepost_arm_1 must be entered as a data.frame")
  }
  
  #reduce columns and update names
  
  ## pre/post visit data
  pre_v2demo_data <- data[c('record_id', 'v2pre_date', 'sample_collected', 'v2_datorg_notes')]
  names(pre_v2demo_data)[1:2] <- c('participant_id', 'v2_date')

  if (isTRUE(return_data)){
    return(pre_v2demo_data)
  }
}

