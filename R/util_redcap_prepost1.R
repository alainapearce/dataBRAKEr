#' util_redcap_prepost1: Organize Pre/Post visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from event visit_1_prepost_arm_1
#'
#'
#' @param data data from REDCap event visit_1_prepost_arm_1
#' @param return_data return organized data (default = TRUE)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw Pre/Post visit 1 datasets
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' prepost_visit1_data <- util_redcap_prepost1(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_prepost1 <- function(data, return_data = TRUE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
    stop("data for REDCap event visit_1_prepost_arm_1 must be entered as a data.frame")
  }
  
  #reduce columns and update names
  
  ## restricted data
  restricted_data <- data[c('record_id', 'county_name', 'municipal_name', 'zipcode', 'census_track')]
  names(restricted_data)[1] <- 'participant_id'

  ## demographics data
  pre_v1demo_data <- data[c('record_id', 'sub_status', 'rural_ruca', 'rural_rucc', 'rural_uic', 'rural_county_pcent', 'school_nces_code', 'school_locale', 'foodrate_cond', 'v1pre_date', 'v1_dataorg_notes')]
  names(pre_v1demo_data)[1] <-'participant_id'
  names(pre_v1demo_data)[c(2, 6:7, 10)] <- c('status', 'pcent_rural', 'school_code', 'v1_date')

  if (isTRUE(return_data)){
    return(list(restricted = restricted_data, demo = pre_v1demo_data))
  }
}

