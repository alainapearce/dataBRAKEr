#' util_redcap_prepost: Organize Pre/Post visits 1-3 data from REDCap 
#'
#' This function organizes REDCap data from event visit_1_prepost_arm_1, visit_2_prepost_arm_1, visit_3_prepost_arm_1
#'
#'
#'
#' @param v1_data data from REDCap events visit_1_prepost_arm_1
#' @param v2_data data from REDCap events visit_2_prepost_arm_1
#' @param v3_data data from REDCap events visit_3_child_arm_1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw protocol dataset for visit 1}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{restricted - identifiable data}
#'    \item{particiant_info - general inforamtion about partiicpants and general protocol}
#'  }
#' @examples
#'
#' # process REDCap data
#' prepost_data <- util_redcap_prepost(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_prepost <- function(v1_data, v2_data, v3_data, v3_child_data) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  v1_data_arg <- methods::hasArg(v1_data)
  
  if (isTRUE(v1_data_arg)) {
    if (!is.data.frame(v1_data)) {
      stop('v1_data must be a data.frame')
    } 
  } else if (isFALSEv1_(data_arg)) {
    stop('data for REDCap event visit_1_prepost_arm_1 must be entered as a data.frame')
  }
  
  v2_data_arg <- methods::hasArg(v2_data)
  
  if (isTRUE(v2_data_arg)) {
    if (!is.data.frame(v2_data)) {
      stop('v2_data must be a data.frame')
    } 
  } else if (isFALSEv2_(data_arg)) {
    stop('data for REDCap event visit_2_prepost_arm_1 must be entered as a data.frame')
  }
  
  v3_data_arg <- methods::hasArg(v3_data)
  
  if (isTRUE(v3_data_arg)) {
    if (!is.data.frame(v3_data)) {
      stop('v3_data must be a data.frame')
    } 
  } else if (isFALSEv3_(data_arg)) {
    stop('data for REDCap event visit_3_prepost_arm_1 must be entered as a data.frame')
  }
  
  # update name of participant ID column
  names(v1_data)[names(v1_data) == 'record_id'] <- 'participant_id'
  names(v2_data)[names(v2_data) == 'record_id'] <- 'participant_id'
  names(v3_data)[names(v3_data) == 'record_id'] <- 'participant_id'
  
  #reduce columns and update names
  
  ## restricted data
  restricted_data <- v1_data[grepl('_id|^visit|name|zip|census', names(v1_data))]
  
  # remove unwanted and re-order
  restricted_data <- restricted_data[!grepl('v1|redcap|video', names(restricted_data))]
  
  restricted_data <- restricted_data[c('participant_id', names(restricted_data)[!grepl('id|visit', names(restricted_data))])]
  
  ## vist 1 info
  pre_v1_data <- v1_data[grepl('_id|status|rural|school|home|date|order|cond|video', names(v1_data))]
  
  # remove unwanted and re-order
  pre_v1_data <- pre_v1_data[!grepl('v1', names(pre_v1_data))]
  
  #code rurality
  pre_v1_data[['school_rural']] <- ifelse(pre_v1_data[['school_nces_code']] > 30, 'rural', 'metro')
  pre_v1_data[['home_rural']] <- ifelse(pre_v1_data[['home_nces_code']] > 30, 'rural', 'metro')
  
  pre_v1_data <- pre_v1_data[c('participant_id', 'sub_status', names(pre_v1_data)[grepl('school', names(pre_v1_data))], names(pre_v1_data)[grepl('home', names(pre_v1_data))], names(pre_v1_data)[grepl('cond', names(pre_v1_data))], names(pre_v1_data)[grepl('order', names(pre_v1_data))], names(pre_v1_data)[grepl('video', names(pre_v1_data))])]
  
  prepost_json <- json_participant_info()
  
  # get sample collection data from v2 and v3
  prepost_data <- merge(pre_v1_data, v2_data[c('participant_id', 'sample_collected')], by = 'participant_id', all = TRUE)
  names(prepost_data)[names(prepost_data) == 'sample_collected'] <- 'v2_sample_collected'
  
  prepost_data <- merge(prepost_data, v3_data[is.na(v3_data['redcap_repeat_instrument']), c('participant_id', 'sample_collected')], by = 'participant_id', all = TRUE)
  names(prepost_data)[names(prepost_data) == 'sample_collected'] <- 'v3_sample_collected'
  
  return(list(restricted = list(data = restricted_data), 
              participant_info = list(data = prepost_data, meta = prepost_json)))
}

