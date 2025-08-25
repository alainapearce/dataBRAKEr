#' util_redcap_child2: Organize child visit 2 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_2_arm_1
#'
#' @param data data from REDCap event child_visit_2_arm_1
#' @inheritParams util_redcap_child1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 2 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit2_info}
#'    \item{loc_data}
#'    \item{sic_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' child_visit2_list <- util_redcap_child2(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap(), util_redcap_dates()]
#'
#' @export

util_redcap_child2 <- function(data, date_data) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_2_arm_1 must be entered as a data.frame")
  }
  
  date_data_arg <- methods::hasArg(date_data)
  
  if (isTRUE(date_data_arg)) {
    if (!is.data.frame(date_data)) {
      stop('date_data must be a data.frame')
    }
  } else if (isFALSE(date_data_arg)) {
    stop('date_data from util_redcap_dates() must be entered as a data.frame')
  }
  
  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'
  
  # add session column
  data['session_id'] <- 'ses-baseline'
  
  # add visit number
  data['visit_protocol'] <- 2
  
  # merge with date_data
  data <- merge(data, date_data[c('participant_id', 'v2_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v2_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])
  
  
  ## protocol information ####
  v2_info <- data[grepl('_id|^visit|bodpod|pizza|wasi|dkefs|spacegame|toolbox|final|general', names(data))]
  
  v2_info <- v2_info[!grepl('ff_check|min_check|other', names(v2_info))]
  
  #update names
  names(v2_info) <- gsub('ff', 'fullness', names(v2_info))
  names(v2_info) <- gsub('_check', '_complete', names(v2_info))
  
  names(v2_info)[names(v2_info) == 'v2_general_notes'] <- 'v2_notes'
  names(v2_info)[names(v2_info) == 'final_notes_v2'] <- 'v2_end_notes'
  
  v2_info <- v2_info[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(v2_info)[grepl('^v2', names(v2_info))], names(v2_info)[grepl('bodpod', names(v2_info))], names(v2_info)[grepl('pizza', names(v2_info))], names(v2_info)[grepl('wasi|dkefs|spacegame|toolbox', names(v2_info))])]
  
  
  v2_info_json <- json_v2_info()
  
  ## LOC data ####
  loc_data <- data[grepl('_id|^visit|loc', names(data))]
  loc_data <- loc_data[!grepl('share|block|other', names(loc_data))]
  
  loc_data <- loc_data[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(loc_data)[grepl('loc', names(loc_data))])]
  
  loc_json <- json_loc()
  
  ## SIC data ####
  sic_data <- data[grepl('_id|^visit|sic', names(data))]
  sic_data <- sic_data[!grepl('other', names(sic_data))]
  
  sic_data <- sic_data[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(sic_data)[grepl('sic', names(sic_data))])]
  
  #score 
  sic_scored <- dataprepr::score_sic(sic_data, base_zero = TRUE, id = 'participant_id')
  
  sic_json <- json_sic()
  
  
  return(list(
    visit2_info = list(data = v2_info, meta = v2_info_json),
    loc_data = list(data = loc_data, meta = loc_json),
    sic_data = list(data = sic_data, meta = sic_json)
  ))
}

