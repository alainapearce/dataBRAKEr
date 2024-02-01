#' util_redcap_child2: Organize child visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_2_arm_1
#'
#'
#' @param data data from REDCap event child_visit_2_arm_1
#' @inheritParams util_redcap_prepost1
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 2 datasets
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' child_visit2_data <- util_redcap_child2(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_child2 <- function(data, return_data = TRUE) {
  
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
  
  #reduce columns and update names
  
  ## child_visit_2_arm_1
  child_visit2_data <- data[c('record_id', 'v2_general_notes', 'bodpod_note', 'pre_pizza_ff_time', 'pre_pizza_ff_notes', 'pizza_meal_book', 'pizza_meal_start_time', 'pizza_meal_end_time', 'pizza_meal_duration', 'pizza_meal_notes', 'post_pizza_ff_time', 'post_pizza_ff_notes', 'wasi_note', 'dkefs_note', 'spacegame_snack', 'spacegame_otherstudy', 'spacegame_otherstudy_id', 'spacegame_otherstudy_date', 'spacegame_notes', 'toolbox_flanker_notes', 'toolbox_cardsort_notes', 'final_notes_v2')]
  
  names(child_visit2_data) <- c('participant_id', 'v2_notes', 'bodpod_note', 'v2_pre_ff_time', 'v2_pre_ff_notes', 'v2_meal_book', 'v2_meal_start', 'v2_meal_end', 'v2_meal_dur', 'pizza_meal_notes', 'v2_post_ff_time', 'v2_post_ff_notes', 'wasi_note', 'dkefs_note', 'spacegame_snack', 'spacegame_otherstudy', 'spacegame_otherstudy_id', 'spacegame_otherstudy_date', 'spacegame_notes', 'nih_flanker_notes', 'nih_dccs_notes', 'v2_final_notes')
  child_visit2_json <- NA
  
  ## LOC data
  loc_data <- data[, grepl('record_id', names(data)) | grepl('loc', names(data))]
  loc_data <- loc_data[, !grepl('wasi', names(loc_data))]
  names(loc_data)[1] <- 'participant_id'
  #loc_json <- json_loc()
  
  ## SIC data
  sic_data <- data[, grepl('record_id', names(data)) | grepl('sic', names(data))]
  names(sic_data)[1] <- 'participant_id'
  
  #sic_scored <- dataprepr::score_sic(sic_data)
  #sic_json <- NA
  
  
  if (isTRUE(return_data)){
    return(list(
      child_visit2_data = list(data = child_visit2_data, meta = child_visit2_json),
      loc_data = list(data = loc_data)
      #sic_data = list(data = sic_data, meta = sic_json)
      ))
  }
}

