#' util_redcap_de: Organize REDCap double entry data (called within proc_redcap.R)
#'
#' This function organizes REDCap double entry data
#'
#'
#' @param data data from REDCap data double entry
#' @inheritParams util_redcap_prepost1
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw data double entry datasets
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' double_enter_data <- util_redcap_de(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_de <- function(data, return_data = TRUE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_1_arm_1 must be entered as a data.frame")
  }
  
  #reduce rows (choose rater 1, matched entry ahead of time)
  data <- data[grepl('--1', data[['record_id']]), ]
  
  names(data)[1] <- 'participant_id'
  data[['participant_id']] <- gsub('--1', '', data[['participant_id']])
  
  #reduce columns and update names
  
  ## intake data
  intake_data <- data[, grepl('participant_id', names(data)) | grepl('_g$', names(data)) | grepl('_kcal$', names(data)) | grepl('*ff_', names(data)) | grepl('_notes$', names(data))]
  intake_data <- intake_data[, names(intake_data) != 'bodbod_nurse_notes']
  intake_json <- NA
  
  ## WASI data
  wasi_data <- data[, grepl('participant_id', names(data)) | grepl('wasi', names(data))] 
  wasi_data <- wasi_data[, !(names(wasi_data) %in% c('wasi_complete'))]
  names(wasi_data)[1] <- 'participant_id'
  wasi_json <- NA
  
  ## DKEFS data
  dkefs_data <- data[, grepl('participant_id', names(data)) | grepl('dkefs', names(data))] 
  dkefs_data <- dkefs_data[, !(names(dkefs_data) %in% c('dkefs_complete'))]
  names(dkefs_data)[1] <- 'participant_id'
  dkefs_json <- NA
  
  ## BodPod data
  bodpod_data <- data[, grepl('participant_id', names(data)) | grepl('bodpod', names(data))] 
  bodpod_data <- bodpod_data[, !(names(bodpod_data) %in% c('bodpod_complete'))]
  names(bodpod_data)[1] <- 'participant_id'
  bodpod_json <- NA
  
  ## NonWear Log data
  nonwear_log_data <- data[, grepl('participant_id', names(data)) | grepl('nonwear', names(data))] 
  nonwear_log_data <- nonwear_log_data[, !(names(nonwear_log_data) %in% c('nonwear_log_complete'))]
  names(nonwear_log_data)[1] <- 'participant_id'
  nonwear_log_json <- NA
  
  if (isTRUE(return_data)){
    return(list(
      intake_data = list(data = intake_data, meta = intake_json),
      wasi_data = list(data = wasi_data, meta = wasi_json),
      dkefs_data = list(data = dkefs_data, meta = dkefs_json),
      bodpod_data = list(data = bodpod_data, meta = bodpod_json),
      nonwear_log_data = list(data = nonwear_log_data, meta = nonwear_log_json)))
  }
}

