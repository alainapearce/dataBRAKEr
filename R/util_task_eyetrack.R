#' util_task_eyetrack: Process raw eyetracking .hdf5 files
#'
#' This function: \itemize{
#' \item{1) converts data to save in BIDS format in rawdata}
#' \item{2) cleans data to ensure it is deidentified}
#' }
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' foodchoice_eye_pardat <- util_task_eyetrack(task_str, sub_str, ses, data_path)
#'
#' \dontrun{
#' }
#'
#' @import data.table
#' @export

util_task_eyetrack <- function(task_str, sub_str, ses, base_wd, overwrite = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  base_wd_arg <- methods::hasArg(base_wd)
  
  if (isTRUE(base_wd_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(base_wd)) {
    stop("base_wd must be entered as a string")
  }
  
  
  # get directory paths
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'eyetrack')
  
  data_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'eyetrack', paste0(sub_str, '_ses-', ses, '_task-', task_str, '_events.hdf5'))
  
  print(sub_str)
    
  #### Organize Data #####
  dat_load <- rhdf5::H5Fopen(data_file, flags = "H5F_ACC_RDONLY")
  
  eye_data <- as.data.frame(rhdf5::h5read(dat_load, name = '//data_collection/events/eyetracker/MonocularEyeSampleEvent'))
  names(eye_data) <- tolower(names(eye_data))
  
  event_data <- as.data.frame(rhdf5::h5read(dat_load, name = '/data_collection/events/experiment/MessageEvent'))
  names(event_data) <- tolower(names(event_data))
  
  key_data <- as.data.frame(rhdf5::h5read(dat_load, name = '/data_collection/events/keyboard/KeyboardInputEvent'))
  names(key_data) <- tolower(names(key_data))

  # merge datasets using rolling join that aligns closest time values
  merged_exp_data <- data.table::setDT(key_data)[, list(time, key)][eye_data, on = "time", roll = TRUE]
  
  merged_eye_data <- data.table::setDT(merged_exp_data)[, list(time, text, key)][eye_data, on = "time", roll = TRUE]
  
  merged_eye_data <- as.data.frame(merged_eye_data)
  
  # clean up variables
  merged_eye_data['participant_id'] <- sub_str
  merged_eye_data['session_id'] <- ses
  
  merged_eye_data <- merged_eye_data[c('participant_id', 'session_id', names(merged_eye_data)[!grepl('participant_id|session_id', names(merged_eye_data))])]
  
  merged_eye_data[names(merged_eye_data) == 'text'] <- 'event_msg'
  
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(file.path(raw_wd, paste0(sub_str, '_task-', task_str, '_recording-eyetrack_physio.tsv.gz'))) | isTRUE(overwrite)) {
    write.table(merged_eye_data, file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-', task_str, '_recording-eyetrack_physio.tsv.gz')), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
  
  rhdf5::h5closeAll()
}
