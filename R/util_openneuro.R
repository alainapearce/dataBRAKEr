#' util_openneuro: Copy BIDS compliant fNIRS files into open-neuro directory for easier upload
#'
#' This function: \itemize{
#' \item{1) copies data from data/bids to data/open-neuro}
#' \item{2) ensures json files are updated}
#' }
#'
#' @param base_wd full path to directory containing both raw_untouched and bids directories
#' @inheritParams util_task_org_sourcedata
#'
#'
#' @examples
#'
#' \dontrun{
#' # organize task data for space game and NIH toolbox in untouchedRaw into sourcedata and raw data
#' util_openneuro(base_wd = base_wd)
#'
#' }
#'
#'
#' @export
#'

util_openneuro <- function(base_wd, overwrite = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that base_wd exist and is a data.frame
  path_arg <- methods::hasArg(base_wd)
  
  if (isTRUE(path_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(path_arg)) {
    stop("base_wd must be entered as a string")
  }
  
  #### Define paths ####
  bids_wd <- file.path(base_wd, 'bids')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives')
  openneuro_wd <- file.path(base_wd, 'open-neuro', 'rawdata')
  scholarsphere_wd <- file.path(base_wd, 'scholar-sphere', 'rawdata')
  
  cp_openneuro <- function(data_path, file_name, meal_desc, overwrite) {
    
    openneuro_path <- file.path(openneuro_wd, data_path)
    
    if (hasArg(meal_desc)) {
      raw_data_path <- file.path(raw_wd, data_path, meal_desc)
    } else {
      raw_data_path <- file.path(raw_wd, data_path)
    }
    
    #make directory if needed
    
    if (!dir.exists(openneuro_path)) {
      dir.create(openneuro_path, recursive = TRUE)
    } 
    
    if (!file.exists(file.path(raw_data_path, file_name)) | isTRUE(overwrite)) {
      
      file.copy(from = file.path(raw_data_path, file_name), to = file.path(openneuro_path, file_name), overwrite = overwrite)
    }
    
    if (grepl('_events.tsv', file_name)){
      scholarsphere_path <- file.path(scholarsphere_wd, data_path)
      
      if (!dir.exists(scholarsphere_path)) {
        dir.create(scholarsphere_path, recursive = TRUE)
      } 
      
      if (!file.exists(file.path(raw_data_path, file_name)) | isTRUE(overwrite)) {
        file.copy(from = file.path(raw_data_path, file_name), to = file.path(scholarsphere_path, file_name), overwrite = overwrite)
      }
    }
  }
  
  # Baseline fNIRS get list of available subjects  ####
  print('-- copying individual nirs baseline files to open-neuro')
  
  baseline_tsv_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'nirs')), pattern = '*.tsv$', recursive = TRUE))
  names(baseline_tsv_list) <- 'filename'
  
  baseline_fnirs_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'nirs')), pattern = '*.snirf', recursive = TRUE))
  names(baseline_fnirs_list) <- 'filename'
  
  baseline_json_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'nirs')), pattern = '*.json', recursive = TRUE))
  names(baseline_json_list) <- 'filename'
  
  baseline_fnirs_list <- rbind(baseline_tsv_list, baseline_json_list, baseline_fnirs_list)
  
  #get list of subject IDs
  baseline_fnirs_list[['sub_str']] <- sapply(baseline_fnirs_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  baseline_fnirs_list[['data_path']] <- file.path(baseline_fnirs_list[['sub_str']], 'ses-baseline', 'nirs')

  #organize data into BIDS sourcedata
  mapply(cp_openneuro, data_path = baseline_fnirs_list[['data_path']], file_name = baseline_fnirs_list[['filename']], MoreArgs = list(overwrite = overwrite))
 
  # Baseline eye-tracking get list of available subjects  ####
  print('-- copying individual eye-trakcing baseline files to open-neuro')
  
  baseline_eyetrack_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'eyetrack')), pattern = '*.tsv.gz', recursive = TRUE))
  names(baseline_eyetrack_list) <- 'filename'
  
  #get list of subject IDs
  baseline_eyetrack_list[['sub_str']] <- sapply(baseline_eyetrack_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  baseline_eyetrack_list[['data_path']] <- file.path(baseline_eyetrack_list[['sub_str']], 'ses-baseline', 'eyetrack')
  
  mapply(cp_openneuro, data_path = baseline_eyetrack_list[['data_path']], file_name = baseline_eyetrack_list[['filename']], MoreArgs = list(overwrite = overwrite))
  
  # Follow-up fNIRS get list of available subjects  ####
  print('-- copying individual nirs followup files to open-neuro')
  
  followup_tsv_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-followup', 'nirs')), pattern = '*.tsv$', recursive = FALSE))
  names(followup_tsv_list) <- 'filename'
  followup_tsv_list['meal_dir'] <- ''
  
  followup_json_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-followup', 'nirs')), pattern = '*.json', recursive = TRUE))
  names(followup_json_list) <- 'filename'
  followup_json_list['meal_dir'] <- ''
  
  
  followup_premeal_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-followup', 'nirs', 'premeal')), pattern = '*', recursive = TRUE))
  names(followup_premeal_list) <- 'filename'
  followup_premeal_list['meal_dir'] <- 'premeal'
  
  followup_postmeal_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-followup', 'nirs', 'postmeal')), pattern = '*', recursive = TRUE))
  names(followup_postmeal_list) <- 'filename'
  followup_postmeal_list['meal_dir'] <- 'postmeal'
  
  followup_fnirs_list <- rbind(followup_tsv_list, followup_json_list, followup_premeal_list, followup_postmeal_list)
  
  #get list of subject IDs
  followup_fnirs_list[['sub_str']] <- sapply(followup_fnirs_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  followup_fnirs_list[['data_path']] <- file.path(followup_fnirs_list[['sub_str']], 'ses-followup', 'nirs')
  
  #organize data into BIDS sourcedata
  mapply(cp_openneuro, data_path = followup_fnirs_list[['data_path']], file_name = followup_fnirs_list[['filename']], meal_desc = followup_fnirs_list[['meal_dir']], MoreArgs = list(overwrite = overwrite))
  
  # copy json files
  #copy over individual *event.json files
  print('-- copying global *events.json files')
  event_json_list <- list.files(path = bids_wd, pattern = 'events.json', recursive = TRUE)
  event_json_list <- event_json_list[grepl('tastetest|foodrating|foodchoice|eyetrack', event_json_list)]
  
  file.copy(from = file.path(bids_wd, event_json_list), to = file.path(base_wd, 'open-neuro', event_json_list), overwrite = overwrite)
}

