#' util_actigraph_org_sourcedata: Organize actigraphy raw data into BIDS format sourcedata
#'
#' This function copies data from raw_untouched and saves it in sourcedata
#' 
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @param dir_name directory name for raw actigraph data (baseline: 'actigraphy'; followup = 'actigraphy_v3')
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#'
#' @examples
#'
#' # organize actigraphy data
#' org_actigraph <- util_actigraph_org_sourcedata(tsub_str = 'sub_001', ses = 'baseline', base_wd, overwrite = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_actigraph_org_sourcedata <- function(sub_str, ses, dir_name, base_wd, overwrite = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
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
  
  
  # check that session exist and is a string
  dir_arg <- methods::hasArg(dir_name)
  
  if (isTRUE(dir_arg)) {
    if (!is.character(dir_name)) {
      stop("dir_name must be entered as a string")
    } else {
      raw_untouched_path <- file.path(base_wd,'raw_untouched', dir_name)
    }
  }
  
  if (!file.exists(raw_untouched_path)) {
    stop(paste0("file path: ", raw_untouched_path, " data does not exist"))
  }
  
  # get all files for sub in raw_untouched
  raw_files <- list.files(path = raw_untouched_path, pattern = sub_str)
  
  # new file name
  rename_files <- gsub('_actigraph', paste0('_ses-', ses, '_tracksys-ActiGraph_motion'), raw_files)
  
  #### Save in sourcedata #####
  # set paths for other directories
  source_wd <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'beh')
  
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'beh')
  
  #make directory if needed
  if (!dir.exists(source_wd)) {
    dir.create(source_wd, recursive = TRUE)
  } 
  
  #make directory if needed
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  } 
  
  # copy files
  if (!file.exists(file.path(source_wd, rename_files[1])) | isTRUE(overwrite)) {  
    
    file.copy(from = file.path(raw_untouched_path, raw_files), to = file.path(source_wd, rename_files), overwrite = overwrite)
    
    file.copy(from = file.path(raw_untouched_path, raw_files), to = file.path(raw_wd, rename_files), overwrite = overwrite)
    
    #return message
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
    
  } else {
    return('exists')
  }
  
}
