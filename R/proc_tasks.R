#' proc_tasks: Process raw data from all computer tasks used in Study BRAKE
#'
#' This function calls task scripts that:
#' 1) copies data from raw_untouched and saves it in sourcedata
#' 2) cleans data to save in BIDS format in rawdata
#' 3) processes rawdata to generate derivative or summary databases for each task
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @param data_path full data path to untouched_raw directory
#' @inheritParams util_task_org_sourcedata
#'
#' @return data.frame for each task with status for each processing step
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' proc_tasks_pardat <- proc_tasks(data_path, overwrite)
#'
#' \dontrun{
#' }
#'
#'
#' @export

proc_tasks <- function(data_path, overwrite = FALSE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  path_arg <- methods::hasArg(data_path)

  if (isTRUE(path_arg)) {
    if (!is.character(data_path)) {
      stop("data_path must be entered as a string")
    } else if (!file.exists(data_path)) {
      stop("data_path entered, but file does not exist. Check data_path string.")
    }
  } else if (isFALSE(path_arg)) {
    stop("data_path must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The proc_tasks.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }

  # find location of slashes so can decompose filepaths
  slash_loc <- unlist(gregexpr('/', data_path))
  
  # set paths for other directories
  base_wd <- substr(data_path, 1, tail(slash_loc, 1))
  
  # Food Rating ####
  # get list of available subjects 
  foodrating_list <- as.data.frame(list.files(path = paste0(data_path, slash, 'foodrating_game', slash), pattern = '.csv'))
  names(foodrating_list) <- 'filename'
  
  #get list of subject IDs
  foodrating_list[['sub_str']] <- sapply(foodrating_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  foodrating_list[['sourcedata_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodrating', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = FALSE), simplify = TRUE)
  
  #process raw data
  foodrating_list[['rawproc_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_foodrating(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = FALSE, return = FALSE), simplify = TRUE)
  
  #json 
  foodrating_json <- json_foodrating()
  
  # Food Choice ####
  # get list of available subjects 
  foodchoice_list <- as.data.frame(list.files(path = paste0(data_path, slash, 'foodchoice_game', slash), pattern = '.csv'))
  names(foodchoice_list) <- 'filename'
  
  #get list of subject IDs
  foodchoice_list[['sub_str']] <- sapply(foodchoice_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #get eye-tracking status
  foodchoice_list[['eye_track']] <- sapply(foodchoice_list[['filename']], function(x) ifelse(grepl('noeye', x), 'N', 'Y'), simplify = TRUE)
  
  #valid choice-pairing assignment
  foodchoice_list[['choice_pairing']] <- sapply(foodchoice_list[['filename']], function(x) ifelse(grepl('999', x), '999', 'rating'), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  foodchoice_list[['sourcedata_done']] <- sapply(foodchoice_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodchoice', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = FALSE), simplify = TRUE)
  
  #process raw data
  foodchoice_list[foodchoice_list[['choice_pairing']] != '999', 'rawproc_done'] <- sapply(foodchoice_list[foodchoice_list[['choice_pairing']] != '999', 'sub_str'], function(x) util_task_foodchoice(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = FALSE, return = FALSE), simplify = TRUE)
  
  #json 
  foodchoice_json <- json_foodchoice()
  
  # Shape Game ####
  # get list of available subjects 
  shape_list <- as.data.frame(list.files(path = paste0(data_path, slash, 'shape_game', slash), pattern = '.csv'))
  names(shape_list) <- 'filename'
  
  #get list of subject IDs
  shape_list[['sub_str']] <- sapply(shape_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #get eye-tracking status
  shape_list[['eye_track']] <- sapply(shape_list[['filename']], function(x) ifelse(grepl('noeye', x), 'N', 'Y'), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  shape_list[['sourcedata_done']] <- sapply(shape_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'shape', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = FALSE), simplify = TRUE)
  
  # Space Game ####
  # get list of available subjects 
  space_list <- as.data.frame(list.files(path = paste0(data_path, slash, 'space_game', slash), pattern = '.csv'))
  names(space_list) <- 'filename'
  
  #get list of subject IDs
  space_list[['sub_str']] <- sapply(space_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  space_list[['sourcedata_done']] <- sapply(space_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'space', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = FALSE), simplify = TRUE)
  
  
  
  
  
  
  # set paths for other directories
  raw_wd_nirs <- paste0(base_wd, '/bids/rawdata/', sub_str, '/nirs/')
  raw_wd_beh <- paste0(base_wd, '/bids/rawdata/', sub_str, '/beh/')
  derivatives_wd_nirs <- paste0(base_wd, '/bids/derivatives/', sub_str, '/nirs-toolbox/')
  derivatives_wd_beh <- paste0(base_wd, '/bids/derivatives/', sub_str, '/beh/')
  
  
  

  #### Food Rating Task ####
  foodrating_list <- foodrating_task(sub, paste0(data_path, 'foodrating_game'), return_data = return_data)

  #### Food Choice Task ####
  foodrating_list <- foodrating_task(sub, paste0(data_path, 'foodchoice_game'), return_data = return_data)

  if (isTRUE(return_data)){
    return(list( foodchoice_dat = dat,
                 foodchoice_labels = meta_json))
  }
}

