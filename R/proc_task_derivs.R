#' proc_task_derivs: Generate derivative task data from individual files in rawdata
#'
#' This function: \itemize{
#' \item{1) processes task data from rawdata to generate derivative data in bids/derivatives}
#' \item{2) generate json files for derivative task files}
#' \item{3) write data and json files}
#' }
#'
#' @param base_wd full path to directory containing both raw_untouched and bids directories
#' @inheritParams util_task_org_sourcedata
#' @param proc_source whether to processes the raw data using proc_tasks.R. Default = FALSE because this takes a long time
#' @param fnirs_overwrite (optional - only needed if proc_source = TRUE) overwrite fNIRS data files - separate argument because the Matlab fNIRS processing writes onset values to the rawdata/*_events.tsv values. Overwriting will overwrite these values with NA.
#' @param task_list tasks to process. Options include 'all' to process all task data or a list of the following:\itemize{
#'  \item{'foodrating' - fNIRS Food Rating task}
#'  \item{'foodchoice' - fNIRS Food Choice task}
#'  \item{'shapegame' - Shape Game data}
#'  \item{'spacegame' - Space Game data (need to finish processing in Matlab)}
#'  \item{'nihtoolbox' - NIH Toolbox data}
#'  \item{'tastetest' - fNIRS Taste-Test task}
#'  \item{'pit' - Pavlovian Instrumental Transfer task data}
#' }
#' @inheritParams util_group_foodrating
#'
#' @examples
#'
#' \dontrun{
#' # organize task data for space game and NIH toolbox in untouchedRaw into sourcedata and raw data
#' proc_task(base_wd = base_wd, task_list = c('spacegame', 'nih_toolbox'))
#'
#' }
#'
#'
#' @export
#'

proc_task_derivs <- function(base_wd, overwrite = FALSE, fnirs_overwrite = FALSE, task_list = 'all', return_data = FALSE) {
  
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
  
  # check that task options correctly specified
  task_list_arg <- methods::hasArg(task_list)
  
  if (isTRUE(task_list_arg)) {
    if (task_list != 'all' & !is.vector(task_list)) {
      stop('Input to task_list must entered as a \'all\' or be vector (e.g., task_list = c("foodrating"")')
    } else {
      if (sum(!task_list %in% c('all', 'foodrating','foodchoice','shapegame', 'spacegame', 'nih_toolbox','tastetest','pit')) > 0) {
        stop(paste0('at least 1 item in tasks is not an option: ', task_list))
      }
    }
  } else {
    stop('Must provide at least 1 option in tasks argument')
  }
  
  #### Define paths ####
  bids_wd <- file.path(base_wd, 'bids')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives')
  
  
  # Food Rating ####
  
  if (task_list == 'all' | 'foodrating' %in% task_list) {
    
    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, fnirs_overwrite = fnirs_overwrite, task_list = 'foodrating')
    }
    
    print('-- creating Food Rating summary data')
    
    # get list of available subjects 
    foodrating_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'nirs')), pattern = '*foodrating_events.tsv', recursive = TRUE))
    names(foodrating_list) <- 'filename'
    
    #get list of subject IDs
    foodrating_list[['sub_str']] <- sapply(foodrating_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #get summary data -> produces derivative dataframe
    foodrating_database <- util_group_foodrating(data_list = foodrating_list, ses = 'baseline', base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
    
  }
  
  # Food Choice ####
  
  if (task_list == 'all' | 'foodchoice' %in% task_list) {
    
    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, fnirs_overwrite = fnirs_overwrite, task_list = 'foodchoice')
    }
    
    print('-- creating Food Choice summary data')
    
    # get list of available subjects 
    foodchoice_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'nirs')), pattern = '*foodchoice_events.tsv', recursive = TRUE))
    names(foodchoice_list) <- 'filename'
    
    #get list of subject IDs
    foodchoice_list[['sub_str']] <- sapply(foodchoice_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #get summary data -> produces derivative dataframe
    foodchoice_database <- util_group_foodchoice(data_list = foodchoice_list, ses = 'baseline', base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
  }
  
  # Shape Game ####
  
  if (task_list == 'all' | 'shapegame' %in% task_list) {
    
    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, fnirs_overwrite = fnirs_overwrite, task_list = 'shapegame')
    }
    
    print('-- creating Shape Game summary data')
    
    # get list of available subjects 
    shapegame_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'beh')), pattern = '*shapegame_events.tsv', recursive = TRUE))
    names(shapegame_list) <- 'filename'
    
    #get list of subject IDs
    shapegame_list[['sub_str']] <- sapply(shapegame_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #get summary data -> produces derivative dataframe
    shapegame_database <- util_group_shapegame(data_list = shapegame_list, ses = 'baseline', base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
  }
  
  # Space Game ####
  
  if (task_list == 'all' | 'spacegame' %in% task_list) {
    
    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, fnirs_overwrite = fnirs_overwrite, task_list = 'spacegame')
    }
    
    print('-- creating Space Game summary data')
    
    # get list of available subjects 
    spacegame_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'beh')), pattern = '*spacegame_events.tsv', recursive = TRUE))
    names(spacegame_list) <- 'filename'
    
    #get list of subject IDs
    spacegame_list[['sub_str']] <- sapply(spacegame_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #get summary data -> produces derivative dataframe
    spacegame_database <- util_group_spacegame(data_list = spacegame_list, ses = 'baseline', base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
  }
  
  # NIH Toolbox ####
  
  if (task_list == 'all' | 'nihtoolbox' %in% task_list) {
    
    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, fnirs_overwrite = fnirs_overwrite, task_list = 'nihtoolbox')
    }
    
    print('-- creating NIH Toolbox summary data')
    
    # get list of available subjects 
    nihtoolbox_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'beh')), pattern = '*nih_toolbox_events.tsv', recursive = TRUE))
    names(nihtoolbox_list) <- 'filename'
    
    #get list of subject IDs
    nihtoolbox_list[['sub_str']] <- sapply(nihtoolbox_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    # generate derivatives file
    
    #create derivative file
    if (!dir.exists(phenotype_wd)) {
      dir.create(phenotype_wd, recursive = TRUE)
    }
    
    nih_scores_dat <- do.call('rbind', sapply(nihtoolbox_list[['sub_str']], function(x) read.table(file.path(raw_wd, x, 'ses-baseline', 'beh', paste0(x, '_ses-baseline_task-nih_toolbox_scores.tsv')), sep = '\t', header = TRUE), simplify = FALSE))
    
    write.table(nih_scores_dat, file.path(phenotype_wd, 'nih_toolbox_scores.tsv'), sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
    
    #generate json file for derivative data
    nihtoolbox_json <- json_nihtoolbox_scores()
    
    nihtoolbox_filename_json <- file.path(phenotype_wd, 'nih_toolbox_scores.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(nihtoolbox_json, nihtoolbox_filename_json)
    }
  }
  
  
  
  
}

