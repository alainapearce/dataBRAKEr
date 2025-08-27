#' proc_tasks: Process task data from raw_untouched to create bids compliant files
#'
#' This function: \itemize{
#' \item{1) copies task data from raw_untouched into bids/sourcedata, using util_task_untouched_to_source}
#' \item{2) processes task sourcedata and exports cleaned dataframes into bids/rawdata using task-specific util_task_{task-name} functions}
#' }
#'
#' @param base_wd full path to directory containing both raw_untouched and bids directories
#' @inheritParams util_task_org_sourcedata
#' @param fnirs_overwrite overwrite fNIRS data files - separate argument because the Matlab fNIRS processing writes onset values to the rawdata/*_events.tsv values. Overwriting will overwrite these values with NA.
#' @param task_list tasks to process. Options include 'all' to process all task data or a list of the following:\itemize{
#'  \item{'foodrating' - fNIRS Food Rating task}
#'  \item{'foodchoice' - fNIRS Food Choice task}
#'  \item{'shapegame' - Shape Game data}
#'  \item{'spacegame' - Space Game data (need to finish processing in Matlab)}
#'  \item{'nihtoolbox' - NIH Toolbox data}
#'  \item{'tastetest' - fNIRS Taste-Test task}
#'  \item{'pit' - Pavlovian Instrumental Transfer task data}
#' }
#'
#'
#' @examples
#'
#' \dontrun{
#' # organize task data for space game and NIH toolbox in untouchedRaw into sourcedata and raw data
#' proc_task(base_wd = base_wd, task_list = c('spacegame', 'nihtoolbox'))
#'
#' }
#'
#'
#' @export
#'

proc_tasks <- function(base_wd, overwrite = FALSE, fnirs_overwrite = FALSE, task_list = 'all') {
  
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
      if (sum(!task_list %in% c('all', 'foodrating','foodchoice','shapegame', 'spacegame','nihtoolbox','tastetest','pit')) > 0) {
        stop(paste0('at least 1 item in tasks is not an option: ', task_list))
      }
    }
  } else {
    stop('Must provide at least 1 option in tasks argument')
  }
  
  #### Define paths ####
  bids_wd <- file.path(base_wd, 'bids')
  data_path <- file.path(base_wd, 'raw_untouched')
  sourcedata_wd <- file.path(base_wd, 'bids', 'sourcedata')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  
  # Food Rating ####
  
  if (task_list == 'all' | 'foodrating' %in% task_list) {
    print('-- processing Food Rating')
    
    # get list of available subjects 
    foodrating_list <- as.data.frame(list.files(path = file.path(data_path, 'foodrating_game'), pattern = '.csv'))
    names(foodrating_list) <- 'filename'
    
    #get list of subject IDs
    foodrating_list[['sub_str']] <- sapply(foodrating_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    foodrating_list[['sourcedata_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodrating', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = fnirs_overwrite), simplify = TRUE)
    
    #process raw data
    foodrating_list[['rawproc_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_foodrating(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = fnirs_overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    foodrating_json <- json_foodrating_events()
    
    foodrating_filename_json <- file.path(bids_wd, 'ses-baseline_task-foodrating_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(foodrating_json, foodrating_filename_json)
    }
    
  }
  
  # Food Choice ####
  
  if (task_list == 'all' | 'foodchoice' %in% task_list) {
    print('-- processing Food Choice')
    
    # get list of available subjects 
    foodchoice_list <- as.data.frame(list.files(path = file.path(data_path, 'foodchoice_game'), pattern = '.csv'))
    names(foodchoice_list) <- 'filename'
    
    #get list of subject IDs
    foodchoice_list[['sub_str']] <- sapply(foodchoice_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #valid choice-pairing assignment
    foodchoice_list[['choice_pairing']] <- sapply(foodchoice_list[['filename']], function(x) ifelse(grepl('999', x), '999', 'rating'), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    foodchoice_list[['sourcedata_done']] <- sapply(foodchoice_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodchoice', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = fnirs_overwrite), simplify = TRUE)
    
    #process raw data
    foodchoice_list['rawproc_done'] <- sapply(foodchoice_list[['sub_str']], function(x) util_task_foodchoice(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = fnirs_overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    foodchoice_json <- json_foodchoice_events()
    
    foodchoice_filename_json <- file.path(bids_wd, 'ses-baseline_task-foodchoice_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(foodchoice_json, foodchoice_filename_json)
    }
  }
  
  # Shape Game ####
  
  if (task_list == 'all' | 'shapegame' %in% task_list) {
    print('-- processing Shape Game')
    
    # get list of available subjects 
    shape_list <- as.data.frame(list.files(path = file.path(data_path, 'shape_game'), pattern = '.csv'))
    names(shape_list) <- 'filename'
    
    #get list of subject IDs
    shape_list[['sub_str']] <- sapply(shape_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    shape_list[['sourcedata_done']] <- sapply(shape_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'shape', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    #process raw data
    shape_list['rawproc_done'] <- sapply(shape_list[['sub_str']], function(x) util_task_shapegame(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    shapegame_json <- json_shapegame_events()
    
    shapegame_filename_json <- file.path(bids_wd, 'ses-baseline_task-shapegame_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(shapegame_json, shapegame_filename_json)
    }
    
  }
  
  
  # Space Game ####
  
  if (task_list == 'all' | 'spacegame' %in% task_list) {
    print('-- processing Space Game')
    
    # get list of available subjects 
    space_list <- as.data.frame(list.files(path = file.path(data_path, 'space_game'), pattern = '.mat'))
    names(space_list) <- 'filename'
    
    #get list of subject IDs
    space_list[['sub_str']] <- sapply(space_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    space_list[['sourcedata_done']] <- sapply(space_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'space', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    #process raw data
    space_list['rawproc_done'] <- sapply(space_list[['sub_str']], function(x) util_task_spacegame(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    spacegame_json <- json_spacegame_events()
    
    spacegame_filename_json <- file.path(bids_wd, 'ses-baseline_task-spacegame_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(spacegame_json, spacegame_filename_json)
    }
    
  }
  
  # NIH Toolbox - raw data ####
  
  if (task_list == 'all' | 'nihtoolbox' %in% task_list) {
    print('-- processing NIH Toolbox')
    
    # get list of available subjects 
    nih_list <- as.data.frame(list.files(path = file.path(data_path, 'nih_toolbox'), pattern = 'events.csv'))
    names(nih_list) <- 'filename'
    
    nih_list_flanker <- as.data.frame(nih_list[grepl('flanker', nih_list[['filename']]), ])
    nih_list_listsort <- as.data.frame(nih_list[grepl('listsort', nih_list[['filename']]), ])
    
    names(nih_list_flanker) <- 'flanker-dccs'
    names(nih_list_listsort) <- 'listsort'
    
    #get list of subject IDs
    nih_list_flanker[['sub_str']] <- sapply(nih_list_flanker[['flanker-dccs']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    nih_list_listsort[['sub_str']] <- sapply(nih_list_listsort[['listsort']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #merge to get 1 set of sub-str
    nih_list <- merge(nih_list_listsort, nih_list_flanker, id = 'sub_str', all = TRUE)
    
    # org
    nih_list[['sourcedata_done']] <- sapply(nih_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'nih', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    # process raw data
    nih_list[['rawdata_done']] <- sapply(nih_list[['sub_str']], function(x) util_task_nihtoolbox(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)
    
    #generate json file for rawdata
    nihtoolbox_json <- json_nihtoolbox_events()
    
    nihtoolbox_filename_json <- file.path(bids_wd, 'ses-baseline_task-nih_toolbox_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(nihtoolbox_json, nihtoolbox_filename_json)
    }
    
  }
  
  # Food Taste-Test ####
  
  if (task_list == 'all' | 'tastetest' %in% task_list) {
    print('-- processing Taste-Test')
    
    # get list of available subjects 
    tastetest_list <- as.data.frame(list.files(path = file.path(data_path, 'tastetest_game'), pattern = '.csv'))
    names(tastetest_list) <- 'filename'
    
    #get list of subject IDs
    tastetest_list[['sub_str']] <- sapply(tastetest_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    tastetest_list[['sourcedata_done']] <- sapply(tastetest_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'tastetest', sub_str = x, ses = 'followup', base_wd = base_wd, task_cat = 'nirs', overwrite = fnirs_overwrite), simplify = TRUE)
    
    #process raw data
    tastetest_list[['rawproc_done']] <- sapply(tastetest_list[['sub_str']], function(x) util_task_tastetest(sub_str = x, ses = 'followup', base_wd = base_wd, overwrite = fnirs_overwrite, return = FALSE), simplify = TRUE)
    
  }
  
  # Food PIT task ####
  if (task_list == 'all' | 'pit' %in% task_list) {
    print('-- processing PIT Task')
    
    # get list of available subjects 
    pit_list <- as.data.frame(list.files(path = file.path(data_path, 'friendsgame_pit'), pattern = '.csv'))
    names(pit_list) <- 'filename'
    
    #get list of subject IDs
    pit_list[['sub_str']] <- sapply(pit_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    pit_list[['sourcedata_done']] <- sapply(pit_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'pit', sub_str = x, ses = 'followup', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    #process raw data
    # pit_list[['rawproc_done']] <- sapply(pit_list[['sub_str']], function(x) util_task_pit(sub_str = x, ses = 'followup', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
    
  }
}

