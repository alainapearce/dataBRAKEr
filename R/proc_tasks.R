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
#' @param base_wd full path to directory containing both raw_untouched and bids directories
#' @inheritParams util_task_org_sourcedata
#'
#' @return data.frame for each task with status for each processing step
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' proc_tasks_pardat <- proc_tasks(base_wd, overwrite)
#'
#' \dontrun{
#' }
#'
#'
#' @export

proc_tasks <- function(base_wd, overwrite = FALSE) {
  
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
  data_path <- file.path(base_wd, 'raw_untouched')
  sourcedata_wd <- file.path(base_wd, 'bids', 'sourcedata')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  
  # Food Rating ####
  # get list of available subjects 
  foodrating_list <- as.data.frame(list.files(path = file.path(data_path, 'foodrating_game'), pattern = '.csv'))
  names(foodrating_list) <- 'filename'
  
  #get list of subject IDs
  foodrating_list[['sub_str']] <- sapply(foodrating_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  foodrating_list[['sourcedata_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodrating', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = overwrite), simplify = TRUE)
  
  #process raw data
  foodrating_list[['rawproc_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_foodrating(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
  
  #json 
  #foodrating_json <- json_foodrating()
  
  # Food Choice ####
  # get list of available subjects 
  foodchoice_list <- as.data.frame(list.files(path = file.path(data_path, 'foodchoice_game'), pattern = '.csv'))
  names(foodchoice_list) <- 'filename'
  
  #get list of subject IDs
  foodchoice_list[['sub_str']] <- sapply(foodchoice_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #get eye-tracking status
  foodchoice_list[['eye_track']] <- sapply(foodchoice_list[['filename']], function(x) ifelse(grepl('noeye', x), 'N', 'Y'), simplify = TRUE)
  
  #valid choice-pairing assignment
  foodchoice_list[['choice_pairing']] <- sapply(foodchoice_list[['filename']], function(x) ifelse(grepl('999', x), '999', 'rating'), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  foodchoice_list[['sourcedata_done']] <- sapply(foodchoice_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodchoice', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = overwrite), simplify = TRUE)
  
  #process raw data
  foodchoice_list['rawproc_done'] <- sapply(foodchoice_list[['sub_str']], function(x) util_task_foodchoice(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
  
  #json 
  #foodchoice_json <- json_foodchoice()
  
  # Shape Game ####
  # get list of available subjects 
  shape_list <- as.data.frame(list.files(path = file.path(data_path, 'shape_game'), pattern = '.csv'))
  names(shape_list) <- 'filename'
  
  #get list of subject IDs
  shape_list[['sub_str']] <- sapply(shape_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #get eye-tracking status
  shape_list[['eye_track']] <- sapply(shape_list[['filename']], function(x) ifelse(grepl('noeye', x), 'N', 'Y'), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  shape_list[['sourcedata_done']] <- sapply(shape_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'shape', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
  
  #process raw data
  shape_list['rawproc_done'] <- sapply(shape_list[['sub_str']], function(x) util_task_shapegame(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
  
  #get summary data -> produces derivative dataframe
  shape_database <- util_group_shapegame(data_list = shape_list, ses = 'baseline', base_wd = base_wd, overwrite = TRUE)
  
  
  # Space Game ####
  # get list of available subjects 
  space_list <- as.data.frame(list.files(path = file.path(data_path, 'space_game'), pattern = '.mat'))
  names(space_list) <- 'filename'
  
  #get list of subject IDs
  space_list[['sub_str']] <- sapply(space_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  space_list[['sourcedata_done']] <- sapply(space_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'space', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
  
  #process raw data
  space_list['rawproc_done'] <- sapply(space_list[['sub_str']], function(x) util_task_spacegame(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
  
  #get summary data -> produces derivative dataframe
  space_database <- util_group_spacegame(data_list = space_list, ses = 'baseline', base_wd = base_wd, overwrite = TRUE)
  
  # NIH Toolbox - raw data ####
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
  
  # generate derivatives file
  
  #create derivative file
  if (!dir.exists(phenotype_wd)) {
    dir.create(phenotype_wd, recursive = TRUE)
  }
  
  nih_events_dat <- do.call('rbind', sapply(nih_list[['sub_str']], function(x) read.table(file.path(raw_wd, x, 'ses-baseline', 'beh', paste0(x, '_ses-baseline_task-nih_toolbox_events.tsv')), sep = '\t', header = TRUE), simplify = FALSE))
  write.table(nih_events_dat, file.path(phenotype_wd, 'nih_toolbox_events.tsv'), sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
  
  nih_scores_dat <- do.call('rbind', sapply(nih_list[['sub_str']], function(x) read.table(file.path(raw_wd, x, 'ses-baseline', 'beh', paste0(x, '_ses-baseline_task-nih_toolbox_scores.tsv')), sep = '\t', header = TRUE), simplify = FALSE))
  write.table(nih_scores_dat, file.path(phenotype_wd, 'nih_toolbox_scores.tsv'), sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
  
                            
  # Food Taste-Test ####
  # get list of available subjects 
  tastetest_list <- as.data.frame(list.files(path = file.path(data_path, 'tastetest_game'), pattern = '.csv'))
  names(tastetest_list) <- 'filename'
  
  #get list of subject IDs
  tastetest_list[['sub_str']] <- sapply(tastetest_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  #organize data into BIDS sourcedata
  tastetest_list[['sourcedata_done']] <- sapply(tastetest_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'tastetest', sub_str = x, ses = 'followup', base_wd = base_wd, task_cat = 'nirs', overwrite = overwrite), simplify = TRUE)
  
  #process raw data
  tastetest_list[['rawproc_done']] <- sapply(tastetest_list[['sub_str']], function(x) util_task_tastetest(sub_str = x, ses = 'followup', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
  
  #json 
  #foodrating_json <- json_foodrating()
  
  
  return(list( foodchoice_dat = dat,
               foodchoice_labels = meta_json))
  
}

