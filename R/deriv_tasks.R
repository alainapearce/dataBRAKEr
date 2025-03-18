#' deriv_tasks: Generate derivative databases for all computer tasks used in Study BRAKE
#'
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
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  deriv_wd <- file.path(base_wd, 'bids', 'phenotype')
  
  #create derivative file
  if (!dir.exists(phenotype_wd)) {
    dir.create(phenotype_wd, recursive = TRUE)
  }
  
  #create derivative file
  if (!dir.exists(phenotype_wd)) {
    dir.create(phenotype_wd, recursive = TRUE)
  }
  
  # Food Rating ####
  # get list of available subjects 
  foodrating_list <- as.data.frame(list.files(path = file.path(data_path, 'foodrating_game'), pattern = '.csv'))
  names(foodrating_list) <- 'filename'
  
  #json 
  #foodrating_json <- json_foodrating()
  
  # Food Choice ####
  # get list of available subjects 
  foodchoice_list <- as.data.frame(list.files(path = file.path(data_path, 'foodchoice_game'), pattern = '.csv'))
  names(foodchoice_list) <- 'filename'

  
  #json 
  #foodchoice_json <- json_foodchoice()
  
  # Shape Game ####
  # get list of available subjects 
  shape_list <- as.data.frame(list.files(path = file.path(data_path, 'shape_game'), pattern = '.csv'))
  names(shape_list) <- 'filename'
  
  
  # Space Game ####
  # get list of available subjects 
  space_list <- as.data.frame(list.files(path = file.path(data_path, 'space_game'), pattern = '.mat'))
  names(space_list) <- 'filename'
  
  
  
  # NIH Toolbox - raw data ####
  # get list of available subjects 
  list.files(file.path(raw_wd), pattern = paste0('task-nih_toolbox_events'), recursive = TRUE, full.names = TRUE)
  
  nih_list <- as.data.frame(list.files(path = file.path(raw_wd), pattern = 'events.csv', , recursive = TRUE, full.names = TRUE))
  names(nih_list) <- 'filename'
  
  # generate derivatives file
  nih_dat_scores <- sapply(nih_list[['sub_str']], function(x) util_task_nihtoolbox(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)
  
  nih_dat_scores <- sapply(nih_list[['sub_str']], function(x) util_task_nihtoolbox(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)
  
  write.table(data, outfiles[1], sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
  
  
  # Food Taste-Test ####
  # get list of available subjects 
  tastetest_list <- as.data.frame(list.files(path = file.path(data_path, 'tastetest'), pattern = '.csv'))
  names(tastetest_list) <- 'filename'
  
  #json 
  #foodrating_json <- json_foodrating()
  
  
  return(list( foodchoice_dat = dat,
               foodchoice_labels = meta_json))
  
}

