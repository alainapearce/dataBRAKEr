#' util_task_org_sourcedata: Organize task raw data into BIDS format sourcedata
#'
#' This function copies data from raw_untouched and saves it in sourcedata
#' 
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @param task_str string for task
#' @param sub_str participant string (i.e., sub_###) as character
#' @param ses session name (i.e., 'baseline', 'followup') 
#' @param base_wd absolute path to the data directory
#' @param task_cat bids category/directory for task (e.g., 'nirs', 'beh')
#' @param overwrite logical indicating if data should be overwritten. Default = FALSE
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' org_foodchoice <- util_task_org_sourcedata(task_str = 'foodchoice', sub_str = 'sub_001', base_wd, overwrite = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_org_sourcedata <- function(task_str, sub_str, ses, base_wd, task_cat, overwrite = FALSE) {
  
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
  
  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The proc_tasks.R has not been thoroughly tested on Windows systems, may have base_wd errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
  
  if (task_str == 'nih'){
    raw_untouched_path <- paste0(base_wd, slash,'raw_untouched', slash, task_str, '_toolbox', slash)
    
  } else {
    raw_untouched_path <- paste0(base_wd, slash,'raw_untouched', slash, task_str, '_game', slash)
  }
  
  # get all files for sub in raw_untouched
  if (task_str == 'nih'){
    raw_files <- list.files(path = raw_untouched_path, pattern = sub_str)
    raw_files <- raw_files[grepl('events', raw_files)]
    
    # new file name
    if (sum(grepl('listsort', raw_files)) == 1){
      rename_files <- gsub('_listsort', paste0('_ses-', ses, '_task-nih_listsort'), raw_files[grepl('listsort', raw_files)])
    } 
    
    if (sum(grepl('flanker', raw_files)) == 1){
      if (exists('rename_files')) {
        rename_files[2] <- gsub('_flanker-dccs', paste0('_ses-', ses, '_task-nih_flanker'), raw_files[grepl('flanker', raw_files)])
        
        rename_files[3] <- gsub('_flanker-dccs', paste0('_ses-', ses, '_task-nih_dccs'), raw_files[grepl('dccs', raw_files)])
      } else {
        rename_files <- gsub('_flanker-dccs', paste0('_ses-', ses, '_task-nih_flanker'), raw_files[grepl('flanker', raw_files)])
        
        rename_files[2] <- gsub('_flanker-dccs', paste0('_ses-', ses, '_task-nih_dccs'), raw_files[grepl('dccs', raw_files)])
      }
    }
    
  } else {
    raw_files <- list.files(path = raw_untouched_path, pattern = sub_str)
    
    # new file name
    if (task_str == 'space'){
      rename_files <- gsub(paste0('_task-', task_str, 'game'), paste0('_ses-', ses, '_task-', task_str, 'game_events'), raw_files)
    } else if (task_str == 'shape') {
      rename_files <- gsub(paste0('_', task_str, 'game-vmac'), paste0('_ses-', ses, '_task-', task_str, 'game_events'), raw_files)
      rename_files <- gsub('_noeyetrack', '', rename_files)
    } else if (task_str == 'tastetest') {
      
      if (grepl('post', raw_files[1])){
        desc_str <- 'post'
      } else {
        desc_str <- 'pre'
      }
      
      rename_files <- gsub(paste0('-', desc_str,'-meal_', task_str), paste0('_ses-', ses, '_task-', task_str, '_desc-', desc_str, 'meal_events'), raw_files)
    } else {
      rename_files <- gsub(paste0('_', task_str), paste0('_ses-', ses, '_task-', task_str, '_events'), raw_files)
      
      rename_files <- gsub('_noeyetracking', '', rename_files)
    }
  }
  
  
  #### Save in sourcedata #####
  # set paths for other directories
  source_wd <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, 'ses-', ses, slash, task_cat, slash, task_str, slash)
  
  if (task_str == 'tastetest'){
    #fix sub str for tastetest
    source_wd <- gsub(sub_str, gsub(paste0('-', desc_str, '-meal'), '', sub_str), source_wd) 
    source_wd <- gsub(task_str, paste0(task_str, '-', desc_str, 'meal'), source_wd)
  } 
  
  #make directory if needed
  if (!dir.exists(source_wd)) {
    dir.create(source_wd, recursive = TRUE)
  } 
  
  # copy files
  if (!file.exists(gsub('csv', 'tsv', paste0(source_wd, rename_files[1]))) | isTRUE(overwrite)) {  
    
    if (task_str != 'space'){
      #copy non .csv  files over
      file.copy(from = paste0(raw_untouched_path, raw_files[!grepl('.csv', raw_files)]), to = paste0(source_wd, rename_files[!grepl('.csv', rename_files)]))
      
      #change data file to .tsv
      rename_tsv <- gsub('.csv', '.tsv', rename_files[grepl('.csv', rename_files)])
      
      if (task_str == 'nih'){
        if (sum(grepl('listsort', raw_files)) == 1){
          dat_listsort <- read.csv(paste0(raw_untouched_path, raw_files[grepl('listsort', raw_files)]), header = TRUE)
          
          write.table(dat_listsort, paste0(source_wd, rename_tsv[grepl('listsort', rename_tsv)]), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
        }
        
        if (sum(grepl('flanker', raw_files)) == 1){
        
          dat_flankerdccs <- read.csv(paste0(raw_untouched_path, raw_files[grepl('flanker', raw_files)]), header = TRUE)
          
          write.table(dat_flankerdccs[grepl('Flanker', dat_flankerdccs[['Inst']]), ], paste0(source_wd, rename_tsv[grepl('flanker', rename_tsv)]), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
          write.table(dat_flankerdccs[!grepl('Flanker', dat_flankerdccs[['Inst']]), ], paste0(source_wd, rename_tsv[grepl('dccs', rename_tsv)]), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
        }
        
      } else {
        dat <- read.csv(paste0(raw_untouched_path, raw_files[grepl('.csv', raw_files)]), header = TRUE)
        write.table(dat, paste0(source_wd, rename_tsv), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
      }
      
      
    } else {
      file.copy(from = paste0(raw_untouched_path, raw_files), to = paste0(source_wd, rename_files))
      
    }
    
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
