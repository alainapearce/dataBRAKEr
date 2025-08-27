#' util_group_foodrating: Get summary data from the Food Rating task
#'
#' This function calculates summary performance data and saves the output
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param data_list A data frame with variable 'sub_str' that includes all participants that have task data in rawdata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_foodrating
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' group_foodrating_data <- util_group_foodrating(data_list, ses, base_wd, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_foodrating <- function(data_list, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(base_wd)
  
  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }
  
  #### Participant Summary Function #####
  
  sum_database_fn <- function(sub_str, ses, base_wd){
    # get directory paths
    raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, 'ses-baseline', 'nirs')
    
    data_file <- file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-foodrating_events.tsv'))
    
    #print(sub_str)
    
    dat <- read.table(data_file, sep='\t', header = TRUE, na.strings = 'n/a')
    
    
    sum_dat <- util_foodrating_summary(dat)
    sum_dat['participant_id'] <- sprintf('sub-%03d', dat[1, 'sub'])
    sum_dat['session_id'] <- paste0('ses-', ses)
    sum_dat['visit_date'] <- dat[['date']][1]
    
    sum_dat <- sum_dat[c('participant_id', 'session_id', 'visit_date', names(sum_dat)[!grepl('_id|^visit', names(sum_dat))])] 
    
    return(as.data.frame(sum_dat))
  }
  
  
  #### Save in derivatives #####
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives', 'nris-beh')
  
  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }
  
  ## Wide/Overall Data ####
  if (!file.exists(file.path(deriv_wd, 'task-foodrating_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database <- as.data.frame(t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'baseline', base_wd = base_wd), simplify = TRUE, USE.NAMES = TRUE)))
    
    sum_database[!grepl('_id|^visit|order', names(sum_database))] <- sapply(sum_database[!grepl('_id|^visit|order', names(sum_database))], function(x) round(as.numeric(x), 3))
    
    sum_database[grepl('_id|^visit|order', names(sum_database))] <- sapply(sum_database[grepl('_id|^visit|order', names(sum_database))], function(x) as.character(x))
    
    write.table(as.data.frame(sum_database), file.path(deriv_wd, 'task-foodrating_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
  }
  
  #generate json file for derivative data
  foodrating_json <- json_foodrating()
  
  foodrating_filename_json <- file.path(deriv_wd, 'task-foodrating_beh.json')
  
  if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
    write(foodrating_json, foodrating_filename_json)
  }
  
  if (isTRUE(return_data)){
    foodrating_data <- list(data = sum_database, meta = foodrating_json)
    
    return(foodrating_data)
  }
}
