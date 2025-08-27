#' util_group_shapegame: Get summary data from the Shape Game (Value-Modulated Attentional Capture Task)
#'
#' This function calculates summary performance data and saves the output in a wide format (overall task) and long format (by block)
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param data_list A data frame with variable 'sub_str' that includes all participants
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
#' group_shapegame_dat <- util_group_shapegame(data_list, ses, base_wd, overwrite, return_data = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_shapegame <- function(data_list, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(base_wd)
  
  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("base_wd must be entered as a string")
  }
  
  
  #### Participant Summary Function #####
  
  sum_database_fn <- function(sub_str, ses, base_wd, format){
    # get directory paths
    raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'beh')
    
    data_file <- file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-shapegame_events.tsv'))
    
    #debug
    #print(sub_str)
    
    dat <- read.table(data_file, sep='\t', header = TRUE, na.strings = 'n/a')
    
    #condition assignment
    cond_colors <- c('orange', 'blue')
    low_trial <- dat[dat['trial_type'] == 'low', grepl('pos', names(dat))][1, ]
    high_trial <- dat[dat['trial_type'] == 'high', grepl('pos', names(dat))][1, ]
    low_color <- cond_colors[cond_colors %in% low_trial]
    high_color <- cond_colors[cond_colors %in% high_trial]
    
    if(format == 'wide'){
      sum_dat <- util_shapegame_summary(dat, format = 'wide')
      
      sum_dat['participant_id'] <- sprintf('sub-%03d', dat[1, 'sub'])
      sum_dat['session_id'] <- paste0('ses-', ses)
      sum_dat['visit_date'] <- dat[['date']][1]
      
      sum_dat['prac_rt_mean'] <- dat[['prac_rt_mean']][1]
      sum_dat['prac_rt_sd'] <- dat[['prac_rt_sd']][1]
      sum_dat['rt_cutoff'] <- dat[['rt_cutoff']][1]
      
      sum_dat['low_color'] <- low_color
      sum_dat['high_color'] <- high_color
      
      sum_dat <- sum_dat[c('participant_id', 'session_id', 'visit_date', names(sum_dat)[grepl('prac|cutoff|color', names(sum_dat))], names(sum_dat)[!grepl('_id|^visit|prac|cutoff|color', names(sum_dat))])] 
      
    } else {
      sum_dat <- do.call(rbind, t(sapply(unique(dat[['block']]), function(x) util_shapegame_summary(dat[dat[['block']] == x, ], format = 'block'), simplify = FALSE)))
      
      sum_dat['participant_id'] <- sprintf('sub-%03d', dat[1, 'sub'])
      sum_dat['session_id'] <- paste0('ses-', ses)
      sum_dat['visit_date'] <- dat[['date']][1]
      sum_dat['block'] <- unique(dat[['block']])
      
      sum_dat['prac_rt_mean'] <- dat[['prac_rt_mean']][1]
      sum_dat['prac_rt_sd'] <- dat[['prac_rt_sd']][1]
      sum_dat['rt_cutoff'] <- dat[['rt_cutoff']][1]
      
      sum_dat['low_color'] <- low_color
      sum_dat['high_color'] <- high_color
      
      sum_dat <- sum_dat[c('participant_id', 'session_id', 'visit_date', 'block', names(sum_dat)[grepl('prac|cutoff|color', names(sum_dat))], names(sum_dat)[!grepl('_id|^visit|block|prac|cutoff|color', names(sum_dat))])] 
      
    }
    
    return(as.data.frame(sum_dat))
  }
  
  
  #### Save in derivatives #####
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives', 'beh')
  
  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }
  
  
  ## Wide/Overall Data ####
  if (!file.exists(file.path(deriv_wd, 'task-shapegame_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database <- as.data.frame(t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'baseline', base_wd = base_wd, format = 'wide'), simplify = TRUE, USE.NAMES = TRUE)))
    
    sum_database[!grepl('_id|^visit|color', names(sum_database))] <- sapply(sum_database[!grepl('_id|^visit|color', names(sum_database))], function(x) round(as.numeric(x), 3))
    
    sum_database[grepl('_id|^visit|color', names(sum_database))] <- sapply(sum_database[grepl('_id|^visit|color', names(sum_database))], function(x) as.character(x))
    
    write.table(sum_database[1:12], file.path(deriv_wd, 'task-shapegame_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
    #generate json file for derivative data
    shapegame_json <- json_shapegame()
    
    shapegame_filename_json <- file.path(deriv_wd, 'task-shapegame_beh.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(shapegame_json, shapegame_filename_json)
    }
    
  }
  
  ## Long Data ####
  
  if (!file.exists(paste0(deriv_wd, 'task-shapegame_desc-long_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database_long <- do.call(rbind.data.frame, sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'baseline', base_wd = base_wd, format = 'long'), simplify = FALSE, USE.NAMES = TRUE))
    
    sum_database_long[!grepl('_id|^visit|color', names(sum_database_long))] <- sapply(sum_database_long[!grepl('_id|^visit|color', names(sum_database_long))], function(x) round(as.numeric(x), 3))
    
    sum_database_long[grepl('_id|^visit|color', names(sum_database_long))] <- sapply(sum_database_long[grepl('_id|^visit|color', names(sum_database_long))], function(x) as.character(x))
    
    write.table(sum_database_long, file.path(deriv_wd, 'task-shapegame_desc-long_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
    #generate json file for derivative data
    shapegame_long_json <- json_shapegame_long()
    
    shapegame_long_filename_json <- file.path(deriv_wd, 'task-shapegame_desc-long_beh.json')
    
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(shapegame_long_json, shapegame_long_filename_json)
    }
    
  }
  
  if (isTRUE(return_data)){
    shapegame_data <- list(
      shapegame_beh = list(data = sum_database, meta = shapegame_json),
      shapegame_beh_long = list(data = sum_database_long, meta = shapegame_long_json))
    
    return(shapegame_data)
  }
}
