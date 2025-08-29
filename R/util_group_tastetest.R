#' util_group_tastetest: Get summary data from the Taste-Test task
#'
#' This function calculates summary performance data and saves the output
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param data_list A data frame with variable 'sub_str' that includes all participants that have task data in rawdata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_tastetest
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' group_tastetest_data <- util_group_tastetest(data_list, ses, base_wd, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_tastetest <- function(data_list, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
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
  
  # extract more info
  data_list['nirs_folder_name'] <- ifelse(grepl('pre', data_list[['filename']]), 'premeal', 'postmeal')
  
  #### Participant Summary Function #####
  
  sum_database_fn <- function(data_list, sub_str, base_wd, format){
    
    # get directory paths
    raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, 'ses-followup', 'nirs')
    
    sub_list <- data_list[data_list['sub_str'] == sub_str, ]
    n_files <- nrow(sub_list)
    
    sum_dat <- as.data.frame(t(sapply(seq(1, n_files), function(x) util_tastetest_summary(sub_str = sub_str, raw_wd = raw_wd, nirs_folder = sub_list[x, 'nirs_folder_name'], file_name = sub_list[x, 'filename']))))
    
    
    if (format == 'wide'){
      #labels
      names_label = c(ifelse(grepl('pre', sub_list[['filename']]), 'pre_', 'post_'))
      
      #make wide
      if (nrow(sum_dat) == 2){
        sum_dat_wide <- cbind.data.frame(sum_dat[1, ], sum_dat[2, !grepl('visit_date', names(sum_dat))])
      } else {
        if (names_label[1] == 'pre_'){
          sum_dat_wide <- cbind.data.frame(sum_dat[1, ], t(rep(NA, ncol(sum_dat)-1)))
        } else {
          sum_dat_wide <- cbind.data.frame(c(sum_dat[1, 'visit_date'], t(rep(NA, ncol(sum_dat)-1)), sum_dat[1, ]))
        }
      }
      
      #add meal label
      names(sum_dat_wide) <- c('visit_date', paste0(names_label[1], names(sum_dat)[!grepl('visit_date', names(sum_dat))]), paste0(names_label[2], names(sum_dat)[!grepl('visit_date', names(sum_dat))]))
      
      # add participant info
      sum_dat_wide['participant_id'] <- sub_str
      sum_dat_wide['session_id'] <- 'ses-baseline'

      sum_dat_wide <- sum_dat_wide[c('participant_id', 'session_id', 'visit_date', names(sum_dat_wide)[grepl('^pre_', names(sum_dat_wide))], names(sum_dat_wide)[grepl('^post_', names(sum_dat_wide))])] 
      
      return(as.data.frame(sum_dat_wide))
      
    } else {
      
      #labels
      names_label = c(ifelse(grepl('pre', sub_list[['filename']]), 'pre_meal', 'post_meal'))
      
      sum_dat['meal_cond'] <- names_label
      
      # add participant info
      sum_dat['participant_id'] <- sub_str
      sum_dat['session_id'] <- 'ses-baseline'
      
      sum_dat <- sum_dat[c('participant_id', 'session_id', 'visit_date', 'meal_cond', names(sum_dat)[!grepl('_id|^visit|meal_cond', names(sum_dat))])] 
      
      return(as.data.frame(sum_dat))
    }
    
  }
  
  
  #### Save in derivatives #####
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives', 'nris-beh')
  
  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }
  
  unique_sub <- unique(data_list[['sub_str']])
  
  ## Wide Data ####
  if (!file.exists(file.path(deriv_wd, 'task-tastetest_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database <- as.data.frame(t(sapply(unique_sub, function(x) sum_database_fn(data_list = data_list, sub_str = x, base_wd = base_wd, format = 'wide'), simplify = TRUE, USE.NAMES = TRUE)))
    
    sum_database <- as.data.frame(sapply(sum_database, function(x) unlist(x)))
    
    sum_database[!grepl('_id|^visit', names(sum_database))] <- sapply(sum_database[!grepl('_id|^visit', names(sum_database))], function(x) round(as.numeric(x), 2))
    
    write.table(as.data.frame(sum_database), file.path(deriv_wd, 'task-tastetest_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
  }
  
  #generate json file for derivative data
  tastetest_json <- json_tastetest()
  
  tastetest_filename_json <- file.path(deriv_wd, 'task-tastetest_beh.json')
  
  if ( isTRUE(overwrite) | !file.exists(tastetest_filename_json) ) {
    write(tastetest_json, tastetest_filename_json)
  }
  
  ## Long Data ####
  if (!file.exists(file.path(deriv_wd, 'task-tastetest_desc-long_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database_long <- do.call(rbind.data.frame, sapply(unique_sub, function(x) sum_database_fn(data_list = data_list, sub_str = x, base_wd = base_wd, format = 'long'), simplify = FALSE, USE.NAMES = TRUE))
    
    sum_database_long <- as.data.frame(sapply(sum_database_long, function(x) unlist(x)))
    
    sum_database_long[!grepl('_id|^visit|meal_cond', names(sum_database_long))] <- sapply(sum_database_long[!grepl('_id|^visit|meal_cond', names(sum_database_long))], function(x) round(as.numeric(x), 2))
    
    write.table(as.data.frame(sum_database_long), file.path(deriv_wd, 'task-tastetest_desc-long_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
  }
  
  #generate json file for derivative data
  tastetest_long_json <- json_tastetest_long()
  
  tastetest_long_filename_json <- file.path(deriv_wd, 'task-tastetest_long_beh.json')
  
  if ( isTRUE(overwrite) | !file.exists(tastetest_long_filename_json) ) {
    write(tastetest_long_json, tastetest_long_filename_json)
  }
  
  if (isTRUE(return_data)){
    tastetest_data <- list(
      tastetest_beh = list(data = sum_database, meta = tastetest_json),
      tastetest_beh_long = list(data = sum_database_long, meta = tastetest_long_json))
    
    return(tastetest_data)
  }
}
