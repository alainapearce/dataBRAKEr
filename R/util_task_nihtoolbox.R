#' util_task_nihtoolbox: Process raw data from the NIH Toolbox
#'
#' This function: 1) cleans data to save in BIDS format in rawdata and 2) generates summary data that can be used to generate a database
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @param return_data logical indicating if data should be returned. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' nihtoolbox_task_pardat <- util_task_nihtoolbox(task, data, base_wd, overwrite, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_nihtoolbox <- function(sub_str, ses, base_wd, overwrite = FALSE, return_data = TRUE) {
  
  print(sub_str)
  
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
  
  # get directory paths
  untouched_raw_wd <- file.path(base_wd, 'raw_untouched', 'nih_toolbox')
  
  # get directory paths
  source_beh_wd <- file.path(bids_wd, 'sourcedata', sub_str, paste0('ses-', ses_str), 'beh')
  raw_beh_wd <- file.path(bids_wd, 'rawdata', sub_str, paste0('ses-', ses_str), 'beh')
  
  # function to process data
  proc_nih <- function(data){
    # make separate columns for task (e.g., 'Flanker Inhibitory Control') and test ages (e.g., 'Ages 8-11 v2.1') from Inst (e.g., 'NIH Toolbox Flanker Inhibitory Control and Attention Test Ages 8-11 v2.1') ??
    # Separate the 'Inst' column into 'Test' and 'Ages' columns
    data <- tidyr::separate(data, Inst, into = c('Test', 'Test_Ages'), sep = 'Test', remove = FALSE)
    
    # Replace values in the 'Test' column
    data <- data %>%
      dplyr::mutate(Test = dplyr::case_when(
        stringr::str_detect(Test, 'Flanker Inhibitory Control') ~ 'flanker',
        stringr::str_detect(Test, 'Dimensional Change Card Sort') ~ 'dccs',
        stringr::str_detect(Test, 'List Sorting Working Memory') ~ 'listsort',
        TRUE ~ 'other'  # Default case
      ))
    
    # remove columns where Test = other
    data <- data[!(data[['Test']] %in% 'other'),]
    
    # add subject column
    data[['sub']] <- sub_str
    data <- data %>% dplyr::relocate('sub') # move sub to first column
    
    # add session column
    data[['ses']] <- ses_str
    data <- data %>% dplyr::relocate('ses', .after = 1) # after col 1
    
    # bids compliance
    names(data) <- tolower(names(data))
    
    # get numeric sub
    names(data) <- gsub('\\.', '_', names(data))
    
    #update names
    names(data)[names(data) == 'deviceid'] <- 'device_id'
    
    return(data)
  }
  
  
  if (length(list.files(path = source_beh_wd, pattern = 'nih_toolbox_events')) > 0){
    events_file <- list.files(path = source_beh_wd, pattern = 'nih_toolbox_events')
    
    data <- read.table(file.path(source_beh_wd, events_file), sep = '\t', header = TRUE)
    
    data <- proc_nih(data)
    
    #update names
    names(data)[names(data) == 'instordr'] <- 'inst_ordr'
    names(data)[names(data) == 'instsctn'] <- 'inst_sctn'
    names(data)[names(data) == 'itmordr'] <- 'itm_ordr'
    names(data)[names(data) == 'itemid'] <- 'item_id'
    names(data)[names(data) == 'datatype'] <- 'data_type'
    names(data)[names(data) == 'responsetime'] <- 'response_time'
    names(data)[names(data) == 'datecreated'] <- 'date_created'
    names(data)[names(data) == 'inststarted'] <- 'inst_started'
    names(data)[names(data) == 'instended'] <- 'inst_ended'
    
    events_data = TRUE
    
  } else {
    print(paste(sub_str, 'has no sst assessment data file'))
  }
  
  if (length(list.files(path = source_beh_wd, pattern = 'nih_toolbox_scores')) > 0){
    scores_file <- list.files(path = source_beh_wd, pattern = 'nih_toolbox_scores')
    
    scores <- read.table(file.path(source_beh_wd, scores_file), sep = '\t', header = TRUE)
    
    scores <- proc_nih(scores)
    
    #update names
    names(scores) <- gsub('standard_score', 'ss', names(scores))
    names(scores)[names(scores) == 'datefinished'] <- 'date_finished'
    names(scores)[names(scores) == 'national_percentile__age_adjusted_'] <- 'national_percentile_age_adjusted'
    names(scores)[names(scores) == 'instrumentbreakoff'] <- 'instrument_breakoff'
    names(scores)[names(scores) == 'instrumentstatus2'] <- 'instrument_status2'
    names(scores)[names(scores) == 'instrumentrcreason'] <- 'instrument_rc_reason'
    names(scores)[names(scores) == 'instrumentrcreasonother'] <- 'instrument_rc_reason_other'
    
    scores_data = TRUE
    
  } else { print(paste(sub_str, 'has no assessment scores file. Aborting task processing for this sub.'))
    return()
  }
  
  #### Save in rawdata #####
  
  # create bids/rawdata directory if it doesn't exist
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }
  
  if (isTRUE(events_data) | isTRUE(scores_data)){
    # define output file with path
    if (isTRUE(events_data)){
      
      outfile_events <- file.path(raw_beh_wd, paste0(sub_str, '_', paste0('ses-', ses_str), '_task-nih_toolbox_events.tsv'))
      
      if (!file.exists(outfile_events) | isTRUE(overwrite)){
        write.table(data, outfile_events, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
      }
    }
    
    if (isTRUE(scores_data)){
      
      outfile_scores <- file.path(raw_beh_wd, paste0(sub_str, '_', paste0('ses-', ses_str), '_task-nih_toolbox_scores.tsv'))
      
      if (!file.exists(outfile_scores) | isTRUE(overwrite)){
        write.table(scores, outfile_scores, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
      }
    }
    
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
}

