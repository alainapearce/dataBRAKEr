#' util_task_nihtoolbox: Process raw data from the NIH Toolbox
#'
#' This function: 1) cleans data to save in BIDS format in rawdata and 2) generates summary data that can be used to generate a database
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @param task task string ('flanker', 'dccs', or 'listserve')
#' @param sub_str_list list of participant strings for group database
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

util_task_nihtoolbox <- function(task, sub_str_list, base_wd, overwrite = FALSE, return_data = TRUE) {
  
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
  
  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The nihtoolbox_task.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
  # get directory paths
  untouched_raw_wd <- paste0(base_wd, slash, 'raw_untouched', slash, 'nih_toolbox', slash)
  
  read_nih_scores <- function(dir, sub_str, task) {
    if (task == 'listsort'){
      data_file <- paste0(dir, slash, sub_str, '_', task, '-scores.csv')
      dat <- suppressWarnings(read.csv(data_file, sep = ',', header = TRUE, na.strings = c('n/a', 'NA')))
    } else {
      data_file <- paste0(dir, slash, sub_str, '_flanker-dccs-scores.csv')
      dat <- suppressWarnings(read.csv(data_file, sep = ',', header = TRUE, na.strings = c('n/a', 'NA')))
      
      if (task == 'flanker'){
        dat <- dat[grepl('Flanker', dat[['Inst']]), ]
      } else {
        dat <- dat[!grepl('Flanker', dat[['Inst']]), ]
      }
    }
    
    return(dat)
  }
  
  #### Get group database #####
  group_dat <- rbind.data.frame(t(sapply(sub_str_list, function(x) read_nih_scores(dir = untouched_raw_wd, sub_str = x, task = task), simplify = TRUE)))
  
  names(group_dat) <- tolower(names(group_dat))
  names(group_dat)[1] <- 'participant_id'
  
  # get numeric sub
  uscore_loc <- unlist(gregexpr('-', sub_str_list[1]))
  group_dat[['participant_id']] <- as.numeric(sapply(sub_str_list, function(x) substr(x, uscore_loc+1, tail(slash_loc, 1)), simplify = TRUE))
  
  # add session
  group_dat[['session']] <- 'baseline'
  
  # organize
  group_dat <- group_dat[c('participant_id', 'session', 'rawscore', 'itmcnt', 'computed.score', 'uncorrected.standard.score', 'age.corrected.standard.score', 'national.percentile..age.adjusted.', 'fully.corrected.t.score', 'instrumentbreakoff', 'instrumentstatus2', 'instrumentrcreason', 'instrumentrcreasonother')]
    
  names(group_dat)[4:13] <- c('item_count', 'computed_score', 'uncorrected_ss', 'age_corrected_ss', 'national_pcentile_age_ss', 'corrected_tscore', 'instrument_breakoff', 'instrument_status', 'instrument_rc_reason', 'instrument_rc_reaso_nother')
  
  return(group_dat)
}

