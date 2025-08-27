#' util_task_shapegame: Process raw data from the Shape Game (Value-Modulated Attentional Capture)
#'
#' This function: \itemize{
#' \item{1) cleans data to save in BIDS format in rawdata}
#' \item{2) generates summary data that can be used to generate a database}
#' }
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @inheritParams util_task_org_sourcedata
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
#' shapegame_task_pardat <- util_task_shapegame(sub_str, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_shapegame <- function(sub_str, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
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
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'beh')
  
  data_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'beh', paste0(sub_str, '_ses-', ses, '_task-shapegame_events.tsv'))
  
  #print(sub_str)
    
  #### Organize Data #####
  dat <- read.csv(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
  
  # get practice rt and rt cutoff
  dat_prac <- dat[!is.na(dat[['practice_trials.thisTrialN']]), ]
  
  # throw out first trial bc sometimes response are longer/may be skew rt/sd estimates
  mean_rt_prac <- mean(dat_prac[2:10, 'prac_key_resp.rt'], na.rm = TRUE)
  sd_rt_prac <- sd(dat_prac[2:10, 'prac_key_resp.rt'], na.rm = TRUE)
  rt_cutoff <- mean_rt_prac + 2*sd_rt_prac
  
  # remove practice and instructions
  dat <- dat[dat[['trial_file']] != '', ]
  
  # add practice info to data
  dat[['prac_rt_mean']] <- mean_rt_prac
  dat[['prac_rt_sd']] <- sd_rt_prac
  dat[['rt_cutoff']] <- rt_cutoff
  
  # remove inter-block message
  dat <- dat[!is.na(dat[['trials.thisN']]), ]
  
  dat[['trialN']] <- dat[['trials.thisN']] + (25*dat[['blocks.thisN']])
  
  dat[['blockN']] <- dat[['blocks.thisN']] + 1
  
  # get target roi
  dat[['target_roi']] <- ifelse(dat[['pos1']] == 'diamond', 'pos1', ifelse(dat[['pos2']] == 'diamond', 'pos2', ifelse(dat[['pos3']] == 'diamond', 'pos3', ifelse(dat[['pos4']] == 'diamond', 'pos4', ifelse(dat[['pos5']] == 'diamond', 'pos5', 'pos6')))))
  
  # get color roi
  if (grepl('blue-high', dat[1, 'trial_file'])){
    dat[['high_roi']] <- ifelse(dat[['pos1']] == 'blue', 'pos1', ifelse(dat[['pos2']] == 'blue', 'pos2', ifelse(dat[['pos3']] == 'blue', 'pos3', ifelse(dat[['pos4']] == 'blue', 'pos4', ifelse(dat[['pos5']] == 'blue', 'pos5', ifelse(dat[['pos6']] == 'blue', 'pos6', NaN))))))
    
    dat[['low_roi']] <- ifelse(dat[['pos1']] == 'orange', 'pos1', ifelse(dat[['pos2']] == 'orange', 'pos2', ifelse(dat[['pos3']] == 'orange', 'pos3', ifelse(dat[['pos4']] == 'orange', 'pos4', ifelse(dat[['pos5']] == 'orange', 'pos5', ifelse(dat[['pos6']] == 'orange', 'pos6', NaN))))))
  } else {
    dat[['low_roi']] <- ifelse(dat[['pos1']] == 'blue', 'pos1', ifelse(dat[['pos2']] == 'blue', 'pos2', ifelse(dat[['pos3']] == 'blue', 'pos3', ifelse(dat[['pos4']] == 'blue', 'pos4', ifelse(dat[['pos5']] == 'blue', 'pos5', ifelse(dat[['pos6']] == 'blue', 'pos6', NaN))))))
    
    dat[['high_roi']] <- ifelse(dat[['pos1']] == 'orange', 'pos1', ifelse(dat[['pos2']] == 'orange', 'pos2', ifelse(dat[['pos3']] == 'orange', 'pos3', ifelse(dat[['pos4']] == 'orange', 'pos4', ifelse(dat[['pos5']] == 'orange', 'pos5', ifelse(dat[['pos6']] == 'orange', 'pos6', NaN))))))
  }
  
  dat[['trial_type']] <- ifelse(dat[['high_roi']] != 'NaN', 'high', ifelse(dat[['low_roi']] != 'NaN', 'low', 'neutral'))
  
  # remove zero-base ordering
  dat[['trialN']] <- dat[['trialN']] + 1
  
  # get points per trial
  dat[['trial_points']] <- ifelse(dat[['trial_key_resp.rt']] < dat[['rt_cutoff']], ifelse(dat[['trial_key_resp.corr']] == 1, ifelse(dat[['trial_type']] == 'high', (dat[['rt_cutoff']] - dat[['trial_key_resp.rt']])*100, ((dat[['rt_cutoff']] - dat[['trial_key_resp.rt']])*100)/10), 0), 0)
  
  dat[['total_points']] <- cumsum(ifelse(is.na(dat[['trial_points']]), 0, dat[['trial_points']]))
  
  # reduce beh data
  dat <- dat[c('participant', 'date', 'expName', 'prac_rt_mean', 'prac_rt_sd', 'rt_cutoff', 'blockN', 'trialN', 'trial_type', 'trial_points', 'total_points', 'pos1', 'pos2', 'pos3', 'pos4', 'pos5', 'pos6', 'direction', 'target_roi', 'high_roi', 'low_roi', 'trial_key_resp.keys', 'trial_key_resp.corr', 'trial_key_resp.rt', 'psychopyVersion', 'frameRate')]
  
  # update names
  names(dat) <- c('sub', 'date', 'exp_name', 'prac_rt_mean', 'prac_rt_sd', 'rt_cutoff', 'block', 'trial', 'trial_type', 'trial_points', 'total_points', 'pos1', 'pos2', 'pos3', 'pos4', 'pos5', 'pos6', 'direction', 'target_roi', 'high_roi', 'low_roi', 'resp', 'resp_corr', 'resp_rt', 'psychopy_ver', 'frame_rate')
  
  # clean up sub values
  dat[['sub']] <- sapply(dat[['sub']], function(x) substr(x, tail(unlist(gregexpr('-', x)), 1)+2, nchar(x)))
  dat[['sub']] <- as.numeric(dat[['sub']])
  
  # clean up date
  dat[['date']] <- lubridate::date(dat[['date']])
  
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(file.path(raw_wd, paste0(sub_str, '_task-shapegame_events.tsv'))) | isTRUE(overwrite)) {
    write.table(dat, file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-shapegame_events.tsv')), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    
    
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
}
