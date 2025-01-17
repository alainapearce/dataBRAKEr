#' util_task_shapegame: Process raw data from the Shape Game (Value-Modulated Attentional Capture)
#'
#' This function: 1) cleans data to save in BIDS format in rawdata and 2) generates summary data that can be used to generate a database
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
  
  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The shapegame_task.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
  # get directory paths
  raw_wd <- paste0(base_wd, slash, 'bids', slash, 'rawdata', slash, sub_str, slash, 'ses-', ses, slash, 'beh', slash)
  
  data_file <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, 'ses-', ses, slash, 'beh', slash, sub_str, '_ses-', ses, '_task-shapegame_events.tsv')
  
  #print(sub_str)
    
  #### Organize Data #####
  dat <- read.csv(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
  
  # get practice rt and rt cuttoff
  dat_prac <- dat[!is.na(dat[['practice_trials.thisTrialN']]), ]
  
  # throw out first trial bc sometimes response are longer/may be skew rt/sd estimates
  mean_rt_prac <- mean(dat_prac[2:10, 'prac_key_resp.rt'], na.rm = TRUE)
  sd_rt_prac <- sd(dat_prac[2:10, 'prac_key_resp.rt'], na.rm = TRUE)
  rt_cuttoff <- mean_rt_prac + 2*sd_rt_prac
  
  # remove practice and instructions
  dat <- dat[dat[['trial_file']] != '', ]
  
  # add practice info to data
  dat[['prac_rt_mean']] <- mean_rt_prac
  dat[['prac_rt_sd']] <- sd_rt_prac
  dat[['rt_cuttoff']] <- rt_cuttoff
  
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
  
  # reduce columns and detect if has eye-tracking or not
  # if ('etRecord.started' %in% names(dat)){
  #   eye_track = TRUE
  #   
  #   eye_dat <- dat[c('participant', 'trialN', 'pos1_roi.started', 'pos2_roi.started', 'pos3_roi.started', 'pos4_roi.started', 'pos5_roi.started', 'pos6_roi.started','etRecord.started', 'pos1_roi.numLooks', 'pos1_roi.timesOn', 'pos1_roi.timesOff', 'pos2_roi.numLooks', 'pos2_roi.timesOn', 'pos2_roi.timesOff', 'pos3_roi.numLooks', 'pos3_roi.timesOn', 'pos3_roi.timesOff', 'pos4_roi.numLooks', 'pos4_roi.timesOn', 'pos4_roi.timesOff', 'pos5_roi.numLooks', 'pos5_roi.timesOn', 'pos5_roi.timesOff', 'pos6_roi.numLooks', 'pos6_roi.timesOn', 'pos6_roi.timesOff')]
  #   
  #   # update names
  #   names(eye_dat) <- c('sub', 'trial', 'pos1_roi_onset', 'pos2_roi_onset', 'pos3_roi_onset', 'pos4_roi_onset', 'pos5_roi_onset', 'pos6_roi_onset' ,'et_record_onset', 'pos1_looks', 'pos1_look_onsets', 'pos1_look_offsets', 'pos2_looks', 'pos2_look_onsets', 'pos2_look_offsets', 'pos3_looks', 'pos3_look_onsets', 'pos3_look_offsets', 'pos4_looks', 'pos4_look_onsets', 'pos4_look_offsets', 'pos5_looks', 'pos5_look_onsets', 'pos5_look_offsets', 'pos6_looks', 'pos6_look_onsets', 'pos6_look_offsets')
  #   
  # } else {
  #   eye_track = FALSE
  # }
  
  # get points per trial
  dat[['trial_points']] <- ifelse(dat[['trial_key_resp.rt']] < dat[['rt_cuttoff']], ifelse(dat[['trial_key_resp.corr']] == 1, ifelse(dat[['trial_type']] == 'high', (dat[['rt_cuttoff']] - dat[['trial_key_resp.rt']])*100, ((dat[['rt_cuttoff']] - dat[['trial_key_resp.rt']])*100)/10), 0), 0)
  
  dat[['total_points']] <- cumsum(ifelse(is.na(dat[['trial_points']]), 0, dat[['trial_points']]))
  
  # reduce beh data
  dat <- dat[c('participant', 'date', 'expName', 'prac_rt_mean', 'prac_rt_sd', 'rt_cuttoff', 'blockN', 'trialN', 'trial_type', 'trial_points', 'total_points', 'pos1', 'pos2', 'pos3', 'pos4', 'pos5', 'pos6', 'direction', 'target_roi', 'high_roi', 'low_roi', 'trial_key_resp.keys', 'trial_key_resp.corr', 'trial_key_resp.rt', 'psychopyVersion', 'frameRate')]
  
  # update names
  names(dat) <- c('sub', 'date', 'exp_name', 'prac_rt_mean', 'prac_rt_sd', 'rt_cuttoff', 'block', 'trial', 'trial_type', 'trial_points', 'total_points', 'pos1', 'pos2', 'pos3', 'pos4', 'pos5', 'pos6', 'direction', 'target_roi', 'high_roi', 'low_roi', 'resp', 'resp_corr', 'resp_rt', 'psychopy_ver', 'frame_rate')
  
  # clean up sub values
  dat[['sub']] <- sapply(dat[['sub']], function(x) substr(x, tail(unlist(gregexpr('-', x)), 1)+2, nchar(x)))
  dat[['sub']] <- as.numeric(dat[['sub']])
  
  # clean up date
  dat[['date']] <- lubridate::date(dat[['date']])
  
  # get long dataset with trial information
  # if (isTRUE(eye_track)) {
  #   eye_dat_long <- util_eyetrack_roi(eye_dat, roi_list = c('pos1', 'pos2', 'pos3', 'pos4', 'pos5', 'pos6'), return_data = TRUE)
  #   eye_dat_long <- merge(eye_dat_long, dat, by = 'trial')
  # } 
    
  
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(paste0(raw_wd, sub_str, '_task-shapegame_beh.tsv')) | isTRUE(overwrite)) {
    write.table(dat, paste0(raw_wd, sub_str, '_ses-', ses, '_task-shapegame_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    
    # if (isTRUE(eye_track)) {
    #   write.table(eye_dat_long, gzfile(paste0(raw_wd, sub_str, '_ses-', ses, '_task-shapegame_recording-eyetrack.tsv.gz')), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    # } 
    
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
}
