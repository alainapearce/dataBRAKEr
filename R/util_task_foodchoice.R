#' util_task_foodchoice: Process raw data from the Food Choice Task
#'
#' This function cleans data to save in BIDS format in rawdata
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
#' foodchoice_task_pardat <- util_task_foodchoice(sub_str, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_foodchoice <- function(sub_str, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
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
    print('The foodchoice_task.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
  # get directory paths
  raw_wd <- paste0(base_wd, slash, 'bids', slash, 'rawdata', slash, sub_str, slash, 'ses-', ses, slash, 'nirs', slash)
  
  data_file <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, 'ses-', ses, slash, 'nirs', slash, sub_str, '_ses-', ses, '_task-foodchoice_events.tsv')
  
  
  #### Organize Data #####
  dat <- read.csv(data_file, sep = '\t', header = TRUE)
  
  # get healthy eating rt
  healthy_rt <- dat[!is.na(dat[['healthyeating_key.rt']]), 'healthyeating_key.rt']
  
  # remove practice
  dat <- dat[!is.na(dat[['fix']]), ]
  
  # reduce columns and detect if has eye-tracking or not
  if ('etRecord.started' %in% names(dat)){
    eye_dat <- dat[c('participant', 'trials.thisN', 'left_img.started', 'right_img.started', 'etRecord.started', 'left_img.numLooks', 'left_img.timesOn', 'left_img.timesOff', 'right_img.numLooks', 'right_img.timesOn', 'right_img.timesOff')]
    
    # update names
    names(eye_dat) <- c('sub', 'trial', 'left_roi_onset', 'right_roi_onset', 'et_record_onset', 'left_looks', 'left_look_onsets', 'left_look_offsets', 'right_looks', 'right_look_onsets', 'right_look_offsets')
    
    # remove zero-base ordering
    eye_dat[['trial']] <- eye_dat[['trial']] + 1
    
  }
  
  # fNIRS/Beh data
  dat <- dat[c('participant', 'date', 'expName', 'condition', 'img1', 'img2', 'fix', 'img1_health', 'img2_health', 'img1_taste', 'img2_taste', 'img1_want', 'img2_want', 'taste_cut', 'health_cut', 'trials.thisN', 'healthyeating_key.rt', 'trial_left_img.started', 'trial_right_img.started', 'choice_prompt.started', 'key_choice.started', 'choice_fix.started', 'key_choice.keys', 'key_choice.rt', 'trial_fixprompt.started', 'tfix.started', 'trial_fixprompt.stopped', 'tfix.stopped', 'psychopyVersion', 'frameRate')]
  
  # update names
  names(dat) <- c('sub', 'date', 'exp_name', 'cond', 'img1', 'img2', 'fix', 'img1_health', 'img2_health', 'img1_taste', 'img2_taste', 'img1_want', 'img2_want', 'taste_cut', 'health_cut', 'trial', 'healthyeating_time', 'left_img_onset', 'right_img_onset', 'choice_prompt_onset', 'key_choice_onset', 'choice_fix_onset', 'choice', 'choice_rt', 'trial_fixprompt_onset', 'tfix_onset', 'trial_fixprompt_offset', 'tfix_offset', 'psychopy_ver', 'frame_rate')
  
  # add duration
  dat[['duration']] <- 2
  
  # clean up sub values
  dat[['sub']] <- sapply(dat[['sub']], function(x) substr(x, tail(unlist(gregexpr('0', x)), 1)+1, nchar(x)))
  dat[['sub']] <- as.numeric(dat[['sub']])
  
  # clean up date
  dat[['date']] <- lubridate::date(dat[['date']])
  
  # add healthy eating rt
  dat[['healthyeating_time']] <- healthy_rt
  
  # add in choice information
  dat[['choice_healthy']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['img1_health']] == dat[['img2_health']], 1, ifelse(dat[['img1_health']] > dat[['img2_health']], ifelse(dat[['choice']] == 1, 1, 0), ifelse(dat[['choice']] == 1, 0, 1))))
  
  dat[['choice_tasty']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['img1_taste']] == dat[['img2_taste']], 1, ifelse(dat[['img1_taste']] > dat[['img2_taste']], ifelse(dat[['choice']] == 1, 1, 0), ifelse(dat[['choice']] == 1, 0, 1))))
  
  dat[['choice_want']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['img1_want']] == dat[['img2_want']], 1, ifelse(dat[['img1_want']] > dat[['img2_want']], ifelse(dat[['choice']] == 1, 1, 0), ifelse(dat[['choice']] == 1, 0, 1))))
  
  # add onset column - will be completed using Matlab during fNIRS processing
  dat[['onset']] <- NA
  
  #re-order columns
  dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'cond', 'img1', 'img2', 'fix', 'img1_health', 'img2_health', 'img1_taste', 'img2_taste', 'img1_want', 'img2_want', 'taste_cut', 'health_cut', 'trial', 'healthyeating_time', 'left_img_onset', 'right_img_onset', 'choice_prompt_onset', 'key_choice_onset', 'choice_fix_onset', 'choice', 'choice_rt', 'trial_fixprompt_onset', 'tfix_onset', 'trial_fixprompt_offset', 'tfix_offset', 'choice_healthy', 'choice_tasty', 'choice_want', 'psychopy_ver', 'frame_rate')]
  
  # get long dataset with trial information
  if ('et_record_onset' %in% names(dat)) {
    #eye_dat_long <- util_eyetrack_roi(eye_dat, roi_list = c('right', 'left'), return_data = TRUE)
    #eye_dat <- merge(dat, eye_dat[c()])
  } 
    
  
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(paste0(raw_wd, sub_str, '_task-foodchioce_events.tsv')) | isTRUE(overwrite)) {
    write.table(dat, paste0(raw_wd, sub_str, '_ses-', ses, '_task-foodchioce_events.tsv'), sep='\t', quote = FALSE, row.names = FALSE)
    
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
  
  if (isTRUE(return_data)){
    foodchioce_json <- json_foodchioce()
    
    return(list( foodchioce_dat = dat,
                 foodchioce_json = foodchioce_json))
  }
}
