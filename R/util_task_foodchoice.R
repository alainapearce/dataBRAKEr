#' util_task_foodchoice: Process raw data from the Food Choice Task
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
  base_wd_arg <- methods::hasArg(base_wd)
  
  if (isTRUE(base_wd_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(base_wd)) {
    stop("base_wd must be entered as a string")
  }
  
  
  # get directory paths
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'nirs')
  
  data_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'nirs', 'foodchoice', paste0(sub_str, '_ses-', ses, '_task-foodchoice_events.tsv'))
  
  #print(sub_str)
  
  #check for '999'
  if (!file.exists(data_file)){
    data_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'nirs', 'foodchoice', paste0(sub_str, '_ses-', ses, '_task-foodchoice_events-999.tsv'))

    if (file.exists(data_file)){
      alt_999 <- TRUE
    } else {
      return(paste0('foodchoice file does not exist in sourcedata for ', sub_str))
    }
  } else {
    alt_999 <- FALSE
  }
    
  #### Organize Data #####
  dat <- read.csv(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
  
  if (isTRUE(alt_999)){
    foodrating_file <- file.path(base_wd, 'raw_untouched', 'foodrating_game', paste0(sub_str, '_foodrating.csv'))
    dat_foodrating <- read.csv(foodrating_file, sep = ',', header = TRUE, na.strings = c('n/a', 'NA'))
    
    # update values for foodchoice
    dat[['participant']] <- sub_str
    
    # separate ratings
    dat_foodrating_health <- dat_foodrating[dat_foodrating[['condFile']] == 'health_stim.csv' & dat_foodrating[['condFile_prac']] == '', ]
    dat_foodrating_taste <- dat_foodrating[dat_foodrating[['condFile']] == 'taste_stim.csv' & dat_foodrating[['condFile_prac']] == '', ]
    dat_foodrating_want <- dat_foodrating[dat_foodrating[['condFile']] == 'want_stim.csv' & dat_foodrating[['condFile_prac']] == '', ]
    
    # copy health ratings over from food rating
    dat[dat[['img1']] != '', 'img1_health'] <- sapply(dat[dat[['img1']] != '', 'img1'], function(x) ifelse(sum(dat_foodrating_health[['stimFile']] == x) == 1, dat_foodrating_health[dat_foodrating_health[['stimFile']] == x, 'rating'], dat[dat[['img1']] == x, 'img1_health']), simplify = TRUE, USE.NAMES = FALSE)
    
    dat[dat[['img2']] != '', 'img2_health'] <- sapply(dat[dat[['img2']] != '', 'img1'], function(x) ifelse(sum(dat_foodrating_health[['stimFile']] == x) == 1, dat_foodrating_health[dat_foodrating_health[['stimFile']] == x, 'rating'], dat[dat[['img2']] == x, 'img2_health']), simplify = TRUE, USE.NAMES = FALSE)
    
    # copy taste ratings over from food rating
    dat[dat[['img1']] != '', 'img1_taste'] <- sapply(dat[dat[['img1']] != '', 'img1'], function(x) ifelse(sum(dat_foodrating_taste[['stimFile']] == x) == 1, dat_foodrating_taste[dat_foodrating_taste[['stimFile']] == x, 'rating'], dat[dat[['img1']] == x, 'img1_taste']), simplify = TRUE, USE.NAMES = FALSE)
    
    dat[dat[['img2']] != '', 'img2_taste'] <- sapply(dat[dat[['img2']] != '', 'img2'], function(x) ifelse(sum(dat_foodrating_taste[['stimFile']] == x) == 1, dat_foodrating_taste[dat_foodrating_taste[['stimFile']] == x, 'rating'], dat[dat[['img2']] == x, 'img2_taste']), simplify = TRUE, USE.NAMES = FALSE)
    
    # copy want ratings over from food rating
    dat[dat[['img1']] != '', 'img1_want'] <- sapply(dat[dat[['img1']] != '', 'img1'], function(x) ifelse(sum(dat_foodrating_want[['stimFile']] == x) == 1, dat_foodrating_want[dat_foodrating_want[['stimFile']] == x, 'rating'], dat[dat[['img1']] == x, 'img1_want']), simplify = TRUE, USE.NAMES = FALSE)
    
    dat[dat[['img2']] != '', 'img2_want'] <- sapply(dat[dat[['img2']] != '', 'img2'], function(x) ifelse(sum(dat_foodrating_want[['stimFile']] == x) == 1, dat_foodrating_want[dat_foodrating_want[['stimFile']] == x, 'rating'], dat[dat[['img2']] == x, 'img2_want']), simplify = TRUE, USE.NAMES = FALSE)
    
    #get usable cutpoints
    taste_cut <- round(mean(dat_foodrating_taste[['rating']]), digits = 1)
    dat[['taste_cut']] <- taste_cut
    
    health_cut <- round(mean(dat_foodrating_health[['rating']]), digits = 1)
    dat[['health_cut']] <- health_cut
    
    dat[['tastehealth_cond_img1']] <- ifelse(dat[['img1_taste']] > taste_cut, ifelse(dat[['img1_health']] > health_cut, 'tasty_healthy', 'tasty_unhealthy'), ifelse(dat[['img1_health']] > health_cut, 'nottasty_healthy', 'nottasty_unhealthy'))
    
    dat[['tastehealth_cond_img2']] <- ifelse(dat[['img2_taste']] > taste_cut, ifelse(dat[['img2_health']] > health_cut, 'tasty_healthy', 'tasty_unhealthy'), ifelse(dat[['img2_health']] > health_cut, 'nottasty_healthy', 'nottasty_unhealthy'))
    
    dat[['condition']] <- ifelse(dat[['tastehealth_cond_img1']] == 'tasty_unhealthy' & dat[['tastehealth_cond_img2']] == 'nottasty_healthy', 'mix_conflict', ifelse(
      dat[['tastehealth_cond_img1']] == 'tasty_unhealthy' & dat[['tastehealth_cond_img2']] == 'tasty_healthy', 'tasty', ifelse(
        dat[['tastehealth_cond_img1']] == 'tasty_unhealthy' & dat[['tastehealth_cond_img2']] == 'nottasty_unhealthy', 'unhealthy', ifelse(
          dat[['tastehealth_cond_img1']] == 'tasty_healthy' & dat[['tastehealth_cond_img2']] == 'nottasty_healthy', 'healthy', ifelse(
            dat[['tastehealth_cond_img1']] == 'nottasty_healthy' & dat[['tastehealth_cond_img2']] == 'nottasty_unhealthy', 'nottasty', ifelse(
              dat[['tastehealth_cond_img1']] == 'tasty_healthy' & dat[['tastehealth_cond_img2']] == 'nottasty_unhealthy', 'mix_noconflict', ifelse(
                dat[['tastehealth_cond_img2']] == 'tasty_unhealthy' & dat[['tastehealth_cond_img1']] == 'nottasty_healthy', 'mix_conflict', ifelse(
                  dat[['tastehealth_cond_img2']] == 'tasty_unhealthy' & dat[['tastehealth_cond_img1']] == 'tasty_healthy', 'tasty', ifelse(
                    dat[['tastehealth_cond_img2']] == 'tasty_unhealthy' & dat[['tastehealth_cond_img1']] == 'nottasty_unhealthy', 'unhealthy', ifelse(
                      dat[['tastehealth_cond_img2']] == 'tasty_healthy' & dat[['tastehealth_cond_img1']] == 'nottasty_healthy', 'healthy', ifelse(
                        dat[['tastehealth_cond_img2']] == 'nottasty_healthy' & dat[['tastehealth_cond_img1']] == 'nottasty_unhealthy', 'nottasty', ifelse(
                          dat[['tastehealth_cond_img2']] == 'tasty_healthy' & dat[['tastehealth_cond_img1']] == 'nottasty_unhealthy', 'mix_noconflict', ifelse(
                            dat[['tastehealth_cond_img1']] == dat[['tastehealth_cond_img2']], 'match', dat[['condition']])))))))))))))
    
  }
  
  # get healthy eating rt
  healthy_rt <- dat[!is.na(dat[['healthyeating_key.rt']]), 'healthyeating_key.rt']
  
  # remove practice
  dat <- dat[!is.na(dat[['fix']]), ]
  
  # reduce columns and detect if has eye-tracking or not
  if ('etRecord.started' %in% names(dat)){
    eye_track = TRUE
    
    eye_dat <- dat[c('participant', 'trials.thisN', 'left_img.started', 'right_img.started', 'etRecord.started', 'left_img.numLooks', 'left_img.timesOn', 'left_img.timesOff', 'right_img.numLooks', 'right_img.timesOn', 'right_img.timesOff')]
    
    # update names
    names(eye_dat) <- c('sub', 'trial', 'left_roi_onset', 'right_roi_onset', 'et_record_onset', 'left_looks', 'left_look_onsets', 'left_look_offsets', 'right_looks', 'right_look_onsets', 'right_look_offsets')
    
    # remove zero-base ordering
    eye_dat[['trial']] <- eye_dat[['trial']] + 1
    
  } else {
    eye_track = FALSE
  }
  
  # fNIRS/Beh data
  dat <- dat[c('participant', 'date', 'expName', 'condition', 'img1', 'img2', 'fix', 'img1_health', 'img2_health', 'img1_taste', 'img2_taste', 'img1_want', 'img2_want', 'taste_cut', 'health_cut', 'trials.thisN', 'healthyeating_key.rt', 'trial_left_img.started', 'trial_right_img.started', 'choice_prompt.started', 'key_choice.started', 'choice_fix.started', 'key_choice.keys', 'key_choice.rt', 'trial_fixprompt.started', 'tfix.started', 'trial_fixprompt.stopped', 'tfix.stopped', 'psychopyVersion', 'frameRate')]
  
  # update names
  names(dat) <- c('sub', 'date', 'exp_name', 'cond', 'left_img', 'right_img', 'fix', 'left_img_health', 'right_img_health', 'left_img_taste', 'right_img_taste', 'left_img_want', 'right_img_want', 'taste_cut', 'health_cut', 'trial', 'healthyeating_time', 'left_img_onset', 'right_img_onset', 'choice_prompt_onset', 'key_choice_onset', 'choice_fix_onset', 'choice', 'choice_rt', 'trial_fixprompt_onset', 'tfix_onset', 'trial_fixprompt_offset', 'tfix_offset', 'psychopy_ver', 'frame_rate')
  
  # add duration
  dat[['duration']] <- 2
  
  # clean up sub values
  dat[['sub']] <- sapply(dat[['sub']], function(x) substr(x, tail(unlist(gregexpr('-', x)), 1)+1, nchar(x)))
  dat[['sub']] <- as.numeric(dat[['sub']])
  
  # clean up date
  dat[['date']] <- lubridate::date(dat[['date']])
  
  # add healthy eating rt
  dat[['healthyeating_time']] <- healthy_rt
  
  # add in choice information
  dat[['choice_healthy']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['left_img_health']] == dat[['right_img_health']], 0, ifelse(dat[['left_img_health']] > dat[['right_img_health']], ifelse(dat[['choice']] == 1, 1, -1), ifelse(dat[['choice']] == 1, -1, 1))))
  
  dat[['choice_tasty']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['left_img_taste']] == dat[['right_img_taste']], 0, ifelse(dat[['left_img_taste']] > dat[['right_img_taste']], ifelse(dat[['choice']] == 1, 1, -1), ifelse(dat[['choice']] == 1, -1, 1))))
  
  dat[['choice_want']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['left_img_want']] == dat[['right_img_want']], 0, ifelse(dat[['left_img_want']] > dat[['right_img_want']], ifelse(dat[['choice']] == 1, 1, -1), ifelse(dat[['choice']] == 1, -1, 1))))
  
  # add onset column - will be completed using Matlab during fNIRS processing
  dat[['onset']] <- NaN
  
  #re-order columns
  dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'cond', 'left_img', 'right_img', 'fix', 'left_img_health', 'right_img_health', 'left_img_taste', 'right_img_taste', 'left_img_want', 'right_img_want', 'taste_cut', 'health_cut', 'trial', 'healthyeating_time', 'left_img_onset', 'right_img_onset', 'choice_prompt_onset', 'key_choice_onset', 'choice_fix_onset', 'choice', 'choice_rt', 'trial_fixprompt_onset', 'tfix_onset', 'trial_fixprompt_offset', 'tfix_offset', 'choice_healthy', 'choice_tasty', 'choice_want', 'psychopy_ver', 'frame_rate')]
  
  # get long dataset with trial information
  if (isTRUE(eye_track)) {
    eye_dat_long <- util_eyetrack_roi(eye_dat, roi_list = c('right', 'left'), return_data = TRUE)
    eye_dat_long <- merge(eye_dat_long, dat, by = 'trial')
  } 
    
  
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(file.path(raw_wd, paste0(sub_str, '_task-foodchoice_events.tsv'))) | isTRUE(overwrite)) {
    write.table(dat, file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-foodchoice_events.tsv')), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    
    if (isTRUE(eye_track)) {
      write.table(eye_dat_long, gzfile(file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-foodchoice_recording-eyetrack.tsv.gz'))), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
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
