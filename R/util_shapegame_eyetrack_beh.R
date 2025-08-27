#' util_shapegame_eyetrack_beh: Get summary data related to eyetracking for a single Shape Game trial for a participant
#'
#' This function calculates summary performance data for a participant and saves the output in a wide format (overall task) and long format (by block)
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param dat_eye_trial A data frame with variable 'sub_str' that includes all participants
#'
#' @return a dataframe with eye-tracking related summary behavior for a trial of the Shape Game
#'
#' @examples
#'
#'
#' @export

util_shapegame_eyetrack_beh <- function(dat_eye_trial){
  #debug
  #print(dat_eye_trial$trial)
  
  #trial
  trial <- dat_eye_trial[1, 'trial']
  
  #trial
  block <- dat_eye_trial[1, 'block']
  
  #n_looks
  n_looks <- dat_eye_trial[1, 'n_looks']
  
  #trial type
  trial_type <- ifelse(dat_eye_trial[1, 'high_roi'] != 'NaN', 'high', ifelse(dat_eye_trial[1, 'low_roi'] != 'NaN', 'low', 'neutral'))
  
  if (dat_eye_trial[1, 'n_looks'] > 0){
    
    #initial look
    first_look <- ifelse(dat_eye_trial[1, 'roi'] == dat_eye_trial[1, 'target_roi'], 'target', ifelse(dat_eye_trial[1, 'roi'] == dat_eye_trial[1, 'high_roi'], 'high', ifelse(dat_eye_trial[1, 'low_roi'] == dat_eye_trial[1, 'low_roi'], 'low', 'other')))
    
    first_look_dur <- dat_eye_trial[1, 'look_dur']
    
    #look number target
    if (sum(dat_eye_trial$roi == dat_eye_trial$target_roi) > 0){
      target_look_dat <- dat_eye_trial[dat_eye_trial$roi == dat_eye_trial$target_roi, ]
      
      n_looks_target <- nrow(target_look_dat)
      target_look_order <- target_look_dat[1, 'look']
      target_onset <- target_look_dat[1, 'look_onset']
    } else {
      n_looks_target <- 0
      target_look_order <- NA
      target_onset <- NA
    }
    
    #look at high value
    if (sum(dat_eye_trial$roi == dat_eye_trial$high_roi) > 0){
      high_look_dat <- dat_eye_trial[dat_eye_trial$roi == dat_eye_trial$high_roi, ]
      
      n_looks_high <- nrow(high_look_dat)
      high_look_order <- high_look_dat[1, 'look']
      high_onset <- high_look_dat[1, 'look_onset']
    } else {
      n_looks_high <- 0
      high_look_order <- NA
      high_onset <- NA
    }
    
    #look at low value
    if (sum(dat_eye_trial$roi == dat_eye_trial$low_roi) > 0){
      low_look_dat <- dat_eye_trial[dat_eye_trial$roi == dat_eye_trial$low_roi, ]
      
      n_looks_low <- nrow(low_look_dat)
      low_look_order <- low_look_dat[1, 'look']
      low_onset <- low_look_dat[1, 'look_onset']
    } else {
      n_looks_low <- 0
      low_look_order <- NA
      low_onset <- NA
    }
  } else {
    first_look <- NA
    first_look_dur <- NA
    n_looks_target <- 0
    target_look_order <- NA
    target_onset <- NA
    n_looks_high <- 0
    high_look_order <- NA
    high_onset <- NA
    n_looks_low <- 0
    low_look_order <- NA
    low_onset <- NA
  }
  
  eye_look_sum <- data.frame(trial, trial_type, block, n_looks, first_look, n_looks_target, target_look_order, target_onset, n_looks_high, high_look_order, high_onset, n_looks_low, low_look_order, low_onset)
  
  return(eye_look_sum)
}
