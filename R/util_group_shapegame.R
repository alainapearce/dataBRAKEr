#' util_group_shapegame: Get summary data from the Shape Game (Value-Modulated Attentional Capture Task)
#'
#' This function calculates summary performance data for a participant and saves the output in a wide format (overall task) and long format (by block)
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
#' util_group_shapegame <- util_task_shapegame(data_list, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_shapegame <- function(data_list, ses, base_wd, overwrite = FALSE) {
  
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
    print('The util_group_shapegame.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
  
  #### Summary Data Function #####
  
  beh_eye_shapegame <- function(dat_eye_trial){
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
  
  beh_sum_shapegame <- function(dat_block, dat_eye, format){
    
    if (format == 'wide'){
      #percent correct
      n_cor <- sum(dat_block[dat_block$block > 3, 'resp_corr'])
      p_cor <- n_cor/nrow(dat_block[dat_block$block > 3, ])
      
      n_cor_high <- sum(dat_block[dat_block$block > 3 & dat_block$trial_type == 'high', 'resp_corr'])
      p_cor_high <- n_cor_high/nrow(dat_block[dat_block$block > 3 & dat_block$trial_type == 'high', ])
      
      n_cor_high <- sum(dat_block[dat_block$block > 3 & dat_block$trial_type == 'high', 'resp_corr'])
      p_cor_high <- n_cor_high/nrow(dat_block[dat_block$block > 3 & dat_block$trial_type == 'high', ])
      
      n_cor_low <- sum(dat_block[dat_block$block > 3 & dat_block$trial_type == 'low', 'resp_corr'])
      p_cor_low <- n_cor_low/nrow(dat_block[dat_block$block > 3 & dat_block$trial_type == 'low', ])
      
      #rt
      mean_rt <- mean(dat_block[dat_block$resp_corr == 1 & dat_block$block > 3, 'resp_rt'])
      mean_rt_high <- mean(dat_block[dat_block$resp_corr == 1 & dat_block$block > 3 & dat_block$trial_type == 'high', 'resp_rt'])
      mean_rt_low <- mean(dat_block[dat_block$resp_corr == 1 & dat_block$block > 3 & dat_block$trial_type == 'low', 'resp_rt'])
      
      score <- dat_block[nrow(dat_block), 'total_points']
      
    } else {
      #percent correct
      n_cor <- sum(dat_block[['resp_corr']])
      p_cor <- n_cor/nrow(dat_block)
      
      n_cor_high <- sum(dat_block[dat_block$trial_type == 'high', 'resp_corr'])
      p_cor_high <- n_cor_high/nrow(dat_block[dat_block$trial_type == 'high', ])
      
      n_cor_high <- sum(dat_block[dat_block$trial_type == 'high', 'resp_corr'])
      p_cor_high <- n_cor_high/nrow(dat_block[dat_block$trial_type == 'high', ])
      
      n_cor_low <- sum(dat_block[dat_block$trial_type == 'low', 'resp_corr'])
      p_cor_low <- n_cor_low/nrow(dat_block[dat_block$trial_type == 'low', ])
      
      #rt
      mean_rt <- mean(dat_block[dat_block$resp_corr == 1, 'resp_rt'])
      mean_rt_high <- mean(dat_block[dat_block$resp_corr == 1 & dat_block$trial_type == 'high', 'resp_rt'])
      mean_rt_low <- mean(dat_block[dat_block$resp_corr == 1 & dat_block$trial_type == 'low', 'resp_rt'])
      
      score <- dat_block[nrow(dat_block), 'total_points']
    }
    
    beh_dat <- data.frame(n_cor, p_cor, n_cor_high, p_cor_high, n_cor_low, p_cor_low, mean_rt, mean_rt_high, mean_rt_low, score)
    
    # eyetracking
    if (is.data.frame(dat_eye)){
      look_dat <- data.frame(t(sapply(unique(dat_eye$trial), function(x) beh_eye_shapegame(dat_eye[dat_eye$trial == x, ]), simplify = TRUE, USE.NAMES = TRUE)))
      
      if (format == 'wide'){
        
        # overall
        mean_nlooks <- mean(unlist(look_dat[look_dat$block < 3, 'n_looks']))
        
        # first look
        n_look1_target <- sum(look_dat[look_dat$block > 3, 'first_look'] == 'target')
        p_look1_target <- n_look1_target/nrow(look_dat[look_dat$block > 3, ])
        
        n_look1_high <- sum(look_dat[look_dat$block > 3, 'first_look'] == 'high')
        p_look1_high <- n_look1_high/nrow(look_dat[look_dat$block > 3 & look_dat$trial_type == 'high', ])
        
        n_look1_low <- sum(look_dat[look_dat$block > 3, 'first_look'] == 'low')
        p_look1_low <- n_look1_low/nrow(look_dat[look_dat$block > 3 & look_dat$trial_type == 'low', ])
        
        # looks by trial type
        mean_target_look_onset <- mean(unlist(look_dat[look_dat$block > 3, 'target_onset']), na.rm = TRUE)
        mean_high_look_onset <- mean(unlist(look_dat[look_dat$block > 3, 'high_onset']), na.rm = TRUE)
        mean_low_look_onset <- mean(unlist(look_dat[look_dat$block > 3, 'low_onset']), na.rm = TRUE)
        
        mean_nlooks_target <- mean(unlist(look_dat[look_dat$block > 3, 'n_looks_target']), na.rm = TRUE)
        mean_nlooks_high <- mean(unlist(look_dat[look_dat$block > 3, 'n_looks_high']), na.rm = TRUE)
        mean_nlooks_low <- mean(unlist(look_dat[look_dat$block > 3, 'n_looks_low']), na.rm = TRUE)
      } else {
        
        # overall
        mean_nlooks <- mean(unlist(look_dat[['n_looks']]))
        
        # first look
        n_look1_target <- sum(look_dat[['first_look']] == 'target')
        p_look1_target <- n_look1_target/nrow(look_dat)
        
        n_look1_high <- sum(look_dat[['first_look']] == 'high')
        p_look1_high <- n_look1_high/nrow(look_dat[look_dat$trial_type == 'high', ])
        
        n_look1_low <- sum(look_dat[['first_look']] == 'low')
        p_look1_low <- n_look1_low/nrow(look_dat[look_dat$trial_type == 'low', ])
        
        # looks by trial type
        mean_target_look_onset <- mean(unlist(look_dat[['target_onset']]), na.rm = TRUE)
        mean_high_look_onset <- mean(unlist(look_dat[['high_onset']]), na.rm = TRUE)
        mean_low_look_onset <- mean(unlist(look_dat[['low_onset']]), na.rm = TRUE)
        
        mean_nlooks_target <- mean(unlist(look_dat[['n_looks_target']]), na.rm = TRUE)
        mean_nlooks_high <- mean(unlist(look_dat[['n_looks_high']]), na.rm = TRUE)
        mean_nlooks_low <- mean(unlist(look_dat[['n_looks_low']]), na.rm = TRUE)
      } 
      
      beh_dat <- data.frame(beh_dat, mean_nlooks, n_look1_target, p_look1_target, n_look1_high, p_look1_high, n_look1_low, p_look1_low, mean_target_look_onset, mean_high_look_onset, mean_low_look_onset, mean_nlooks_target, mean_nlooks_high, mean_nlooks_low)
    } else {
      beh_dat <- data.frame(beh_dat, t(rep(NA, 13)))
      names(beh_dat)[11:23] <- c('mean_nlooks', 'n_look1_target', 'p_look1_target', 'n_look1_high', 'p_look1_high', 'n_look1_low', 'p_look1_low', 'mean_target_look_onset', 'mean_high_look_onset', 'mean_low_look_onset', 'mean_nlooks_target', 'mean_nlooks_high', 'mean_nlooks_low')
    }
    
    return(beh_dat)
  }
  
  #### Participant Summary Function #####
  
  sum_database_fn <- function(sub_str, ses, base_wd, format){
    # get directory paths
    raw_wd <- paste0(base_wd, slash, 'bids', slash, 'rawdata', slash, sub_str, slash, 'ses-', ses, slash, 'beh', slash)
    
    data_file <- paste0(raw_wd, sub_str, '_ses-', ses, '_task-shapegame_beh.tsv')
    data_file_eye <- paste0(raw_wd, sub_str, '_ses-', ses, '_task-shapegame_recording-eyetrack.tsv.gz')
    
    #debug
    #print(sub_str)
    
    dat <- read.table(data_file, sep='\t', header = TRUE, na.strings = 'n/a')
    
    if (file.exists(data_file_eye)){
      dat_eye <- read.table(data_file_eye, sep='\t', header = TRUE, na.strings = 'n/a')
      dat_eye_file = TRUE
    } else {
      dat_eye_file = FALSE
    }
    
    if(format == 'wide'){
      if(isTRUE(dat_eye_file)){
        sum_dat <- beh_sum_shapegame(dat, dat_eye, format = 'wide')
      } else{
        sum_dat <- beh_sum_shapegame(dat, NA, format = 'wide')
      }
      
      sum_dat$sub <- dat[1, 'sub']
      sum_dat$ses <- ses
      sum_dat <- sum_dat[c(24:25, 1:23)]
      
    } else {
      if(isTRUE(dat_eye_file)){
        sum_dat <- do.call(rbind, t(sapply(unique(dat[['block']]), function(x) beh_sum_shapegame(dat[dat[['block']] == x, ], dat_eye[dat_eye[['block']] == x, ], format = 'block'), simplify = FALSE)))
        
      } else{
        sum_dat <- do.call(rbind, t(sapply(unique(dat[['block']]), function(x) beh_sum_shapegame(dat[dat[['block']] == x, ], NA, format = 'block'), simplify = FALSE)))
      }
      
      sum_dat$sub <- dat[1, 'sub']
      sum_dat$ses <- ses
      sum_dat <- sum_dat[c(24:25, 1:23)]
    }
    
    return(as.data.frame(sum_dat))
  }
  
  
  #### Save in derivatives #####
  deriv_wd <- paste0(base_wd, slash, 'bids', slash, 'derivatives', slash, 'beh', slash)
  
  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }
  
  
  ## Wide/Overall Data ####
  if (!file.exists(paste0(deriv_wd, 'task-shapegame_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database <- as.data.frame(t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'baseline', base_wd = base_wd, format = 'wide'), simplify = TRUE, USE.NAMES = TRUE)))
    
    sum_database_names <- names(sum_database)
    sum_database <- data.frame(sapply(seq(1, ncol(sum_database)), function(x) unlist(sum_database[x]), simplify = TRUE, USE.NAMES = TRUE))
    names(sum_database) <- sum_database_names
    
    rownames(sum_database) <- NULL
    
    write.table(sum_database[1:12], paste0(deriv_wd, 'task-shapegame_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
    if (isTRUE(overwrite)){
      return_msg <- 'overwrote with new version'
    } else {
      return_msg <- 'complete'
    }
  } else {
    return_msg <- 'exists'
  }
  
  ## Long Data ####
  
  if (!file.exists(paste0(deriv_wd, 'task-shapegame_desc-long_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database_long <- do.call(rbind.data.frame, sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'baseline', base_wd = base_wd, format = 'long'), simplify = FALSE, USE.NAMES = TRUE))
    
    sum_database_long_names <- names(sum_database_long)
    sum_database_long <- data.frame(sapply(seq(1, ncol(sum_database_long)), function(x) unlist(sum_database_long[x]), simplify = TRUE, USE.NAMES = TRUE))
    names(sum_database_long) <- sum_database_long_names
    
    rownames(sum_database_long) <- NULL
    
    write.table(sum_database_long[1:12], paste0(deriv_wd, 'task-shapegame_desc-long_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
    if (isTRUE(overwrite)){
      return_msg_long <- 'overwrote with new version'
    } else {
      return_msg_long <- 'complete'
    }
  } else {
    return_msg_long <- 'exists'
  }
  
  return(list(return_msg, return_msg_long))
}
