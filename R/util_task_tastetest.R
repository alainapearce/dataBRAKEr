#' util_task_tastetest: Process raw data from the fNIRS Taste-Test Task
#'
#' This function: \itemize{
#' \item{1) cleans data to save in BIDS format in rawdata}
#' \item{2) generates summary data that can be used to generate a database}
#' }
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @param return logical indicating if computed summary data should be returned. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' tastetest_task_pardat <- util_task_tastetest(sub_str, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_tastetest <- function(sub_str, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
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
  
  
  # get version
  if (grepl('post', sub_str)){
    desc_str <- 'post'
  } else {
    desc_str <- 'pre'
  }
  
  sub_str <- gsub(paste0('-', desc_str, '-meal'), '', sub_str)
  
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'nirs', paste0(desc_str, 'meal'))
  data_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'nirs', paste0(desc_str, 'meal'), paste0(sub_str, '_ses-', ses, '_task-taste_desc-', desc_str, '_events.tsv'))
  
  #### Organize Data #####
  dat <- read.table(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
  
  # need to adjust for first few participants that didn't have responses recorded (still have triggers so can process fNIRS data)
  
  if (names(dat)[1] == 'trials.thisRepN') {
    
    # adjust columns for initial participants missing RT information
    
    dat[['foodItem']] <- NaN
    dat[['food_condition']] <- NaN
    dat[['trigger_taste']] <- NaN
    dat[['trigger_want']] <- NaN
    dat[['trigger_like']] <- NaN
    dat[['trigger_sip']] <- NaN
    dat[['want_rating']] <- NaN
    dat[['like_rating']] <- NaN
    dat[['onset']] <- NA
    dat[['duration']] <- NA
    dat[['task_component']] <- NA
    
    dat <- dat[c('onset', 'duration', 'participant', 'date', 'expName', 'EDorder', 'order', 'foodItem', 'food_condition', 'task_component', 'trigger_taste', 'trigger_want', 'trigger_like', 'trigger_sip', 'trials.thisN', 'block_timed_txt.started', 'want_rating_slider.started', 'want_rating', 'tastetest_trial.started', 'like_rating_slider.started', 'like_rating', 'sip_txt.started', 'sip_key.rt', 'psychopyVersion', 'frameRate')]
    
    if (is.na(dat[nrow(dat), 'frameRate'])){
      dat[['participant']] <- dat[1, 'participant']
      dat[['date']] <- dat[1, 'date']
      dat[['expName']] <- dat[1, 'expName']
      dat[['EDorder']] <- dat[1, 'EDorder']
      dat[['order']] <- dat[1, 'order']
      dat[['psychopyVersion']] <- dat[1, 'psychopyVersion']
      dat[['frameRate']] <- dat[1, 'frameRate']
    }
    
    # remove practice
    dat <- dat[!is.na(dat[['block_timed_txt.started']]), ]
    
    # add trial structure
    dat <- rbind.data.frame(dat, dat, dat, dat, dat, dat, dat, dat, dat, dat, dat, dat, dat, dat, dat)
    
    # remove copied onsets as not valid - will get during fNIRS Matlab processing
    dat[['block_timed_txt.started']] <- NA
    dat[['want_rating_slider.started']] <- NA
    dat[['tastetest_trial.started']] <- NA
    dat[['like_rating_slider.started']] <- NA
    dat[['sip_txt.started']] <- NA
    dat[['sip_key.rt']] <- NA
    
    
    if (dat[1, 'EDorder'] == 1) {
      dat[['food_condition']] <- c('meal', 'meal', 'meal', 'low_ed', 'low_ed', 'low_ed', 'meal', 'meal', 'meal', 'high_ed', 'high_ed', 'high_ed', 'meal', 'meal', 'meal')
      dat[['foodItem']] <- c('Chicken Nugget', 'Macarroni and Cheese', 'Grape', 'Orange', 'Broccoli', 'Green Bean', 'Grape', 'Chicken Nugget', 'Macarroni and Cheese', 'Chocolate', 'Cracker', 'Fruit Chew', 'Macarroni and Cheese', 'Grape', 'Chicken Nugget')
      dat[['trigger_taste']] <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 3, 3, 3, 1, 1, 1)
    } else {
      dat[['food_condition']] <- c('meal', 'meal', 'meal', 'high_ed', 'high_ed', 'high_ed', 'meal', 'meal', 'meal', 'low_ed', 'low_ed', 'low_ed', 'meal', 'meal', 'meal')
      dat[['foodItem']] <- c('Chicken Nugget', 'Macarroni and Cheese', 'Grape', 'Chocolate', 'Cracker', 'Fruit Chew', 'Grape', 'Chicken Nugget', 'Macarroni and Cheese', 'Orange', 'Broccoli', 'Green Bean', 'Macarroni and Cheese', 'Grape', 'Chicken Nugget')
      dat[['trigger_taste']] <- c(1, 1, 1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1)
    }
    
    dat[['trigger_want']] <- 4
    dat[['trigger_like']] <- 5
    dat[['trigger_sip']] <- 6
    
  } else {
    # remove practice
    dat <- dat[!is.na(dat[['trigger_taste']]), ]
    dat[['onset']] <- NA
    dat[['duration']] <- NA
    dat[['task_component']] <- NA
    
    # reduce columns
    dat <- dat[c('onset', 'duration', 'participant', 'date', 'expName', 'EDorder', 'order', 'foodItem', 'food_condition', 'task_component', 'trigger_taste', 'trigger_want', 'trigger_like', 'trigger_sip', 'trials.thisN', 'block_timed_txt.started', 'want_rating_slider.started', 'want_rating', 'tastetest_trial.started', 'like_rating_slider.started', 'like_rating', 'sip_txt.started', 'sip_key.rt', 'psychopyVersion', 'frameRate')]
  }
  
  # update names
  names(dat) <- c('onset', 'duration', 'sub', 'date', 'exp_name', 'exp_cond_num', 'cond', 'food_item', 'trial_cond', 'task_component', 'trigger_taste', 'trigger_want', 'trigger_like', 'trigger_sip', 'trial_index', 'food_onset', 'want_slider_onset', 'want_rating', 'taste_onset', 'like_slider_onset', 'like_rating', 'sip_onset', 'sip_dur', 'psychopy_ver', 'frame_rate')
  
  
  ## separate so that onset and duration columns are common to all task components and then rbind
  
  # wanting
  want_dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'exp_cond_num', 'cond', 'food_item', 'trial_cond', 'task_component', 'trigger_want', 'trial_index', 'want_slider_onset', 'want_rating', 'psychopy_ver', 'frame_rate')]
  
  want_dat[['onset']] <- want_dat[['want_slider_onset']]
  want_dat[['duration']] <- 4
  want_dat[['task_component']] <- 'want_rating'
  
  want_dat <- want_dat[, !grepl('want_slider_onset', names(want_dat))]
  
  names(want_dat)[names(want_dat) == 'want_rating'] <- 'rating'
  names(want_dat)[names(want_dat) == 'trigger_want'] <- 'trigger'
  
  # taste
  taste_dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'exp_cond_num', 'cond', 'food_item', 'trial_cond', 'task_component', 'trigger_taste', 'trial_index', 'taste_onset', 'psychopy_ver', 'frame_rate')]
  
  taste_dat[['onset']] <- taste_dat[['taste_onset']]
  taste_dat[['duration']] <- 10
  taste_dat[['task_component']] <- 'taste_test'
  
  taste_dat <- taste_dat[, !grepl('taste_onset', names(taste_dat))]
  
  taste_dat[['rating']] <- NA
  names(taste_dat)[names(taste_dat) == 'trigger_taste'] <- 'trigger'
  
  taste_dat <- taste_dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'exp_cond_num', 'cond', 'food_item', 'trial_cond', 'task_component', 'trigger', 'trial_index', 'rating', 'psychopy_ver', 'frame_rate')]
  
  # like
  like_dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'exp_cond_num', 'cond', 'food_item', 'trial_cond', 'task_component', 'trigger_like', 'trial_index', 'like_slider_onset', 'like_rating', 'psychopy_ver', 'frame_rate')]
  
  like_dat[['onset']] <- like_dat[['like_slider_onset']]
  like_dat[['duration']] <- 4
  like_dat[['task_component']] <- 'like_rating'
  
  like_dat <- like_dat[, !grepl('like_slider_onset', names(like_dat))]
  
  names(like_dat)[names(like_dat) == 'like_rating'] <- 'rating'
  names(like_dat)[names(like_dat) == 'trigger_like'] <- 'trigger'
  
  # sip
  sip_dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'exp_cond_num', 'cond', 'food_item', 'trial_cond', 'task_component', 'trigger_sip', 'trial_index', 'sip_onset', 'sip_dur', 'psychopy_ver', 'frame_rate')]
  
  sip_dat[['onset']] <- sip_dat[['sip_onset']]
  sip_dat[['duration']] <- ifelse(sip_dat[['sip_dur']] <= 10, sip_dat[['sip_dur']], 10)
  sip_dat[['task_component']] <- 'sip'
  
  sip_dat <- sip_dat[, !grepl('sip_onset', names(sip_dat))]
  
  sip_dat[['rating']] <- NA
  names(sip_dat)[names(sip_dat) == 'trigger_sip'] <- 'trigger'
  
  sip_dat <- sip_dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'exp_cond_num', 'cond', 'food_item', 'trial_cond', 'task_component', 'trigger', 'trial_index', 'rating', 'psychopy_ver', 'frame_rate')]
  
  # compbind datasets by task component
  dat_proc <- rbind.data.frame(want_dat, taste_dat, like_dat, sip_dat)
  dat_proc <- dat_proc[order(dat_proc[[c('onset')]]), ]
  
  # clean up sub values
  dat_proc[['sub']] <- sapply(dat_proc[['sub']], function(x) substr(x, tail(unlist(gregexpr('0', x)), 1)+1, nchar(x)))
  dat_proc[['sub']] <- as.numeric(dat_proc[['sub']])
  
  # clean up date
  print(sub_str)
  dat_proc[['date']] <- lubridate::date(dat_proc[['date']])

  
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-taste_', desc_str, '_events.tsv'))) | isTRUE(overwrite)) {
    write.table(dat_proc, file.path(raw_wd, paste0(sub_str, '_ses-', ses, '_task-taste_', desc_str, '_events.tsv')), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
}

