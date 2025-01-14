#' util_task_spacegame: Process raw data from the Space Game (2-stage reinforcement learning task)
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
#' spacegame_task_pardat <- util_task_spacegame(sub_str, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_spacegame <- function(sub_str, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
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
    print('The spacegame_task.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
  # get directory paths
  raw_wd <- paste0(base_wd, slash, 'bids', slash, 'rawdata', slash, sub_str, slash, 'ses-', ses, slash, 'beh', slash)
  
  data_file <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, 'ses-', ses, slash, 'beh', slash, sub_str, '_ses-', ses, '_task-spacegame_events.mat')
  
  #print(sub_str)
    
  #### Organize Data #####
  matdat <- R.matlab::readMat(data_file)
  
  # process .mat file
  dat_list <- matdat[['data']]
  
  dat_columns <- c('rews', 'block', 's', 'stimuli', 'stake', 'choice', 'rt', 'score', 'points', 'timeout')
  dat <- cbind.data.frame(sapply(dat_columns, function(x) t(dat_list[row.names(dat_list) == x]), simplify = TRUE))
  
  # add in sub/session information
  dat[['sub']] <- as.numeric(unlist(dat_list[row.names(dat_list) == 'id']))
  dat[['date']] <- lubridate::date(lubridate::dmy_hms(unlist(dat_list[row.names(dat_list) == 'date'])))
  
  # add stakes into points calculation
  dat[['points']] <- dat[['points']]
  
  # rename
  names(dat) <- c('rewards1', 'rewards2', 'block', 'state_earth', 'state_planet', 'stim_left', 'stim_right', 'stake', 'choice_earth', 'rt_earth', 'rt_planet', 'score', 'points', 'timeout_earth', 'timeout_planet', 'sub', 'date')
  
  # add trials by block
  # dat[dat[['block']] == 1, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 1, ]))
  # dat[dat[['block']] == 2, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 2, ]))
  # dat[dat[['block']] == 3, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 3, ]))
  # dat[dat[['block']] == 4, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 4, ]))
  
  # kool didn't consider blocks a reset
  dat[['trial']] <- seq(1, nrow(dat), 1)
  
  # get key press
  dat[['response']] <- ifelse(dat[['timeout_earth']] == 1, 3, ifelse(dat[['choice_earth']] == dat[['stim_left']], 1, 2))
  
  # get missed states
  dat[['missed_earth']] <- ifelse(dat[['timeout_earth']] == 1, 1, 0)
  dat[['missed']] <- ifelse(dat[['timeout_earth']] == 1 | dat[['timeout_planet']] == 1, 1, 0)
  
  # reward dif
  dat[['reward_dif']] <- abs(dat[['rewards1']] -  dat[['rewards2']])
  
  # get stay for planet by block
  # dat[dat[['block']] == 1, 'stay_planet'] <- sapply(dat[dat[['block']] == 1, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 1 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 1 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  # 
  # dat[dat[['block']] == 2, 'stay_planet'] <- sapply(dat[dat[['block']] == 2, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 2 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 2 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  # 
  # dat[dat[['block']] == 3, 'stay_planet'] <- sapply(dat[dat[['block']] == 3, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 3 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 3 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  # 
  # dat[dat[['block']] == 4, 'stay_planet'] <- sapply(dat[dat[['block']] == 4, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 4 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 4 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  
  # get earth same/different
  # dat[dat[['block']] == 1, 'earth_same'] <- sapply(dat[dat[['block']] == 1, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 1 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 1 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  # 
  # dat[dat[['block']] == 2, 'earth_same'] <- sapply(dat[dat[['block']] == 2, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 2 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 2 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  # 
  # dat[dat[['block']] == 3, 'earth_same'] <- sapply(dat[dat[['block']] == 3, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 3 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 3 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  # 
  # dat[dat[['block']] == 4, 'earth_same'] <- sapply(dat[dat[['block']] == 4, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 4 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 4 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  
  
  # kool didn't consider blocks a reset
  
  #get info for trial-by-trail analyses (stay probabilities)
  dat[['prev_reward_diff']] <- c(0, dat[2:nrow(dat), 'reward_dif'])
  dat[['prev_points']] <- c(0, dat[2:nrow(dat), 'points'])
  dat[['prev_stake']] <- c(0, dat[2:nrow(dat), 'stake'])
  dat[['prev_missed']] <- c(0, dat[2:nrow(dat), 'missed'])
  
  dat[['stay_planet']] <- sapply(dat[['trial']], function(x) ifelse(x == 1, 0, ifelse(dat[dat[['trial']] == x, 'state_planet'] == dat[dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  
  dat[['earth_same']] <- sapply(dat[['trial']], function(x) ifelse(x == 1, 0, ifelse(dat[dat[['trial']] == x, 'state_earth'] == dat[dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  
  # re-order columns
  dat <- dat[c('sub', 'date', 'block', 'trial', 'timeout_earth', 'timeout_planet', 'state_earth', 'state_planet', 'stim_left', 'stim_right', 'rt_earth', 'rt_planet', 'choice_earth', 'response', 'points', 'stake', 'score', 'rewards1', 'rewards2', 'missed_earth', 'missed', 'prev_missed', 'prev_reward_diff', 'prev_points', 'prev_stake', 'earth_same', 'stay_planet')]
    
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(paste0(raw_wd, sub_str, '_task-spacegame_beh.tsv')) | isTRUE(overwrite)) {
    write.table(dat, paste0(raw_wd, sub_str, '_ses-', ses, '_task-spacegame_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
    if (isTRUE(overwrite)){
      return_msg <- 'overwrote with new version'
    } else {
      return_msg <- 'complete'
    }
  } else {
    return_msg <- 'exists'
  }
}
