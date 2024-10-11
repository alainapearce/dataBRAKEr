#' util_group_spacegame: Get summary data from the Space Game (2-stage reinforcement learning task)
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
#' util_group_spacegame <- util_task_spacegame(data_list, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_spacegame <- function(data_list, ses, base_wd, overwrite = FALSE) {
  
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
    print('The util_group_spacegame.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
    
  #### Summary Data Function #####
  
  beh_sum_spacegame <- function(dat_block){
    rt_mean_earth <- mean(dat_block[['rt_earth']], na.rm = TRUE)
    rt_median_earth <- median(dat_block[['rt_earth']], na.rm = TRUE)
    
    rt_mean_planet <- mean(dat_block[['rt_planet']], na.rm = TRUE)
    rt_median_planet <- median(dat_block[['rt_planet']], na.rm = TRUE)
    
    n_timeout_earth <- sum(dat_block[['timeout_earth']])
    n_timeout_planet <- sum(dat_block[['timeout_planet']])
    
    score <- dat_block[nrow(dat_block), 'score']
    
    ## missed trial
    #indicate if: 1) previous trial was missed OR 2) trial was missed
    dat_block[['miss_trial']] <- ifelse(dat_block[['missed_earth']] == 1 | dat_block[['missed_planet']] == 1, 1, 0)
    
    ## reward rate
    rr <- mean(dat_block[dat_block['miss_trial'] == 0, 'points'], na.rm = TRUE)
    avg_reward <- mean(rowMeans(dat_block[dat_block['miss_trial'] == 0, c('rewards1', 'rewards2')]))
    rr_adj <- rr - avg_reward
    
    #by stake - high
    rr_s5 <- mean(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 5, 'points'], na.rm = TRUE)
    avg_reward_s5 <- mean(rowMeans(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 5, c('rewards1', 'rewards2')]))
    rr_adj_s5 <- rr_s5 - avg_reward_s5
    
    #by stake - low
    rr_s1 <- mean(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 1, 'points'], na.rm = TRUE)
    avg_reward_s1 <- mean(rowMeans(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 5, c('rewards1', 'rewards2')]))
    rr_adj_s1 <- rr_s1 - avg_reward_s1
    
    ## reward rate - Scaled
    rr_scaled <- mean(dat_block[dat_block['miss_trial'] == 0, 'points']/9, na.rm = TRUE)
    avg_reward_scaled <- mean(rowMeans(dat_block[dat_block['miss_trial'] == 0, c('rewards1', 'rewards2')])/9)
    rr_scaled_adj <- rr_scaled - avg_reward_scaled
    
    #by stake - high
    rr_scaled_s5 <- mean(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 5, 'points']/9, na.rm = TRUE)
    avg_reward_scaled_s5 <- mean(rowMeans(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 5, c('rewards1', 'rewards2')])/9)
    rr_scaled_adj_s5 <- rr_scaled_s5 - avg_reward_scaled_s5
    
    #by stake - low
    rr_scaled_s1 <- mean(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 1, 'points']/9, na.rm = TRUE)
    avg_reward_scaled_s1 <- mean(rowMeans(dat_block[dat_block['miss_trial'] == 0 & dat_block['stake'] == 1, c('rewards1', 'rewards2')])/9)
    rr_scaled_adj_s1 <- rr_scaled_s1 - avg_reward_scaled_s1
    
    
      beh_dat <- data.frame(rt_mean_earth, rt_median_earth, rt_mean_planet, rt_median_planet, n_timeout_earth, n_timeout_planet, rr, rr_adj, rr_s5, rr_adj_s5, rr_s1, rr_adj_s1, rr_scaled, rr_scaled_adj, rr_scaled_s5, rr_scaled_adj_s5, rr_scaled_s1, rr_scaled_adj_s1)
    
    
    return(beh_dat)
  }
  
  #### Participant Summary Function #####
  
  sum_database_fn <- function(sub_str, ses, base_wd, format){
    # get directory paths
    raw_wd <- paste0(base_wd, slash, 'bids', slash, 'rawdata', slash, sub_str, slash, 'ses-', ses, slash, 'beh', slash)
    
    data_file <- paste0(raw_wd, sub_str, '_ses-', ses, '_task-spacegame_beh.tsv')
    
    #print(sub_str)
    
    dat <- read.table(data_file, sep='\t', header = TRUE, na.strings = 'n/a')
    
    if(format == 'wide'){
      sum_dat <- beh_sum_spacegame(dat)
      sum_dat$sub <- dat[1, 'sub']
      sum_dat$ses <- ses
      sum_dat <- sum_dat[c(15:16, 1:14)]
    } else {
      #by block
      
      sum_dat <- do.call(rbind, t(sapply(unique(dat[['block']]), function(x) beh_sum_spacegame(dat[dat[['block']] == x, ]), simplify = FALSE)))
      sum_dat$sub <- dat[1, 'sub']
      sum_dat$ses <- ses
      sum_dat$block <- seq(1, nrow(sum_dat), 1)
      sum_dat <- sum_dat[c(15:17, 1:14)]
    }
    
    return(as.data.frame(sum_dat))
  }
  
  
  #### Save in derivatives #####
  deriv_wd <- paste0(base_wd, slash, 'bids', slash, 'derivatives', slash, 'beh', slash)
  
  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }
  
  ## remove sub-026 (only did 1 round)
  data_list <- data_list[data_list['sub_str'] != 'sub-026', ]
  
  ## Wide/Overall Data ####
  if (!file.exists(paste0(deriv_wd, 'task-spacegame_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database <- t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'baseline', base_wd = base_wd, format = 'wide'), simplify = TRUE, USE.NAMES = TRUE))
    
    write.table(sum_database, paste0(deriv_wd, 'task-spacegame_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
    if (isTRUE(overwrite)){
      return_msg <- 'overwrote with new version'
    } else {
      return_msg <- 'complete'
    }
  } else {
    return_msg <- 'exists'
  }
  
  ## Long Data ####
  
  if (!file.exists(paste0(deriv_wd, 'task-spacegame-long_beh.tsv')) | isTRUE(overwrite)) {
    
    # generate summary database
    sum_database_long <- do.call(rbind.data.frame, sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'baseline', base_wd = base_wd, format = 'long'), simplify = FALSE, USE.NAMES = TRUE))
    rownames(sum_database_long) <- NULL
    
    
    write.table(sum_database_long, paste0(deriv_wd, 'task-spacegame-long_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    
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
