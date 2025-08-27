#' util_spacegame_summary: Get summary data from from individual participants for Space Game (2-stage reinforcement learning task)
#'
#' This function calculates summary performance from individual participant data
#'
#'
#' @param ind_data Processed individual dataset from rawdata for the Space Game task
#'
#' @return a data.frame with 1 row including summary performance and task metrics for a participant
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' spacegame_summary_dat <- util_spacegame_summary(ddata_list, ses, base_wd, overwrite, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_spacegame_summary <- function(ind_data) {
  
  #### 1. Set up/initial checks #####
  
  # check that data exist and is a data.frame
  data_arg <- methods::hasArg(ind_data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(ind_data)) {
      stop('ind_data must be entered as a data.frame')
    } 
  } else if (isFALSE(data_arg)) {
    stop('ind_data must be entered as a data.frame')
  }
  
  
  #### Summary Data Function #####
  
  rt_mean_earth <- mean(ind_data[['rt_earth']], na.rm = TRUE)
  rt_median_earth <- median(ind_data[['rt_earth']], na.rm = TRUE)
  
  rt_mean_planet <- mean(ind_data[['rt_planet']], na.rm = TRUE)
  rt_median_planet <- median(ind_data[['rt_planet']], na.rm = TRUE)
  
  n_timeout_earth <- sum(ind_data[['timeout_earth']])
  n_timeout_planet <- sum(ind_data[['timeout_planet']])
  
  score <- ind_data[nrow(ind_data), 'score']
  
  ## reward rate
  rr <- mean(ind_data[ind_data['missed'] == 0, 'points'], na.rm = TRUE)
  avg_reward <- mean(rowMeans(ind_data[ind_data['missed'] == 0, c('rewards1', 'rewards2')]))
  rr_adj <- rr - avg_reward
  
  #by stake - high
  rr_s5 <- mean(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 5, 'points'], na.rm = TRUE)
  avg_reward_s5 <- mean(rowMeans(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 5, c('rewards1', 'rewards2')]))
  rr_adj_s5 <- rr_s5 - avg_reward_s5
  
  #by stake - low
  rr_s1 <- mean(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 1, 'points'], na.rm = TRUE)
  avg_reward_s1 <- mean(rowMeans(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 5, c('rewards1', 'rewards2')]))
  rr_adj_s1 <- rr_s1 - avg_reward_s1
  
  ## reward rate - Scaled
  rr_scaled <- mean(ind_data[ind_data['missed'] == 0, 'points']/9, na.rm = TRUE)
  avg_reward_scaled <- mean(rowMeans(ind_data[ind_data['missed'] == 0, c('rewards1', 'rewards2')])/9)
  rr_scaled_adj <- rr_scaled - avg_reward_scaled
  
  #by stake - high
  rr_scaled_s5 <- mean(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 5, 'points']/9, na.rm = TRUE)
  avg_reward_scaled_s5 <- mean(rowMeans(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 5, c('rewards1', 'rewards2')])/9)
  rr_scaled_adj_s5 <- rr_scaled_s5 - avg_reward_scaled_s5
  
  #by stake - low
  rr_scaled_s1 <- mean(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 1, 'points']/9, na.rm = TRUE)
  avg_reward_scaled_s1 <- mean(rowMeans(ind_data[ind_data['missed'] == 0 & ind_data['stake'] == 1, c('rewards1', 'rewards2')])/9)
  rr_scaled_adj_s1 <- rr_scaled_s1 - avg_reward_scaled_s1
  
  #stay probability
  stay_prob <- sum(ind_data[ind_data['missed'] != 1 & ind_data['prev_missed'] != 1, 'stay_planet'], na.rm = TRUE)/nrow(ind_data[ind_data['missed'] != 1 & ind_data['prev_missed'] != 1, ])
  
  #by stake - high
  stay_prob_s5 <- sum(ind_data[ind_data['missed'] != 1 & ind_data['prev_missed'] != 1 & ind_data['stake'] == 5, 'stay_planet'], na.rm = TRUE)/nrow(ind_data[ind_data['missed'] != 1 & ind_data['prev_missed'] != 1 & ind_data['stake'] == 5, ])
  
  #by stake - high
  stay_prob_s1 <- sum(ind_data[ind_data['missed'] != 1 & ind_data['prev_missed'] != 1 & ind_data['stake'] == 1, 'stay_planet'], na.rm = TRUE)/nrow(ind_data[ind_data['missed'] != 1 & ind_data['prev_missed'] != 1 & ind_data['stake'] == 1, ])
  
  beh_dat <- data.frame(rt_mean_earth, rt_median_earth, rt_mean_planet, rt_median_planet, n_timeout_earth, n_timeout_planet, rr, rr_adj, rr_s5, rr_adj_s5, rr_s1, rr_adj_s1, rr_scaled, rr_scaled_adj, rr_scaled_s5, rr_scaled_adj_s5, rr_scaled_s1, rr_scaled_adj_s1, stay_prob, stay_prob_s5, stay_prob_s1)
  
  
  return(beh_dat)
}