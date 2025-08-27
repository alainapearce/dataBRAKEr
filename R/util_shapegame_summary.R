#' util_shapegame_summary: Get summary data from individual participants for the Shape Game (Value-Modulated Attentional Capture Task)
#'
#' This function calculates summary performance for individual participants data and saves the output in a wide format (overall task) and long format (by block)
#'
#' @param ind_data Processed individual-level dataset from rawdata for the Food Choice task 
#' @param format Which format to process the data in - options: 'wide' or 'long': \itemize{
#' \item{wide: summary metrics computed across last 3 blocks to only average across established performance/remove learning}
#' \itme{long: summary metrics computed for block data provided and then compiled in util_group_shapegame.R}
#' }
#'
#' @return a data.frame with 1 row including summary performance and task metrics for a participant
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' shapegame_summary_data <- util_shapegame_summary(ind_data, format)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_shapegame_summary <- function(ind_data, format) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(ind_data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(ind_data)) {
      stop('ind_data must be entered as a data.frame')
    } 
  } else if (isFALSE(data_arg)) {
    stop('ind_data must be entered as a data.frame')
  }
  
  #### Summary Data  #####
  
  # for wide format - only compute across last 3 blocks
  if (format == 'wide'){
    ind_dat <- ind_data[ind_data$block > 3, ]
  }
  
  #percent correct
  n_cor <- sum(ind_data[['resp_corr']])
  p_cor <- n_cor/nrow(ind_data)
  
  n_trials_high <- nrow(ind_data[ind_data$trial_type == 'high', ])
  n_cor_high <- sum(ind_data[ind_data$trial_type == 'high', 'resp_corr'])
  p_cor_high <- n_cor_high/n_trials_high
  
  n_trials_low <- nrow(ind_data[ind_data$trial_type == 'low', ])
  n_cor_low <- sum(ind_data[ind_data$trial_type == 'low', 'resp_corr'])
  p_cor_low <- n_cor_low/n_trials_low
  
  n_trials_neutral <- nrow(ind_data[ind_data$trial_type == 'neutral', ])
  n_cor_neutral <- sum(ind_data[ind_data$trial_type == 'neutral', 'resp_corr'])
  p_cor_neutral <- n_cor_neutral/n_trials_neutral
  
  #rt
  mean_rt <- mean(ind_data[ind_data$resp_corr == 1, 'resp_rt'])
  mean_rt_high <- mean(ind_data[ind_data$resp_corr == 1 & ind_data$trial_type == 'high', 'resp_rt'])
  mean_rt_low <- mean(ind_data[ind_data$resp_corr == 1 & ind_data$trial_type == 'low', 'resp_rt'])
  mean_rt_neutral <- mean(ind_data[ind_data$resp_corr == 1 & ind_data$trial_type == 'neutral', 'resp_rt'])
  
  rt_dif_high_low <- mean_rt_high - mean_rt_low
  rt_dif_high_neutral <- mean_rt_high - mean_rt_neutral
  rt_dif_low_neutral <- mean_rt_low - mean_rt_neutral
  
  total_points <- ind_data[nrow(ind_data), 'total_points']
  n_points_high <- sum(ind_data[ind_data$trial_type == 'high', 'trial_points'], na.rm = TRUE)
  n_points_low <- sum(ind_data[ind_data$trial_type == 'low', 'trial_points'], na.rm = TRUE)
  n_points_neutral <- sum(ind_data[ind_data$trial_type == 'neutral', 'trial_points'], na.rm = TRUE)
  
  mean_points_high <- mean(ind_data[ind_data$trial_type == 'high', 'trial_points'], na.rm = TRUE)
  mean_points_low <- mean(ind_data[ind_data$trial_type == 'low', 'trial_points'], na.rm = TRUE)
  mean_points_neutral <- mean(ind_data[ind_data$trial_type == 'neutral', 'trial_points'], na.rm = TRUE)
  
  beh_dat <- data.frame(n_cor, p_cor, n_trials_high, n_cor_high, p_cor_high, n_trials_low, n_cor_low, p_cor_low, n_trials_neutral, n_cor_neutral, p_cor_neutral, mean_rt, mean_rt_high, mean_rt_low, mean_rt_neutral, rt_dif_high_low, rt_dif_high_neutral, rt_dif_low_neutral, total_points, n_points_high, n_points_low, n_points_neutral, mean_points_high, mean_points_low, mean_points_neutral)
  
  
  return(beh_dat)
}

