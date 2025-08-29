#' util_foodchoice_summary: Get summary data from individual participants for the Food Choice task
#'
#' This function generates summary performance data for an individual participant
#'
#'
#' @param ind_data Processed individual dataset from rawdata for the Food Choice task 
#'
#' @return a data.frame with 1 row including summary performance and task metrics for a participant
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' foodchoice_summary_dat <- util_foodchoice_summary(ind_data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_foodchoice_summary <- function(ind_data) {
  
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
  
  
  #### Helper Functions #####
  mean_ratingdif_bychoice <- function(ind_data, trial_type, choice_type, choice_val, rating_var) {
    
    if (trial_type == 'not_sc') {
      rating_vals_chosen <- ifelse(ind_data[['choice']] == 'None', NA, ifelse(ind_data[['choice']] == '1', ind_data[ind_data['cond'] != 'mix_conflict' & ind_data[choice_type] == choice_val, paste0('left_img_', rating_var)], ind_data[ind_data['cond'] != 'mix_conflict' & ind_data[choice_type] == choice_val, paste0('right_img_', rating_var)]))
      
      rating_vals_notchosen <- ifelse(ind_data[['choice']] == 'None', NA, ifelse(ind_data[['choice']] == '1', ind_data[ind_data['cond'] != 'mix_conflict' & ind_data[choice_type] == choice_val, paste0('right_img_', rating_var)], ind_data[ind_data['cond'] != 'mix_conflict' & ind_data[choice_type] == choice_val, paste0('left_img_', rating_var)]))
    } else {
      rating_vals_chosen <- ifelse(ind_data[['choice']] == 'None', NA, ifelse(ind_data[['choice']] == '1', ind_data[ind_data['cond'] == trial_type & ind_data[choice_type] == choice_val, paste0('left_img_', rating_var)], ind_data[ind_data['cond'] == trial_type & ind_data[choice_type] == choice_val, paste0('right_img_', rating_var)]))
      
      rating_vals_notchosen <- ifelse(ind_data[['choice']] == 'None', NA, ifelse(ind_data[['choice']] == '1', ind_data[ind_data['cond'] == trial_type & ind_data[choice_type] == choice_val, paste0('right_img_', rating_var)], ind_data[ind_data['cond'] == trial_type & ind_data[choice_type] == choice_val, paste0('left_img_', rating_var)]))
    }
    
    
    rating_dif <- rating_vals_chosen - rating_vals_notchosen
    
    mean_dif_rating <- mean(rating_dif, na.rm = TRUE)
    
    return(mean_dif_rating)
  }
  
  mean_rating_bychoice <- function(ind_data, trial_type, choice_type, choice_val, rating_var) {
    
    if (trial_type == 'not_sc'){
      rating_vals <- ifelse(ind_data[['choice']] == 'None', NA, ifelse(ind_data[['choice']] == '1', ind_data[ind_data['cond'] != 'mix_conflict' & ind_data[choice_type] == choice_val, paste0('left_img_', rating_var)], ind_data[ind_data['cond'] != 'mix_conflict' & ind_data[choice_type] == choice_val, paste0('right_img_', rating_var)]))
    } else {
      rating_vals <- ifelse(ind_data[['choice']] == 'None', NA, ifelse(ind_data[['choice']] == '1', ind_data[ind_data['cond'] == trial_type & ind_data[choice_type] == choice_val, paste0('left_img_', rating_var)], ind_data[ind_data['cond'] == trial_type & ind_data[choice_type] == choice_val, paste0('right_img_', rating_var)]))
    }
    
    
    mean_rating <- mean(rating_vals, na.rm = TRUE)
    
    return(mean_rating)
  }
  
  #### Summary Data  #####
  
  #get trial counts
  n_sc <- nrow(ind_data[ind_data['cond'] == 'mix_conflict', ])
  n_notsc <- nrow(ind_data[ind_data['cond'] != 'mix_conflict', ])
  n_mix_noconflict <- nrow(ind_data[ind_data['cond'] == 'mix_noconflict', ])
  n_tasty <- nrow(ind_data[ind_data['cond'] == 'tasty', ])
  n_nottasty <- nrow(ind_data[ind_data['cond'] == 'nottasty', ])
  n_healthy <- nrow(ind_data[ind_data['cond'] == 'healthy', ])
  n_nothealthy <- nrow(ind_data[ind_data['cond'] == 'unhealthy', ])
  
  #cut points (use tasty >, not tasty <=; healhty >, not healthy <=)
  taste_cut <- ind_data[['taste_cut']][1]
  health_cut <- ind_data[['health_cut']][1]
  
  # avg rating by side
  health_mean_left <- mean(ind_data[['left_img_health']], na.rm = TRUE)
  health_mean_right <- mean(ind_data[['right_img_health']], na.rm = TRUE)
  
  taste_mean_left <- mean(ind_data[['left_img_taste']], na.rm = TRUE)
  taste_mean_right <- mean(ind_data[['right_img_taste']], na.rm = TRUE)
  
  want_mean_left <- mean(ind_data[['left_img_want']], na.rm = TRUE)
  want_mean_right <- mean(ind_data[['right_img_want']], na.rm = TRUE)
  
  # choices - overall
  n_choice_missed <- nrow(ind_data[ind_data['choice'] == 'None', ])
  p_choice_missed <- n_choice_missed/nrow(ind_data)
  
  n_choice_healthy <-  nrow(ind_data[ind_data['choice_healthy'] == '1', ])
  p_choice_healthy <- n_choice_healthy/(nrow(ind_data) - n_choice_missed)
  rt_mean_choice_healthy <- mean(ind_data[ind_data['choice_healthy'] == '1', 'choice_rt'], na.rm = TRUE)
  rt_med_choice_healthy <- median(ind_data[ind_data['choice_healthy'] == '1', 'choice_rt'], na.rm = TRUE)
  
  n_choice_tasty <-  nrow(ind_data[ind_data['choice_tasty'] == '1', ])
  p_choice_tasty <- n_choice_tasty/(nrow(ind_data) - n_choice_missed)
  rt_mean_choice_tasty <- mean(ind_data[ind_data['choice_tasty'] == '1', 'choice_rt'], na.rm = TRUE)
  rt_med_choice_tasty <- median(ind_data[ind_data['choice_tasty'] == '1', 'choice_rt'], na.rm = TRUE)
  
  n_choice_want <-  nrow(ind_data[ind_data['choice_want'] == '1', ])
  p_choice_want <- n_choice_want/(nrow(ind_data) - n_choice_missed)
  rt_mean_choice_want <- mean(ind_data[ind_data['choice_want'] == '1', 'choice_rt'], na.rm = TRUE)
  rt_med_choice_want <- median(ind_data[ind_data['choice_want'] == '1', 'choice_rt'], na.rm = TRUE)
  
  # choices - selfcontrol
  n_sc_choice_missed <- nrow(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] == 'None', ])
  p_sc_choice_missed <- n_sc_choice_missed/n_sc
  
  ## conflict amount - overall
  healthdif_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict', 'left_img_health'] - ind_data[ind_data['cond'] == 'mix_conflict', 'right_img_health']), na.rm = TRUE)
  tastedif_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict', 'left_img_taste'] - ind_data[ind_data['cond'] == 'mix_conflict', 'right_img_taste']), na.rm = TRUE)
  wantdif_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict', 'left_img_want'] - ind_data[ind_data['cond'] == 'mix_conflict', 'right_img_want']), na.rm = TRUE)
  
  ## conflict amount - missed
  healthdif_missed_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] == 'None', 'left_img_health'] - ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] == 'None', 'right_img_health']), na.rm = TRUE)
  tastedif_missed_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] == 'None', 'left_img_taste'] - ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] == 'None', 'right_img_taste']), na.rm = TRUE)
  wantdif_missed_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] == 'None', 'left_img_want'] - ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] == 'None', 'right_img_want']), na.rm = TRUE)
  
  ## conflict amount - chosen
  healthdif_complete_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] != 'None', 'left_img_health'] - ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] != 'None', 'right_img_health']), na.rm = TRUE)
  tastedif_complete_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] != 'None', 'left_img_taste'] - ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] != 'None', 'right_img_taste']), na.rm = TRUE)
  wantdif_complete_sc_trials <- mean(abs(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] != 'None', 'left_img_want'] - ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice'] != 'None', 'right_img_want']), na.rm = TRUE)
  
  ## healthy choice - counts
  n_sc_choice_healthy <- nrow(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice_healthy'] == '1', ])
  p_sc_choice_healthy <- n_sc_choice_healthy/(n_sc - n_sc_choice_missed)
  
  ## healthy choice - speed
  rt_mean_sc_choice_healthy <- mean(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice_healthy'] == '1', 'choice_rt'], na.rm = TRUE)
  rt_med_sc_choice_healthy <- median(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice_healthy'] == '1', 'choice_rt'], na.rm = TRUE)
  
  ## healthy choice - food attributes
  health_mean_sc_choice_healthy <- mean_rating_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_healthy', '1', 'health')
  taste_mean_sc_choice_healthy <- mean_rating_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_healthy', '1', 'taste')
  want_mean_sc_choice_healthy <- mean_rating_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_healthy', '1', 'want')
  
  healthdif_mean_sc_choice_healthy <- mean_ratingdif_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_healthy', '1', 'health')
  tastedif_mean_sc_choice_healthy <- mean_ratingdif_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_healthy', '1', 'taste')
  wantdif_mean_sc_choice_healthy <- mean_ratingdif_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_healthy', '1', 'want')
  
  ## tasty/not healthy choice - counts
  n_sc_choice_tasty <- nrow(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice_tasty'] == '1', ])
  p_sc_choice_tasty <- n_sc_choice_tasty/(n_sc - n_sc_choice_missed)
  
  ## tasty/not healthy choice - speed
  rt_mean_sc_choice_tasty <- mean(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice_tasty'] == '1', 'choice_rt'], na.rm = TRUE)
  rt_med_sc_choice_tasty <- median(ind_data[ind_data['cond'] == 'mix_conflict' & ind_data['choice_tasty'] == '1', 'choice_rt'], na.rm = TRUE)
  
  ## tasty/not healthy choice - food attributes
  health_mean_sc_choice_tasty <- mean_rating_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_tasty', '1', 'health')
  taste_mean_sc_choice_tasty <- mean_rating_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_tasty', '1', 'taste')
  want_mean_sc_choice_tasty <- mean_rating_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_tasty', '1', 'want')
  
  healthdif_mean_sc_choice_tasty <- mean_ratingdif_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_tasty', '1', 'health')
  tastedif_mean_sc_choice_tasty <- mean_ratingdif_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_tasty', '1', 'taste')
  wantdif_mean_sc_choice_tasty <- mean_ratingdif_bychoice(ind_data, trial_type = 'mix_conflict', choice_type = 'choice_tasty', '1', 'want')
  
  
  
  
  # choices - non selfcontrol trials
  n_notsc_choice_missed <- nrow(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] == 'None', ])
  p_notsc_choice_missed <- n_notsc_choice_missed/(n_notsc - n_notsc_choice_missed)
  
  ## conflict amount - overall
  healthdif_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict', 'left_img_health'] - ind_data[ind_data['cond'] != 'mix_conflict', 'right_img_health']), na.rm = TRUE)
  tastedif_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict', 'left_img_taste'] - ind_data[ind_data['cond'] != 'mix_conflict', 'right_img_taste']), na.rm = TRUE)
  wantdif_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict', 'left_img_want'] - ind_data[ind_data['cond'] != 'mix_conflict', 'right_img_want']), na.rm = TRUE)
  
  ## conflict amount - missed
  healthdif_missed_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] == 'None', 'left_img_health'] - ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] == 'None', 'right_img_health']), na.rm = TRUE)
  tastedif_missed_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] == 'None', 'left_img_taste'] - ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] == 'None', 'right_img_taste']), na.rm = TRUE)
  wantdif_missed_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] == 'None', 'left_img_want'] - ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] == 'None', 'right_img_want']), na.rm = TRUE)
  
  ## conflict amount - chosen
  healthdif_complete_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] != 'None', 'left_img_health'] - ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] != 'None', 'right_img_health']), na.rm = TRUE)
  tastedif_complete_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] != 'None', 'left_img_taste'] - ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] != 'None', 'right_img_taste']), na.rm = TRUE)
  wantdif_complete_notsc_trials <- mean(abs(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] != 'None', 'left_img_want'] - ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice'] != 'None', 'right_img_want']), na.rm = TRUE)
  
  ## healthy choice - counts
  n_notsc_choice_healthy <- nrow(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice_healthy'] == '1', ])
  p_notsc_choice_healthy <- n_notsc_choice_healthy/(n_notsc - n_notsc_choice_missed)
  
  ## healthy choice - speed
  rt_mean_notsc_choice_healthy <- mean(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice_healthy'] == '1', 'choice_rt'], na.rm = TRUE)
  rt_med_notsc_choice_healthy <- median(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice_healthy'] == '1', 'choice_rt'], na.rm = TRUE)
  
  ## healthy choice - food attributes
  health_mean_notsc_choice_healthy <- mean_rating_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_healthy', '1', 'health')
  taste_mean_notsc_choice_healthy <- mean_rating_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_healthy', '1', 'taste')
  want_mean_notsc_choice_healthy <- mean_rating_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_healthy', '1', 'want')
  
  healthdif_mean_notsc_choice_healthy <- mean_ratingdif_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_healthy', '1', 'health')
  tastedif_mean_notsc_choice_healthy <- mean_ratingdif_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_healthy', '1', 'taste')
  wantdif_mean_notsc_choice_healthy <- mean_ratingdif_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_healthy', '1', 'want')
  
  ## tasty/not healthy choice - counts
  n_notsc_choice_tasty <- nrow(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice_tasty'] == '1', ])
  p_notsc_choice_tasty <- n_notsc_choice_tasty/(n_notsc - n_notsc_choice_missed)
  
  ## tasty/not healthy choice - speed
  rt_mean_notsc_choice_tasty <- mean(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice_tasty'] == '1', 'choice_rt'], na.rm = TRUE)
  rt_med_notsc_choice_tasty <- median(ind_data[ind_data['cond'] != 'mix_conflict' & ind_data['choice_tasty'] == '1', 'choice_rt'], na.rm = TRUE)
  
  ## tasty/not healthy choice - food attributes
  health_mean_notsc_choice_tasty <- mean_rating_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_tasty', '1', 'health')
  taste_mean_notsc_choice_tasty <- mean_rating_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_tasty', '1', 'taste')
  want_mean_notsc_choice_tasty <- mean_rating_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_tasty', '1', 'want')
  
  healthdif_mean_notsc_choice_tasty <- mean_ratingdif_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_tasty', '1', 'health')
  tastedif_mean_notsc_choice_tasty <- mean_ratingdif_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_tasty', '1', 'taste')
  wantdif_mean_notsc_choice_tasty <- mean_ratingdif_bychoice(ind_data, trial_type = 'not_sc', choice_type = 'choice_tasty', '1', 'want')
  
  # compile
  beh_dat <- data.frame(n_sc, n_notsc, n_mix_noconflict, n_tasty, n_nottasty, n_healthy, n_nothealthy, taste_cut, health_cut, health_mean_left, health_mean_right ,taste_mean_left, taste_mean_right, want_mean_left, want_mean_right, n_choice_missed, p_choice_missed, n_choice_healthy, p_choice_healthy, rt_mean_choice_healthy, rt_med_choice_healthy, n_choice_tasty, p_choice_tasty, rt_mean_choice_tasty, rt_med_choice_tasty, n_choice_want, p_choice_want, rt_mean_choice_want, rt_med_choice_want, n_sc_choice_missed, p_sc_choice_missed, healthdif_sc_trials, tastedif_sc_trials, wantdif_sc_trials, healthdif_missed_sc_trials, tastedif_missed_sc_trials, wantdif_missed_sc_trials, healthdif_complete_sc_trials, tastedif_complete_sc_trials, wantdif_complete_sc_trials, n_sc_choice_healthy, p_sc_choice_healthy, rt_mean_sc_choice_healthy, rt_med_sc_choice_healthy, health_mean_sc_choice_healthy, taste_mean_sc_choice_healthy, want_mean_sc_choice_healthy, healthdif_mean_sc_choice_healthy, tastedif_mean_sc_choice_healthy, wantdif_mean_sc_choice_healthy, n_sc_choice_tasty, p_sc_choice_tasty, rt_mean_sc_choice_tasty, rt_med_sc_choice_tasty, health_mean_sc_choice_tasty, taste_mean_sc_choice_tasty, want_mean_sc_choice_tasty, healthdif_mean_sc_choice_tasty, tastedif_mean_sc_choice_tasty, wantdif_mean_sc_choice_tasty, n_notsc_choice_missed, p_notsc_choice_missed, healthdif_notsc_trials, tastedif_notsc_trials, wantdif_notsc_trials, healthdif_missed_notsc_trials, tastedif_missed_notsc_trials, wantdif_missed_notsc_trials, healthdif_complete_notsc_trials, tastedif_complete_notsc_trials, wantdif_complete_notsc_trials, n_notsc_choice_healthy, p_notsc_choice_healthy, rt_mean_notsc_choice_healthy, rt_med_notsc_choice_healthy, health_mean_notsc_choice_healthy, taste_mean_notsc_choice_healthy, want_mean_notsc_choice_healthy, healthdif_mean_notsc_choice_healthy, tastedif_mean_notsc_choice_healthy, wantdif_mean_notsc_choice_healthy, n_notsc_choice_tasty, p_notsc_choice_tasty, rt_mean_notsc_choice_tasty, rt_med_notsc_choice_tasty, health_mean_notsc_choice_tasty, taste_mean_notsc_choice_tasty, want_mean_notsc_choice_tasty, healthdif_mean_notsc_choice_tasty, tastedif_mean_notsc_choice_tasty, wantdif_mean_notsc_choice_tasty)
  
  
  return(beh_dat)
}
