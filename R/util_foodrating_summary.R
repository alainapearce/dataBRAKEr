#' util_foodrating_summary: Get summary data from individual participants for the Food Rating task
#'
#' This function calculates summary performance data for an individual participant
#'
#'
#' @param ind_data Processed individual dataset from rawdata for the Food Rating task 
#'
#' @return a data.frame with 1 row including summary performance and task metrics for a participant
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' foodrating_summary_beh <- util_foodrating_summary(ind_data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_foodrating_summary <- function(ind_data) {
  
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
  
  #### Individual CV funciton ####
  icv <- function(resp_data){
    mean_data <- mean(resp_data, na.rm = TRUE)
    sd_data <- sd(resp_data, na.rm = TRUE)
    ind_cv <- (mean_data/sd_data)*100
    
    return(ind_cv)
  }
  
  #### Summary Data  #####
  
  order <- ifelse(ind_data[['exp_cond_num']][1] == 1, 'htw', ifelse(ind_data[['exp_cond_num']][1] == 2, 'wht', 'twh'))
  
  want_mean <- mean(ind_data[ind_data['cond'] == 'want', 'rating'], na.rm = TRUE)
  health_mean <- mean(ind_data[ind_data['cond'] == 'health', 'rating'], na.rm = TRUE)
  taste_mean <- mean(ind_data[ind_data['cond'] == 'taste', 'rating'], na.rm = TRUE)
  
  want_med <- median(ind_data[ind_data['cond'] == 'want', 'rating'], na.rm = TRUE)
  health_med <- median(ind_data[ind_data['cond'] == 'health', 'rating'], na.rm = TRUE)
  taste_med <- median(ind_data[ind_data['cond'] == 'taste', 'rating'], na.rm = TRUE)
  
  want_sd <- sd(ind_data[ind_data['cond'] == 'want', 'rating'], na.rm = TRUE)
  health_sd <- sd(ind_data[ind_data['cond'] == 'health', 'rating'], na.rm = TRUE)
  taste_sd <- sd(ind_data[ind_data['cond'] == 'taste', 'rating'], na.rm = TRUE)
  
  want_icv <- icv(ind_data[ind_data['cond'] == 'want', 'rating'])
  health_icv <- icv(ind_data[ind_data['cond'] == 'health', 'rating'])
  taste_icv <- icv(ind_data[ind_data['cond'] == 'taste', 'rating'])
  
  #by ED - high
  want_high_ed_mean <- mean(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  health_high_ed_mean <- mean(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  taste_high_ed_mean <- mean(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  
  want_high_ed_med <- median(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  health_high_ed_med <- median(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  taste_high_ed_med <- median(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  
  want_high_ed_sd <- sd(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  health_high_ed_sd <- sd(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  taste_high_ed_sd <- sd(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'high_ed', 'rating'], na.rm = TRUE)
  
  want_high_ed_icv <- icv(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'high_ed', 'rating'])
  health_high_ed_icv <- icv(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'high_ed', 'rating'])
  taste_high_ed_icv <- icv(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'high_ed', 'rating'])
  
  #by ED - high
  want_low_ed_mean <- mean(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  health_low_ed_mean <- mean(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  taste_low_ed_mean <- mean(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  
  want_low_ed_med <- median(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  health_low_ed_med <- median(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  taste_low_ed_med <- median(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  
  want_low_ed_sd <- sd(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  health_low_ed_sd <- sd(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  taste_low_ed_sd <- sd(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'low_ed', 'rating'], na.rm = TRUE)
  
  want_low_ed_icv <- icv(ind_data[ind_data['cond'] == 'want' & ind_data['image_ed'] == 'low_ed', 'rating'])
  health_low_ed_icv <- icv(ind_data[ind_data['cond'] == 'health' & ind_data['image_ed'] == 'low_ed', 'rating'])
  taste_low_ed_icv <- icv(ind_data[ind_data['cond'] == 'taste' & ind_data['image_ed'] == 'low_ed', 'rating'])
  
  # compile
  beh_dat <- data.frame(order, want_mean, health_mean, taste_mean, want_med, health_med, taste_med, want_sd, health_sd, taste_sd, want_icv, health_icv, taste_icv, want_high_ed_mean, health_high_ed_mean, taste_high_ed_mean, want_high_ed_med, health_high_ed_med, taste_high_ed_med, want_high_ed_sd, health_high_ed_sd, taste_high_ed_sd, want_high_ed_icv, health_high_ed_icv, taste_high_ed_icv, want_low_ed_mean, health_low_ed_mean, taste_low_ed_mean, want_low_ed_med, health_low_ed_med, taste_low_ed_med, want_low_ed_sd, health_low_ed_sd, taste_low_ed_sd, want_low_ed_icv, health_low_ed_icv, taste_low_ed_icv)
  
  beh_dat <- beh_dat[c('order', names(beh_dat)[grepl('want', names(beh_dat))], names(beh_dat)[grepl('health', names(beh_dat))], names(beh_dat)[grepl('taste', names(beh_dat))])]
  
  
  return(beh_dat)
}