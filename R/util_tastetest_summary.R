#' util_tastetest_summary: Get summary data from individual participants for the Taste-Test task
#'
#' This function calculates summary performance data for an individual participant
#'
#'
#' @inheritParams util_task_org_sourcedata
#' @param raw_wd individual participant rawdata directory path
#' @param nirs_folder name of folder within the nirs directory - option: premeal, postmeal
#' @param file_name file name of individual *_events.tsv data file to load
#' 
#'
#' @return a data.frame with 1 row including summary performance and task metrics for a participant
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' foodrating_summary_beh <- util_tastetest_summary(sub_str, raw_wd, nirs_folder, file_name)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_tastetest_summary <- function(sub_str, raw_wd, nirs_folder, file_name) {
  
  #### 1. Set up/initial checks #####
  
  # check that inputs exist and is a data.frame
  sub_str_arg <- methods::hasArg(sub_str)
  wd_arg <- methods::hasArg(raw_wd)
  nirs_folder_arg <- methods::hasArg(nirs_folder)
  file_name_arg <- methods::hasArg(file_name)
  
  arg_check <- c(sub_str_arg, wd_arg, nirs_folder_arg, file_name_arg)
  arg_str <- c('sub_str', 'hasArg', 'nirs_folder', 'file_name')
  arg <- c(sub_str, raw_wd, nirs_folder, file_name)
  
  
  arg_check_fn <- function(arg_check, arg, arg_str){
    if (isTRUE(arg_check)) {
      if (!is.character(arg)) {
        stop(paste0(arg_str, ' must be entered as a character string'))
      } 
    } else if (isFALSE(arg_check)) {
      stop(paste0(arg_str, ' must be entered as a character string'))
    }
  }
  
  sapply(seq(1, length(arg_check)), function(x) arg_check_fn(arg_check[x], arg[x], arg_str[x]))
  
  #### Load Data ####
  data_file <- file.path(raw_wd, nirs_folder, file_name)
  ind_data <- read.table(data_file, sep='\t', header = TRUE, na.strings = 'n/a')
  
  #### Summary Data  #####
  
  cond_order <- ind_data[['exp_cond_num']][1]
  visit_date <- ind_data[['date']][1]
  
  #average ratings
  meal_mean_want <- mean(ind_data[ind_data['trial_cond'] == 'meal' & ind_data['task_component'] == 'want_rating' , 'rating'], na.rm = TRUE)
  meal_mean_like <- mean(ind_data[ind_data['trial_cond'] == 'meal' & ind_data['task_component'] == 'like_rating' , 'rating'], na.rm = TRUE)
  
  low_ed_mean_want <- mean(ind_data[ind_data['trial_cond'] == 'low_ed' & ind_data['task_component'] == 'want_rating' , 'rating'], na.rm = TRUE)
  low_ed_mean_like <- mean(ind_data[ind_data['trial_cond'] == 'low_ed' & ind_data['task_component'] == 'like_rating' , 'rating'], na.rm = TRUE)
  
  high_ed_mean_want <- mean(ind_data[ind_data['trial_cond'] == 'high_ed' & ind_data['task_component'] == 'want_rating' , 'rating'], na.rm = TRUE)
  high_ed_mean_like <- mean(ind_data[ind_data['trial_cond'] == 'high_ed' & ind_data['task_component'] == 'like_rating' , 'rating'], na.rm = TRUE)
  
  #by food ratings
  carrot_want <- ind_data[ind_data['food_item'] == 'carrot' & ind_data['task_component'] == 'want_rating' , 'rating'][1]
  carrot_like <- ind_data[ind_data['food_item'] == 'carrot' & ind_data['task_component'] == 'like_rating' , 'rating'][1]
  
  cknug1_want <- ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'want_rating' , 'rating'][1]
  cknug1_like <- ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'like_rating' , 'rating'][1]
  
  cknug2_want <- ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'want_rating' , 'rating'][2]
  cknug2_like <- ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'like_rating' , 'rating'][2]
  
  cknug3_want <- ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'want_rating' , 'rating'][3]
  cknug3_like <- ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'like_rating' , 'rating'][3]
  
  cknug_mean_want <- mean(ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'want_rating' , 'rating'], na.rm = TRUE)
  cknug_mean_like <- mean(ind_data[ind_data['food_item'] == 'Chicken Nugget' & ind_data['task_component'] == 'like_rating' , 'rating'], na.rm = TRUE)
  
  mac1_want <- ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'want_rating' , 'rating'][1]
  mac1_like <- ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'like_rating' , 'rating'][1]
  
  mac2_want <- ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'want_rating' , 'rating'][2]
  mac2_like <- ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'like_rating' , 'rating'][2]
  
  mac3_want <- ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'want_rating' , 'rating'][2]
  mac3_like <- ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'like_rating' , 'rating'][2]
  
  mac_mean_want <- mean(ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'want_rating' , 'rating'], na.rm = TRUE)
  mac_mean_like <- mean(ind_data[ind_data['food_item'] == 'Macarroni and Cheese' & ind_data['task_component'] == 'like_rating' , 'rating'], na.rm = TRUE)
  
  grape1_want <- ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'want_rating' , 'rating'][1]
  grape1_like <- ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'like_rating' , 'rating'][1]
  
  grape2_want <- ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'want_rating' , 'rating'][2]
  grape2_like <- ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'like_rating' , 'rating'][2]
  
  grape3_want <- ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'want_rating' , 'rating'][3]
  grape3_like <- ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'like_rating' , 'rating'][3]
  
  grape_mean_want <- mean(ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'want_rating' , 'rating'], na.rm = TRUE)
  grape_mean_like <- mean(ind_data[ind_data['food_item'] == 'Grape' & ind_data['task_component'] == 'like_rating' , 'rating'], na.rm = TRUE)
  
  orange_want <- ind_data[ind_data['food_item'] == 'Orange' & ind_data['task_component'] == 'want_rating' , 'rating']
  orange_like <- ind_data[ind_data['food_item'] == 'Orange' & ind_data['task_component'] == 'like_rating' , 'rating']
  
  broccoli_want <- ind_data[ind_data['food_item'] == 'Broccoli' & ind_data['task_component'] == 'want_rating' , 'rating']
  broccoli_like <- ind_data[ind_data['food_item'] == 'Broccoli' & ind_data['task_component'] == 'like_rating' , 'rating']
  
  gbean_want <- ind_data[ind_data['food_item'] == 'Green Bean' & ind_data['task_component'] == 'want_rating' , 'rating']
  gbean_like <- ind_data[ind_data['food_item'] == 'Green Bean' & ind_data['task_component'] == 'like_rating' , 'rating']
  
  hershey_want <- ind_data[ind_data['food_item'] == 'Chocolate' & ind_data['task_component'] == 'want_rating' , 'rating']
  hershey_like <- ind_data[ind_data['food_item'] == 'Chocolate' & ind_data['task_component'] == 'like_rating' , 'rating']
  
  cracker_want <- ind_data[ind_data['food_item'] == 'Cracker' & ind_data['task_component'] == 'want_rating' , 'rating']
  cracker_like <- ind_data[ind_data['food_item'] == 'Cracker' & ind_data['task_component'] == 'like_rating' , 'rating']
  
  fruitsnack_want <- ind_data[ind_data['food_item'] == 'Fruit Chew' & ind_data['task_component'] == 'want_rating' , 'rating']
  fruitsnack_like <- ind_data[ind_data['food_item'] == 'Fruit Chew' & ind_data['task_component'] == 'like_rating' , 'rating']
  
  
  
  # compile
  beh_dat <- data.frame(visit_date, cond_order, meal_mean_want, meal_mean_like, low_ed_mean_want, low_ed_mean_like, high_ed_mean_want, high_ed_mean_like, carrot_want, carrot_like, cknug1_want, cknug1_like, cknug2_want, cknug2_like, cknug3_want, cknug3_like, cknug_mean_want, cknug_mean_like, mac1_want, mac1_like, mac2_want, mac2_like, mac3_want, mac3_like, mac_mean_want, mac_mean_like, grape1_want, grape1_like, grape2_want, grape2_like, grape3_want, grape3_like, grape_mean_want, grape_mean_like, orange_want, orange_like, broccoli_want, broccoli_like, gbean_want, gbean_like, hershey_want, hershey_like, cracker_want, cracker_like, fruitsnack_want, fruitsnack_like)
  
  beh_dat <- beh_dat[c('visit_date', 'cond_order', names(beh_dat)[grepl('want', names(beh_dat))], names(beh_dat)[grepl('like', names(beh_dat))])]
  
  
  return(beh_dat)
}