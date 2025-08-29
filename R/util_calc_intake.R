#' util_calc_intake: Computes intake variables
#'
#' This function computes intake variables
#'
#' @param intake_data Intake data (i.e., pre and post weights) extracted double entry data
#' 
#' 
#' @return A dataframe of intake_data (input) variables and calculated intake variables
#'
#' @examples
#'
#' # process data
#' intake_calc <- util_calc_intake(intake_data)
#'
#' @seealso [proc_redcap(), util_merge_intake(), ref_brake_ed()]
#'
#' @export

util_calc_intake <- function(intake_data) {
  
  # convert all intake columns to numeric
  intake_data[!grepl('id|visit|notes|time|test_meal_book', names(intake_data))] <- sapply(intake_data[!grepl('id|visit|notes|time|test_meal_book', names(intake_data))], as.numeric)
  
  # make dataframe with energy density data
  data(brake_ed)
  
  #### calculate item amounts consumed ####
  
  consumed_fn <- function(food_str, intake_data){

    pre_var <- paste(food_str, '_pre_w_plate', sep = '')
    post_var <- paste(food_str, '_post_w_plate', sep = '')
    consumed_g_var <- paste(food_str, '_g_consumed', sep = '')
    consumed_kcal_var <- paste(food_str, '_kcal_consumed', sep = '')
    
    food_data <- data.frame(matrix(ncol = 0, nrow = nrow(intake_data)))
    
    
    food_data[[consumed_g_var]] <- intake_data[[pre_var]] - intake_data[[post_var]]
    
    # if less than 0 g (post > pre), set to 0
    food_data[[consumed_g_var]] <- sapply(food_data[[consumed_g_var]], function(x) ifelse(!is.na(x), ifelse(x < 0, 0, x), NA))
    
    # calculate kcal using EDs in brake_ed
    food_data[[consumed_kcal_var]] <- food_data[[consumed_g_var]] * brake_ed[brake_ed['food'] == food_str, 'ed']
    
    return(food_data)
  }
  
  foods <- brake_ed$food
  
  kcal_consumed_data <- cbind.data.frame(sapply(foods, function(x) consumed_fn(x, intake_data), USE.NAMES = FALSE, simplify = FALSE))
  
  # merge with intake data - need to do a workaround for grilled cheese kcal name - acting odd
  intake_names <- names(intake_data)
  consumed_names <- names(kcal_consumed_data)
  intake_data <- cbind.data.frame(c(intake_data, kcal_consumed_data))
  
  names(intake_data) <- c(intake_names, consumed_names)
  
  #### calculate total amounts consumed ####
  # note: by using na.rm = FALSE -- total amounts will only be calculated if there is data for all food items to be summed
  
  ## meal
  
  # sum across meal_foods_g_vars columns
  intake_data['meal_g_consumed'] <- rowSums(intake_data[grepl('cknug_g|mac_g|carrots_g|grapes_g|ketchup_g', names(intake_data))], na.rm = FALSE)
  
  intake_data['meal_g_consumed_inc_water'] <- rowSums(intake_data[grepl('cknug_g|mac_g|carrots_g|grapes_g|ketchup_g|^water_g', names(intake_data))], na.rm = FALSE)
  
  intake_data['meal_kcal_consumed'] <- rowSums(intake_data[grepl('cknug_k|mac_k|carrots_k|grapes_k|ketchup_k', names(intake_data))], na.rm = FALSE)
  
  ## EAH
  intake_data['eah_g_consumed'] <- rowSums(intake_data[grepl('brownie_g|fritos_g|hershey_g|cream_g|oreos_g|popcorn_g|pretzel_g|skittles_g|starburst_g', names(intake_data))], na.rm = FALSE)
  
  intake_data['eah_g_consumed_inc_water'] <- rowSums(intake_data[grepl('brownie_g|fritos_g|hershey_g|cream_g|oreos_g|popcorn_g|pretzel_g|skittles_g|starburst_g|eah_water_g', names(intake_data))], na.rm = FALSE)
  
  intake_data['eah_kcal_consumed'] <- rowSums(intake_data[grepl('brownie_k|fritos_k|hershey_k|cream_k|oreos_k|popcorn_k|pretzel_k|skittles_k|starburst_k', names(intake_data))], na.rm = FALSE)
  
  
  ## total (meal + eah)
  intake_data['total_g_consumed'] <- rowSums(intake_data[c('meal_g_consumed', 'eah_g_consumed')], na.rm = FALSE)
  
  intake_data['total_g_consumed_inc_water'] <- rowSums(intake_data[c('meal_g_consumed_inc_water', 'eah_g_consumed_inc_water')], na.rm = FALSE)
  
  intake_data['total_kcal_consumed'] <- rowSums(intake_data[c('meal_kcal_consumed', 'eah_kcal_consumed')], na.rm = FALSE)
  
  # return data
  return(intake_data)
  
}
