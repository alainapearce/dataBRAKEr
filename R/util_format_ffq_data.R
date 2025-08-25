#' util_format_ffq_data: Prepare Helix FFQ data for scoring
#'
#' This function prepares Helix FFQ data for scoring with dataprepr::score_ffq_helix()
#'
#'
#' @param ffq_data ffq data (Helix Food Frequency Questionnaire) extracted from data from REDCap events
#'
#' @examples
#'
#' # process ffq data
#' ffq_data_formatted <- util_format_ffq_data(ffq_data)
#'
#' @seealso [util_redcap_parent1()], [util_redcap_parent3()]
#'
#' @export


util_format_ffq_data <- function(ffq_data) {
  
  # fix naming
  names(ffq_data)[names(ffq_data) == 'ffq_milk'] <- 'ffq_dairy1'
  names(ffq_data)[names(ffq_data) == 'ffq_yogurt'] <- 'ffq_dairy2'
  names(ffq_data)[names(ffq_data) == 'ffq_prebiotic'] <- 'ffq_dairy3'
  names(ffq_data)[names(ffq_data) == 'ffq_cheese'] <- 'ffq_dairy4'
  names(ffq_data)[names(ffq_data) == 'ffq_egg'] <- 'ffq_egg1'
  names(ffq_data)[names(ffq_data) == 'ffq_poultry'] <- 'ffq_meat1'
  names(ffq_data)[names(ffq_data) == 'ffq_redmeat'] <- 'ffq_meat2'
  names(ffq_data)[names(ffq_data) == 'ffq_sausage'] <- 'ffq_meat3'
  names(ffq_data)[names(ffq_data) == 'ffq_ham'] <- 'ffq_meat4'
  names(ffq_data)[names(ffq_data) == 'ffq_leanfish'] <- 'ffq_fish1'
  names(ffq_data)[names(ffq_data) == 'ffq_fattyfish'] <- 'ffq_fish2'
  names(ffq_data)[names(ffq_data) == 'ffq_canfish'] <- 'ffq_fish3'
  names(ffq_data)[names(ffq_data) == 'ffq_seafood'] <- 'ffq_fish4'
  names(ffq_data)[names(ffq_data) == 'ffq_dairy_desert'] <- 'ffq_dairy5'
  names(ffq_data)[names(ffq_data) == 'ffq_raw_veg'] <- 'ffq_veg1'
  names(ffq_data)[names(ffq_data) == 'ffq_cook_veg'] <- 'ffq_veg2'
  names(ffq_data)[names(ffq_data) == 'ffq_potatoes'] <- 'ffq_potato1'
  names(ffq_data)[names(ffq_data) == 'ffq_legumes'] <- 'ffq_legume1'
  names(ffq_data)[names(ffq_data) == 'ffq_frenchfries'] <- 'ffq_potato2'
  names(ffq_data)[names(ffq_data) == 'ffq_fruits'] <- 'ffq_fruit1'
  names(ffq_data)[names(ffq_data) == 'ffq_fruitjuice'] <- 'ffq_fruit2'
  names(ffq_data)[names(ffq_data) == 'ffq_nuts'] <- 'ffq_nuts1'
  names(ffq_data)[names(ffq_data) == 'ffq_canfruit'] <- 'ffq_fruit3'
  names(ffq_data)[names(ffq_data) == 'ffq_dryfruit'] <- 'ffq_fruit4'
  names(ffq_data)[names(ffq_data) == 'ffq_whitebread'] <- 'ffq_cereal1'
  names(ffq_data)[names(ffq_data) == 'ffq_wheatbread'] <- 'ffq_cereal2'
  names(ffq_data)[names(ffq_data) == 'ffq_sugarcereal'] <- 'ffq_cereal3'
  names(ffq_data)[names(ffq_data) == 'ffq_othercereal'] <- 'ffq_cereal4'
  names(ffq_data)[names(ffq_data) == 'ffq_ricepasta'] <- 'ffq_cereal5'
  names(ffq_data)[names(ffq_data) == 'ffq_ricecakes'] <- 'ffq_cereal6'
  names(ffq_data)[names(ffq_data) == 'ffq_cookies'] <- 'ffq_bakery1'
  names(ffq_data)[names(ffq_data) == 'ffq_cakes'] <- 'ffq_bakery2'
  names(ffq_data)[names(ffq_data) == 'ffq_chocolate'] <- 'ffq_sweet1'
  names(ffq_data)[names(ffq_data) == 'ffq_sugar'] <- 'ffq_sweet2'
  names(ffq_data)[names(ffq_data) == 'ffq_sweets'] <- 'ffq_sweet3'
  names(ffq_data)[names(ffq_data) == 'ffq_soda'] <- 'ffq_bev1'
  names(ffq_data)[names(ffq_data) == 'ffq_dietsoda'] <- 'ffq_bev2'
  names(ffq_data)[names(ffq_data) == 'ffq_oliveoil'] <- 'ffq_fats1'
  names(ffq_data)[names(ffq_data) == 'ffq_otheroil'] <- 'ffq_fats2'
  names(ffq_data)[names(ffq_data) == 'ffq_butter'] <- 'ffq_fats3'
  names(ffq_data)[names(ffq_data) == 'ffq_margarine'] <- 'ffq_fats4'
  names(ffq_data)[names(ffq_data) == 'ffq_sauces'] <- 'ffq_dressing1'
  names(ffq_data)[names(ffq_data) == 'ffq_chips'] <- 'ffq_saltysnack1'
  
  # return data
  return(ffq_data)
}
