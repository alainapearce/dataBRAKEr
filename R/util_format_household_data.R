#' util_format_household_data: process household data for Study BRAKE visit 1 and 5
#'
#' This function process household data
#'
#'
#' @param household_data Household extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' household_data_formatted <- util_format_household_data(household_data)
#'
#' @seealso [util_redcap_parent1()], [util_redcap_parent3()]
#'
#' @export

util_format_household_data <- function(household_data) {

  # rename columns
  names(household_data)[names(household_data) == 'demo_otherparent_height_ft'] <- 'demo_parent2_rep_height_ft'
  names(household_data)[names(household_data) == 'demo_otherparent_height_in'] <- 'demo_parent2_rep_height_in'
  names(household_data)[names(household_data) == 'demo_otherparent_weight_lb'] <- 'demo_parent2_rep_weight_lbs'

  # calculate parent age
  household_data[['demo_parent_dob']] <- lubridate::as_date(household_data[['demo_parent_dob']])
  household_data[['visit_date']] <- lubridate::as_date(household_data[['visit_date']])

  household_data[['demo_parent_age']] <- round(lubridate::interval(household_data[['visit_date']], household_data[['visit_date']])/lubridate::years(1), 1)


  # remove birthdate and timestamp variables
  household_data <- household_data[, !grepl('dob', names(household_data))]

  # combine parent2 feet and inch components into 1 height variable in meters
  household_data['demo_parent2_rep_height_m'] <- ((household_data[['demo_parent2_rep_height_ft']]*12) + household_data[['demo_parent2_rep_height_in']])*0.0254

  # convert parent2 lbs variable in kg
  household_data['demo_parent2_rep_weight_kg'] <- household_data[['demo_parent2_rep_weight_lbs']]*0.453592

  # calculate parent2 BMI (kg/m2)
  household_data['demo_parent2_rep_bmi'] <- household_data[['demo_parent2_rep_weight_kg']]/household_data[['demo_parent2_rep_height_m']]^2

  # food assistance programs
  names(household_data)[names(household_data) == 'demo_programs___0'] <- 'demo_assist_program_no'
  names(household_data)[names(household_data) == 'demo_programs___1'] <- 'demo_assist_program_snap'
  names(household_data)[names(household_data) == 'demo_programs___2'] <- 'demo_assist_program_wic'
  names(household_data)[names(household_data) == 'demo_programs___3'] <- 'demo_assist_program_tnaf'
  names(household_data)[names(household_data) == 'demo_programs___4'] <- 'demo_assist_program_medicaid'
  names(household_data)[names(household_data) == 'demo_programs___5'] <- 'demo_assist_program_liheap'
  names(household_data)[names(household_data) == 'demo_programs___6'] <- 'demo_assist_program_pfr_lunch'
  names(household_data)[names(household_data) == 'demo_programs___7'] <- 'demo_assist_program_fr_lunch'
  names(household_data)[names(household_data) == 'demo_programs___8'] <- 'demo_assist_program_other'

  # return data
  return(household_data)

}
