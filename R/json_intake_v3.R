#' json_meal_info_v3: Generates a json file for meal information, fullness, liking, and wanting
#'
#' This function generates a json file for meal information, fullness, liking, and wanting
#'
#' @return A string with data stored in JSON format containing meta-data for meal information, fullness, liking, and wanting
#'
#'
#' @export

json_meal_info_v3 <- function() {

  intake_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1 (baseline)',
                                          '2' =	'Child visit protocol 2 (baseline)',
                                          '3'	= 'Child visit protocol 3 (follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    
    # fullness variables ----
    pre_meal_fullness_time = list( Description = 'Pre-meal fullness rating time',
                                   Unit = "hh:mm:ss"),
    pre_meal_fullness_notes = list( Description = 'Notes about pre-meal fullness rating'),
    post_meal_fullness_time = list( Description = 'Post-meal fullness rating time',
                                   Unit = "hh:mm:ss"),
    post_meal_fullness_notes = list( Description = 'Notes about post-meal fullness rating'),
    
    # food_paradigm_info_v3 variables ----
    test_meal_book = list( Description = 'Book the child selected to listen to during the test meal'),
    test_meal_start_time = list( Description = 'Meal start time',
                                 Unit = "hh:mm"),
    test_meal_end_time = list( Description = 'Meal end time',
                               Unit = "hh:mm"),
    test_meal_duration = list( Description = 'Meal duration. Derived in redcap from test_meal_start_time and test_meal_end_time',
                               Derivative = TRUE),
    test_meal_notes = list( Description = 'Notes about meal protocol'))

  # convert formatting to JSON
  intake_json <- RJSONIO::toJSON(intake_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(intake_json, asText = TRUE))){
    print('Intake visit JSON file may be invalid')
  }

  return(intake_json)

}
