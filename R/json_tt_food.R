#' json_tt_food: Generates a json file for Taste-test sample data from double entry REDCap
#'
#' This function generates a json file for Taste-test sample
#'
#' @return A string with data stored in JSON format containing meta-data for Taste-test sample
#'
#'
#' @export

json_tt_food <- function() {

  tt_food_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    tt_carrot = list( Description = 'Taste-Test sample weight for carrot',
                                Unit = "grams"),
    tt_cknug1 = list( Description = 'Taste-Test sample weight for chicken nuggets - sample 1',
                                 Unit = "grams"),
    tt_cknug2 = list( Description = 'Taste-Test sample weight for chicken nuggets - sample 2',
                                   Unit = "grams"),
    tt_cknug3 = list( Description = 'Taste-Test sample weight for chicken nuggets - sample 3',
                                 Unit = "grams"),
    tt_mac1 = list( Description = 'Taste-Test sample weight for macaroni and cheese - sample 1',
                                         Unit = "grams"),
    tt_mac2 = list( Description = 'Taste-Test sample weight for macaroni and cheese - sample 2',
                                       Unit = "grams"),
    tt_mac3 = list( Description = 'Taste-Test sample weight for macaroni and cheese - sample 3',
                                        Unit = "grams"),
    tt_grape1 = list( Description = 'Taste-Test sample weight for grapes - sample 1',
                                 Unit = "grams"),
    tt_grape2 = list( Description = 'Taste-Test sample weight for grapes - sample 2',
                               Unit = "grams"),
    tt_grape3 = list( Description = 'Taste-Test sample weight for grapes - sample 3',
                                  Unit = "grams"),
    tt_broc = list( Description = 'Taste-Test sample weight for broccoli',
                                Unit = "grams"),
    tt_gbeans = list( Description = 'Taste-Test sample weight for green beans',
                                 Unit = "grams"),
    tt_orange = list( Description = 'Taste-Test sample weight for canned mandarine oranges',
                            Unit = "grams"),
    tt_hershey = list( Description = 'Taste-Test sample weight for hershey kiss',
                              Unit = "grams"),
    tt_fruitsnack = list( Description = 'Taste-Test sample weight for cherry fruit snack',
                               Unit = "grams"),
    tt_cracker = list( Description = 'Taste-Test sample weight for cracker',
                                Unit = "grams")
  )

  # convert formatting to JSON
  tt_food_json <- RJSONIO::toJSON(tt_food_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(tt_food_json, asText = TRUE))){
    print('Double-entered Taste-Test samples JSON file may be invalid')
  }

  return(tt_food_json)

}
