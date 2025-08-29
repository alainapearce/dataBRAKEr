#' json_child_anthro: Generates a json file for anthropometrics recorded in REDCap
#'
#' This function generates a json file for anthropometrics recorded in REDCap
#'
#' @return A json file documenting the Study BRAKE anthropometrics recorded in REDCap
#'
#'
#' @export

json_child_anthro <- function() {
  
  child_anthro_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1 (baseline)',
                                          '3'	= 'Child visit protocol 3 (follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    height1_cm = list( Description = 'child height measurement 1',
                             Unit = "cm"),
    height2_cm = list( Description = 'child height measurement 2',
                             Unit = "cm"),
    weight1_kg = list( Description = 'child weight measurement 1',
                             Unit = "kg"),
    weight2_kg = list( Description = 'child weight measurement 2',
                             Unit = "kg"),
    height_mean_cm = list( Description = 'average of height1_cm and height2_cm',
                                 Unit = "cm",
                                 Derivative = TRUE),
    weight_mean_kg = list( Description = 'average of weight1_kg and weight2_kg',
                                 Unit = "kg",
                                 Derivative = TRUE),
    parent1_relationship = list( Description = 'Relationship of parent being measured to the child',
                        Levels = list ('0' = 'Mom',
                                       '1' = 'Dad')),
    parent1_height1_cm = list( Description = 'parent height measurement 1',
                               Unit = "cm"),
    parent1_height2_cm = list( Description = 'parent height measurement 2',
                               Unit = "cm"),
    parent1_weight1_kg = list( Description = 'parent weight measurement 1',
                               Unit = "kg"),
    parent1_weight2_kg = list( Description = 'parent weight measurement 2',
                               Unit = "kg"),
    parent1_height_mean_cm = list( Description = 'average of parent1_height1_cm and parent1_height2_cm',
                                   Unit = "cm",
                                   Derivative = TRUE),
    parent1_weight_mean_kg = list( Description = 'average of parent1_weight1_kg and parent1_weight2_kg',
                                   Unit = "kg",
                                   Derivative = TRUE),
    heightweight_notes = list( Description = 'notes about measured height and weight'))
  
  # convert formatting to JSON
  child_anthro_json <- RJSONIO::toJSON(child_anthro_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(child_anthro_json, asText = TRUE))){
    print('child anthro info JSON file may be invalid')
  }
  
  return(child_anthro_json)
  
}