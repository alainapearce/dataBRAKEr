#' json_participants: Generates a json file for participants.tsv
#'
#' This function generates a json file for participants.tsv
#'
#' @return A string with data stored in JSON format containing meta-data for participants.tsv
#'
#'
#' @export

json_participants <- function() {
  
  participants_list <- list(
    participant_id = list( Description = 'participant id number'),
    sex = list( Description = 'Parent-reported child sex',
                Levels = list ('female' = 'female',
                               'male' = 'male')),
    ethnicity = list( Description = 'Parent-reported child ethnicity',
                      Levels = list ('0' = 'Not Hispanic or Latino',
                                     '1' = 'Hispanic or Latino')),
    race = list( Description = 'Parent-reported child race',
                 Levels = list ('0' = 'American Indian/Alaskan Native',
                                '1' = 'Asian',
                                '2' = 'Black or African American',
                                '3' = 'White',
                                '4' = 'Hawaiian/Pacific Islander',
                                '5' = 'Other')),
    child_other_race = list( Description = 'If other race, please specify'),
    child_weight_status = list( Description = 'Child weight status based on CDC cutoffs by child BMI percential',
                                Levels = list ('obese' = 'has obesity; child_bmi_p >= 95 ',
                                               'overweight' = 'has overwegiht; child_bmi_p >= 85 and < 95',
                                               'healthy weight' = 'does not have overweight/obesity; child_bmi_p < 85')),
    child_protocol_1_age = list( Description = 'Child age at child visit protocol 1. Computed as difference between child_protocol_1_date and child date of birth (not shared)',
                                 Unit = 'years',
                                 Derivative = TRUE),
    child_protocol_2_age = list( Description = 'Child age at child visit protocol 2. Computed as difference between child_protocol_2_date and child date of birth (not shared)',
                                 Unit = 'years',
                                 Derivative = TRUE),
    child_protocol_3_age = list( Description = 'Child age at child visit protocol 3. Computed as difference between child_protocol_3_date and child date of birth (not shared)',
                                 Unit = 'years',
                                 Derivative = TRUE),
    protocol_visits_order = list( Description = 'Child protocol visit order based on child_protocol dates',
                                  Derivative = TRUE),
    child_protocol_1_date = list( Description = 'Date of child visit protocol 1 (YYYY-MM-DD)'),
    child_protocol_2_date = list( Description = 'Date of child visit protocol 2 (YYYY-MM-DD)'),
    child_protocol_3_date = list( Description = 'Date of child visit protocol 3 (YYYY-MM-DD)')
  )
  
  # convert formatting to JSON
  participants_json <- RJSONIO::toJSON(participants_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(participants_json, asText = TRUE))){
    print('Participants JSON file may be invalid')
  }
  
  return(participants_json)
  
}
