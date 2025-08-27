#' json_cams: Generates a json file for Child Anxiety Meter scale data from double entry REDCap
#'
#' This function generates a json file for Child Anxiety Meter scale data
#'
#' @return A string with data stored in JSON format containing meta-data for Child Anxiety Meter scale data
#'
#'
#' @export

json_cams <- function() {

  cams_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Childrens Anxiety Meter - State',
      Reference = 'Ersig, Anne L., Charmaine Kleiber, Ann Marie McCarthy, and Kirsten Hanrahan. "Validation of a clinically useful measure of children\'s state anxiety before medical procedures." Journal for Specialists in Pediatric Nursing 18, no. 4 (2013): 311-319.',
      TermURL = 'https://pmc.ncbi.nlm.nih.gov/articles/PMC4282760/pdf/nihms650963.pdf'),
    participant_id = list( Description = 'participant id number'),
    cams_pre_baseline = list( Description = 'Pre-fNIRS anxiety at baseline'),
    cams_post_baseline = list( Description = 'Post-fNIRS anxiety at baseline'),
    cams_pre_premeal_followup = list( Description = 'Pre-meal, Pre-fNIRS anxiety at follow-up'),
    cams_post_premeal_followup = list( Description = 'Pre-meal, Post-fNIRS anxiety at follow-up'),
    cams_pre_postmeal_followup = list( Description = 'Post-meal, Pre-fNIRS anxiety at follow-up'),
    cams_post_postmeal_followup = list( Description = 'Post-meal, Post-fNIRS anxiety at follow-up')
  )

  # convert formatting to JSON
  cams_json <- RJSONIO::toJSON(cams_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(cams_json, asText = TRUE))){
    print('Double-entered Taste-Test samples JSON file may be invalid')
  }

  return(cams_json)

}
