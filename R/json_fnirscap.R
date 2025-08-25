#' json_fnirscap: Generates a json file for information about fnirs cap and headsize
#'
#' This function generates a json file about fnirs cap and headsize
#'
#' @return A json file documenting the Study BRAKE participant fnirs cap and headsize
#'
#'
#' @export

json_fnirscap <- function() {
  
  fnirscap_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1 (baseline)',
                                          '3'	= 'Child visit protocol 3 (follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    fnirs_frontback_cm = list( Description = 'measurement from nassion to inion of the head',
                               Units = 'cm'),
    fnirs_ears_cm = list( Description = 'measurement from ear to ear of the head',
                          Units = 'cm'),
    fnirs_circ_cm = list( Description = 'measurement around the circumference of the head',
                         Units = 'cm'),
    fnirs_capsize = list( Description = 'chosen capsize based on fnirs_frontback_cm, fnirs_ears_cm, and fnirs_circ_cm'),
    fnirs_notes = list( Description = 'notes about fNIRS cap fit and measurements'))
  
  # convert formatting to JSON
  fnirscap_json <- RJSONIO::toJSON(fnirscap_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(fnirscap_json, asText = TRUE))){
    print('fNIRS cap fit info JSON file may be invalid')
  }
  
  return(fnirscap_json)
  
}