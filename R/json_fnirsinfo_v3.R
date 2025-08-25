#' json_fnirsinfo_v3: Generates a json file for contextual information about the fnirs task during visit 3/follow-up
#'
#' This function generates a json file for contextual information about the fnirs taskduring visit 3/follow-up
#'
#' @return A json file documenting the Study BRAKE's contextual information about the fnirs taskduring visit 3/follow-up
#'
#'
#' @export

json_fnirsinfo_v3 <- function() {
  
  fnirsinfo_v3_list <- list(
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
    premeal_tastetest_complete = list( Description = 'Did child complete the pre-meal Taste-Test task?',
                               Levels = list ('0' = 'No',
                                              '1' = 'Yes')),
    premeal_tastetest_fnirs_complete = list( Description = 'Was fNIRS recorded during the pre-meal Taste-Test task?',
                            Levels = list ('0' = 'No',
                                           '1' = 'Yes')),
    premeal_tastetest_notes = list( Description = 'Notes about the pre-meal Taste-Test task'),
    postmeal_tastetest_complete = list( Description = 'Did child complete the post-meal Taste-Test task?',
                                       Levels = list ('0' = 'No',
                                                      '1' = 'Yes')),
    postmeal_tastetest_fnirs_complete = list( Description = 'Was fNIRS recorded during the post-meal Taste-Test task?',
                                             Levels = list ('0' = 'No',
                                                            '1' = 'Yes')),
    postmeal_tastetest_notes = list( Description = 'Notes about the post-meal Taste-Test task'))
  
  # convert formatting to JSON
  fnirsinfo_v3_json <- RJSONIO::toJSON(fnirsinfo_v3_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(fnirsinfo_v3_json, asText = TRUE))){
    print('fNIRS v3 task info JSON file may be invalid')
  }
  
  return(fnirsinfo_v3_json)
  
}