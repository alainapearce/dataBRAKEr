#' json_fnirstasks_info: Generates a json file for contextual information about the fnirs tasks
#'
#' This function generates a json file for contextual information about the fnirs tasks
#'
#' @return A json file documenting the Study BRAKE's contextual information about the fnirs tasks
#'
#'
#' @export

json_fnirstasks_info <- function() {
  
  fnirstasks_info_list <- list(
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
    pre_fnirs_hungry = list( Description = 'Was the child hungry prior to the start of fNIRS? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes'),
                                 Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    pre_fnirs_snack = list( Description = 'Did the child eat the snack prior to the start of fNIRS?',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    pre_fnirs_postsnack_hungry = list( Description = 'Was the child hungry after the first snack prior to the start of fNIRS? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                                           Levels = list ('0' = 'No',
                                                          '1' = 'Yes'),
                                           Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    foodrating_complete = list( Description = 'Did child complete the Food Rating Game task?',
                               Levels = list ('0' = 'No',
                                              '1' = 'Yes')),
    foodrating_fnirs_complete = list( Description = 'Was fNIRS recorded during the Food Rating Game task?',
                            Levels = list ('0' = 'No',
                                           '1' = 'Yes')),
    foodrating_notes = list( Description = 'Notes about the Food Rating Game task'),
    foodchoice_complete = list( Description = 'Did child complete the Food Choice Game task?',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    foodchoice_fnirs_complete = list( Description = 'Was fNIRS recorded during the Food Choice Game task?',
                                      Levels = list ('0' = 'No',
                                                     '1' = 'Yes')),
    foodchoice_eye_good = list( Description = 'Did eye-tracking work without error during the Food Choice Game task? Note: this does not reflect eye-tracking data quality, just the ability to collect the data',
                                    Levels = list ('0' = 'No',
                                                   '1' = 'Yes')),
    foodchoice_notes = list( Description = 'Notes about the Food Choice Game task'))
  
  # convert formatting to JSON
  fnirstasks_info_json <- RJSONIO::toJSON(fnirstasks_info_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(fnirstasks_info_json, asText = TRUE))){
    print('fNIRS task info JSON file may be invalid')
  }
  
  return(fnirstasks_info_json)
  
}