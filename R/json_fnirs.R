#' json_fnirs: Generates a json file for information about fnirs 
#'
#' This function generates a json file about fnirs
#'
#' @return A json file documenting the Study BRAKE participant fnirs
#'
#'
#' @export

json_fnirs <- function() {
  
  fnirs_list <- list(
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
    fnirs_notes = list( Description = 'notes about fNIRS cap fit and measurements'),
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
    foodchoice_notes = list( Description = 'Notes about the Food Choice Game task'),
    cams_pre_baseline = list( Description = 'Pre-fNIRS anxiety at baseline'),
    cams_post_baseline = list( Description = 'Post-fNIRS anxiety at baseline'),
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
    postmeal_tastetest_notes = list( Description = 'Notes about the post-meal Taste-Test task'),
    cams_pre_premeal_followup = list( Description = 'Pre-meal, Pre-fNIRS anxiety at follow-up'),
    cams_post_premeal_followup = list( Description = 'Pre-meal, Post-fNIRS anxiety at follow-up'),
    cams_pre_postmeal_followup = list( Description = 'Post-meal, Pre-fNIRS anxiety at follow-up'),
    cams_post_postmeal_followup = list( Description = 'Post-meal, Post-fNIRS anxiety at follow-up'))
  
  # convert formatting to JSON
  fnirs_json <- RJSONIO::toJSON(fnirs_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(fnirs_json, asText = TRUE))){
    print('fNIRS info JSON file may be invalid')
  }
  
  return(fnirs_json)
  
}