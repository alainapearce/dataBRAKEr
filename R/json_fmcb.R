#' json_fmcb: Generates a json file for the External Food Cue Responsiveness Scale
#'
#' This function generates a json file for the scored External Food Cue Responsiveness Scale and raw participant responses. This function provides accurate json files ONLY if data is processed using score_fmcb function in dataprepr and is only accurate for data collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the External Food Cue Responsiveness Scale
#'
#'
#' @export

json_fmcb <- function() {
  
  fmcb_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'External Food Cue Responsiveness Scale.',
      Reference = 'Masterson TD, Gilbert-Diamond D, Lansigan RK, Kim SJ, Schiffelbein JE, Emond JA. Measurement of external food cue responsiveness in preschool-age children: Preliminary evidence for the use of the external food cue responsiveness scale. Appetite. 2019;139:119-126. doi:10.1016/j.appet.2019.04.024', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/31047939/'),
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
    fmcb1 = list( Description = 'I give snacks or drinks to distract or keep my child quiet when my child is acting out (throwing a tantrum, whining, etc).',
                 Levels = list ('0' = 'Never',
                                '1' = 'Rarely',
                                '2' = 'Sometimes',
                                '3' = 'Often',
                                '4' = 'Always')),
    fmcb2 = list( Description = 'I offer my child his/her favorite foods as a reward for good behavior.',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb3 = list( Description = 'I withhold sweets/desserts from my child in response to bad behavior.',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb4 = list( Description = 'I give snacks or drinks as a way to distract and keep my child quiet when my child is sad or upset.',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb5 = list( Description = 'I offer food to get my child to do certain things I want him/her to do.',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb6 = list( Description = 'I give snacks or drinks to distract or keep my child busy when I am trying to get something done at home (example: on the phone, cleaning the house, preparing dinner, getting dressed, etc).',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb7 = list( Description = 'I offer my child a \'treat\' or \'dessert\' for eating everything on his/her plate.',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb8 = list( Description = 'I offer my child a \'treat\' or \'dessert\' to get my child to eat his/her vegetables.',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb9 = list( Description = 'I give snacks or drinks as a way to distract and keep my child quiet when we are in public settings (examples: church, shopping, doctor\'s office, theater, etc).',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb9 = list( Description = 'I give snacks or drinks to distract or keep my child quiet when I am feeling frustrated, stressed, or tired.',
                  Levels = list ('0' = 'Never',
                                 '1' = 'Rarely',
                                 '2' = 'Sometimes',
                                 '3' = 'Often',
                                 '4' = 'Always')),
    fmcb_score = list( Description = 'Feeding to manage child behavior total score.', 
                     Derivative = TRUE),
    fmcb_fts = list( Description = 'Food to soothe score.', 
                       Derivative = TRUE),
    fmcb_far = list( Description = 'Food as reward score.', 
                       Derivative = TRUE))
  
  # convert formatting to JSON
  fmcb_json <- RJSONIO::toJSON(fmcb_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(fmcb_json, asText = TRUE))){
    print('fmcb JSON file may be invalid')
  }
  
  return(fmcb_json)
  
}