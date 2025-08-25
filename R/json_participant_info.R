#' json_participant_info: Generates a json file for general participant info
#'
#' This function generates a json file for general participant information and protocol information pertaining to rurality status and task/study status for Study BRAKE.
#'
#' @return A json file documenting the Study BRAKE participant information in the participant_protocol dataset
#'
#'
#' @export

json_participant_info <- function() {
  
  par_info_list <- list(
    participant_id = list( Description = 'participant id number'),
    sub_status = list( Description = 'protocol status for participant',
                       Levels = list ('0' = 'Complete',
                                      '1' = 'Withdrew at Visit 2',
                                      '2' = 'No Show at Visit 2',
                                      '3' = 'Lost to Follow-Up/Visit 3')),
    school_nces_code = list( Description = 'School rurality code using the National Center for Education Statistics',
                             TermURL = 'https://nces.ed.gov/ccd/schoolmap/'),
    school_locale = list( Description = 'Locale classification from the National Center for Education Statistics code',
                          Levels = list ('rural remote' = 'school_nces_code = 43',
                                         'rural distant' = 'school_nces_code = 42',
                                         'rural fringe' = 'school_nces_code = 41',
                                         'town remote' = 'school_nces_code = 33',
                                         'town distant' = 'school_nces_code = 32',
                                         'town fringe' = 'school_nces_code = 31',
                                         'suburb small' = 'school_nces_code = 23',
                                         'suburb midsize' = 'school_nces_code = 22',
                                         'suburb large' = 'school_nces_code = 21',
                                         'city small' = 'school_nces_code = 13',
                                         'city midsize' = 'school_nces_code = 12',
                                         'city large' = 'school_nces_code = 11'),
                          TermURL = 'https://nces.ed.gov/ccd/schoolmap/'),
    school_rural = list( Description = 'Locale classification from the National Center for Education Statistics code',
                         Levels = list ('metro' = 'Metropolitan area (school_nces_code <= 30)',
                                        'rural' = 'Rural area (school_nces_code > 30)'),
                         TermURL = 'https://nces.ed.gov/ccd/schoolmap/',
                         Derivative = TRUE),
    home_nces_code = list( Description = 'School rurality code using the National Center for Education Statistics',
                           TermURL = 'https://nces.ed.gov/ccd/schoolmap/'),
    home_locale = list( Description = 'Locale classification from the National Center for Education Statistics code',
                        Levels = list ('rural remote' = 'school_nces_code = 43',
                                       'rural distant' = 'school_nces_code = 42',
                                       'rural fringe' = 'school_nces_code = 41',
                                       'town remote' = 'school_nces_code = 33',
                                       'town distant' = 'school_nces_code = 32',
                                       'town fringe' = 'school_nces_code = 31',
                                       'suburb small' = 'school_nces_code = 23',
                                       'suburb midsize' = 'school_nces_code = 22',
                                       'suburb large' = 'school_nces_code = 21',
                                       'city small' = 'school_nces_code = 13',
                                       'city midsize' = 'school_nces_code = 12',
                                       'city large' = 'school_nces_code = 11'),
                        TermURL = 'https://nces.ed.gov/ccd/schoolmap/',
                        Derivative = TRUE),
    home_rural = list( Description = 'Locale classification from the National Center for Education Statistics code',
                       Levels = list ('metro' = 'Metropolitan area (school_nces_code <= 30)',
                                      'rural' = 'Rural area (school_nces_code > 30)'),
                       TermURL = 'https://nces.ed.gov/ccd/schoolmap/',
                       Derivative = TRUE),
    shapegame_cond = list( Description = 'Assigned condition for the Shape Game to designate the color with the \'bonus\' or high reward)',
                           Levels = list ('1' = 'Orange indicates high reward/bonus',
                                          '2' = 'Blue indicates high reward/bonus')),
    foodrate_cond = list( Description = 'Assigned order for rating the taste, health, and wanting for foods', 
                          Levels = list ('1' = 'Order: health, taste, want',
                                         '2' = 'Order: want, health, taste',
                                         '3' = 'Order: taste, want, health')), 
    friendsgame_cond = list( Description = 'Assigned side toy/food keys and which friend was paried with which stimulus', 
                             Levels = list ('1' = 'Keys: food = right/j, toy = left/f; Friend (CS): 1 = food, 2 = toy, 3 = neutral',
                                            '2' = 'Keys: food = right/j, toy = left/f; Friend (CS): 1 = neutral, 2 = food, 3 = toy',
                                            '3' = 'Keys: food = right/j, toy = left/f; Friend (CS): 1 = toy, 2 = neutral, 3 = food',
                                            '4' = 'Keys: toy = right/j, food = left/f; Friend (CS): 1 = food, 2 = toy, 3 = neutral',
                                            '5' = 'Keys: toy = right/j, food = left/f; Friend (CS): 1 = neutral, 2 = food, 3 = toy',
                                            '6' = 'Keys: toy = right/j, food = left/f; Friend (CS): 1 = toy, 2 = neutral, 3 = food')),
    ed_order_premeal = list( Description = 'Assigned order for the taste test foods', 
                             Levels = list ('1' = 'meal foods, low energy dense foods, meal foods, high energy dense foods, meal foods',
                                            '2' = 'meal foods, high energy dense foods, meal foods, low energy dense foods, meal foods')),
    ed_order_postmeal = list( Description = 'Assigned order for the taste test foods', 
                              Levels = list ('1' = 'meal foods, low energy dense foods, meal foods, high energy dense foods, meal foods',
                                             '2' = 'meal foods, high energy dense foods, meal foods, low energy dense foods, meal foods')),
    video_consent_save = list( Description = 'Consent choice to save video data on Databrary',  
                               Levels = list ('1' = 'yes',
                                              '0' = 'no')),
    video_consent_public = list( Description = 'Consent choice to save video data on Databrary AND make publically available',  
                                 Levels = list ('1' = 'yes',
                                                '0' = 'no')),
    v2_sample_collected = list( Description = 'Urine sample collected at baseline',
                                Levels = list ('1' = 'yes',
                                               '0' = 'no')),
    v3_sample_collected = list( Description = 'Urine sample collected at baseline',
                                Levels = list ('1' = 'yes',
                                               '0' = 'no')))
  
  # convert formatting to JSON
  par_info_json <- RJSONIO::toJSON(par_info_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(par_info_json, asText = TRUE))){
    print('participant info JSON file may be invalid')
  }
  
  return(par_info_json)
  
}