#' json_v2_info: Generates a json file for contextual information about the visit 2 protocol tasks
#'
#' This function generates a json file for contextual information about the visit 2 protocol tasks
#'
#' @return A json file documenting the Study BRAKE's contextual information about the visit 2 protocol tasks
#'
#'
#' @export

json_v2_info <- function() {
  
  v2_info_list <- list(
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
    v2_notes = list( Description = 'General notes about visit protcol 2'),
    v2_end_notes = list( Description = 'Final/end of visit notes about visit protcol 2'),
    bodpod_note = list( Description = 'Notes about the BodPod'),
    pre_pizza_fullness_time = list( Description = 'Pre-pizza meal fullness rating time',
                                     Unit = "hh:mm:ss"),
    pre_pizza_fullness_notes = list( Description = 'Notes about pre-pizza meal fullness rating'),
    pizza_meal_book = list( Description = 'Book the child selected to listen to during the test meal'),
    pizza_meal_start_time = list( Description = 'Meal start time',
                                 Unit = "hh:mm"),
    pizza_meal_end_time = list( Description = 'Meal end time',
                               Unit = "hh:mm"),
    pizza_meal_duration = list( Description = 'Meal duration. Derived in redcap from pizza_meal_end_time and pizza_meal_duration',
                               Derivative = TRUE),
    pizza_meal_notes = list( Description = 'Notes about meal protocol'),
    post_pizza_fullness_time = list( Description = 'Post-pizza meal fullness rating time',
                                    Unit = "hh:mm:ss"),
    post_pizza_fullness_notes = list( Description = 'Notes about psot-pizza meal fullness rating'),
    
    
    wasi_block_complete = list( Description = 'Did child complete the Block Design component of the WASI?',
                               Levels = list ('0' = 'No',
                                              '1' = 'Yes')),
    wasi_vocab_complete = list( Description = 'Did child complete the Vocab component of the WASI?',
                            Levels = list ('0' = 'No',
                                           '1' = 'Yes')),
    wasi_matrix_complete = list( Description = 'Did child complete the Matrix component of the WASI??',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    
    wasi_note = list( Description = 'Notes about the WASI'),
    dkefs_trails_complete = list( Description = 'Did child complete the Trails component of the DKEFS?',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    dkefs_verbal_complete = list( Description = 'Did child complete the Verbal Fluency component of the DKEFS?',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    dkefs_design_complete = list( Description = 'Did child complete the Design Fluency component of the DKEFS?',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    dkefs_note = list( Description = 'Notes about the DKEFs'),
    spacegame_snack = list( Description = 'Candy prize chosen for the Space Game',
                                  Levels = list ('0' = 'Skittles',
                                                 '1' = 'M & M\'s')),
    spacegame_notes = list( Description = 'Notes about the Space Game'),
    toolbox_flanker_complete = list( Description = 'Did child complete the Flanker component of the NIH Toolbox?',
                                  Levels = list ('0' = 'No',
                                                 '1' = 'Yes')),
    toolbox_flanker_notes = list( Description = 'Notes about the Flanker component of the NIH Toolbox'),
    toolbox_cardsort_complete = list( Description = 'Did child complete the Dimmensional Card Sort component of the NIH Toolbox?',
                                     Levels = list ('0' = 'No',
                                                    '1' = 'Yes')),
    toolbox_cardsort_notes = list( Description = 'Notes about the Dimmensional Card Sort component of the NIH Toolbox'))
  
  # convert formatting to JSON
  v2_info_json <- RJSONIO::toJSON(v2_info_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(v2_info_json, asText = TRUE))){
    print('fNIRS task info JSON file may be invalid')
  }
  
  return(v2_info_json)
  
}