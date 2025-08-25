#' json_shapegame_info: Generates a json file for contextual information about the shapegame
#'
#' This function generates a json file for contextual information about the shapegame
#'
#' @return A json file documenting the Study BRAKE's contextual information about the shapegame
#'
#'
#' @export

json_shapegame_info <- function() {
  
  shapegame_info_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1 (baseline)',
                                          '3'	= 'Child visit protocol 3 (follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    pre_shapegame_hungry = list( Description = 'Was the child hungry prior to the Shape Game? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes'),
                                 Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    pre_shapegame_snack = list( Description = 'Did the child eat the snack prior to the Shape Game?',
                                Levels = list ('0' = 'No',
                                               '1' = 'Yes')),
    pre_shapegame_snack_notes = list( Description = 'Notes about the pre-Shape Game snack'),
    pre_shapegame_postsnack_hungry = list( Description = 'Was the child hungry after the first snack prior to the Shape Game? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                                           Levels = list ('0' = 'No',
                                                          '1' = 'Yes'),
                                           Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.'),
    pre_shapegame_snack2 = list( Description = 'Did the child eat the second snack prior to the Shape Game?',
                                 Levels = list ('0' = 'No',
                                                '1' = 'Yes')),
    pre_shapegame_snack2_notes = list( Description = 'Notes about the second pre-Shape Game snack'),
    shapegame_complete = list( Description = 'Did child complete the Shape Game?',
                               Levels = list ('0' = 'No',
                                              '1' = 'Yes')),
    shapegame_candy = list( Description = 'Prize candy child choose for the Shape Game',
                            Levels = list ('0' = 'Skittles',
                                           '1' = 'M & M\'s')),
    shapegame_prize_bags = list( Description = 'Number for prize bags won'),
    shapegame_eyetrack_good = list( Description = 'Did eye-tracking work without error during the Shape Game? Note: this does not reflect eye-tracking data quality, just the ability to collect the data',
                                    Levels = list ('0' = 'No',
                                                   '1' = 'Yes')),
    shapegame_notes = list( Description = 'Notes about the Shape Game task'))
  
  # convert formatting to JSON
  shapegame_info_json <- RJSONIO::toJSON(shapegame_info_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(shapegame_info_json, asText = TRUE))){
    print('Shpae Game info JSON file may be invalid')
  }
  
  return(shapegame_info_json)
  
}