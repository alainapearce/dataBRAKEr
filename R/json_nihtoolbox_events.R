#' json_nihtoolbox_events: Generates a json file for the NIH toolbox events data (data in rawdata/beh)
#'
#' This function generates a json file for cleaned NIH toolbox assessment events (response) data
#'
#' @return A string with data stored in JSON format containing meta-data for NIH toolbox assessment events (response) data
#'
#'
#' @export

json_nihtoolbox_events <- function() {

  toolbox_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'NIH Toolbox Assessment data. Children were administered 3 tasks from the NIH toolbox: Flanker Inhibitory Control and Attention Test, Dimensional Change Card Sort Test, List Sorting Working Memory Test.',
      Reference = '',
      TermURL = 'https://nihtoolbox.zendesk.com/hc/en-us'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    test = list( Description = 'test done',
                 Levels = list( 'flanker' = 'Flanker Inhibitory Control and Attention Test',
                                'listsort' = 'List Sorting Working Memory Test',
                                'cardsort' = 'Dimensional Change Card Sort Test')),
    test_ages = list( Description = 'Age version for the test',
                      Levels = list( 'Age 7+ v2.1' = 'ages 7 years and older version 2.1',
                                     '8-11 v2.1' = 'ages 8-11 years old version 2.1')),
    pin = list( Description = 'unique ID assigned to participant on iPad at administration'),
    itm_ordr = list( Description = 'The position of an item within an item block (InstSctn). This may or may not match the order of administration based on how a study was set-up.'),
    item_id = list( Description = 'ID of the item administered'),
    response = list( Description = 'Position within response options of participant selection. For example, the response option listed first is coded as 1. If a participant skipped an item, the Rspnse field will be blank'),
    score = list( Description = 'Score of the participant selection. Please see data dictionary'),
    theta = list( Description = 'Instrument score'),
    tscore = list( Description = 'Transformed score based on a formula (10*Theta +50 for PROMIS and NeuroQOL instruments, different transformation for NIH Toolbox Emotional instruments)'),
    se = list( Description = 'Standard Error for administered instrument'),
    data_type = list( Description = 'Response type (e.g. integer, string)'),
    position = list( Description = 'Administration order of item'),
    response_time = list( Description = 'timespan between item presented and the response selected (please refer to instruments scores documentation)'),
    date_created = list( Description = 'time stamp when an item response was submitted'),
    app_version = list( Description = 'App version'),
    ipad_version = list( Description = 'Ipad version'),
    firmware_version = list( Description = 'Firmware Version')
  )


  # convert formatting to JSON
  toolbox_json <- RJSONIO::toJSON(toolbox_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(toolbox_json, asText = TRUE))){
    print('NIH toolbox beh JSON file may be invalid')
  }

  return(toolbox_json)

}
