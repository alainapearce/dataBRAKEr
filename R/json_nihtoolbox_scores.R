#' json_nihtoolbox_scores: Generates a json file for the NIH toolbox behavioral response data (data in rawdata/beh)
#'
#' This function generates a json file for cleaned NIH toolbox assessment (response) data
#'
#' @return A string with data stored in JSON format containing meta-data for NIH toolbox assessment (response) data
#'
#'
#' @export

json_nihtoolbox_scores <- function() {

  toolbox_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'NIH Toolbox Assessment data. Children were administered 3 tasks from the NIH toolbox: Flanker Inhibitory Control and Attention Test, Dimensional Change Card Sort Test, List Sorting Working Memory Test.',
      Reference = '',
      TermURL = 'https://nihtoolbox.zendesk.com/hc/en-us'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    test = list( Description = 'test done',
                 Levels = list( 'flanker' = 'Flanker Inhibitory Control and Attention Test',
                         'listsort' = 'List Sorting Working Memory Test',
                         'cardsort' = 'Dimensional Change Card Sort Test')),
    test_ages = list( Description = 'Age version for the test',
                      Levels = list( 'Age 7+ v2.1' = 'ages 7 years and older version 2.1',
                                     '8-11 v2.1' = 'ages 8-11 years old version 2.1')),
    pin = list( Description = 'unique ID assigned to participant on iPad at administration'),
    rawscore = list( Description = 'raw score'),
    theta = list( Description = 'Instrument theta score'),
    tscore = list( Description = 'Transformed score based on a formula (10*Theta +50 for PROMIS and NeuroQOL instruments, different transformation for NIH Toolbox Emotional instruments)'),
    se = list( Description = 'Standard Error for administered instrument'),
    itmcnt = list( Description = 'Number of trials'),
    computed_score = list( Description = 'A computed score is used on certain tests where an "interim" calculation is required to go from the raw score to the normative score'),
    uncorrected_ss = list( Description = 'This is an additional available score, which also uses a standard score metric (normative mean = 100, SD = 15). It compares the performance of the test-taker to those in the entire NIH Toolbox nationally representative normative sample, regardless of age or any other variable. The Uncorrected Standard Score provides a glimpse of the given participant\'s overall performance or ability when compared with the general U.S. population. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    age_corrected_ss = list( Description = 'This score compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample at the same age, where a score of 100 indicates performance that was at the national average for the test-taking participant\'s age. Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    national_percentile_age_adjusted = list( Description = 'Age adjusted national percentile'),
    fully_corrected_t_score = list( Description = 'This score (which has a mean of 50 and an SD of 10, unlike age-corrected performance normative scores) compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample, while adjusting for key demographic variables collected during the NIH Toolbox national norming study.  Please see NIH Toolbox Scoring and Interpretation Guide for more helpful information (section: Normative scores provided for performance measures).'),
    app_version = list( Description = 'App version'),
    ipad_version = list( Description = 'Ipad version'),
    firmware_version = list( Description = 'Firmware Version')
  )


  # convert formatting to JSON
  toolbox_json <- RJSONIO::toJSON(toolbox_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(toolbox_json, asText = TRUE))){
    print('NIH toolbox scores JSON file may be invalid')
  }

  return(toolbox_json)

}
