#' json_pit_summary: Generates a json file for the Pavlovian Instrumental Transfer task summary data
#'
#' This function generates a json file for cleaned Pavlovian Instrumental Transfer task summary data
#'
#' @return A string with data stored in JSON format containing meta-data for the Pavlovian Instrumental Transfer task summary data
#'
#'
#' @export

json_pit_summary <- function() {

  pit_summary_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Pavlovian Instrumental Transfer Task adapted for kids with toy vs food stimuli',
      Reference = 'Adapated from: Alarcón, Daniel E., and Charlotte Bonardi. "Under the influence of the environment: Children’s responding invigorated and biased by predictive cues." Journal of Experimental Child Psychology 191 (2020): 104741. https://doi.org/10.1016/j.jecp.2019.104741',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/31809989/',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('followup' = '1 year follow-up')),
    cond = list( Description = 'Condition',
                Levels = list ('1' = 'CS Child 1: Food, right/j key; CS Child 2: Toy, left/f key; CS Child 3: neutral',
                               '2' = 'CS Child 1: neutral; CS Child 2: Food, right/j key; CS Child 3: Toy, left/f key',
                               '3' = 'CS Child 1: Toy, left/f key; CS Child 2: neutral; CS Child 3: Food, right/j key',
                               '4' = 'CS Child 1: Food, left/f key; CS Child 2: Toy, right/j key; CS Child 3: neutral',
                               '5' = 'CS Child 1: neutral; CS Child 2: Food, left/k key; CS Child 3: Toy, right/j key',
                               '6' = 'CS Child 1: Toy, right/j key; CS Child 2: neutral; CS Child 3: Food, left/k key')),
    inst_ntrials = list( Description = 'number of instramental conditioning trials'),
    inst_ntrials_food30 = list( Description = 'number of instramental conditioning trials to earn 30 food outcomes'),
    inst_ntrials_toy30 = list( Description = 'number of instramental conditioning trials to earn 30 toy outcomes'),
    inst_foodkey_nper_foodout = list( Description = 'number of key presses associated with food to earn food outcome'),
    inst_toykey_nper_foodout = list( Description = 'number of key presses associated with toy to per food outcome'),
    inst_foodkey_nper_toyout = list( Description = 'number of key presses associated with food to per toy outcome'),
    inst_toykey_nper_toyout = list( Description = 'number of key presses associated with toy to earn toy outcome'),
    inst_mean_trial_dur = list( Description = 'Mean trial duration for instramental conditioning (i.e., time to earn an outcome)',
                  Units = 'ms'),
    inst_foodkey_mean_rt = list( Description = 'Average reaction time for key associated with food',
                          Unit = "ms"),
    inst_toykey_mean_rt = list( Description = 'Average reaction time for key associated with toy',
                                 Unit = "ms"),
    inst_mean_food_rpm = list( Description = 'Average responses per minute for key associated with food',
                                 Unit = "responses per minute"),
    inst_mean_toy_rpm = list( Description = 'Average responses per minute for key associated with toy',
                                  Unit = "responses per minute"),
    resp_type = list( Description = 'Response type',
                            Levels = list ('food' = 'CS associated with food',
                                           'toy' = 'CS associated with toy',
                                           'control' = 'Neutral CS')),
    pit_pre_mean_rpm = list( Description = 'Pre CS: Average responses per minute',
                               Unit = "responses per minute"),
    pit_cs_mean_rpm = list( Description = 'CS: Average responses per minute',
                             Unit = "responses per minute"),
    pit_iti_mean_rpm = list( Description = 'Inter-trial-interval: Average responses per minute',
                             Unit = "responses per minute"),
    pit_pre_food_mean_rpm = list( Description = 'Pre CS: Average responses per minute for trials with CS associated with food',
                             Unit = "responses per minute"),
    pit_cs_food_mean_rpm = list( Description = 'CS: Average responses per minute for trials with CS associated with food',
                            Unit = "responses per minute"),
    pit_iti_mfood_ean_rpm = list( Description = 'Inter-trial-interval: Average responses per minute for trials with CS associated with food',
                             Unit = "responses per minute"),
    pit_pre_toy_mean_rpm = list( Description = 'Pre CS: Average responses per minute for trials with CS associated with toy',
                                  Unit = "responses per minute"),
    pit_cs_toy_mean_rpm = list( Description = 'CS: Average responses per minute for trials with CS associated with toy',
                                 Unit = "responses per minute"),
    pit_iti_mtoy_ean_rpm = list( Description = 'Inter-trial-interval: Average responses per minute for trials with CS associated with toy',
                                  Unit = "responses per minute"),
    pit_score  = list( Description = 'PIT score: pit_cs_mean_rpm - pit_pre_mean_rpm'),
    pit_food_score  = list( Description = 'PIT score: pit_cs_food_mean_rpm - pit_pre_food_mean_rpm'),
    pit_toy_score  = list( Description = 'PIT score: pit_cs_toy_mean_rpm - pit_pre_toy_mean_rpm'),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    frame_rate = list( Description = 'frame rate',
                       Units = 'frames per sec'))


  # convert formatting to JSON
  pit_summary_json <- RJSONIO::toJSON(pit_summary_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(pit_summary_json, asText = TRUE))){
    print('PIT summary data JSON file may be invalid')
  }

  return(pit_summary_json)

}
