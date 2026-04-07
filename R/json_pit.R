#' json_pit: Generates a json file for the Pavlovian Instrumental Transfer task
#'
#' This function generates a json file for cleaned Pavlovian Instrumental Transfer task
#'
#' @return A string with data stored in JSON format containing meta-data for the Pavlovian Instrumental Transfer task
#'
#'
#' @export

json_pit <- function() {

  pit_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Pavlovian Instrumental Transfer Task adapted for kids with toy vs food stimuli',
      Reference = 'Adapated from: Alarcón, Daniel E., and Charlotte Bonardi. "Under the influence of the environment: Children’s responding invigorated and biased by predictive cues." Journal of Experimental Child Psychology 191 (2020): 104741. https://doi.org/10.1016/j.jecp.2019.104741',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/31809989/'),
    sub = list( Description = 'Participant ID'),
    ses = list( Description = 'BIDS session ID indicating when data was collected',
                             Levels = list ('ses-1' = 'session 1 / baseline',
                                            'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    cond = list( Description = 'Condition',
                Levels = list ('1' = 'CS Child 1: Food, right/j key; CS Child 2: Toy, left/f key; CS Child 3: neutral',
                               '2' = 'CS Child 1: neutral; CS Child 2: Food, right/j key; CS Child 3: Toy, left/f key',
                               '3' = 'CS Child 1: Toy, left/f key; CS Child 2: neutral; CS Child 3: Food, right/j key',
                               '4' = 'CS Child 1: Food, left/f key; CS Child 2: Toy, right/j key; CS Child 3: neutral',
                               '5' = 'CS Child 1: neutral; CS Child 2: Food, left/k key; CS Child 3: Toy, right/j key',
                               '6' = 'CS Child 1: Toy, right/j key; CS Child 2: neutral; CS Child 3: Food, left/k key')),
    cs_img = list( Description = 'Conditioned stimulus image'),
    stim_file_name = list( Description = 'stimulus file'),
    pav_stim_img = list( Description = 'Pavlovian conditioning: Stimuli image file'),
    pav_side = list( Description = 'Pavlovian conditioning:Side of screen stimuli image was presented',
                     Levels = list ('L' = 'left',
                                    'R' = 'right')),
    pav_block_num = list( Description = 'Pavlovian conditioning: block number'),
    pav_trial_num = list( Description = 'Pavlovian conditioning: trial number'),
    pav_fix_onset = list( Description = 'Pavlovian conditioning: block fixation onset',
                  Units = 'sec'),
    pav_trial_onset = list( Description = 'Pavlovian conditioning: trial onset',
                          Unit = "sec"),
    inst_food_img = list( Description = 'Intrumental conditioning: food stimuli image option'),
    inst_toy_img = list( Description = 'Intrumental conditioning: toy stimuli image option'),
    inst_trial_num = list( Description = 'Intrumental conditioning: trial number'),
    inst_trial_onset = list( Description = 'Intrumental conditioning: trial onset',
                            Unit = "sec"),
    inst_outcome_img_onset = list( Description = 'Intrumental conditioning: trial onset',
                             Unit = "sec"),
    inst_left_trial = list( Description = 'Intrumental conditioning: left key presses between image presentations (outcome food/toy image presented on an average of every 3 presses or VR 3 scale)'),
    inst_right_trial = list( Description = 'Intrumental conditioning: right key presses between image presentations (outcome food/toy image presented on an average of every 3 presses or VR 3 scale)'),
    inst_food_earned = list( Description = 'Intrumental conditioning: number of food outcome stimuli earned'),
    inst_toy_earned = list( Description = 'Intrumental conditioning: number of toy outcome stimuli earned'),
    pit_trial_num = list( Description = 'PIT test: trial number'),
    pit_fix_onset = list( Description = 'PIT test: inital fixation onset'),
    pit_pre_right_n = list( Description = 'PIT test: number of right key presses prior to presentation of CS'),
    pit_pre_left_n = list( Description = 'PIT test: number of left key presses prior to presentation of CS'),
    pit_pre_rt = list( Description = 'PIT test: average reaction time of responses prior to presentation of CS',
                      Units = 'sec'),
    pit_cs_onset = list( Description = 'PIT test: onset of the CS image'),
    pit_cs_right_n = list( Description = 'PIT test: number of right key presses during presentation of CS'),
    pit_cs_left_n = list( Description = 'PIT test: number of left key presses during presentation of CS'),
    pit_cs_rt = list( Description = 'PIT test: average reaction time of responses during presentation of CS',
                      Units = 'sec'),
    pit_iti_onset = list( Description = 'PIT test: onset of the inter-trial-interval fixation'),
    pit_iti_right_n = list( Description = 'PIT test: number of right key presses during the inter-trial-interval'),
    pit_iti_left_n = list( Description = 'PIT test: number of left key presses during the inter-trial-interval'),
    pit_iti_rt = list( Description = 'PIT test: average reaction time of responses during the inter-trial-interval',
                       Units = 'sec'),
    rectest_onset = list( Description = 'Recognition test: CS image onset',
                          Units = 'sec'),
    rectest_cs_img = list( Description = 'Recognition test: CS image'),
    rectest_left_img = list( Description = 'Recognition test: Left side image'),
    rectest_right_img = list( Description = 'Recognition test: Right side image'),
    rectest_rt = list( Description = 'Recognition test: reaction time',
                       Units = 'sec'),
    rectest_key = list( Description = 'Recognition test: key response'),
    rectest_correct = list( Description = 'Recognition test: correct key response',
                            Levels = list ('0' = 'incorrect',
                                           '1' = 'correct')),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    frame_rate = list( Description = 'frame rate',
                       Units = 'frames per sec'))


  # convert formatting to JSON
  pit_json <- RJSONIO::toJSON(pit_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(pit_json, asText = TRUE))){
    print('PIT events JSON file may be invalid')
  }

  return(pit_json)

}
