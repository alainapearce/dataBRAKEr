#' json_foodchoice: Generates a json file for the Food Choice Events file
#'
#' This function generates a json file for the scored Binge Eating Scale and raw participant responses. This function provides accurate json files ONLY if data is processed using score_bes function in dataprepr and is only accurate for data collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the Binge Eating Scale
#'
#'
#' @export

json_foodchoice <- function() {
  
  foodchoice_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Food Choice Task has participants choose between foods. Food image pairings are based on participants own health and taste choices during the Food choice Task.',
      Reference = 'Pearce AL, Adise S, Roberts NJ, White C, Geier CF, Keller KL. Individual differences in the influence of taste and health impact successful dietary self-control: A mouse tracking food choice study in children. Physiology & Behavior. 2020;223:112990. doi:10.1016/j.physbeh.2020.112990', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/32505786/',
      DatasetType = 'raw'),
    sub = list( Description = 'participant number'),
    date = list( Description = 'visit date', Units = 'YYY-MM-DD'),
    exp_name = list( Description = 'name of PsychoPy experiment file'),
    cond = list( Description = 'task condition',
                 Levels = list (healthy = 'both images rated as healthy but differ in taste',
                                tasty = 'both images rated as tasty but differ in health',
                                nottasty = 'both images rated as not tasty but differ in health',
                                unhealthy = 'both images rated as not healthy but differ in taste',
                                mix_conflict = 'one image rated as healthy/not tasty and the other as unhealthy/tasty',
                                mix_noconflict = 'one image rated as unhealthy/not tasty and the other as healthy/tasty')),
    img1 = list( Description = 'left image file'),
    img2 = list( Description = 'right image file'),
    img1_health = list( Description = 'participant health choice for left image from the Food choice Task'),
    img2_health = list( Description = 'participant health choice for right image from the Food choice Task'),
    img1_taste = list( Description = 'participant taste choice for left image from the Food choice Task'),
    img2_taste = list( Description = 'participant taste choice for right image from the Food choice Task'),
    img1_want = list( Description = 'participant wanting choice for left image from the Food choice Task'),
    img2_want = list( Description = 'participant wanting choice for right image from the Food choice Task'),
    taste_cut = list( Description = 'taste cutoff point to make tasty/not tasty designation - start at 1.5 and adjust if needed to get trial pairings'),
    health_cut = list( Description = 'health cutoff point to make tasty/not tasty designation - start at 1.5 and adjust if needed to get trial pairings'),
    trial = list( Description = 'trial number'),
    healthyeating_time = list( Description = 'time reading the healthy eating infographic'),
    left_img_onset = list( Description = 'onset time for left image', Units = 'sec'),
    right_img_onset = list( Description = 'onset time for right image', Units = 'sec'),
    choice_prompt_onset = list( Description = 'onset time for choice prompt', Units = 'sec'),
    key_choice_onset = list( Description = 'onset time for key choice', Units = 'sec'),
    choice_fix_onset = list( Description = 'onset time for choice fixation', Units = 'sec'),
    left_roi_onset = list( Description = 'onset time for eye-tracking left image region of interest', Units = 'sec'),
    right_roi_onset = list( Description = 'onset time for eye-tracking right image region of interest', Units = 'sec'),
    et_record_onset = list( Description = 'onset time for eye-tracking right image region of interest', Units = 'sec'),
    choice = list( Description = 'participant choice',
                   Levels = list ( '1' = 'left image',
                                   '2' = 'right image',
                                   'None' = 'missing response')),
    choice_rt = list( Description = 'participant choice reaction time', Units = 'sec'),
    choice_health = list( Description = 'indicates if participant chose the food item that they rated as healthier during the Food choice Game',
                          Levels = list ( '0' = 'chose food rated as less healthy',
                                          '1' = 'chose food rated as healthier',
                                          'NA' = 'missing response')),
    choice_taste = list( Description = 'indicates if participant chose the food item that they rated as tastier during the Food choice Game',
                         Levels = list ( '0' = 'chose food rated as less tasty',
                                         '1' = 'chose food rated as more tasty',
                                         'NA' = 'missing response')),
    choice_want = list( Description = 'indicates if participant chose the food item that they rated as wanting to eat more the Food choice Game',
                        Levels = list ( '0' = 'chose food rated as wanting to eat more',
                                        '1' = 'chose food rated as wanting to eat less',
                                        'NA' = 'missing response')),
    left_looks = list( Description = 'number of looks recorded for left image region of interest'),
    left_look_onsets = list( Description = 'onset times for each recorded look for the left image region of interest', Units = 'sec'),
    left_look_offsets = list( Description = 'offset times for each recorded look for the left image region of interest', Units = 'sec'),
    right_looks = list( Description = 'number of looks recorded for right image region of interest'),
    right_look_onsets = list( Description = 'onset times for each recorded look for the right image region of interest', Units = 'sec'),
    right_look_offsets = list( Description = 'offset times for each recorded look for the right image region of interest', Units = 'sec'),
    trial_fixprompt_onset = list( Description = 'onset time for fixation', Units = 'sec'),
    tfix_onset = list( Description = 'onset time for fixation', Units = 'sec'),
    trial_fixprompt_offset = list( Description = 'offset time for fixation', Units = 'sec'),
    tfix_offset = list( Description = 'offset time for fixation', Units = 'sec'),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    frame_rate = list( Description = 'frame rate', Units = 'frames per sec'))
  
  # convert formatting to JSON
  foodchoice_json <- RJSONIO::toJSON(foodchoice_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodchoice_json, asText = TRUE))){
    print('foodchoice JSON file may be invalid')
  }
  
  return(foodchoice_json)
  
}