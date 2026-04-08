#' json_foodrating_events: Generates a json file for the Food Rating Events file
#'
#' This function generates a json file for the individual level Food Rating events files in rawdata
#'
#' @return A json file documenting the individual level Food Rating events files in rawdata
#'
#'
#' @export

json_foodrating_events <- function() {
  
  foodrating_events_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Food Rating Task has participants view and rate food images. The same food images are rated for: health, taste, wanting. All ratings are completed for one condition before moving to the next rating condition with order counterballanced.',
      Reference = 'Pearce AL, Adise S, Roberts NJ, White C, Geier CF, Keller KL. Individual differences in the influence of taste and health impact successful dietary self-control: A mouse tracking food choice study in children. Physiology & Behavior. 2020;223:112990. doi:10.1016/j.physbeh.2020.112990', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/32505786/',
      DatasetType = 'raw'),
    onset = list( Description = 'trial onset times if fNIRS was collected during task'),
    duration = list( Description = 'trial duration'),
    sub = list( Description = 'participant number'),
    date = list( Description = 'visit date', 
                 Units = 'YYY-MM-DD'),
    exp_name = list( Description = 'name of PsychoPy experiment file'),
    cond = list( Description = 'task condition',
                 Levels = list ('health' = 'rate health of each food',
                                'taste' = 'rate taste of each food',
                                'want' = 'rate how much you want to eat each food')),
    exp_cond_num = list( Description = 'condition order entered into task start-up information. Corresponds to randomization table'),
    stim_file = list( Description = 'stimulus file'),
    image_ed = list( Description = 'energy density of food image'),
    fix = list( Description = 'fixation duration', Units = 'sec'),
    cond_trial = list( Description = 'trial number in condition block'),
    trial_index = list( Description = 'index number from trial setup files'),
    food_onset = list( Description = 'onset time for food stimuli', 
                       Units = 'sec'),
    slider_onset = list( Description = 'onset time for rating slider', 
                         Units = 'sec'),
    prompt_onset = list( Description = 'onset time for prompt', 
                         Units = 'sec'),
    rating = list( Description = 'participant rating',
                   Levels = list ( 'health' = list ( '0' = 'Very Unhealthy',
                                                   '1' = 'Unhealthy',
                                                   '2' = 'Healthy',
                                                   '3' = 'Very Healthy'),
                                   'taste' = list ( '0' = 'Very Bad',
                                                  '1' = 'Bad',
                                                  '2' = 'Good',
                                                  '3' = 'Very Good'),
                                   'want' = list ( '0' = 'Not At All',
                                                 '1' = 'A Little',
                                                 '2' = 'A Lot',
                                                 '3' = 'Very Much'))),
    jitter_fix_onset = list( Description = 'onset time for jittered fixation', 
                             Units = 'sec'),
    jitter_prompt_onset = list( Description = 'onset time for jittered fixation prompt', 
                                Units = 'sec'),
    jitter_fix_offset = list( Description = 'offset time for jittered fixation', 
                              Units = 'sec'),
    jitter_prompt_offset = list( Description = 'offset time for jittered fixationprompt', 
                                 Units = 'sec'),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    frame_rate = list( Description = 'frame rate', 
                       Units = 'frames per sec'))
  
  # convert formatting to JSON
  foodrating_events_json <- RJSONIO::toJSON(foodrating_events_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodrating_events_json, asText = TRUE))){
    print('foodrating events JSON file may be invalid')
  }
  
  return(foodrating_events_json)
  
}