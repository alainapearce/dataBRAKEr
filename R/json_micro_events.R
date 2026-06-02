#' json_micro_events: Generates a json file for micro_events 
#'
#' This function generates a json file for micro_events 
#'
#' @return A json file documenting the Study BRAKE's micro_events 
#'
#'
#' @export

json_micro_events <- function() {
  
  micro_events_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Meal microstructure coding following the extablished protocol',
      Reference = 'Pearce, A. L., N. V. Neuwald, J. S. Evans, O. Romano, B. J. Rolls, and K. L. Keller. "Child eating behaviors are consistently linked to intake across meals that vary in portion size." Appetite 196 (2024): 107258. https://doi.org/10.1016/j.appet.2024.107258',
      TermURL = 'https://zenodo.org/records/8140896'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    ph1_coder = list( Description = 'Initials of research assistant who completed phase 1 coding'),
    ph2_coder = list( Description = 'Initials of research assistant who completed phase 2 coding'),
    n_coders = list( Description = 'Number of coders - if 2, means it was double coded'),
    observation = list( Description = 'Name of observation recorded'),
    date_time = list( Description = 'date and time'),
    time = list( Description = 'date'),
    time_frames = list( Description = 'time frames'),
    time_hmsf = list( Description = 'time in hours, minutes, seconds, and fraction of seconds'),
    time_hms = list( Description = 'time in hours, minutes, and seconds'),
    time_relative_frames = list( Description = 'time realtive to start in frames'),
    time_relative = list( Description = 'time realtive to start in seconds'),
    end_of_meal_affect = list( Description = 'end of meal affect', 
                               Levels = list ('2' = 'Laughter, clapping, moving hands to show excitement, open mouth smiles, explicit and direct verbal excitement and approval', 
                                              '1' = 'Slight smiles, shaking head yes, verbal suggestion of approval', 
                                              '0' = 'Shows no emotion or reaction to meal start/end. Completely neutral', 
                                              '-1' = 'Frowning, shaking head, looking confused/disgusted, any verbal suggestion of dislike or disapproval', 
                                              '-2' = 'Vigorously shaking head or waving hands to reject food, explicit and direct negative verbal remark')),
    start_of_meal_affect = list( Description = 'start of meal affect', 
                                 Levels = list ('2' = 'Laughter, clapping, moving hands to show excitement, open mouth smiles, explicit and direct verbal excitement and approval', 
                                                '1' = 'Slight smiles, shaking head yes, verbal suggestion of approval', 
                                                '0' = 'Shows no emotion or reaction to meal start/end. Completely neutral', 
                                                '-1' = 'Frowning, shaking head, looking confused/disgusted, any verbal suggestion of dislike or disapproval', 
                                                '-2' = 'Vigorously shaking head or waving hands to reject food, explicit and direct negative verbal remark')),
    duration = list( Description = 'duration in seconds'),
    event_log = list( Description = 'data export type'), 
    behavior = list( Description = 'behavior coded', 
                     Levels = list ('Active Eating Time' = 'active eating time',
                                    'Meal Durtaiton' = 'meal duration',
                                    'Latency to First Bite' = 'bite latency',
                                    'Bite' = 'food bite',
                                    'Sips' = 'water sip')),
    grape = list( Description = 'inidicates when the coded bite included some amount of grape'),
    carrot = list( Description = 'inidicates when the coded bite included some amount of carrot'),
    cknug = list( Description = 'inidicates when the coded bite included some amount of chicken nugget'),
    mac = list( Description = 'inidicates when the coded bite included some amount of macarroni and cheese'),
    ketchup = list( Description = 'inidicates when the coded bite included some amount of ketchup'),
    hand = list( Description = 'inidicates when the coded bite was consumed using the child\'s hands'),
    fork = list( Description = 'inidicates when the coded bite was consumed using the a fork'),
    other = list( Description = 'other modifier for a bite'),
    child_activity = list( Description = 'type of child activity',
                           Levels = list ('Leave Room' = 'child leaves room',
                                          'Leaving Chair' = 'child gets up from chair',
                                          'Leaving Table' = 'child moves away from table')),
    child_activity = list( Description = 'type of child activity',
                           Levels = list ('State start' = 'start of a duration behavior',
                                          'State point' = 'point behavior',
                                          'State stop' = 'end of a duration behavior')),
    comment = list( Description = 'comments'),
    end_of_meal_cut = list( Description = 'end of meal video cut off', 
                            Levels = list ('Yes' = 'meal video cut off before tray leaves the room', 
                                           'No' = 'meal video complete')),
    engagement_with_story = list( Description = 'child engagment with story',
                                  Levels = list ('During Eating' = 'Engagment/distraction by story while eating',
                                                 'No During Eating' = 'No obvious engagment/distraction by story while eating', 
                                                 'No' = 'No obvious engagment/distraction by story')),
    parent_involvement = list( Description = 'Did the parent enter room/become involved in any way with the child meal?'),
    pocket_food = list( Description = 'Did the child pocket food from the meal',
                        Levels = list ('Yes' = 'child put some food in pocket', 
                                       'No' = 'no')),
    food = list( Description = 'food/food combination consumed'),
    food_ed = list( Description = 'energy density of food',
                    Levels = list ('h_ed' = 'high energy dense food', 
                                   'l_ed' = 'low energy dense food')),
    food_sip = list( Description = 'combination of food/food combinations and sips,'),
    food_sip_distract = list( Description = 'combination of food/food combinations,  sips, and distraction behaviors'),
    food_switch = list( Description = 'switch between food/food combinations only', 
                        Levels = list ('0' = 'same/no switch', 
                                       '1' = 'switch')),
    food_ed_switch = list( Description = 'switch between food energy density consumed', 
                           Levels = list ('0' = 'same/no switch', 
                                          '1' = 'switch')),
    food_sip_switch = list( Description = 'switch between food/food combinations and sips of water', 
                            Levels = list ('0' = 'same/no switch', 
                                           '1' = 'switch')),
    food_sip_distract_switch = list( Description = 'switch between food/food combinations, sips of water, and other coded distraction behaviors', 
                                     Levels = list ('0' = 'same/no switch', 
                                                    '1' = 'switch'))
  )
  
  # convert formatting to JSON
  micro_events_json <- RJSONIO::toJSON(micro_events_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(micro_events_json, asText = TRUE))){
    print('micro_events JSON file may be invalid')
  }
  
  return(micro_events_json)
  
}