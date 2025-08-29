#' json_tastetest_events: Generates a json file for the Taste-Test Events file
#'
#' This function generates a json file for the individual level Taste-Test events files in rawdata
#'
#' @return A json file documenting the individual level Taste-Test events files in rawdata
#'
#'
#' @export

json_tastetest_events <- function() {
  
  tastetest_events_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Taste-Test Task has participants rate how much they want to eat a sample of food, consume the food, rate how much they liked the food sample, and then take a sip of water both before and after a meal.',
      DatasetType = 'raw'),
    onset = list( Description = 'trial onset times if fNIRS was collected during task'),
    duration = list( Description = 'trial duration'),
    sub = list( Description = 'participant number'),
    date = list( Description = 'visit date', 
                 Units = 'YYY-MM-DD'),
    exp_name = list( Description = 'name of PsychoPy experiment file'),
    exp_cond_num = list( Description = 'order of experimental conditions',
                         Levels = list ('1' = 'meal, low-ed, meal, high-ed, meal',
                                        '2' = 'meal, high-ed, meal, low-ed, meal')),
    cond = list( Description = 'task meal condition',
                 Levels = list ('postmeal' = 'completd after the meal',
                                'premeal' = 'completed prior to the meal')),
    food_item = list( Description = 'food item presented'),
    trial_cond = list( Description = 'trial conditon',
                       Levels = list ('meal' = 'meal foods',
                                      'low-ed' = 'low-ed non-meal foods',
                                      'high-ed' = 'high-ed non-meal foods')),
    task_component = list( Description = 'stage/component of trial', 
                           Levels = list ('want_rating' = 'rating how much they want to each food sample',
                                          'taste-test' = 'tasting/consuming sample', 
                                          'like_rating' = 'rating how much they liked the food sample',
                                          'sip' = 'water sip')),
    trigger = list( Description = 'task trigger',
                    Levels = list ('1' = 'meal item taste-test', 
                                   '2' = 'low-ed item taste-test', 
                                   '3' = 'high-ed item taste-test', 
                                   '4' = 'want_rating',
                                   '5' = 'like_rating',
                                   '6' = 'sip')),
    trial_index = list( Description = 'trial number'),
    rating = list( Description = 'participant rating',
                   Levels = list ('like_rating' = list ( '0' = 'Very Bad',
                                                  '1' = 'Bad',
                                                  '2' = 'Good',
                                                  '3' = 'Very Good'),
                                   'want_rating' = list ( '0' = 'Not At All',
                                                 '1' = 'A Little',
                                                 '2' = 'A Lot',
                                                 '3' = 'Very Much'))),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    frame_rate = list( Description = 'frame rate', 
                       Units = 'frames per sec'))
  
  # convert formatting to JSON
  tastetest_events_json <- RJSONIO::toJSON(tastetest_events_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(tastetest_events_json, asText = TRUE))){
    print('tastetest events JSON file may be invalid')
  }
  
  return(tastetest_events_json)
  
}