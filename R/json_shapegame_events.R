#' json_shapegame_events: Generates a json file for the Shape Game events files
#'
#' This function generates a json file for the individual level Shape Game events files in rawdata
#'
#' @return A json file documenting the individual level Shape Game events files in rawdata
#'
#'
#' @export

json_shapegame_events <- function() {
  
  shapegame_events_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Shape Game is a Value-Modulated Attentional Capture task modified for use with children.',
      Reference = 'Sali, A. W., Anderson, B. A., Yantis, S., Mostofsky, S. H., & Rosch, K. S. (2018). Reduced value-driven attentional capture among children with ADHD compared to typically developing controls. Journal of Abnormal Child Psychology, 46(6), 1187-1200. doi: 10.1007/s10802-017-0345-y.', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/28913698/',
      DatasetType = 'raw'),
    sub = list( Description = 'participant number'),
    date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    exp_name = list( Description = 'name of PsychoPy experiment file'),
    prac_rt_mean = list( Description = 'Practice mean reatcton time',
                         Units = 'sec'),
    prac_rt_sd = list( Description = 'Practice reatcton time standard deviation',
                       Units = 'sec'),
    rt_cutoff = list( Description = 'Reaction time cutoff to earn points (prac_rt_mean + 2*prac_rt_sd)',
                      Units = 'sec',
                      derivative = TRUE),
    block = list( Description = 'task block number'),
    trial = list( Description = 'index number from trial setup files'),
    trial_type = list( Description = 'trial condition',
                       Levels = list( 'high' = 'high value/bonus',
                                      'low' = 'low value',
                                      'neutral' = 'neutral')),
    trial_points = list( Description = 'points earned on the trial'),
    total_points = list( Description = 'cumulative number of points by trial'),
    pos1 = list( Description = 'stimuli color/type by at position 1 on the screen'),
    pos2 = list( Description = 'stimuli color/type by at position 2 on the screen'),
    pos3 = list( Description = 'stimuli color/type by at position 3 on the screen'),
    pos4 = list( Description = 'stimuli color/type by at position 4 on the screen'),
    pos5 = list( Description = 'stimuli color/type by at position 5 on the screen'),
    pos6 = list( Description = 'stimuli color/type by at position 6 on the screen'),
    direction = list( Description = 'direction the target arrow is facing',
                      Levels = list( 'right' = 'right',
                                     'left' = 'left')),
    target_roi = list( Description = 'position on screen of arrow target'),
    high_roi = list( Description = 'position on screen of high value color stimuli'),
    low_roi = list( Description = 'position on screen of lowvalue color stimuli'),
    resp = list( Description = 'participant response',
                 Levels = list( '1' = 'left',
                                '2' = 'right',
                                'None' = 'missed/no response')),
    resp_corr = list( Description = 'correctness of participant response',
                      Levels = list( '0' = 'incorrect',
                                     '1' = 'correct')),
    resp_rt = list( Description = 'response reaction time',
                    Units = 'sec'),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    frame_rate = list( Description = 'frame rate', 
                       Units = 'frames per sec')
    
    )
  
  # convert formatting to JSON
  shapegame_events_json <- RJSONIO::toJSON(shapegame_events_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(shapegame_events_json, asText = TRUE))){
    print('shapegame events JSON file may be invalid')
  }
  
  return(shapegame_events_json)
  
}