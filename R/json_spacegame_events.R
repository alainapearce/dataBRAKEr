#' json_spacegame_events: Generates a json file for the Space Game events files
#'
#' This function generates a json file for the individual level Space Game events files in rawdata
#'
#' @return A json file documenting the individual level Space Game events files in rawdata
#'
#'
#' @export

json_spacegame_events <- function() {
  
  spacegame_events_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Space Game is a Reward-Related Decision Making task modified for use with children.',
      Reference = 'Kool, W., Gershman, S. J., & Cushman, F. A. (2017). Cost-benefit arbitration between multiple reinforcement-learning systems. Psychological science, 28(9), 1321-1333.doi: 10.1177/0956797617708288', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/28731839/',
      DatasetType = 'raw'),
    sub = list( Description = 'participant number'),
    date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    block = list( Description = 'task block number'),
    trial = list( Description = 'index number from trial setup files'),
    timeout_earth = list( Description = 'Earth Stage - Did response time for the time out (i.e., participant missed the trial)',
                       Levels = list( '0' = 'no',
                                      '1' = 'yes')),
    timeout_planet = list( Description = 'Planet Stage - Did response time for the time out (i.e., participant missed the trial)',
                          Levels = list( '0' = 'no',
                                         '1' = 'yes')),
    state_earth = list( Description = 'Earth Stage - State',
                          Levels = list( '1' = 'state 1 (randomized pairing of rockets that is stable throughout task)',
                                         '2' = 'state 2 (randomized pairing of rockets that is stable throughout task')),
    state_planet = list( Description = 'Planet Stage - State',
                        Levels = list( '1' = 'state 1 - (randomize alien type)',
                                       '2' = 'state 2 - (randomize alien type)')),
    stim_left = list( Description = 'rocket stimuli number on left side of planet earth'),
    stim_right = list( Description = 'rocket stimuli number on right side of planet earth'),
    rt_earth = list( Description = 'Earth Stage - reaction time',
                     Units = 'sec'),
    rt_planet = list( Description = 'Planet Stage - reaction time',
                     Units = 'sec'),
    choice_earth = list( Description = 'Earth Stage - reaction time',
                     Units = 'sec'),
    response = list( Description = 'Earth Stage - Response',
                         Levels = list( '1' = 'left key/left stimuli',
                                        '2' = 'right key/right stimuli')),
    points = list( Description = 'Points earned (not counting stake)'),
    stake = list( Description = 'Stake',
                  Levels = list( '1' = 'low/1x multiplier',
                                 '5' = 'high/5x mulitiplier')),
    score = list( Description = 'Cumulative/running total score, including stakes'),
    rewards1 = list( Description = 'reward associated with state 1'),
    rewards2 = list( Description = 'reward associated with state 2'),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    missed_earth = list( Description = 'Earth Stage - missed',
                         Levels = list( '0' = 'no',
                                        '1' = 'yes')),
    missed = list( Description = 'Trial missed at either the Earth or Planet stage',
                   Levels = list( '0' = 'no',
                                  '1' = 'yes')),
    prev_missed = list( Description = 'Previous trial missed',
                   Levels = list( '0' = 'no',
                                  '1' = 'yes')),
    prev_reward_diff = list( Description = 'Difference in rewards between states at previous trial'),
    prev_points = list( Description = 'Points earned (not counting stakes) at previous trial'),
    prev_stake = list( Description = 'Stake at previous trial'),
    earth_same = list( Description = 'Earth Stage - Same as previous trial missed',
                        Levels = list( '0' = 'no',
                                       '1' = 'yes')),
    stay_planet = list( Description = 'Planet Stage - Choose same as previous trial missed',
                        Levels = list( '0' = 'no',
                                       '1' = 'yes'))
    )
  
  # convert formatting to JSON
  spacegame_events_json <- RJSONIO::toJSON(spacegame_events_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(spacegame_events_json, asText = TRUE))){
    print('spacegame events JSON file may be invalid')
  }
  
  return(spacegame_events_json)
  
}