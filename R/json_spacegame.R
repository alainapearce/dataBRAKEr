#' json_spacegame: Generates a json file for the Space Game summary behavioral dataset
#'
#' This function generates a json file for the Space Game summary behavioral dataset
#'
#' @return A json file documenting the Space Game summary behavioral dataset
#'
#'
#' @export

json_spacegame <- function() {
  
  spacegame_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Space Game is a Reward-Related Decision Making task modified for use with children.',
      Reference = 'Kool, W., Gershman, S. J., & Cushman, F. A. (2017). Cost-benefit arbitration between multiple reinforcement-learning systems. Psychological science, 28(9), 1321-1333.doi: 10.1177/0956797617708288', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/28731839/',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    rt_mean_earth = list( Description = 'Earth - mean reaction time',
                          Units = 'sec'),
    rt_median_earth = list( Description = 'Earth - median reaction time',
                          Units = 'sec'),
    rt_mean_planet = list( Description = 'Planet - mean reaction time',
                          Units = 'sec'),
    rt_median_planet = list( Description = 'Planet - median reaction time',
                            Units = 'sec'),
    n_timeout_earth = list( Description = 'Earth - number of missed trials'),
    rr = list( Description = 'Reward rate - average points (not counting stakes) per trial'),
    rr_adj = list( Description = 'Adjusted reward rate - reward rate minus the average of the available points across the two stages for a trial'),
    rr_s5 = list( Description = 'Stake 5 Reward rate - average points (not counting stakes) per trial'),
    rr_adj_s5 = list( Description = 'Stake 5 Adjusted reward rate - reward rate minus the average of the available points across the two stages for a trial'),
    rr_s1 = list( Description = 'Stake 1 Reward rate - average points (not counting stakes) per trial'),
    rr_adj_s1 = list( Description = 'Stake 1 Adjusted reward rate - reward rate minus the average of the available points across the two stages for a trial'),
    rr_scaled = list( Description = 'Scaled Reward Rate - average of the scaled points (not counting stakes) per trile. Scale points = points/max number of points available'),
    rr_scaled_adj = list( Description = 'Adjusted reward rate - scaled reward rate minus the scaled average of the available points across the two stages for a trial. Scaled average is the mean of the available points/max number of points available.'),
    rr_scaled_s5 = list( Description = 'Stake 5 Scaled Reward Rate - average of the scaled points (not counting stakes) per trile. Scale points = points/max number of points available'),
    rr_scaled_adj_s5 = list( Description = 'Stake 5 Adjusted reward rate - scaled reward rate minus the scaled average of the available points across the two stages for a trial. Scaled average is the mean of the available points/max number of points available.'),
    rr_scaled_s1 = list( Description = 'Stake 1 Scaled Reward Rate - average of the scaled points (not counting stakes) per trile. Scale points = points/max number of points available'),
    rr_scaled_adj_s1 = list( Description = 'Stake 1 Adjusted reward rate - scaled reward rate minus the scaled average of the available points across the two stages for a trial. Scaled average is the mean of the available points/max number of points available.'),
    stay_prob = list( Description = 'Probability of choosing to go to the same planet as prior trial'),
    stay_prob_s5 = list( Description = 'Stake 5 Probability of choosing to go to the same planet as prior trial'),
    stay_prob_s1 = list( Description = 'Stake 1 Probability of choosing to go to the same planet as prior trial')
  )
  
  # convert formatting to JSON
  spacegame_json <- RJSONIO::toJSON(spacegame_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(spacegame_json, asText = TRUE))){
    print('spacegame events JSON file may be invalid')
  }
  
  return(spacegame_json)
  
}