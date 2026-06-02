#' json_shapegame: Generates a json file for the Shape Game derivative file
#'
#' This function generates a json file for the derivative Shape Game database
#'
#' @return A json file documenting wide formatted Shape Game database
#'
#'
#' @export

json_shapegame <- function() {
  
  shapegame_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Shape Game is a Value-Modulated Attentional Capture task modified for use with children.',
      Reference = 'Sali, A. W., Anderson, B. A., Yantis, S., Mostofsky, S. H., & Rosch, K. S. (2018). Reduced value-driven attentional capture among children with ADHD compared to typically developing controls. Journal of Abnormal Child Psychology, 46(6), 1187-1200. doi: 10.1007/s10802-017-0345-y.', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/28913698/',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    prac_rt_mean = list( Description = 'Practice mean reatcton time',
                         Units = 'sec'),
    prac_rt_sd = list( Description = 'Practice reatcton time standard deviation',
                       Units = 'sec'),
    rt_cutoff = list( Description = 'Reaction time cutoff to earn points (prac_rt_mean + 2*prac_rt_sd)',
                      Units = 'sec'),
    low_color = list( Description = 'Color indicating low-value trial'),
    high_color = list( Description = 'Color indicating high-value trial'),
    n_cor = list( Description = 'All Trials - number correct for arrow detection'),
    p_cor = list( Description = 'All Trials - proportion correct for arrow detection'),
    n_trials_high = list( Description = 'High Value/Bonus Trials - number of trials'),
    n_cor_high = list( Description = 'High Value/Bonus Trials - number correct for arrow detection'),
    p_cor_high = list( Description = 'High Value/Bonus Trials - proportion correct for arrow detection'),
    n_trials_low = list( Description = 'Low Value - number of trials'),
    n_cor_low = list( Description = 'Low Value Trials - number correct for arrow detection'),
    p_cor_low = list( Description = 'Low Value Trials - proportion correct for arrow detection'),
    n_trials_neutral = list( Description = 'Neutral Trials - number of trials'),
    n_cor_neutral = list( Description = 'Neutral Trials - number correct for arrow detection'),
    p_cor_neutral = list( Description = 'Neutral Trials - proportion correct for arrow detection'),
    mean_rt = list( Description = 'All Trials - mean reaction time for correct trials',
                    Units = 'sec'),
    mean_rt_high = list( Description = 'High Value/Bonus Trials - mean reaction time for correct trials',
                         Units = 'sec'),
    mean_rt_low = list( Description = 'Low Value Trials - mean reaction time for correct trials',
                        Units = 'sec'),
    mean_rt_neutral = list( Description = 'Neutral Trials - mean reaction time for correct trials',
                            Units = 'sec'),
    rt_dif_high_low = list( Description = 'Difference in reaction time between high and low value trials',
                            Units = 'sec'),
    rt_dif_high_neutral = list( Description = 'Difference in reaction time between high value and neutral trials',
                                Units = 'sec'),
    rt_dif_low_neutral = list( Description = 'Difference in reaction time between low value and neutral trials',
                               Units = 'sec'),
    total_points = list( Description = 'All Trials - total points'),
    n_points_high = list( Description = 'High Value/Bonus Trials - total points'),
    n_points_low = list( Description = 'Low Value Trials - total points'),
    n_points_neutral = list( Description = 'Neutral Trials - total points'),
    mean_points_high = list( Description = 'High Value/Bonus Trials - average number of points per trial'),
    mean_points_low = list( Description = 'Low Value Trials - average number of points per trial'),
    mean_points_neutral = list( Description = 'Neutral Trials - average number of points per trial')
    )
  
  # convert formatting to JSON
  shapegame_json <- RJSONIO::toJSON(shapegame_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(shapegame_json, asText = TRUE))){
    print('shapegame JSON file may be invalid')
  }
  
  return(shapegame_json)
  
}