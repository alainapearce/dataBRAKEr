#' json_foodchoice: Generates a json file for the Food Choice derivative database file
#'
#' This function generates a json file for the Food Choice derivative summary database
#'
#' @return A json file documenting the Food Choice summary database
#'
#'
#' @export

json_foodchoice <- function() {
  
  foodchoice_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Food Choice Task has participants choose between foods. Food image pairings are based on participants own health and taste choices during the Food choice Task.',
      Reference = 'Pearce AL, Adise S, Roberts NJ, White C, Geier CF, Keller KL. Individual differences in the influence of taste and health impact successful dietary self-control: A mouse tracking food choice study in children. Physiology & Behavior. 2020;223:112990. doi:10.1016/j.physbeh.2020.112990', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/32505786/',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    n_sc = list( Description = 'Number of self-control trials'),
    n_notsc = list( Description = 'Number of non-self-control trials that present healthy/not tasty vs not healthy/tasty (based on participant ratings, health_cut, and taste_cut)'),
    n_mix_noconflict = list( Description = 'Number of trials that presents a mix of tasty and healthy foods but without conflict - healthy/tasty vs not healthy/not tasty (based on participant ratings, health_cut, and taste_cut)'),
    n_tasty = list( Description = 'Number of tasty trials (based on participant ratings and taste_cut) - matched on taste but vary in health'),
    n_nottasty = list( Description = 'Number of non tasty trials (based on participant ratings and taste_cut) - matched on taste but vary in health'),
    n_healthy = list( Description = 'Number of healthy trials (based on participant ratings and health_cut) - matched on taste but vary in health'),
    n_nothealthy = list( Description = 'Number of not healthy trials (based on participant ratings and health_cut) - matched on taste but vary in health'),
    taste_cut = list( Description = 'Taste rating value that is the cut-point for distinguishing tasty (> cutpoint) or not tasty (<= cutput). This is set to 1.5 but is then optimized to get appropriate number of self-control trials if participant ratings are skewed'),
    taste_cut = list( Description = 'Health rating value that is the cut-point for distinguishing healthy (> cutpoint) or not healtht (<= cutput). This is set to 1.5 but is then optimized to get appropriate number of self-control trials if participant ratings are skewed'),
    health_mean_left = list( Description = 'Average health rating for image presented on the left'),
    health_mean_right = list( Description = 'Average health rating for image presented on the right'),
    taste_mean_left = list( Description = 'Average taste rating for image presented on the left'),
    taste_mean_right = list( Description = 'Average taste rating for image presented on the right'),
    want_mean_left = list( Description = 'Average want rating for image presented on the left'),
    want_mean_right = list( Description = 'Average want rating for image presented on the right'),
    n_choice_missed = list( Description = 'All Trials - Number of missed choices (time limit = 2 sec)'),
    p_choice_missed = list( Description = 'All Trials - Proportion trials with missed choices (time limit = 2 sec)'),
    n_choice_healthy = list( Description = 'All Trials - Healthy Choice: Number of trials'),
    p_choice_healthy = list( Description = 'All Trials - Healthy Choice: Proportion of non-missed trials'),
    rt_mean_choice_healthy = list( Description = 'All Trials - Healthy Choice: Average reaction time',
                                   Units = 'sec'),
    rt_med_choice_healthy = list( Description = 'All Trials - Healthy Choice: Median reaction time',
                                  Units = 'sec'),
    n_choice_tasty = list( Description = 'All Trials - Tasty Choice: Number of trials'),
    p_choice_tasty = list( Description = 'All Trials - Tasty Choice: Proportion of non-missed trials'),
    rt_mean_choice_tasty = list( Description = 'All Trials - Tasty Choice: Average reaction time',
                                 Units = 'sec'),
    rt_med_choice_tasty = list( Description = 'All Trials - Tasty Choice: Median reaction time',
                                Units = 'sec'),
    n_choice_want = list( Description = 'All Trials - Want Choice: Number of trials'),
    p_choice_want = list( Description = 'All Trials - Want Choice: Proportion of non-missed trials'),
    rt_mean_choice_want = list( Description = 'All Trials - Want Choice: Average reaction time',
                                Units = 'sec'),
    rt_med_choice_want = list( Description = 'All Trials - Want Choice: Median reaction time',
                               Units = 'sec'),
    n_sc_choice_missed = list( Description = 'Number of missed self-control trial choices (time limit = 2 sec)'),
    p_sc_choice_missed = list( Description = 'Proportion self-control trials with missed choices (time limit = 2 sec)'),
    healthdif_sc_trials = list( Description = 'Self-Control Trials - Average absolute difference in participant health ratings between presented images'),
    tastedif_sc_trials = list( Description = 'Self-Control Trials - Average absolute difference in participant taste ratings between presented images'),
    wantdif_sc_trials = list( Description = 'Self-Control Trials - Average absolute difference in participant wanting ratings between presented images'),
    healthdif_missed_sc_trials = list( Description = 'Self-Control Trials - Missed: Average absolute difference in participant health ratings between presented images'),
    tastedif_missed_sc_trials = list( Description = 'Self-Control Trials - Missed: Average absolute difference in participant taste ratings between presented images'),
    wantdif_missed_sc_trials = list( Description = 'Self-Control Trials - Missed: Average absolute difference in participant wanting ratings between presented images'),
    healthdif_complete_sc_trials = list( Description = 'Self-Control Trials - Completed: Average absolute difference in participant health ratings between presented images'),
    tastedif_complete_sc_trials = list( Description = 'Self-Control Trials - Completed: Average absolute difference in participant taste ratings between presented images'),
    wantdif_complete_sc_trials = list( Description = 'Self-Control Trials - Completed: Average absolute difference in participant wanting ratings between presented images'),
    n_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: Number of trials'),
    p_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: Proportion of non-missed trials'),
    rt_mean_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: Average reaction time',
                                      Units = 'sec'),
    rt_med_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: Median reaction time',
                                     Units = 'sec'),
    health_mean_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: Average health rating for chosen item'),
    taste_mean_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: Average taste rating for chosen item'),
    want_mean_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: Average wanting rating for chosen item'),
    healthdif_mean_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: absolute mean difference in participant health ratings between presented images'),
    tastedif_mean_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: absolute mean difference in participant taste ratings between presented images'),
    wantdif_mean_sc_choice_healthy = list( Description = 'Self-Control Trials - Healthy Choice: absolute mean difference in participant wanting ratings between presented images'),
    n_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: Number of trials'),
    p_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: Proportion of non-missed trials'),
    rt_mean_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: Average reaction time',
                                    Units = 'sec'),
    rt_med_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: Median reaction time',
                                   Units = 'sec'),
    health_mean_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: Average health rating for chosen item'),
    taste_mean_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: Average taste rating for chosen item'),
    want_mean_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: Average wanting rating for chosen item'),
    healthdif_mean_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: absolute mean difference in participant health ratings between presented images'),
    tastedif_mean_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: absolute mean difference in participant taste ratings between presented images'),
    wantdif_mean_sc_choice_tasty = list( Description = 'Self-Control Trials - Tasty Choice: absolute mean difference in participant wanting ratings between presented images'),
    n_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: Number of trials'),
    p_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: Proportion of non-missed trials'),
    rt_mean_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: Average reaction time',
                                         Units = 'sec'),
    rt_med_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: Median reaction time',
                                        Units = 'sec'),
    health_mean_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: Average health rating for chosen item'),
    taste_mean_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: Average taste rating for chosen item'),
    want_mean_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: Average wanting rating for chosen item'),
    healthdif_mean_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: absolute mean difference in participant health ratings between presented images'),
    tastedif_mean_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: absolute mean difference in participant taste ratings between presented images'),
    wantdif_mean_notsc_choice_healthy = list( Description = 'Non Self-Control Trials - Healthy Choice: absolute mean difference in participant wanting ratings between presented images'),
    n_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: Number of trials'),
    p_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: Proportion of non-missed trials'),
    rt_mean_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: Average reaction time',
                                       Units = 'sec'),
    rt_med_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: Median reaction time',
                                      Units = 'sec'),
    health_mean_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: Average health rating for chosen item'),
    taste_mean_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: Average taste rating for chosen item'),
    want_mean_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: Average wanting rating for chosen item'),
    healthdif_mean_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: absolute mean difference in participant health ratings between presented images'),
    tastedif_mean_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: absolute mean difference in participant taste ratings between presented images'),
    wantdif_mean_notsc_choice_tasty = list( Description = 'Non Self-Control Trials - Tasty Choice: absolute mean difference in participant wanting ratings between presented images')
  )
  
  # convert formatting to JSON
  foodchoice_json <- RJSONIO::toJSON(foodchoice_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodchoice_json, asText = TRUE))){
    print('foodchoice JSON file may be invalid')
  }
  
  return(foodchoice_json)
  
}