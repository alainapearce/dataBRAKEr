#' json_pref_rank: Generates a json file for Taste-Test foods preference-rank 
#'
#' This function generates a json file for Taste-Test foods preference-rank 
#'
#' @return A json file documenting the Study BRAKE's Taste-Test foods preference-rank 
#'
#'
#' @export

json_pref_rank <- function() {
  
  pref_rank_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1 (baseline)',
                                          '2' =	'Child visit protocol 2 (baseline)',
                                          '3'	= 'Child visit protocol 3 (follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    premeal_pref_rank_chnug = list( Description = 'Pre-Meal order Chicken Nuggets was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_mac = list( Description = 'Pre-Meal order Macaroni and Cheese was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_grape = list( Description = 'Pre-Meal order the Grape was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_carrot = list( Description = 'Pre-Meal order the Carrot was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_orange = list( Description = 'Pre-Meal order the Orange was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_broccoli = list( Description = 'Pre-Meal order Broccoli was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_gbean = list( Description = 'Pre-Meal order the Green Bean was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_chocolate = list( Description = 'Pre-Meal order the Hershey Kiss was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_cracker = list( Description = 'Pre-Meal order the Cracker was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    premeal_pref_rank_fruitsnack = list( Description = 'Pre-Meal order the Fruit Snack was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_chnug = list( Description = 'Post-Meal order Chicken Nuggets was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_mac = list( Description = 'Post-Meal order Macaroni and Cheese was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_grape = list( Description = 'Post-Meal order the Grape was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_carrot = list( Description = 'Post-Meal order the Carrot was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_orange = list( Description = 'Post-Meal order the Orange was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_broccoli = list( Description = 'Post-Meal order Broccoli was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_gbean = list( Description = 'Post-Meal order the Green Bean was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_chocolate = list( Description = 'Post-Meal order the Hershey Kiss was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_cracker = list( Description = 'Post-Meal order the Cracker was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'),
    postmeal_postf_rank_fruitsnack = list( Description = 'Post-Meal order the Fruit Snack was picked in response to the question: Can pick out the food you liked the most of the options in front of you? (once chosen, the food was removed)'))
  
  # convert formatting to JSON
  pref_rank_json <- RJSONIO::toJSON(pref_rank_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(pref_rank_json, asText = TRUE))){
    print('preference-rank JSON file may be invalid')
  }
  
  return(pref_rank_json)
  
}