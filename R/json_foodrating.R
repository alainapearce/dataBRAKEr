#' json_foodrating: Generates a json file for the Food Rating derivative database file
#'
#' This function generates a json file for the sFood Rating derivative database
#'
#' @return A json file documenting the Food Rating derivative database
#'
#'
#' @export

json_foodrating <- function() {
  
  foodrating_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'The Food Rating Task has participants view and rate food images. The same food images are rated for: health, taste, wanting. All ratings are completed for one condition before moving to the next rating condition with order counterballanced.',
      Reference = 'Pearce AL, Adise S, Roberts NJ, White C, Geier CF, Keller KL. Individual differences in the influence of taste and health impact successful dietary self-control: A mouse tracking food choice study in children. Physiology & Behavior. 2020;223:112990. doi:10.1016/j.physbeh.2020.112990', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/32505786/',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    order = list( Description = 'Order participant rated the 3 food attributes',
                 Levels = list ('htw' = 'health, taste, wanting',
                                'twh' = 'taste, wanting, health',
                                'wht' = 'wanting, health, taste')),
    want_mean = list( Description = 'Wanting - mean rating'),
    want_med = list( Description = 'Wanting - median rating'),
    want_sd = list( Description = 'Wanting - standard deviation of individual ratings'),
    want_icv = list( Description = 'Wanting - percent individual coefficient of variability'),
    want_high_ed_mean = list( Description = 'Wanting: high energy dense foods - mean rating'),
    want_high_ed_med = list( Description = 'Wanting: high energy dense foods - median rating'),
    want_high_ed_sd = list( Description = 'Wanting: high energy dense foods - standard deviation of individual ratings'),
    want_high_ed_icv = list( Description = 'Wanting: high energy dense foods - percent individual coefficient of variability'),
    want_low_ed_mean = list( Description = 'Wanting: low energy dense foods - mean rating'),
    want_low_ed_med = list( Description = 'Wanting: low energy dense foods - median rating'),
    want_low_ed_sd = list( Description = 'Wanting: low energy dense foods - standard deviation of individual ratings'),
    want_low_ed_icv = list( Description = 'Wanting: low energy dense foods - percent individual coefficient of variability'),
    health_mean = list( Description = 'Health - mean rating'),
    health_med = list( Description = 'Health - median rating'),
    health_sd = list( Description = 'Health - standard deviation of individual ratings'),
    health_icv = list( Description = 'Health - percent individual coefficient of variability'),
    health_high_ed_mean = list( Description = 'Health: high energy dense foods - mean rating'),
    health_high_ed_med = list( Description = 'Health: high energy dense foods - median rating'),
    health_high_ed_sd = list( Description = 'Health: high energy dense foods - standard deviation of individual ratings'),
    health_high_ed_icv = list( Description = 'Health: high energy dense foods - percent individual coefficient of variability'),
    health_low_ed_mean = list( Description = 'Health: low energy dense foods - mean rating'),
    health_low_ed_med = list( Description = 'Health: low energy dense foods - median rating'),
    health_low_ed_sd = list( Description = 'Health: low energy dense foods - standard deviation of individual ratings'),
    health_low_ed_icv = list( Description = 'Health: low energy dense foods - percent individual coefficient of variability'),
    taste_mean = list( Description = 'Taste - mean rating'),
    taste_med = list( Description = 'Taste - median rating'),
    taste_sd = list( Description = 'Taste - standard deviation of individual ratings'),
    taste_icv = list( Description = 'Taste - percent individual coefficient of variability'),
    taste_high_ed_mean = list( Description = 'Taste: high energy dense foods - mean rating'),
    taste_high_ed_med = list( Description = 'Taste: high energy dense foods - median rating'),
    taste_high_ed_sd = list( Description = 'Taste: high energy dense foods - standard deviation of individual ratings'),
    taste_high_ed_icv = list( Description = 'Taste: high energy dense foods - percent individual coefficient of variability'),
    taste_low_ed_mean = list( Description = 'Taste: low energy dense foods - mean rating'),
    taste_low_ed_med = list( Description = 'Taste: low energy dense foods - median rating'),
    taste_low_ed_sd = list( Description = 'Taste: low energy dense foods - standard deviation of individual ratings'),
    taste_low_ed_icv = list( Description = 'Taste: low energy dense foods - percent individual coefficient of variability'))
  
  # convert formatting to JSON
  foodrating_json <- RJSONIO::toJSON(foodrating_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodrating_json, asText = TRUE))){
    print('foodrating JSON file may be invalid')
  }
  
  return(foodrating_json)
  
}