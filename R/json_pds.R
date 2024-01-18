#' json_pds: Generates a json file for Parent-Report Pubertal Development Scale
#'
#' This function generates a json file for the scored Parent-Pubertal Development Scale and and raw participant responses. This function provides accurate json files ONLY if data is processed using score_pds function in dataprepr and is only accurate for data collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the Pubertal Development Score.
#'
#' @export
#' 
json_pds <- function() {
  
  pds_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Pubertal Development Scale.',
      Reference = 'Carskadon, Mary A., and Christine Acebo. A self-administered rating scale for pubertal development. Journal of Adolescent Health 14, no. 3 (1993): 190-195. https://doi.org/10.1016/1054-139X(93)90004-9', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/8323929/'),
    participant_id = list( Description = 'participant id number'),
    sex = list( Description = 'sex', 
                Levels = list ('0' = 'male',
                               '1' = 'female')),
    pds_1 = list( Description = 'Would you say that your child\'s growth in height:',
                  Levels = list ('1' = 'Has not yet begun to spurt ("spurt" means more growth than usual)',
                                 '2' = 'Has barely started',
                                 '3' = 'Is definitely underway',
                                 '4' = 'Seems completed',
                                 '99' = 'I don\'t know')),
    pds_2 = list( Description = 'And how about the growth of your child\'s body hair? ("Body hair" means hair any place other than your head, such as under your arms.)',
                  Levels = list ('1' = 'Has not yet begun to grow',
                                 '2' = 'Has barely started to grow',
                                 '3' = 'Is definitely underway',
                                 '4' = 'Seems completed',
                                 '99' = 'I don\'t know')),
    pds_3 = list( Description = 'Have you noticed any skin changes, especially pimples?',
                  Levels = list ('1' = 'Skin has not yet started changing',
                                 '2' = 'Skin has barely started changing',
                                 '3' = 'Skin changes are definitely underway',
                                 '4' = 'Skin changes seem complete',
                                 '99' = 'I don\'t know')),
    pds_4m = list( Description = 'Have you noticed a deepening of your child\'s voice?',
                   Levels = list ('1' = 'Voice has not yet started changing',
                                  '2' = 'Voice has barely started changing',
                                  '3' = 'Voice changes are definitely underway',
                                  '4' = 'Voice changes seem complete',
                                  '99' = 'I don\'t know')),
    pds_5m = list( Description = 'Has your child begun to grow hair on his face?',
                   Levels = list ('1' = 'Facial hair has not yet started changing',
                                  '2' = 'Facial hair has barely started changing',
                                  '3' = 'Facial hair changes are definitely underway',
                                  '4' = 'Facial hair changes seem complete',
                                  '99' = 'I don\'t know')),
    pds_4f = list( Description = 'Have you noticed that her breasts have begun to grow?',
                   Levels = list ('1' = 'Have not yet started growing',
                                  '2' = 'Have barely started growing',
                                  '3' = 'Breast growth is definitely underway',
                                  '4' = 'Breast growth seems complete',
                                  '99' = 'I don\'t know')),
    pds_5fa = list( Description = 'Has your daughter begun to menstruate (started to have your period)?',
                    Levels = list ('1' = 'Yes',
                                   '0' = 'No',
                                   '99' = 'I don\'t know')),
    pds_5fb = list( Description = 'How old was she when she started to menstruate (age in years)?', 
                    Units = 'years'),
    pds_5fc = list( Description = 'What was the first day of her last menstrual period?', 
                    Units = 'MM-DD-YYYY'),
    pds_5fd = list( Description = 'How many days does her cycle usually last (i.e. how many days are typically between the start of one period to the start of next period)?', 
                    Units = 'days'),
    pds_6 = list( Description = 'Do you think your childs development is any earlier or later than most other [boys/girls] at your childs age?',
                  Levels = list ('1' = 'Much earlier',
                                 '2' = 'Somewhat earlier',
                                 '3' = 'Somewhat later',
                                 '4' = 'Much later',
                                 '99' = 'I don\'t know')),
    pds_score_na = list( Description = 'Number of responses parents marked "I don\'t know" or choose not to respond to.', 
                         Derivative = TRUE),
    pds_score = list( Description = 'Pubertal Development Scale score: average of all responses for each sex with menarche yes = 4 points and menarche no = 1 point.', 
                      Derivative = TRUE),
    pds_tanner_sum = list( Description = 'Sum of scored values for pubertal rating.', 
                           Derivative = TRUE),
    pds_tanner_cat = list( Description = 'Tanner equivaluent category.', 
                           Derivative = TRUE))
  
  # convert formatting to JSON
  pds_json <- RJSONIO::toJSON(pds_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(pds_json, asText = TRUE))){
    print('PDS JSON file may be invalid')
  }
  
  return(pds_json)
}