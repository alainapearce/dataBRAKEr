#' json_efcr: Generates a json file for the External Food Cue Responsiveness Scale
#'
#' This function generates a json file for the scored External Food Cue Responsiveness Scale and raw participant responses. This function provides accurate json files ONLY if data is processed using score_efcr function in dataprepr and is only accurate for data collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the External Food Cue Responsiveness Scale
#'
#'
#' @export

json_efcr <- function() {
  
  efcr_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'External Food Cue Responsiveness Scale.',
      Reference = 'Masterson TD, Gilbert-Diamond D, Lansigan RK, Kim SJ, Schiffelbein JE, Emond JA. Measurement of external food cue responsiveness in preschool-age children: Preliminary evidence for the use of the external food cue responsiveness scale. Appetite. 2019;139:119-126. doi:10.1016/j.appet.2019.04.024', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/31047939/'),
    participant_id = list( Description = 'participant id number'),
    efcr1 = list( Description = 'My child asks for food or drinks that other kids eat',
                 Levels = list ('1' = 'Never',
                                '2' = 'Rarely',
                                '3' = 'Sometimes',
                                '4' = 'Often',
                                '5' = 'Always')),
    efcr2 = list( Description = 'My child points out snack or drink vending machines',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr3 = list( Description = 'My child wants to eat when people talk about food',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr4 = list( Description = 'My child gets excited when he/she sees restaurant logos',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr5 = list( Description = 'My child wants snacks at check-out aisles',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr6 = list( Description = 'My child likes certain snacks because of the packaging',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr7 = list( Description = 'My child gets excited by the sound of food cooking',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr8 = list( Description = 'My child wants to eat if he/she hears a snack being opened',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr9 = list( Description = 'My child expects a snack when in the car',
                  Levels = list ('1' = 'Never',
                                 '2' = 'Rarely',
                                 '3' = 'Sometimes',
                                 '4' = 'Often',
                                 '5' = 'Always')),
    efcr_score = list( Description = 'External food cue responsivness.', 
                     Derivative = TRUE))
  
  # convert formatting to JSON
  efcr_json <- RJSONIO::toJSON(efcr_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(efcr_json, asText = TRUE))){
    print('EFCR JSON file may be invalid')
  }
  
  return(efcr_json)
  
}