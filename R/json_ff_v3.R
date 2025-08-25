#' json_ff_v3: Generates a json file for follow-up Freddy Fullness ratings from double entry REDCap
#'
#' This function generates a json file for follow-up Freddy Fullness ratings
#'
#' @return A string with data stored in JSON format containing meta-data for follow-up Freddy Fullness ratings
#'
#'
#' @export

json_ff_v3 <- function() {

  ff_v3_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Freddy Fullness Scale',
      Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/16828929/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    fullness_premeal_prefnirs = list( Description = 'Pre-meal pre-fNIRS fullness rating on a 150 mm visual analgue scale',
                              Units = 'mm'),
    fullness_premeal_postfnirs = list( Description = 'Pre-meal post-fNIRS fullness rating on a 150 mm visual analgue scale',
                                       Units = 'mm'),
    fullness_premeal = list( Description = 'Pre-meal fullness rating on a 150 mm visual analgue scale after the second snack',
                                    Units = 'mm'),
    fullness_postmeal = list( Description = 'Post-meal fullness rating on a 150 mm visual analgue scale after the second snack',
                             Units = 'mm'),
    fullness_postmeal_prefnirs = list( Description = 'Post-meal pre-fNIRS fullness rating on a 150 mm visual analgue scale',
                                      Units = 'mm'),
    fullness_postmeal_postfnirs = list( Description = 'Post-meal post-fNIRS fullness rating on a 150 mm visual analgue scale',
                                       Units = 'mm'))

  # convert formatting to JSON
  ff_v3_json <- RJSONIO::toJSON(ff_v3_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(ff_v3_json, asText = TRUE))){
    print('Double-entered Taste-Test samples JSON file may be invalid')
  }

  return(ff_v3_json)

}
