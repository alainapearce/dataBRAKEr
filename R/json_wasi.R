#' json_wasi: Generates a json file for WASI 
#'
#' This function generates a json file for WASI 
#'
#' @return A json file documenting the Study BRAKE's WASI 
#'
#'
#' @export

json_wasi <- function() {
  
  wasi_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Wechsler Abbreviated Scale of Intelligence-II',
      Reference = 'Wechsler, David. "Wechsler abbreviated scale of intelligence--Second Edition" (1999).',
      TermURL = 'https://doi.org/10.1037/t15171-000'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    wasi_collect = list( Description = 'Was the WASI completed?',
                           Levels = list( '0' = 'No',
                                          '1' = 'Yes')),
    wasi_no_reason = list( Description = 'If not completed, reason why'),
    wasi_block_raw = list( Description = 'WASI Block Design raw score'),
    wasi_vocab_raw = list( Description = 'WASI Vocab raw score'),
    wasi_matrix_raw = list( Description = 'WASI Matrix Design raw score'),
    wasi_block_t = list( Description = 'WASI Block Design T-score'),
    wasi_vocab_t = list( Description = 'WASI Vocab T-score'),
    wasi_matrix_t = list( Description = 'WASI Matrix Design T-score'),
    wasi_pri_t = list( Description = 'WASI Perceptual T-score'),
    wasi_fsiq_t = list( Description = 'WASI full-scale T-score based on Vocab and Matrix'),
    wasi_pri = list( Description = 'WASI Perceptual IQ'),
    wasi_fsiq = list( Description = 'WASI full-scale IQ based on Vocab and Matrix'),
    wasi_pri_p = list( Description = 'WASI Perceptual percentile'),
    wasi_fsiq_p = list( Description = 'WASI full-scale IQ percentile based on Vocab and Matrix'))
  
  # convert formatting to JSON
  wasi_json <- RJSONIO::toJSON(wasi_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(wasi_json, asText = TRUE))){
    print('WASI JSON file may be invalid')
  }
  
  return(wasi_json)
  
}