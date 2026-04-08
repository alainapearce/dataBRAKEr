#' json_bodpod: Generates a json file for BodPod 
#'
#' This function generates a json file for BodPod 
#'
#' @return A json file documenting the Study BRAKE's BodPod 
#'
#'
#' @export

json_bodpod <- function() {
  
  bodpod_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    bodpod_collect = list( Description = 'Was the BodPod completed?',
                           Levels = list( '0' = 'No',
                                          '1' = 'Yes')),
    bodpod_no_reason = list( Description = 'If not completed, reason why'),
    bodpod_fat_p = list( Description = 'Body fat percentage'),
    bodpod_fatfree_p = list( Description = 'Body fat-free percentage'),
    bodpod_fat_kg = list( Description = 'Body fat',
                          Unit = 'kg'),
    bodpod_fatfree_kg = list( Description = 'Body fat-free mass',
                              Unit = 'kg'),
    bodpod_bodymass_kg = list( Description = 'Total body mass',
                               Unit = 'kg'),
    bodpod_bodyvol_l = list( Description = 'Total body volume',
                             Unit = 'l'),
    bodpod_bodydensity = list( Description = 'Body density',
                               Unit = 'kg/L'),
    bodpod_thoracic_gasvol_l = list( Description = 'Thorasic gas volume',
                                     Unit = 'L'))
  
  # convert formatting to JSON
  bodpod_json <- RJSONIO::toJSON(bodpod_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(bodpod_json, asText = TRUE))){
    print('BodPod JSON file may be invalid')
  }
  
  return(bodpod_json)
  
}