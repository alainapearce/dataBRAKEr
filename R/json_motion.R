#' json_motion: Generates a json file for the actigraph data
#'
#' This function generates a json file for the scored Binge Eating Scale and raw participant responses. This function provides accurate json files ONLY if data is processed using score_bes function in dataprepr and is only accurate for data collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the Binge Eating Scale
#'
#'
#' @export

json_motion <- function() {
  
  motion_list <- list(
    TaskName = 'Actigraph',
    TaskDescription = '1 week free living worn on non-dominant had day and night',
    SamplingFrequency = 30,
    TrackingSystemName = 'ActiGraph non-dominant hand',
    MotionChannelCount = 1,
    RecordingDuration = '7 days',
    Manufacturer = 'ActiGraph',
    ManufacturersModelName = 'wGT3XBT')
  
  # convert formatting to JSON
  bes_deid_json <- RJSONIO::toJSON(bes_deid_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(bes_deid_json, asText = TRUE))){
    print('bes JSON file may be invalid')
  }
  
  return(bes_deid_json)
  
}