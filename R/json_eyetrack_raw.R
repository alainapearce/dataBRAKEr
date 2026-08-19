#' json_eyetrack_raw: Generates a json file for the Food Choice derivative database file
#'
#' This function generates a json file for the Food Choice derivative summary database
#'
#' @return A json file documenting the Food Choice summary database
#'
#'
#' @export

json_eyetrack_raw_events <- function() {
  
  eyetrack_raw_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'data converted from .hdf5 files with the MonocularEyeSampleEvent, MessageEvent, and KeyboardInputEvent data lists merged to make BIDS format',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    time = list( Description = 'task time tracked by PsychoPy'),
    text = list( Description = 'PsychoPy task internal event message'),
    key = list( Description = 'key pressed in PsychoPy task'),
    experiment_id = list( Description = 'experiment id - set internally'),
    device_id = list( Description = 'device id - set internally'),
    event_id = list( Description = 'event id - set internally'),
    type = list( Description = 'event type - set internally'),
    confidence_interval = list( Description = 'confidence interval'),
    delay = list( Description = 'time delay'),
    logged_time = list( Description = 'logged time'),
    filter_id = list( Description = 'filter id - set internally'),
    eye = list( Description = 'eye'),
    gaze_x = list( Description = 'gaze in x-axis'),
    gaze_y = list( Description = 'gaze in y-axis'),
    gaze_z = list( Description = 'gaze in z-axis'),
    eye_cam_x = list( Description = 'eye in x-axis'),
    eye_cam_y = list( Description = 'eye in y-axis'),
    eye_cam_z = list( Description = 'eye in z-axis'),
    angle_x = list( Description = 'angle in x-axis'),
    angle_y = list( Description = 'angle in y-axis'),
    raw_x = list( Description = 'raw in x-axis'),
    raw_y = list( Description = 'raw in y-axis'),
    pupil_measure1 = list( Description = 'pupil measure 1'),
    pupil_measure1_type = list( Description = 'pupil measure 1 type'),
    pupil_measure2 = list( Description = 'pupil measure 2'),
    pupil_measure2_type = list( Description = 'pupil measure 2 type'),
    ppd_x = list( Description = 'ppd x-axis'),
    ppd_y = list( Description = 'ppd y-axis'),
    velocity_x = list( Description = 'velocity x-axis'),
    velocity_y = list( Description = 'velocity y-axis'),
    velocity_xy = list( Description = 'velocity x- and y-axis'),
    status = list( Description = 'status'))
  
  # convert formatting to JSON
  eyetrack_raw_json <- RJSONIO::toJSON(eyetrack_raw_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(eyetrack_raw_json, asText = TRUE))){
    print('eyetrack_raw JSON file may be invalid')
  }
  
  return(eyetrack_raw_json)
  
}