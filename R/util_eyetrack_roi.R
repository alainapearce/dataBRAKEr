#' util_eyetrack_roi: Process raw data from ROI-based eye-tracking data from PscyhoPy for a single participant
#'
#' This function cleans ROI-based eye-tracking data from PscyhoPy for a single participant
#'
#' To use this function, the data must include the following: trial number, number of looks per ROI for a trial, and the onset/offset times for each ROI look for a trial
#'
#' @param data a data.frame with roi-based eye-tracking
#' @param roi_list a list with character string labels for each ROI (c('left', 'right'))
#' @inheritParams util_task_foodrating
#'
#' @return If return_data is set to TRUE, will return a long dataset with a row per ROI look
#'
#' @examples
#'
#' # process eye-trackign data 
#' yetrack_roi_dat <- util_eyetrack_roi(data, roi_list = c('right', 'left), return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_eyetrack_roi <- function(data, roi_list, return_data = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be entered as a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
    stop("data must be entered as a data.frame")
  }
  
  #get matrix of look timing and roi
  look_matrix <- function(t_dat, roi_str){
    
    #decompose onset string
    onset_str <- t_dat[[paste0(roi_str, '_look_onsets')]]
    onset_list <- gsub('\\[|\\]| ', '', unlist(as.list(strsplit(onset_str, ','))))

    n_looks <- t_dat[[paste0(roi_str, '_looks')]]
    
    look_mat <- matrix(c(rep(roi_str, n_looks), onset_list), ncol = 2, byrow = FALSE)
  }
  
  trial_long_fn <- function(t_dat, roi_list){
    #look number
    n_looks <- sapply(roi_list, function(x) as.numeric(t_dat[[paste0(x, '_looks')]]), simplify = TRUE)
    
    # get onsets and offsets paired with roi
    if (sum(n_looks == 0) > 0){
      # if one roi has 0 looks
      
    } else if (sum(n_looks == 0) > 0){
      # if only looked at 1 ROI
      
      t_eye_onsets <- rbind.data.frame(t(sapply(roi_list, function(x) look_matrix(t_dat, x, 'onsets'), simplify = TRUE)))
      
      t_eye_offsets <- rbind.data.frame(t(sapply(roi_list, function(x) look_matrix(t_dat, x, 'offsets'), simplify = TRUE)))
      
    } else if (sum(n_looks == 1) == length(roi_list)){
      # if both only have 1 look
      
      t_eye_onsets <- rbind.data.frame(t(sapply(roi_list, function(x) look_matrix(t_dat, x, 'onsets'), simplify = TRUE)))
      
      t_eye_offsets <- rbind.data.frame(t(sapply(roi_list, function(x) look_matrix(t_dat, x, 'offsets'), simplify = TRUE)))
      
    } else if (sum(n_looks > 1) >= 1) {
      #if both rois have at least 1 look and 1 roi has > 1 look
      t_eye_onsets <- data.frame(do.call(rbind, sapply(roi_list, function(x) look_matrix(t_dat, x, 'onsets'), simplify = TRUE)))
      
      t_eye_offsets <- data.frame(do.call(rbind, sapply(roi_list, function(x) look_matrix(t_dat, x, 'offsets'), simplify = TRUE)))
    }
    
    
    #make data.frame
    t_eye_dat <- cbind.data.frame(t_eye_onsets, t_eye_offsets[2])
    names(t_eye_dat) <- c('roi', 'onset', 'offset')
    
    #make onset numeric
    t_eye_dat[['onset']] <- as.numeric(t_eye_dat[['onset']])
    t_eye_dat[['offset']] <- as.numeric(t_eye_dat[['offset']])
    
    # look length
    t_eye_dat[['look_dur']] <- t_eye_dat[['offset']] - t_eye_dat[['onset']] 
    
    # look length
    t_eye_dat[['look_dur']] <- t_eye_dat[['offset']] - t_eye_dat[['onset']] 
    
    #sort by timing
    t_eye_dat <- t_eye_dat[order(t_eye_dat[['onset']], decreasing = FALSE), ]
    
    #look number
    t_eye_dat[['look']] <- seq(1, n_looks, 1)
    
    #look latency
    t_eye_dat[['letency_first_look']] <- t_eye_dat[1, 'onset'] - t_dat[['et_record_onset']]
    
    #add trial info
    t_eye_dat[['sub']] <- t_dat[['sub']]
    t_eye_dat <- t_eye_dat[c(7, 5, 6, 1:4)]
    
    return(t_eye_dat)
  }
  
  #add trial information
  eye_dat_long <- sapply(data[['trial']], function() trial_long_fn(t_dat = data[data[['trial']] == x, ], roi_list = roi_list))
  
  return(eye_dat_long)
}
