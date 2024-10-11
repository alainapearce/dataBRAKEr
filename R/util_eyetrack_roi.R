#' util_eyetrack_roi: Process raw data from ROI-based eye-tracking data from PscyhoPy for a single participant
#'
#' This function cleans ROI-based eye-tracking data from PscyhoPy for a single participant
#'
#' To use this function, the data must include the following: trial number, number of looks per ROI for a trial, and the onset/offset times for each ROI look for a trial
#'
#' @param data a data.frame with roi-based eye-tracking
#' @param roi_list a list with character string labels for each ROI (c('left', 'right')). These strings must be the prefix for variable names related to the rois (e.g., 'right_looks', 'right_look_onsets', 'right_look_offsets')
#' @inheritParams util_task_foodrating
#'
#' @return If return_data is set to TRUE, will return a long dataset with a row per ROI look
#'
#' @examples
#'
#' # process eye-tracking data 
#' eyetrack_roi_dat <- util_eyetrack_roi(data, roi_list = c('right', 'left'), return = TRUE)
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
  look_matrix <- function(t_dat, roi_str, var){
    
    #decompose onset string
    var_str <- t_dat[[paste0(roi_str, '_look_', var)]]
    n_looks_roi <- t_dat[[paste0(roi_str, '_looks')]]
    
    if (!is.na(var_str)){
      look_list <- gsub('\\[|\\]| ', '', unlist(as.list(strsplit(var_str, ','))))
      look_mat <- matrix(c(rep(roi_str, n_looks_roi), look_list), ncol = 2, byrow = FALSE)
    }
  }
  
  trial_long_fn <- function(t_dat, roi_list){
    #debug
    #print(t_dat$trial)
    
    #look number
    n_looks <- sapply(roi_list, function(x) as.numeric(t_dat[[paste0(x, '_looks')]]), simplify = TRUE)
    
    if (sum(n_looks) == 0){
      no_looks = TRUE
    } else {
      no_looks = FALSE
    }
    
    # get onsets and offsets paired with roi
    if (sum(n_looks == 0) == length(roi_list)){
      # if no rois have looks
      t_eye_onsets = data.frame(matrix(c(NaN, NaN), byrow = TRUE, ncol = 2))
      t_eye_offsets = data.frame(matrix(c(NaN, NaN), byrow = TRUE, ncol = 2))
    } else if (sum(n_looks == 0) > 0){
      # if only looked at 1 ROI
      t_eye_onsets <- data.frame(do.call(rbind, sapply(roi_list, function(x) look_matrix(t_dat, x, 'onsets'), simplify = TRUE)))
      
      t_eye_offsets <- data.frame(do.call(rbind, sapply(roi_list, function(x) look_matrix(t_dat, x, 'offsets'), simplify = TRUE)))
      
    } else if (sum(n_looks == 1) == length(roi_list)){
      # if both only have 1 look
      
      t_eye_onsets <- rbind.data.frame(t(sapply(roi_list, function(x) look_matrix(t_dat, x, 'onsets'), simplify = TRUE)))
      
      t_eye_offsets <- rbind.data.frame(t(sapply(roi_list, function(x) look_matrix(t_dat, x, 'offsets'), simplify = TRUE)))
      
    } else if (sum(n_looks > 1) >= 1) {
      #if both rois have at least 1 look and 1 roi has > 1 look
      t_eye_onsets <- data.frame(do.call(rbind, sapply(roi_list, function(x) look_matrix(t_dat, x, 'onsets'), simplify = FALSE)))
      
      t_eye_offsets <- data.frame(do.call(rbind, sapply(roi_list, function(x) look_matrix(t_dat, x, 'offsets'), simplify = FALSE)))
    }
    
    #make data.frame
    t_eye_dat <- cbind.data.frame(t_eye_onsets, t_eye_offsets[2])
    names(t_eye_dat) <- c('roi', 'look_onset', 'look_offset')
    
    #make onset numeric
    t_eye_dat[['look_onset']] <- as.numeric(t_eye_dat[['look_onset']])
    t_eye_dat[['look_offset']] <- as.numeric(t_eye_dat[['look_offset']])

    # look length
    t_eye_dat[['look_dur']] <- t_eye_dat[['look_offset']] - t_eye_dat[['look_onset']] 
    
    #sort by timing
    t_eye_dat <- t_eye_dat[order(t_eye_dat[['look_onset']], decreasing = FALSE), ]
    
    #look number
    if (isTRUE(no_looks)){
      t_eye_dat[['look']] <- NaN
    } else {
      t_eye_dat[['look']] <- c(seq(1, sum(n_looks), 1))
    }
    
    #look latency
    t_eye_dat[['letency_first_look']] <- t_eye_dat[1, 'look_onset'] - t_dat[['et_record_onset']]
    
    #add trial info
    t_eye_dat[['sub']] <- t_dat[['sub']]
    t_eye_dat[['trial']] <- t_dat[['trial']]
    t_eye_dat[['n_looks']] <- sum(n_looks)
    t_eye_dat <- t_eye_dat[c(7:9, 5, 6, 1:4)]
    
    #remove row.names
    row.names(t_eye_dat) <- NULL
    return(t_eye_dat)
  }
  
  
  #add trial information
  eye_dat_long <- data.frame(do.call(rbind, sapply(data[['trial']], function(x) trial_long_fn(t_dat = data[data[['trial']] == x, ], roi_list = c(roi_list)), simplify = FALSE)))
  
  return(eye_dat_long)
}
