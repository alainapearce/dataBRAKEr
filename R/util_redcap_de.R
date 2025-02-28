#' util_redcap_de: Organize double-entry data from REDCap
#'
#' This function organizes double-entry REDCap data for all visits
#'
#'
#' @param data data from REDCap data double entry
#' @inheritParams util_redcap_prepost1
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) double-entered raw data across all visits
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' data_de_proc <- util_redcap_de(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_de <- function(data, return_data = TRUE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_1_arm_1 must be entered as a data.frame")
  }
  
  #reduce columns and update names
  
  ## bod pod ####
  bod_pod_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('bodpod', names(redcap_de_data))]
  names(bod_pod_data) <- c('participant_id', 'baseline_bodpod_collect', 'baseline_bodpod_no', 'baseline_bodpod_date', 'baseline_fat_p', 'bodpod_fatfree_p', 'baseline_fat_kg', 'baseline_fatfree_kg', 'baseline_bodymass_kg', 'baseline_bodyvol_l', 'baseline_bodydensity', 'baseline_thoracic_gasvol_l', 'baseline_bodpod_complete', 'followup_bodpod_collect', 'followup_bodpod_no3', 'followup_bodpod_date', 'followup_fat_p', 'followup_fatfree_p', 'followup_fat_kg', 'followup_fatfree_kg', 'followup_bodymass_kg', 'followup_bodyvol_l', 'followup_bodydensity', 'followup_thoracic_gasvol_l', 'followup_bodpod_complete')
  
  bod_pod_data <- bod_pod_data[, !(grepl('complete', names(bod_pod_data)))]
  
  ## wasi ####
  wasi_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('wasi', names(redcap_de_data))]
  names(wasi_data)[c(1, 16:17)] <- c('participant_id', 'wasi_pri_p', 'wasi_fsiq_p')
  wasi_data <- wasi_data[, !(grepl('complete', names(wasi_data)))]

  ## dkefs ####
  dkefs_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('dkefs', names(redcap_de_data))]
  names(dkefs_data)[c(1)] <- c('participant_id')
  dkefs_data <- dkefs_data[, !(grepl('complete', names(dkefs_data)))]
  
  ## intake data ####
  intake_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('_g', names(redcap_de_data)) | grepl('_kcal', names(redcap_de_data)) | grepl('meal', names(redcap_de_data)) | grepl('eah', names(redcap_de_data))]
  
  intake_data <- intake_data[, !grepl('cams', names(intake_data)) & !grepl('complete', names(intake_data)) & !grepl('tt', names(intake_data)) & !grepl('bodpod', names(intake_data))]
  
  ## baseline
  baseline_intake_data <- intake_data[, !grepl('v3', names(intake_data))]
  
  #make meal and eah consistant in naming
  names(baseline_intake_data)[grepl('intake', names(baseline_intake_data))] <- sapply(names(baseline_intake_data[grepl('intake', names(baseline_intake_data))]), function(x) gsub('intake_', '', x))
  
  names(baseline_intake_data)[1] <- 'participant_id'
  names(baseline_intake_data)[c(2:32, 83:88)] <- paste0('baseline_', names(baseline_intake_data)[c(2:32, 83:88)])
  
  ## follow-up
  followup_intake_data <- intake_data[,  grepl('record_id', names(intake_data)) | grepl('v3', names(intake_data))]
  names(followup_intake_data) <- sapply(names(followup_intake_data), function(x) gsub('_v3', '', x))
  
  names(followup_intake_data)[1] <- 'participant_id'
  names(followup_intake_data)[c(2:32, 37:40)] <- paste0('followup_', names(followup_intake_data)[c(2:32, 37:40)])
  
  ## taste-test data ###
  tt_data <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('tt_', names(redcap_de_data))]
  
  tt_data <- tt_data[, !(grepl('collect', names(tt_data))) & !(grepl('complete', names(tt_data)))]
  
  names(tt_data)[1] <- 'participant_id'
  
  ## cams data ###
  cams <- redcap_de_data[, grepl('record_id', names(redcap_de_data)) | grepl('cams', names(redcap_de_data))]
  
  #baseline
  baseline_cams <- cams[, !(grepl('v3', names(cams))) & !(grepl('visit_3', names(cams))) & !(grepl('complete', names(cams))) & !(grepl('collect', names(cams)))]
  
  names(baseline_cams)[1] <- 'participant_id'

  #follow-up
  followup_cams <- cams[, grepl('record_id', names(cams)) | grepl('v3', names(cams))]
  followup_cams <- followup_cams[, !grepl('collect', names(followup_cams))]
  
  names(followup_cams)[1] <- 'participant_id'
  names(followup_cams) <- sapply(names(followup_cams), function(x) gsub('_v3', '', x))
  
  names(followup_cams)[2:9] <- c('cams_prefnirs_premeal_no', 'cams_prefnirs_premeal', 'cams_postfnirs_premeal_no', 'cams_postfnirs_premeal', 'cams_prefnirs_postmeal_no', 'cams_prefnirs_postmeal', 'cams_postfnirs_postmeal_no', 'cams_postfnirs_postmeal')
  
  ## NonWear Log data
  nonwear_log_data <- data[, grepl('participant_id', names(data)) | grepl('nonwear', names(data))] 
  nonwear_log_data <- nonwear_log_data[, !(grepl('complete', names(nonwear_log_data)))]
  
  names(nonwear_log_data)[1] <- 'participant_id'

  if (isTRUE(return_data)){
    return(list(
      bodpod = list(data = bod_pod_data), 
      wasi = list(data = wasi_data),
      dkefs = list(data = dkefs_data), 
      baseline_intake = list(data = baseline_intake_data), 
      followup_intake = list(data = followup_intake_data),
      taste_test = list(data = tt_data),
      baseline_cams = list(data = baseline_cams),
      followup_cams = list(data = followup_cams)))
  }
}

