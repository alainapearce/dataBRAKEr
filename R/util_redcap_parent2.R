#' util_redcap_parent2: Organize parent visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_2_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_2_arm_1
#' @inheritParams util_redcap_prepost1
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent visit 2 datasets
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' parent_visit2_data <- util_redcap_parent2(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_parent2 <- function(data, return_data = TRUE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
  }
  
  #reduce columns and update names
  
  ## CHSQ data
  cshq_data <- data[, grepl('record_id', names(data)) | grepl('cshq', names(data))]
  cshq_data <- cshq_data[, !(names(cshq_data) %in% c('cshq_missingcheck'))]
  names(cshq_data)[1] <- 'participant_id'
  
  cshq_scored <- dataprepr::score_cshq(cshq_data, score_base = FALSE, id = 'participant_id',reverse_score = TRUE)
  cshq_json <- json_cshq()
  
  ## BES Data
  bes_data <- data[, grepl('record_id', names(data)) | grepl('bes', names(data))]
  bes_data <- bes_data[, !(names(bes_data) %in% c('bes_missingcheck'))]
  names(bes_data)[1] <- 'participant_id'
  
  bes_scored <- dataprepr::score_bes(bes_data, score_base = TRUE, id = 'participant_id', pna = 4)
  bes_json <- json_bes()
  
  
  ## FFBS Data
  ffbs_data <- data[, grepl('record_id', names(data)) | grepl('ffbs', names(data))]
  ffbs_data <- ffbs_data[, !(names(ffbs_data) %in% c('ffbs_missingcheck'))]
  names(ffbs_data)[1] <- 'participant_id'
  
  ffbs_scored <- dataprepr::score_ffbs(ffbs_data, score_base = TRUE, id = 'participant_id')
  #ffbs_scored <- score_ffbs(ffbs_data, score_base = TRUE, id = 'participant_id')
  
  ffbs_json <- json_ffbs()
  
  ## HFE Data
  hfe_data <- data[, grepl('record_id', names(data)) | grepl('hfe', names(data))]
  hfe_data <- hfe_data[, !(names(hfe_data) %in% c('hfe_fam_qcheck', 'hfe_track_qcheck', 'hfe_rules_qcheck', 'hfe_available_qcheck', 'hfe_like_qcheck', 'hfe_shop_qcheck', 'hfe_shoploc_qcheck', 'hfe_eatout_qcheck', 'hfe_eatout_qcheck2', 'hfe_neighborhood_qcheck', 'hfe_p1_qcheck', 'hfe_p2_qcheck', 'hfe_p3_qcheck', 'hfe_p4_qcheck', 'hfe_p5_qcheck', 'hfe_p6_qcheck'))]
  names(hfe_data)[1] <- 'participant_id'
  
  #hfe_scored <- dataprepr::score_nik_hfe(hfe_data, score_base = TRUE, id = 'participant_id')
  hfe_scored <- score_nik_hfe(hfe_data, score_base = FALSE, id = 'participant_id')
  
  hfe_json <- json_nki_hfe()
  
  ## SPSRQ Data
  spsrq_data <- data[, grepl('record_id', names(data)) | grepl('spsrq', names(data))]
  spsrq_data <- spsrq_data[, !(names(spsrq_data) %in% c('spsrq_missingcheck'))]
  names(spsrq_data)[1] <- 'participant_id'
  
  spsrq_scored <- dataprepr::score_spsrq(spsrq_data, score_base = TRUE, id = 'participant_id')
  spsrq_json <- json_spsrq()
  
  ## CBQ Data
  cbq_data <- data[, grepl('record_id', names(data)) | grepl('cbq', names(data))]
  cbq_data <- cbq_data[, !(names(cbq_data) %in% c('cbq_missingcheck'))]
  names(cbq_data)[1] <- 'participant_id'
  
  cbq_scores <- dataprepr::score_cbq(cbq_data, score_base = TRUE, id = 'participant_id')
  cbq_json <- NA
  
  ## PWLB Data
  pwlb_data <- data[, grepl('record_id', names(data)) | grepl('pwlb', names(data))]
  pwlb_data <- pwlb_data[, !(names(pwlb_data) %in% c('pwlb_missingcheck'))]
  names(pwlb_data)[1] <- 'participant_id'
  pwlb_json <- NA
  
  ## SCPF Data
  scpf_data <- data[, grepl('record_id', names(data)) | grepl('scpf', names(data))]
  scpf_data <- scpf_data[, !(names(scpf_data) %in% c('scpf_missingcheck'))]
  names(scpf_data)[1] <- 'participant_id'
  scpf_json <- NA
  
  ## FMCB Data
  fmcb_data <- data[, grepl('record_id', names(data)) | grepl('fmcb', names(data))]
  fmcb_data <- fmcb_data[, !(names(fmcb_data) %in% c('fmcb_missingcheck'))]
  names(fmcb_data)[1] <- 'participant_id'
  fmcb_json <- NA
  
  ## TFEQ Data
  tfeq_data <- data[, grepl('record_id', names(data)) | grepl('tfeq', names(data))]
  tfeq_data <- tfeq_data[, !(names(tfeq_data) %in% c('tfeq_missingcheck'))]
  names(tfeq_data)[1] <- 'participant_id'
  tfeq_json <- NA
  
  if (isTRUE(return_data)){
    return(list(
      cshq_data = list(data = cshq_scored, meta = cshq_json),
      bes_data = list(data = bes_scored, meta = bes_json),
      ffbs_data = list(data = ffbs_scored, meta = ffbs_json),
      hfe_data = list(data = hfe_scored, meta = hfe_json), 
      spsrq_data = list(data = spsrq_scored, meta = spsrq_json), 
      cbq_data = list(data = cbq_data, meta = cbq_json), 
      pwlb_data = list(data = pwlb_data, meta = pwlb_json), 
      scpf_data = list(data = scpf_data, meta = scpf_json), 
      fmcb_data = list(data = fmcb_data, meta = fmcb_json), 
      tfeq_data = list(data = tfeq_data, meta = tfeq_json)))
  }
}

