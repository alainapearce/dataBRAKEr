#' util_redcap_parent2: Organize parent visit 2 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_2_arm_1
#'
#' @param data data from REDCap event parent_visit_2_arm_1
#' @inheritParams util_redcap_child1
#' 
#' 
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw parent visit 2 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{cshq_data}
#'    \item{bes_data}
#'    \item{ffbs_data}
#'    \item{hfe_data}
#'    \item{spsrq_data}
#'    \item{cbq_data}
#'    \item{pwlb_data}
#'    \item{scpf_data}
#'    \item{fmcb_data}
#'    \item{tfeq_data}
#'  }
#'  
#'  
#' @examples
#'
#' # process REDCap data
#' parent_visit2_list <- util_redcap_parent2(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap(), util_redcap_dates()]
#'
#' @export
#' 

util_redcap_parent2 <- function(data, date_data) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop('data must be a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('child data for REDCap event child_visit_1_arm_1 must be entered as a data.frame')
  }
  
  date_data_arg <- methods::hasArg(date_data)
  
  if (isTRUE(date_data_arg)) {
    if (!is.data.frame(date_data)) {
      stop('date_data must be a data.frame')
    }
  } else if (isFALSE(date_data_arg)) {
    stop('date_data from util_redcap_dates() must be entered as a data.frame')
  }
  
  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'
  
  # add session column
  data['session_id'] <- 'ses-baseline'
  
  # add visit number
  data['visit_protocol'] <- 2
  
  # merge with date_data
  data <- merge(data, date_data[c('participant_id', 'v2_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v2_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])
  
  
  #reduce columns and update names
  
  ## CHSQ data ####
  cshq_data <- data[grepl('_id|^visit|cshq', names(data))]
  
  # remove extra columns, add columns, and re-order
  cshq_data <- cshq_data[!grepl('check', names(cshq_data))]
  
  cshq_data <- cshq_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(cshq_data)[grepl('cshq', names(cshq_data))])]
  
  # process data
  cshq_scored <- dataprepr::score_cshq(cshq_data, base_zero = FALSE, id = 'participant_id', reverse_score = TRUE)
  
  cshq_json <- json_cshq()
  
  ## BES Data ####
  bes_data <- data[grepl('_id|^visit|bes', names(data))]
  
  # remove extra columns, add columns, and re-order
  bes_data <- bes_data[!grepl('check', names(bes_data))]
  
  bes_data <- bes_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(bes_data)[grepl('bes', names(bes_data))])]
  
  # process data
  bes_scored <- dataprepr::score_bes(bes_data, base_zero = TRUE, id = 'participant_id', pna = 4)
  
  bes_json <- json_bes()
  
  
  ## FFBS Data ####
  ffbs_data <- data[grepl('_id|^visit|ffbs', names(data))]
  
  # remove extra columns, add columns, and re-order
  ffbs_data <- ffbs_data[!grepl('check', names(ffbs_data))]
  
  ffbs_data <- ffbs_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(ffbs_data)[grepl('ffbs', names(ffbs_data))])]
  
  # process data
  ffbs_scored <- dataprepr::score_ffbs(ffbs_data, base_zero = TRUE, id = 'participant_id')
  
  ffbs_json <- json_ffbs()
  
  ## HFE Data ####
  hfe_data <- data[grepl('_id|^visit|hfe', names(data))]
  
  # remove extra columns, add columns, and re-order
  hfe_data <- hfe_data[!grepl('check', names(hfe_data))]
  
  hfe_data <- hfe_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(hfe_data)[grepl('hfe', names(hfe_data))])]
  
  # process data
  hfe_scored <- dataprepr::score_nik_hfe(hfe_data, base_zero = FALSE, id = 'participant_id')
  
  hfe_json <- json_nki_hfe()
  
  ## SPSRQ Data ####
  spsrq_data <- data[grepl('_id|^visit|spsrq', names(data))]
  
  # remove extra columns, add columns, and re-order
  spsrq_data <- spsrq_data[!grepl('check', names(spsrq_data))]
  
  spsrq_data <- spsrq_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(spsrq_data)[grepl('spsrq', names(spsrq_data))])]
  
  # process data
  spsrq_scored <- dataprepr::score_spsrq(spsrq_data, base_zero = TRUE, id = 'participant_id')
  
  spsrq_json <- json_spsrq()
  
  ## CBQ Data ####
  cbq_data <- data[grepl('_id|^visit|cbq', names(data))]
  
  # remove extra columns, add columns, and re-order
  cbq_data <- cbq_data[!grepl('check', names(cbq_data))]
  
  cbq_data <- cbq_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(cbq_data)[grepl('cbq', names(cbq_data))])]
  
  # process data
  cbq_scored <- dataprepr::score_cbq(cbq_data, pna_value = 99, base_zero = FALSE, id = 'participant_id')
  
  cbq_json <- json_cbq()
  
  ## PWLB Data ####
  pwlb_data <- data[grepl('_id|^visit|pwlb', names(data))]
  
  # remove extra columns, add columns, and re-order
  pwlb_data <- pwlb_data[!grepl('check', names(pwlb_data))]
  
  pwlb_data <- pwlb_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(pwlb_data)[grepl('pwlb', names(pwlb_data))])]
  
  # process data
  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, base_zero = TRUE, id = 'participant_id')
  
  pwlb_json <- json_pwlb()
  
  ## SCPF Data ####
  scpf_data <- data[grepl('_id|^visit|scpf', names(data))]
  
  # remove extra columns, add columns, and re-order
  scpf_data <- scpf_data[!grepl('check', names(scpf_data))]
  
  scpf_data <- scpf_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(scpf_data)[grepl('scpf', names(scpf_data))])]
  
  # process data
  scpf_scored <- dataprepr::score_scpf(scpf_data, base_zero = TRUE, id = 'participant_id')
  
  scpf_json <- json_scpf()
  
  ## FMCB Data ####
  fmcb_data <- data[grepl('_id|^visit|fmcb', names(data))]
  
  # remove extra columns, add columns, and re-order
  fmcb_data <- fmcb_data[!grepl('check', names(fmcb_data))]
  
  fmcb_data <- fmcb_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(fmcb_data)[grepl('fmcb', names(fmcb_data))])]
  
  # process data
  fmcb_scored <- dataprepr::score_fmcb(fmcb_data, base_zero = TRUE, id = 'participant_id')
  
  fmcb_json <- json_fmcb()
  
  ## TFEQ Data ####
  tfeq_data <- data[grepl('_id|^visit|tfeq', names(data))]
  
  # remove extra columns, add columns, and re-order
  tfeq_data <- tfeq_data[!grepl('check', names(tfeq_data))]
  
  tfeq_data <- tfeq_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(tfeq_data)[grepl('tfeq', names(tfeq_data))])]
  
  # process data
  tfeq_scored <- dataprepr::score_tfeq18(tfeq_data, base_zero = TRUE, id = 'participant_id')
  
  tfeq_json <- json_tfeq()
  
  return(list(
    cshq_data = list(data = cshq_scored, meta = cshq_json),
    bes_data = list(data = bes_scored, meta = bes_json),
    ffbs_data = list(data = ffbs_scored, meta = ffbs_json),
    hfe_data = list(data = hfe_scored, meta = hfe_json), 
    spsrq_data = list(data = spsrq_scored, meta = spsrq_json), 
    cbq_data = list(data = cbq_scored, meta = cbq_json), 
    pwlb_data = list(data = pwlb_scored, meta = pwlb_json), 
    scpf_data = list(data = scpf_scored, meta = scpf_json), 
    fmcb_data = list(data = fmcb_scored, meta = fmcb_json), 
    tfeq_data = list(data = tfeq_scored, meta = tfeq_json)))
  
}

