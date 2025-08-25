#' #' util_redcap_parent3: Organize parent visit 3 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_3_arm_1
#'
#' @param data data from REDCap event parent_visit_3_arm_1
#' @inheritParams util_redcap_child1
#' 
#' 
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw parent visit 3 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{household_data}
#'    \item{sleep_data}
#'    \item{hfi_data}
#'    \item{puberty_data}
#'    \item{cfq_data}
#'    \item{efcr_data}
#'    \item{ffq_data}
#'    \item{cshq_data}
#'    \item{bes_data}
#'    \item{hfe_data}
#'    \item{pwlb_data}
#'  }
#'  
#'  
#' @examples
#'
#' # process REDCap data
#' parent_visit3_list <- util_redcap_parent3(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap(), util_redcap_dates()]
#'
#'
#' @export

util_redcap_parent3 <- function(data, return_data = TRUE) {
  
  #### 1. Set up/initial checks #####
  
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
  data['session_id'] <- 'ses-followup'
  
  # add visit number
  data['visit_protocol'] <- 3
  
  # merge with date_data
  data <- merge(data, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v3_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])
  
  
  #reduce columns and update names
  
  ## demographics data ####
  # this data will be split into 3 dataframes:
  # (1) demo_data: data collected as part of the 'Visit 1 Demographics' qualtrics form that will go into participants.tsv (or demographics.tsv) file
  # (2) infancy_data: data collected as part of the 'Visit 1 Demographics' qualtrics form that will go into infancy.tsv file
  # (3) household_data: data collected as part of the 'Parent Household Demographics' qualtrics form
  
  # select all demo variables
  demo_data_all <- data[grepl('_id|^visit|demo', names(data))]
  
  # remove extra columns, add columns, and re-order
  demo_data_all <- demo_data_all[!grepl('missingcheck', names(demo_data_all))]
  names(demo_data_all)[names(demo_data_all) == 'demo_mod_ed'] <- 'demo_mom_ed'
  
  demo_data_all <- demo_data_all[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(demo_data_all)[grepl('demo', names(demo_data_all))])]
  
  # process data
  household_data <- util_format_household_data(demo_data_all)
  
  names(household_data) <- gsub('work_hours', 'workhours', names(household_data))
  
  household_json <- json_household()
  
  ## Sleep Week data ####
  sleep_data <- data[grepl('_id|^visit|_mon|_tu|_wed|_th|_fri|_sat|_sun', names(data))]
  
  # remove extra columns, add columns, and re-order
  sleep_data <- sleep_data[!grepl('hfi', names(sleep_data))]
  
  sleep_data <- sleep_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(sleep_data)[!grepl('_id|^visit', names(sleep_data))])]
  
  # process data
  sleep_wk_scored <- dataprepr::score_sleeplog(sleep_data, id = 'participant_id', summer_start = '2024-06-06', summer_end = '2024-08-23')
  
  child_sleep_json <- json_sleeplog()
  
  ## HFI data ####
  hfi_data <- data[grepl('_id|^visit|hfi', names(data))]
  
  # remove extra columns, add columns, and re-order
  hfi_data <- hfi_data[!grepl('check', names(hfi_data))]
  
  hfi_data <- hfi_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(hfi_data)[!grepl('_id|^visit', names(hfi_data))])]
  
  # process data
  names(hfi_data) <- gsub('___', '', names(hfi_data))
  names(hfi_data) <- gsub('visible', 'accesible', names(hfi_data))
  
  hfi_scored <- dataprepr::score_hfi(hfi_data, id = 'participant_id', base_zero = TRUE)
  
  hfi_json <- json_hfi()
  
  ## Puberty Data ####
  puberty_data <- data[grepl('_id|^visit|pds|tanner', names(data))]
  
  # process puberty data
  puberty_data <- util_format_puberty_data(puberty_data)
  
  puberty_data <- puberty_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'sex', names(puberty_data)[grepl('pds|tanner_choice', names(puberty_data))])]
  
  # process data
  names(puberty_data) <- gsub('pds_sex', 'sex', names(puberty_data))
  
  #re-code sex to match demo_c_sex
  puberty_data[['sex']] <- ifelse(puberty_data[['sex']] == 0, 1, 0)
  
  puberty_scored <- dataprepr::score_pds(puberty_data, base_zero = FALSE, respondent = 'parent', male = 0, female = 1, id = 'participant_id')
  
  puberty_json <- json_pds()
  
  ## CFQ Data ####
  cfq_data <- data[grepl('_id|^visit|cfq', names(data))]
  
  # remove extra columns, add columns, and re-order
  cfq_data <- cfq_data[!grepl('check', names(cfq_data))]
  
  cfq_data <- cfq_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(cfq_data)[!grepl('_id|^visit', names(cfq_data))])]
  
  # process data
  cfq_scored <- dataprepr::score_cfq(cfq_data, pcw_na_value = 5, base_zero = TRUE, restriction_split = FALSE, id = 'participant_id')
  
  cfq_json <- json_cfq()
  
  ## EFCR Data ####
  efcr_data <- data[grepl('_id|^visit|efcr', names(data))]
  
  # remove extra columns, add columns, and re-order
  efcr_data <- efcr_data[!grepl('check', names(efcr_data))]
  
  efcr_data <- efcr_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(efcr_data)[!grepl('_id|^visit', names(efcr_data))])]
  
  # process data
  efcr_scored <- dataprepr::score_efcr(efcr_data, base_zero = FALSE, id = 'participant_id')
  
  efcr_json <- json_efcr()
  
  ## FFQ Data ####
  ffq_data <- data[grepl('_id|^visit|ffq', names(data))]
  
  # remove extra columns, add columns, and re-order
  ffq_data <- ffq_data[!grepl('check', names(ffq_data))]
  
  ffq_data <- ffq_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(ffq_data)[!grepl('_id|^visit', names(ffq_data))])]
  
  # process data
  ffq_data <- util_format_ffq_data(ffq_data)
  
  ffq_scored <- dataprepr::score_ffq_helix(ffq_data, base_zero = TRUE, id = 'participant_id')
  
  ffq_json <- json_ffq_helix()
  
  ## CSHQ ####
  cshq_data <- data[grepl('_id|^visit|cshq', names(data))]
  
  # remove extra columns, add columns, and re-order
  cshq_data <- cshq_data[!grepl('check', names(cshq_data))]
  
  cshq_data <- cshq_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(cshq_data)[!grepl('_id|^visit', names(cshq_data))])]
  
  # process data
  cshq_scored <- dataprepr::score_cshq(cshq_data, base_zero = FALSE, id = 'participant_id',reverse_score = TRUE)
  
  cshq_json <- json_cshq()
  
  ## BES ####
  bes_data <- data[grepl('_id|^visit|bes', names(data))]
  
  # remove extra columns, add columns, and re-order
  bes_data <- bes_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(bes_data)[!grepl('_id|^visit', names(bes_data))])]
  
  # process data
  bes_scored <- dataprepr::score_bes(bes_data, base_zero = TRUE, id = 'participant_id', pna = 4)
  
  bes_json <- json_bes()
  
  ## HFE Data ####
  hfe_data <- data[grepl('_id|^visit|hfe', names(data))]
  
  # remove extra columns, add columns, and re-order
  hfe_data <- hfe_data[!grepl('check', names(hfe_data))]
  
  hfe_data <- hfe_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(hfe_data)[!grepl('_id|^visit', names(hfe_data))])]
  
  # process data
  hfe_scored <- dataprepr::score_nik_hfe(hfe_data, base_zero = FALSE, id = 'participant_id')
  
  hfe_json <- json_nki_hfe()
  
  ## PWLB ####
  pwlb_data <- data[grepl('_id|^visit|pwlb', names(data))]
  
  # remove extra columns, add columns, and re-order
  pwlb_data <- pwlb_data[!grepl('check', names(pwlb_data))]
  
  pwlb_data <- pwlb_data[c('participant_id', 'session_id', 'visit_date', 'visit_protocol', names(pwlb_data)[!grepl('_id|^visit', names(pwlb_data))])]
  
  # process data
  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, base_zero = TRUE, id = 'participant_id')
  
  pwlb_json <- json_pwlb()
  
  ## compile and return data ####
  return(list(
    household_data = list(data = household_data, meta = household_json),
    sleep_data = list(data = sleep_wk_scored, meta = child_sleep_json),
    hfi_data = list(data = hfi_scored, meta = hfi_json),
    puberty_data = list(data = puberty_scored, meta = puberty_json),
    cfq_data = list(data = cfq_scored, meta = cfq_json),
    efcr_data = list(data = efcr_scored, meta = efcr_json),
    ffq_data = list(data = ffq_scored, meta = ffq_json),
    cshq_data = list(data = cshq_scored, meta = cshq_json),
    bes_data = list(data = bes_scored, meta = bes_json),
    hfe_data = list(data = hfe_scored, meta = hfe_json),
    pwlb_data = list(data = pwlb_scored, meta = pwlb_json)
  ))
}

