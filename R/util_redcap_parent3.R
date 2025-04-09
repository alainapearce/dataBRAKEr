#' util_redcap_parent3: Organize parent visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @inheritParams util_redcap_prepost1
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' parent_visit3_data <- util_redcap_parent3(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_parent3 <- function(data, return_data = TRUE) {
  
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
  
  ## demographics data ####
  household_data <- data[, grepl('record_id', names(data)) | grepl('demo', names(data))]
  household_data <- household_data[, !(names(household_data) %in% c('demographics_timestamp', 'demo_v1_missingcheck', 'demographics_complete', 'parent_household_demographics_questionnaire_timestamp', 'demo_missingcheck', 'demo_missingcheck_2', 'demo_missingcheck_3', 'parent_household_demographics_questionnaire_complete'))]
  names(household_data)[1] <- 'participant_id'
  #names(household_data)[??] <- 'demo_mom_ed'
  
  demo_data <- household_data[c('participant_id', 'demo_income', 'demo_mod_ed', 'demo_dad_ed')]

  ## Sleep Week data ####
  sleep_data <- data[c('record_id', 'date_mon', 'date_tu', 'date_wed', 'date_th', 'date_fri', 'date_sat', 'date_sun', 'bedtime_mon', 'bedtime_tu', 'bedtime_wed', 'bedtime_th', 'bedtime_fri', 'bedtime_sat', 'bedtime_sun', 'attempt_mon', 'attempt_tu', 'attempt_wed', 'attempt_th', 'attempt_fri', 'attempt_sat', 'attempt_sun', 'asleep_mon', 'asleep_tu', 'asleep_wed', 'asleep_th', 'asleep_fri', 'asleep_sat', 'asleep_sun', 'times_mon', 'times_tu', 'times_wed', 'times_th', 'times_fri', 'times_sat', 'times_sun', 'waso_mon', 'waso_tu', 'waso_wed', 'waso_th', 'waso_fri', 'waso_sat', 'waso_sun', 'awake_mon', 'awake_tu', 'awake_wed', 'awake_th', 'awake_fri', 'awake_sat', 'awake_sun', 'out_on_mon', 'out_on_tu', 'out_on_wed', 'out_on_th', 'out_on_fri', 'out_on_sat', 'out_on_sun')]
  names(sleep_data)[1] <- 'participant_id'
  
  sleep_wk_scored <- dataprepr::score_sleeplog(sleep_data, id = 'participant_id', summer_start = '2024-06-06', summer_end = '2024-08-23')
  child_sleep_json <- json_sleeplog()
  
  ## HFI data ####
  hfi_data <- data[, grepl('record_id', names(data)) | grepl('hfi', names(data))] 
  hfi_data <- hfi_data[, !grepl('qcheck', names(hfi_data))]
  names(hfi_data)[1] <- 'participant_id'
  
  names(hfi_data) <- gsub('___', '', names(hfi_data))
  names(hfi_data) <- gsub('visible', 'accesible', names(hfi_data))
  
  hfi_scored <- dataprepr::score_hfi(hfi_data, id = 'participant_id', base_zero = TRUE)
  hfi_json <- json_hfi()
  
  ## Puberty Data ####
  puberty_data <- data[, grepl('record_id', names(data)) | grepl('pds', names(data))]
  names(puberty_data)[1] <- 'participant_id'

  names(puberty_data) <- gsub('pds_sex', 'sex', names(puberty_data))
  
  #re-code sex to match demo_c_sex
  puberty_data[['sex']] <- ifelse(puberty_data[['sex']] == 0, 1, 0)

  puberty_scored <- dataprepr::score_pds(puberty_data, base_zero = FALSE, respondent = 'parent', male = 0, female = 1, id = 'participant_id')
  puberty_json <- json_pds()
  
  ## CFQ Data ####
  cfq_data <- data[, grepl('record_id', names(data)) | grepl('cfq', names(data))]
  cfq_data <- cfq_data[, !(names(cfq_data) %in% c('cfq_resp_missingcheck', 'cfq_pwc_missingcheck', 'cfq_cwc_missingcheck', 'cfq_conc_missingcheck', 'cfq_presrest_missingcheck', 'cfq_mon_missingcheck'))]
  names(cfq_data)[1] <- 'participant_id'
  
  
  cfq_scored <- dataprepr::score_cfq(cfq_data, pcw_na_value = 5, base_zero = TRUE, restriction_split = FALSE, id = 'participant_id')
  cfq_json <- json_cfq()
  
  ## EFCR Data ####
  efcr_data <- data[, grepl('record_id', names(data)) | grepl('efcr', names(data))]
  efcr_data <- efcr_data[, !(names(efcr_data) %in% c('efcr_missingcheck'))]
  names(efcr_data)[1] <- 'participant_id'
  
  efcr_scored <- dataprepr::score_efcr(efcr_data, base_zero = FALSE, id = 'participant_id')
  efcr_json <- json_efcr()
  
  ## FFQ Data ####
  ffq_data <- data[, grepl('record_id', names(data)) | grepl('ffq', names(data))]
  ffq_data <- ffq_data[, !(names(ffq_data) %in% c('ffq1_check', 'ffq2_check', 'ffq3_check', 'ffq4_check', 'ffq5_check', 'ffq_sup_check'))]
  
  names(ffq_data)[1:44] <- c('participant_id', 'ffq_dairy1', 'ffq_dairy2', 'ffq_dairy3', 'ffq_dairy4', 'ffq_egg1', 'ffq_meat1', 'ffq_meat2', 'ffq_meat3', 'ffq_meat4', 'ffq_fish1', 'ffq_fish2', 'ffq_fish3', 'ffq_fish4', 'ffq_dairy5', 'ffq_veg1', 'ffq_veg2', 'ffq_potato1', 'ffq_legume1', 'ffq_potato2', 'ffq_fruit1', 'ffq_fruit2', 'ffq_nuts1', 'ffq_fruit3', 'ffq_fruit4', 'ffq_cereal1', 'ffq_cereal2', 'ffq_cereal3', 'ffq_cereal4', 'ffq_cereal5', 'ffq_cereal6', 'ffq_bakery1', 'ffq_bakery2', 'ffq_sweet1', 'ffq_sweet2', 'ffq_sweet3', 'ffq_bev1', 'ffq_bev2', 'ffq_fats1', 'ffq_fats2', 'ffq_fats3', 'ffq_fats4', 'ffq_dressing1', 'ffq_saltysnack1')
  
  ffq_scored <- dataprepr::score_ffq_helix(ffq_data, base_zero = TRUE, id = 'participant_id')
  ffq_json <- json_ffq_helix()
  
  ## CSHQ ####
  cshq_data <- data[, grepl('record_id', names(data)) | grepl('cshq', names(data))]
  cshq_data <- cshq_data[, !(names(cshq_data) %in% c('cshq_missingcheck'))]
  names(cshq_data)[1] <- 'participant_id'
  
  cshq_scored <- dataprepr::score_cshq(cshq_data, base_zero = FALSE, id = 'participant_id',reverse_score = TRUE)
  cshq_json <- json_cshq()
  
  ## BES ####
  bes_data <- data[, grepl('record_id', names(data)) | grepl('bes', names(data))]
  bes_data <- bes_data[, !(names(bes_data) %in% c('bes_missingcheck'))]
  names(bes_data)[1] <- 'participant_id'
  
  bes_scored <- dataprepr::score_bes(bes_data, base_zero = TRUE, id = 'participant_id', pna = 4)
  bes_json <- json_bes()
  
  ## HFE Data ####
  hfe_data <- data[, grepl('record_id', names(data)) | grepl('hfe', names(data))]
  hfe_data <- hfe_data[, !(names(hfe_data) %in% c('hfe_fam_qcheck', 'hfe_track_qcheck', 'hfe_rules_qcheck', 'hfe_available_qcheck', 'hfe_like_qcheck', 'hfe_shop_qcheck', 'hfe_shoploc_qcheck', 'hfe_eatout_qcheck', 'hfe_eatout_qcheck2', 'hfe_neighborhood_qcheck', 'hfe_p1_qcheck', 'hfe_p2_qcheck', 'hfe_p3_qcheck', 'hfe_p4_qcheck', 'hfe_p5_qcheck', 'hfe_p6_qcheck'))]
  names(hfe_data)[1] <- 'participant_id'
  
  hfe_scored <- dataprepr::score_nik_hfe(hfe_data, base_zero = FALSE, id = 'participant_id')

  hfe_json <- json_nki_hfe()
  
  ## PWLB ####
  pwlb_data <- data[, grepl('record_id', names(data)) | grepl('pwlb', names(data))]
  pwlb_data <- pwlb_data[, !(names(pwlb_data) %in% c('pwlb_missingcheck'))]
  names(pwlb_data)[1] <- 'participant_id'
  
  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, base_zero = TRUE, id = 'participant_id')
  
  pwlb_json <- NA
  
  
  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      demo_data = list(data = demo_data),
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
}

