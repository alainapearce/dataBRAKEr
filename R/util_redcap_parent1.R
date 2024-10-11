#' util_redcap_parent1: Organize parent visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @param v1_date_data dataset that includes ID variable and date of first visit
#' @inheritParams util_redcap_prepost1
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'
#' @examples
#'
#' # process REDCap data
#' parent_visit1_data <- util_redcap_parent1(data)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_redcap_parent1 <- function(data, v1_date_data, return_data = TRUE) {
  
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
  
  demo_data <- household_data[c('participant_id', 'demo_c_dob', 'demo_c_sex', 'demo_race', 'demo_ethnicity', 'demo_child_other_race', 'demo_income', 'demo_mod_ed', 'demo_dad_ed')]

  ## Puberty Data ####
  puberty_data <- data[, grepl('record_id', names(data)) | grepl('pds', names(data))]
  names(puberty_data)[1] <- 'participant_id'
  names(puberty_data) <- gsub('pds_sex', 'sex', names(puberty_data))
  
  #re-code sex to match demo_c_sex
  puberty_data[['sex']] <- ifelse(puberty_data[['sex']] == 0, 1, 0)

  puberty_scored <- dataprepr::score_pds(puberty_data, score_base = FALSE, respondent = 'parent', male = 0, female = 1, id = 'participant_id')
  puberty_json <- json_pds()
  
  ## CFQ Data ####
  cfq_data <- data[, grepl('record_id', names(data)) | grepl('cfq', names(data))]
  cfq_data <- cfq_data[, !(names(cfq_data) %in% c('cfq_resp_missingcheck', 'cfq_pwc_missingcheck', 'cfq_cwc_missingcheck', 'cfq_conc_missingcheck', 'cfq_presrest_missingcheck', 'cfq_mon_missingcheck'))]
  names(cfq_data)[1] <- 'participant_id'
  
  cfq_scored <- dataprepr::score_cfq(cfq_data, score_base = TRUE, restriction_split = FALSE, id = 'participant_id')
  cfq_json <- json_cfq()
  
  ## CEBQ Data ####
  cebq_data <- data[, grepl('record_id', names(data)) | grepl('cebq', names(data))]
  cebq_data <- cebq_data[, !(names(cebq_data) %in% c('cebq_missingcheck'))]
  names(cebq_data)[1] <- 'participant_id'
  
  cebq_scored <- dataprepr::score_cebq(cebq_data, score_base = TRUE, id = 'participant_id')
  cebq_json <- json_cebq()
  
  ## EFCR Data ####
  efcr_data <- data[, grepl('record_id', names(data)) | grepl('efcr', names(data))]
  efcr_data <- efcr_data[, !(names(efcr_data) %in% c('efcr_missingcheck'))]
  names(efcr_data)[1] <- 'participant_id'
  
  efcr_scored <- dataprepr::score_efcr(efcr_data, score_base = FALSE, id = 'participant_id')
  efcr_json <- json_efcr()
  
  ## LBC Data  ####
  lbc_data <- data[, grepl('record_id', names(data)) | grepl('lbc', names(data))]
  lbc_data <- lbc_data[, !(names(lbc_data) %in% c('lbc_missingcheck'))]
  names(lbc_data)[1] <- 'participant_id'
  
  lbc_scored <- dataprepr::score_lbc(lbc_data, score_base = FALSE, id = 'participant_id')
  lbc_json <- json_lbc()
  
  ## BRIEF Data ####
  brief_data <- data[, grepl('record_id', names(data)) | grepl('demo_c_sex', names(data)) | grepl('demo_c_dob', names(data)) | grepl('brief', names(data))]
  brief_data <- brief_data[, !(names(brief_data) %in% c('brief_missing_check'))]
  names(brief_data)[1] <- 'participant_id'
  
  # merge and calculate age
  brief_data <- merge(brief_data, v1_date_data, by = 'participant_id')
  
  brief_data[['v1_date']] <- lubridate::as_date(brief_data[['v1_date']])
  brief_data[['age']] <- lubridate::interval(brief_data[['demo_c_dob']], brief_data[['v1_date']])/lubridate::years(1)

  #organize data
  brief_data <- brief_data[c(1, 3, 68, 4:66)]
  names(brief_data)[2] <- 'sex'

  brief_scored <- dataprepr::score_brief2(brief_data, age_var = 'age', sex_var = 'sex', score_base = FALSE, male = 0, female = 1, id = 'participant_id')
  brief_json <- json_brief2()
  
  ## FFQ Data ####
  ffq_data <- data[, grepl('record_id', names(data)) | grepl('ffq', names(data))]
  ffq_data <- ffq_data[, !(names(ffq_data) %in% c('ffq1_check', 'ffq2_check', 'ffq3_check', 'ffq4_check', 'ffq5_check', 'ffq_sup_check'))]

  names(ffq_data)[1:44] <- c('participant_id', 'ffq_dairy1', 'ffq_dairy2', 'ffq_dairy3', 'ffq_dairy4', 'ffq_egg1', 'ffq_meat1', 'ffq_meat2', 'ffq_meat3', 'ffq_meat4', 'ffq_fish1', 'ffq_fish2', 'ffq_fish3', 'ffq_fish4', 'ffq_dairy5', 'ffq_veg1', 'ffq_veg2', 'ffq_potato1', 'ffq_legume1', 'ffq_potato2', 'ffq_fruit1', 'ffq_fruit2', 'ffq_nuts1', 'ffq_fruit3', 'ffq_fruit4', 'ffq_cereal1', 'ffq_cereal2', 'ffq_cereal3', 'ffq_cereal4', 'ffq_cereal5', 'ffq_cereal6', 'ffq_bakery1', 'ffq_bakery2', 'ffq_sweet1', 'ffq_sweet2', 'ffq_sweet3', 'ffq_bev1', 'ffq_bev2', 'ffq_fats1', 'ffq_fats2', 'ffq_fats3', 'ffq_fats4', 'ffq_dressing1', 'ffq_saltysnack1')
  
  ffq_scored <- dataprepr::score_ffq_helix(ffq_data, score_base = TRUE, id = 'participant_id')
  ffq_json <- json_ffq_helix()
  
  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      demo_data = list(data = demo_data),
      puberty_data = list(data = puberty_scored, meta = puberty_json),
      cfq_data = list(data = cfq_scored, meta = cfq_json),
      cebq_data = list(data = cebq_scored, meta = cebq_json), 
      efcr_data = list(data = efcr_scored, meta = efcr_json), 
      lbc_data = list(data = lbc_scored, meta = lbc_json), 
      brief_data = list(data = brief_scored, meta = brief_json), 
      ffq_data = list(data = ffq_scored, meta = ffq_json)))
  }
}

