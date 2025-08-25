#' util_redcap_parent1: Organize parent visit 1 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @inheritParams util_redcap_child1
#' 
#' 
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw parent visit 1 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{demo_data}
#'    \item{infancy_data}
#'    \item{household_data}
#'    \item{puberty_data}
#'    \item{cfq_data}
#'    \item{cebq_data}
#'    \item{lbc_data}
#'    \item{lbc_data}
#'    \item{brief_data}
#'    \item{ffq_data}
#'  }
#'  
#'  
#' @examples
#'
#' # process REDCap data
#' parent_visit1_list <- util_redcap_parent1(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap(), util_redcap_dates()]
#'
#'
#' @export

util_redcap_parent1 <- function(data, date_data) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
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
  data['visit_protocol'] <- 1
  
  # merge with date_data
  data <- merge(data, date_data[c('participant_id', 'v1_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v1_date'] <- 'visit_date'
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
  
  # select columns for participants_data
  demo_data <- demo_data_all[grepl('_id|sex|race|ethnicity', names(demo_data_all))]
  demo_data <- demo_data[!grepl('parent', names(demo_data))]
  
  # select columns for infancy_data
  infancy_data <- demo_data_all[c('participant_id', 'session_id', 'visit_date', 'demo_birth_in', 'demo_birthweight_lb', 'demo_birthweight_oz', 'demo_premature', 'demo_premature_weeks', 'demo_feeding', 'demo_exclusive_feeding', 'demo_tot_breastfeeding', 'demo_solid_food')]
  
  # derive total birthweight in ounces from lb and oz components
  infancy_data['birthweight_ounces_total'] <- (infancy_data[['demo_birthweight_lb']])*16 + infancy_data[['demo_birthweight_oz']]
  
  # harmonize names
  names(infancy_data)[names(infancy_data) == 'demo_birth_in'] <- 'demo_birth_length'
  names(infancy_data)[names(infancy_data) == 'demo_birthweight_lb'] <- 'demo_birthweight_pounds'
  names(infancy_data)[names(infancy_data) == 'demo_birthweight_oz'] <- 'demo_birthweight_ounces'
  
  infancy_json <- json_infancy()
  
  # select columns for household_data
  household_data <- demo_data_all[, !(names(demo_data_all) %in% c(names(demo_data[!grepl('_id', names(demo_data))]), names(infancy_data[!grepl('_id|visit_date', names(infancy_data))]), names(data[grepl('birth_in|birthweight', names(data))])))]
  
  # process household data
  household_data <- util_format_household_data(household_data)
  
  names(household_data) <- gsub('work_hours', 'workhours', names(household_data))
  
  household_json <- json_household()
  
  ## Puberty Data ####
  
  #select all demo variables
  puberty_data <- data[grepl('_id|^visit|pds|tanner', names(data))]
  
  # process puberty data
  puberty_data <- util_format_puberty_data(puberty_data)
  
  puberty_data <- puberty_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'sex', names(puberty_data)[grepl('pds|tanner_choice', names(puberty_data))])]
  
  puberty_scored <- dataprepr::score_pds(puberty_data, base_zero = TRUE, respondent = 'parent', male = 'male', female = 'female', id = 'participant_id')
  
  puberty_json <- json_pds()
  
  ## CFQ Data ####
  cfq_data <- data[grepl('_id|^visit|cfq', names(data))]
  
  # remove extra columns, add columns, and re-order
  cfq_data <- cfq_data[!grepl('missingcheck', names(cfq_data))]
  
  cfq_data <- cfq_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(cfq_data)[grepl('cfq', names(cfq_data))])]
  
  
  # process data
  cfq_scored <- dataprepr::score_cfq(cfq_data, pcw_na_value = 5, base_zero = TRUE, restriction_split = FALSE, id = 'participant_id')
  
  cfq_json <- json_cfq()
  
  ## CEBQ Data ####
  cebq_data <- data[grepl('_id|^visit|cebq', names(data))]
  
  # remove extra columns, add columns, and re-order
  cebq_data <- cebq_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(cebq_data)[grepl('cebq', names(cebq_data))])]
  
  
  # process data
  cebq_scored <- dataprepr::score_cebq(cebq_data, base_zero = TRUE, id = 'participant_id')
  
  cebq_json <- json_cebq()
  
  ## EFCR Data ####
  efcr_data <- data[grepl('_id|^visit|efcr', names(data))]
  
  # remove extra columns, add columns, and re-order
  efcr_data <- efcr_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(efcr_data)[grepl('efcr', names(efcr_data))])]
  
  # process data
  efcr_scored <- dataprepr::score_efcr(efcr_data, base_zero = FALSE, id = 'participant_id')
  
  efcr_json <- json_efcr()
  
  ## LBC Data  ####
  lbc_data <- data[grepl('_id|^visit|lbc', names(data))]
  
  # remove extra columns, add columns, and re-order
  lbc_data <- lbc_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(lbc_data)[grepl('lbc', names(lbc_data))])]
  
  # process data
  lbc_scored <- dataprepr::score_lbc(lbc_data, base_zero = FALSE, id = 'participant_id')
  
  lbc_json <- json_lbc()
  
  ## BRIEF Data ####
  brief_data <- data[grepl('_id|^visit|brief|c_dob|sex', names(data))]
  
  # remove extra columns, add columns, and re-order
  brief_data <- brief_data[!grepl('missing_check', names(brief_data))]
  
  # merge and calculate age
  brief_data['demo_c_dob'] <- lubridate::as_date(brief_data[['demo_c_dob']])
  brief_data['age'] <- lubridate::interval(brief_data[['demo_c_dob']], brief_data[['visit_date']])/lubridate::years(1)
  
  brief_data['age'] <- round(brief_data[['age']], 1)
  
  names(brief_data)[names(brief_data) == 'demo_c_sex'] <- 'sex'
  
  brief_data <- brief_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'age', 'sex', names(brief_data)[grepl('brief', names(brief_data))])]
  
  # process data
  brief_scored <- dataprepr::score_brief2(brief_data, age_var = 'age', sex_var = 'sex', base_zero = FALSE, male = 0, female = 1, id = 'participant_id')
  
  brief_json <- json_brief2()
  
  ## FFQ Data ####
  ffq_data <- data[grepl('_id|^visit|ffq', names(data))]
  
  # remove extra columns, add columns, and re-order
  ffq_data <- ffq_data[!grepl('check', names(ffq_data))]
  
  ffq_data <- ffq_data[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(ffq_data)[grepl('ffq', names(ffq_data))])]
  
  # process data
  ffq_data <- util_format_ffq_data(ffq_data)
  
  ffq_scored <- dataprepr::score_ffq_helix(ffq_data, base_zero = TRUE, id = 'participant_id')
  
  ffq_json <- json_ffq_helix()
  
  ## compile and return data ####
  return(list(
    demo_data = list(data = demo_data),
    infancy_data = list(data = infancy_data, meta = infancy_json),
    household_data = list(data = household_data, meta = household_json),
    puberty_data = list(data = puberty_scored, meta = puberty_json),
    cfq_data = list(data = cfq_scored, meta = cfq_json),
    cebq_data = list(data = cebq_scored, meta = cebq_json), 
    efcr_data = list(data = efcr_scored, meta = efcr_json), 
    lbc_data = list(data = lbc_scored, meta = lbc_json), 
    brief_data = list(data = brief_scored, meta = brief_json), 
    ffq_data = list(data = ffq_scored, meta = ffq_json)))
}

