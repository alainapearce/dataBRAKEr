#' util_merged_demo: merges and formats demographic data
#'
#' This function merges demographic data across visits and formats/calculates necessary values
#'
#'
#' @param visit1_demo visit 1 demo data.frame
#' @param household_all merged household data.frame
#' @param merged_anthro merged anthro data.fram
#' @inheritParams util_redcap_parent1
#'
#' @examples
#'
#' # process data
#' merge_demo_data <- util_merged_demo(visit1_demo = parent_v1_data$demo_data$data, household_all = merged_qs_list$household_all$data, merged_anthro = merged_anthro$data, date_data = date_data)
#'
#' @seealso [proc_redcap(), util_redcap_parent1(), util_redcap_de(), util_merge_questionnaires()]
#'
#' @export
#'


util_merged_demo <- function(visit1_demo, household_all, merged_anthro, date_data) {

  # combine demo data from demo_data and household form
  demo_data <- merge(visit1_demo[!grepl('session_id|sex', names(visit1_demo))], household_all[c('session_id', 'participant_id', 'demo_mom_ed', 'demo_dad_ed', 'demo_income')], by = 'participant_id', all = TRUE)

  # add dates and ages at start of sessions (V1 and V5) from date_data form
  demo_data <- merge(demo_data, date_data[c('participant_id', 'v1_date', 'v3_date', 'v1_age', 'v3_age', 'sex')], by = 'participant_id', all.x = TRUE)

  demo_data['visit_date'] <- ifelse(demo_data[['session_id']] == 'ses-baseline', as.character(demo_data[['v1_date']]), as.character(demo_data[['v3_date']]))

  demo_data['age'] <- ifelse(demo_data[['session_id']] == 'ses-baseline', demo_data[['v1_age']], demo_data[['v3_age']])

  demo_data <- demo_data[!grepl('v1_|v3', names(demo_data))]

  # add key anthro data
  demo_data <- merge(demo_data, merged_anthro[c('participant_id', 'session_id', 'child_bmi', 'child_bmi_p', 'child_bmi_z', 'mom_bmi', 'mom_anthro_method', 'dad_bmi', 'dad_anthro_method')], by=c('participant_id', 'session_id'), all = TRUE)

  # add risk status - compute based on ses-1 maternal_bmi
  ses1_dat <- demo_data[demo_data['session_id'] == 'ses-baseline', ]

  # calculate risk based on maternal bmi
  ses1_dat['risk_status'] <- ifelse(ses1_dat['mom_bmi'] <= 26, 'low-risk', ifelse(ses1_dat['mom_bmi'] >= 29, 'high-risk', NA))

  # merge 'risk_status' variable into demo_data
  demo_data <- merge(demo_data, ses1_dat[, c('participant_id', 'risk_status')], by = 'participant_id', all = TRUE)
  
  # child weight status
  demo_data['child_weight_status'] <- ifelse(demo_data['child_bmi_p'] >= 95, 'obese', ifelse(demo_data['child_bmi_p'] >= 85, 'overweight', 'healthy weight'))

  # rename columns
  names(demo_data) <- gsub('demo_', '', names(demo_data))

  # reorder columns
  demo_data <- demo_data[c('participant_id', 'session_id', 'visit_date',   'sex', 'age', 'ethnicity', 'race', 'child_other_race', 'income', 'mom_ed', 'dad_ed', 'child_bmi', 'child_bmi_p', 'child_bmi_z', 'child_weight_status', 'mom_bmi', 'dad_bmi', 'risk_status', 'mom_anthro_method', 'dad_anthro_method')]

  demo_json <- json_demo()
  
  # return data
  return(demo_data = list(data = demo_data, meta = demo_json))
  
}
