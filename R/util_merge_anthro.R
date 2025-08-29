#' util_merged_anthro: merges and formats anthropometrics data
#'
#' This function merges anthropometrics data across visits and formats/calculates necessary values
#'
#' @param visit1_anthro measured anthropometrics at visit 1 from util_redcap_child1()
#' @param visit3_anthro measured anthropometrics at visit 3 from util_redcap_child3()
#' @param household_all merged household data.frame from merged_qs_list()
#' @inheritParams util_redcap_parent_v1
#'
#' @examples
#'
#' # process data
#' merge_anthro_data <- util_merged_anthro(bodpod_data = proc_de_data$bodpod_data$data, household_all = merged_qs_list$household_all$data, date_data = date_data)
#'
#' @seealso [proc_redcap(), util_redcap_child1(), util_redcap_child3(), util_merge_questionnaires()]
#'
#' @export
#'


util_merged_anthro <- function(visit1_anthro, visit3_anthro, household_all, date_data) {

  anthro_merge <- rbind.data.frame(visit1_anthro, visit3_anthro)
  
  # Extract parent 2 BMI from household_data and stack
  parent2_anthro_data <- household_all[c('participant_id','session_id', 'demo_parent2_rep_height_ft', 'demo_parent2_rep_height_in', 'demo_parent2_rep_weight_lbs', 'demo_parent2_rep_height_m', 'demo_parent2_rep_weight_kg', 'demo_parent2_rep_bmi','demo_relationship')]

  # may need to update with double entered values
  merged_anthro <- merge(anthro_merge, parent2_anthro_data, by=c('participant_id', 'session_id'), all = TRUE)
  merged_anthro <- merge(merged_anthro, date_data[c('participant_id', 'sex', 'v1_age', 'v3_age')], by=c('participant_id'), all = TRUE)

  merged_anthro['age'] <- ifelse(merged_anthro[['session_id']] == 'ses-baseline', merged_anthro[['v1_age']], merged_anthro[['v3_age']])

  merged_anthro <- merged_anthro[!grepl('v1_age|v5_age', names(merged_anthro))]

  # compute bmi variables
  merged_anthro['child_bmi'] <- round(merged_anthro['weight_mean_kg'] /((merged_anthro['height_mean_cm'] / 100) ^ 2), digits = 2)

  merged_anthro['child_bmi_z'] <- round(childsds::sds(value = merged_anthro[['child_bmi']], age = merged_anthro[['age']], sex = merged_anthro[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'SDS', male = 'male', female = 'female'), digits = 2)

  merged_anthro['child_bmi_p'] <- round((childsds::sds(value = merged_anthro[['child_bmi']], age = merged_anthro[['age']], sex = merged_anthro[['sex']], item = 'bmi', ref = childsds::cdc.ref, type = 'perc', male = 'male', female = 'female')) * 100, digits = 2)

  merged_anthro['parent1_bmi'] <- round(merged_anthro['parent1_weight_mean_kg'] / ((merged_anthro['parent1_height_mean_cm'] / 100) ^ 2), digits = 2)

  # Define parental BMI values and method
  merged_anthro['mom_anthro_method'] <- ifelse(merged_anthro['parent1_relationship'] == 0, 'measured', 'reported')

  merged_anthro['mom_bmi'] <- ifelse(merged_anthro[['mom_anthro_method']] == 'measured', merged_anthro[['parent1_bmi']], ifelse(merged_anthro[['mom_anthro_method']] == 'reported', merged_anthro[['demo_parent2_rep_bmi']], NA))

  merged_anthro['dad_anthro_method'] <- ifelse(merged_anthro['parent1_relationship'] == 1, 'measured', 'reported')

  merged_anthro['dad_bmi'] <- ifelse(merged_anthro[['dad_anthro_method']] == 'measured', merged_anthro[['parent1_bmi']], ifelse(merged_anthro[['dad_anthro_method']] == 'reported', merged_anthro[['demo_parent2_rep_bmi']], NA))

  # fix names
  names(merged_anthro) <- gsub('demo_', '', names(merged_anthro))

  merged_anthro <- merged_anthro[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'relationship', 'age', 'sex', names(merged_anthro)[grepl('^weight.+kg$|^height.+cm*|child', names(merged_anthro))], names(merged_anthro)[grepl('parent|mom|dad', names(merged_anthro))],  names(merged_anthro)[grepl('note', names(merged_anthro))])]
  
  names(merged_anthro) <- gsub('^relationship', 'child_relationship', names(merged_anthro))

  merged_anthro_json <- json_anthro()
  
  
  # return data
  return(anthro_all = list(data = merged_anthro, meta = merged_anthro_json))

}
