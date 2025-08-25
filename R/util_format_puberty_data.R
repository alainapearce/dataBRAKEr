#' util_format_puberty_data: Prepare parent-reported puberty data for scoring
#'
#' This function prepares parent-reported puberty data for scoring with dataprepr::score_puberty()
#'
#'
#' @param puberty_data Puberty data (Parental Rating Scale for Pubertal Development and Tanner) extracted from data from REDCap events
#'
#' @examples
#'
#' # process puberty data
#' puberty_data_formatted <- util_format_puberty_data(puberty_data, respondent = 'child')
#'
#' @seealso [util_redcap_parent1()], [util_redcap_parente()]
#'
#' @export


util_format_puberty_data <- function(puberty_data) {
  
  # fix sex
  names(puberty_data) <- gsub('pds_sex', 'sex', names(puberty_data))
  
  # fix tanner choice
  puberty_data['tanner_choice'] <- ifelse(puberty_data[['sex']] == 0, puberty_data[['tanner_f']], puberty_data[['tanner_m']])
  w2
  # set 4 to '99'
  puberty_data[grepl('pds', names(puberty_data))] <- sapply(names(puberty_data[grepl('pds', names(puberty_data))]), function(x) ifelse(puberty_data[[x]] == 4, 99, puberty_data[[x]]))
  
  # for pds_5fa, set 2 to 99
  puberty_data['pds_5fa'] <- ifelse(puberty_data[['pds_5fa']] == 2, 99, puberty_data[['pds_5fa']])
  
  # re-label sex
  puberty_data$sex <- ifelse(puberty_data$sex == 0, 'female', ifelse(puberty_data$sex == 1, 'male', NA))
  
  # return data
  return(puberty_data)
}
