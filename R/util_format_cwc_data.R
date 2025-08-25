#' util_format_cwc_data: process Child Weight Concerns Questionnaire data
#'
#' This function process Child Weight Concerns Questionnaire data
#'
#'
#' @param cwc_data rank extracted from data from REDCap events
#'
#' @examples
#'
#' # process data
#' cwc_data_formatted <- util_format_cwc_data(cwc_data)
#'
#' @seealso [util_redcap_chile_v5()]
#'
#' @export


util_format_cwc_data <- function(cwc_data) {

  # get cwc_1 by sex - logic didn't work so missing this one
  # cwc_data['cwc_1'] <- ifelse(cwc_data[['sex']] == 0, cwc_data[['cwc_1_m']], cwc_data[['cwc_1_f']])

  cwc_data['cwc_1'] <- NA
  
  # set pna value for all q's to be the same
  qs_5scale <- c('cwc_1', 'cwc_2', 'cwc_5')

  cwc_data[qs_5scale] <- sapply(qs_5scale, function(x) ifelse(cwc_data[[x]] == 6, 99, cwc_data[[x]]))

  # 7-chioce question
  cwc_data['cwc_3'] <- ifelse(cwc_data[['cwc_3']] == 8, 99, cwc_data[['cwc_3']])

  # 4-chioce question
  cwc_data['cwc_4'] <- ifelse(cwc_data[['cwc_4']] == 5, 99, cwc_data[['cwc_4']])

  cwc_data <- cwc_data[!grepl('_f|_m', names(cwc_data))]

  # return data
  return(cwc_data)

}
