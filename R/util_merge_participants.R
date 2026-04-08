#' util_merged_participants: merges and formats participants data for BIDS
#'
#' This function merges demographic data across visits and formats/calculates necessary values to make the participants.tsv BIDS file
#'
#'
#' @param merged_demo merged demo data.frame
#' @inheritParams util_redcap_parent1
#'
#' @examples
#'
#' # process data
#' merged_participants <- util_merged_participants(merged_demo = merged_demo$data, date_data)
#'
#' @seealso [proc_redcap()], [util_merge_demo()]
#'
#' @export
#'


util_merged_participants <- function(merged_demo, date_data) {

  # add risk status -- take from merged_demo
  participants_data <- merge(date_data, merged_demo[merged_demo[['session_id']] == 'ses-baseline', c('participant_id', 'ethnicity', 'race', 'child_other_race', 'child_weight_status')], by = 'participant_id', all = TRUE)


  # remove birthday and other columns
  participants_data <- participants_data[!grepl('birthdate|brief|dob', names(participants_data))]

  # rename columns
  names(participants_data)[names(participants_data) == 'v1_date'] <- 'child_protocol_1_date'
  names(participants_data)[names(participants_data) == 'v2_date'] <- 'child_protocol_2_date'
  names(participants_data)[names(participants_data) == 'v3_date'] <- 'child_protocol_3_date'
  names(participants_data)[names(participants_data) == 'v1_age'] <- 'child_protocol_1_age'
  names(participants_data)[names(participants_data) == 'v2_age'] <- 'child_protocol_2_age'
  names(participants_data)[names(participants_data) == 'v3_age'] <- 'child_protocol_3_age'
  
  # make column child_protocol_order (e.g., 13425) based on order of child protocol dates - only include visits that have dates (i.e., occurred)

  ## define function to get order of dates for each row
  get_order <- function(row) {

    # specify date columns
    date_cols <- names(participants_data)[grepl('date', names(participants_data))]

    # get number of missing visits (date cols with NA)
    n_na <- sum(is.na(row[date_cols]))

    # get order of dates w/ missing visits at end, collapse integers into single string
    order_all_visits <- paste0(order(row[date_cols], na.last = TRUE), collapse = '')

    # remove last n_na characters from order_all_visits, will yeild a string with length = number of visits attended
    stringr::str_sub(order_all_visits, end=-(n_na + 1))
  }

  ## apply function to get visit order
  participants_data['protocol_visits_order'] <- apply(participants_data, 1, get_order)

  # reorder columns
  participants_data <- participants_data[c('participant_id', 'sex', 'ethnicity', 'race', 'child_other_race', 'child_weight_status', names(participants_data)[grepl('age|order', names(participants_data))], names(participants_data)[grepl('date', names(participants_data))])]

  participants_json <- json_participants()
  
  # return data
  return(participants_data = list(data = participants_data, meta = participants_json))
}
