#' util_redcap_dates: Organize child date data from REDCap across all visits (called within proc_redcap.R)
#'
#' This function organizes date data from REDCap across visits and events
#'
#' @param child_v1 data from REDCap child visit 1 event
#' @param child_v2 data from REDCap child visit 2 event
#' @param child_v3 data from REDCap child visit 3 event
#' @param child_v4 data from REDCap child visit 4 event
#' @param child_v5 data from REDCap child visit 5 event
#' @param parent_v1 data from REDCap parent visit 1 event
#'
#' @export



util_redcap_dates <- function(child_v1, child_v2, child_v3, child_v4, child_v5, parent_v1) {

  # merge necessary data
  date_data <- merge(child_v1[, c('record_id', 'v1pre_date')], child_v2[, c('record_id', 'v2pre_date')], by = 'record_id', all = TRUE)
  date_data <- merge(date_data, child_v3[, c('record_id', 'v3pre_date')], by = 'record_id', all = TRUE)


  # add child sex and dob to date_data
  date_data <- merge(date_data, parent_v1[, c('record_id', 'pds_sex', 'demo_c_dob')], by = 'record_id', all = TRUE)

  # update names
  names(date_data)[names(date_data) == 'v1pre_date'] <- 'v1_date'
  names(date_data)[names(date_data) == 'v2pre_date'] <- 'v2_date'
  names(date_data)[names(date_data) == 'v3pre_date'] <- 'v3_date'
  
  
  # convert to dates
  date_data[['v1_date']] <- lubridate::as_date(date_data[['v1_date']])
  date_data[['v2_date']] <- lubridate::as_date(date_data[['v2_date']])
  date_data[['v3_date']] <- lubridate::as_date(date_data[['v3_date']])
  
  date_data[['brief_date']] <- date_data[['v1_date']]
  date_data[['demo_c_dob']] <- lubridate::as_date(date_data[['demo_c_dob']])

  # add ages
  date_data[['v1_age']] <- round(lubridate::interval(date_data[['demo_c_dob']], date_data[['v1_date']])/lubridate::years(1), 1)
  date_data[['v2_age']] <- round(lubridate::interval(date_data[['demo_c_dob']], date_data[['v2_date']])/lubridate::years(1), 1)
  date_data[['v3_age']] <- round(lubridate::interval(date_data[['demo_c_dob']], date_data[['v3_date']])/lubridate::years(1), 1)
  
  date_data[['brief_age']] <- round(lubridate::interval(date_data[['demo_c_dob']], date_data[['brief_date']])/lubridate::years(1),1)

  # re-label sex var and save to sex
  date_data$sex <- ifelse(date_data$pds_sex == 0, 'female', ifelse(date_data$pds_sex == 1, 'male', NA))
  date_data <- date_data[,!(names(date_data) %in% c('pds_sex'))] # remove prs_sex

  # update column names in date_data
  names(date_data)[names(date_data) == 'record_id'] <- 'participant_id'

  #return
  return(date_data)
}
