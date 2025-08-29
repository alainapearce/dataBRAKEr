#' util_merged_fnirs: merges and formats all fNIRS related data for BIDS
#'
#' This function merges all fNIRS related data across visits
#'
#'
#' @inheritParams util_merged_intake
#' @inheritParams util_merged_intake
#' @inheritParams util_merged_intake
#'
#' @examples
#'
#' # process data
#' merged_participants <- util_merged_fnirs(child_v1_data, child_v3_data, proc_de_data)
#'
#' @seealso [proc_redcap()], [util_merge_demo()]
#'
#' @export
#'


util_merged_fnirs <- function(child_v1_data, child_v3_data, proc_de_data) {

  fnirs_v1 <- merge(child_v1_data$fnirs_cap$data, child_v1_data$fnirs_tasks_info$data[!grepl('session_id|^visit', names(child_v1_data$fnirs_tasks_info$data))], by = 'participant_id', all = TRUE)
  fnirs_v1 <- merge(fnirs_v1, proc_de_data$cams_data$data[grep('id|baseline', names(proc_de_data$cams_data$data))], by = 'participant_id', all = TRUE)
  
  fnirs_v3 <- merge(child_v3_data$fnirs_cap$data, child_v3_data$fnirs_tasks_info$data[!grepl('session_id|^visit', names(child_v3_data$fnirs_tasks_info$data))], by = 'participant_id', all = TRUE)
  fnirs_v3 <- merge(fnirs_v3, proc_de_data$cams_data$data[grep('id|followup', names(proc_de_data$cams_data$data))], by = 'participant_id', all = TRUE)
  
  fnirs_all <- rbind(data.table::setDT(fnirs_v1), data.table::setDT(fnirs_v3), fill=TRUE)
  fnirs_all <- as.data.frame(fnirs_all)
  
  fnirs_all <- fnirs_all[!is.na(fnirs_all['participant_id']), ]
  
  fnirs_json <- json_fnirs()
  
  # return data
  return(fnirs_info = list(data = fnirs_all, meta = fnirs_json))
}
