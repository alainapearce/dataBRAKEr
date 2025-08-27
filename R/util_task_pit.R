#' util_task_pit: Clean and organize PIT data into BIDS rawdata
#'
#' This function formats and organizes RRV data from bids/sourcedata into bids/rawdata for a given subject
#'
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_foodrating
#'
#'
#' @return statement of task completed
#'
#' @examples
#'
#' \dontrun{

#' }
#' @importFrom utils read.table
#' @importFrom rlang .data
#' @export

util_task_pit <- function(ub_str, ses, base_wd, overwrite = FALSE, return_data = FALSE) {

  #### Check args #####

  # check that base_wd_arg exist and is a string
  base_wd_arg <- methods::hasArg(base_wd)

  if (isTRUE(base_wd_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(base_wd)) {
    stop("base_wd must be entered as a string")
  }

  # get directory paths
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'beh')
  
  data_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'beh', paste0(sub_str, '_ses-', ses, '_task-pit_events.tsv'))
  
  
  #### Organize Data #####
  dat <- read.csv(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
  
  dat <- dat[!is.na(dat['condFile']) & dat['condFile'] != '', ]
  
  #### Clean data #####

  # do something to pit_data

  #### Export Data  #####

  # make raw beh directory if it doesn't exist
  raw_beh_wd <- file.path(base_wd, 'rawdata', sub_str, ses_str, 'beh')

  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # export files if don't exist or overwrite = TRUE
  beh_outfile <- file.path(raw_beh_wd, paste0(sub_str, '_ses-', ses, '_task-pit_beh.tsv'))
  if (!file.exists(beh_outfile) | isTRUE(overwrite)) {
    utils::write.table(pit_data, beh_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a")
  }


  #### Return data #####

  if (isTRUE(return_data)) {
    return(pit_data)
  }
}

