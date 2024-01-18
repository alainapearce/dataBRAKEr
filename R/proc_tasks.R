#' proc_tasks: Process raw data from all computer tasks used in Study BRAKE
#'
#' This function calls task scripts that:
#' 1) copies data from raw_untouched and saves it in sourcedata
#' 2) cleans data to save in BIDS format in rawdata
#' 3) processes rawdata to generate derivative or summary databases for each task
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @param sub participant number
#' @param data_path data_path absolute path to the raw_untouched directory
#' @inheritParams util_task_foodrating
#' @inheritParams util_task_foodrating
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw datasets meta-data for each task
#'  2) processed derivative datasets with meta-data for each task
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' proc_tasks_pardat <- proc_tasks(sub = 1, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

proc_tasks <- function(sub = sub, data_path = data_path, overwrite = FALSE, return_data = FALSE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data_path)

  if (isTRUE(data_arg)) {
    if (!is.character(data_path)) {
      stop("data_path must be entered as a string")
    } else if (!file.exists(data_path)) {
      stop("data_path entered, but file does not exist. Check data_path string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("data_path must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The proc_tasks.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }

  # find location of slashes so can decompose filepaths
  slash_loc <- unlist(gregexpr('/', data_path))

  # get sub string
  sub_str <- paste0('sub-', sprintf('%03d', sub))

  # set paths for other directories
  base_wd <- substr(data_path, 1, tail(slash_loc, 1))
  raw_wd_nirs <- paste0(base_wd, '/bids/rawdata/', sub_str, '/nirs/')
  raw_wd_beh <- paste0(base_wd, '/bids/rawdata/', sub_str, '/beh/')
  derivatives_wd_nirs <- paste0(base_wd, '/bids/derivatives/', sub_str, '/nirs-toolbox/')
  derivatives_wd_beh <- paste0(base_wd, '/bids/derivatives/', sub_str, '/beh/')

  #### Food Rating Task ####
  foodrating_list <- foodrating_task(sub, paste0(data_path, 'foodrating_game'), return_data = return_data)

  #### Food Choice Task ####
  foodrating_list <- foodrating_task(sub, paste0(data_path, 'foodchoice_game'), return_data = return_data)

  if (isTRUE(return_data)){
    return(list( foodchoice_dat = dat,
                 foodchoice_labels = meta_json))
  }
}

