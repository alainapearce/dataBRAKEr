#' util_actigraph_clean: Compile and clean up processed actigraphy data
#'
#' This function compiles and cleans up processed actigraphy data from GGIR and mMARCH.AC
#'
#' To use this function, util_actigraph_org_rawdata.R and util_actigraph_mMARCH.R must be completed so all actigraphy information is available.
#'
#'
#' @param save_path full file path to the derivative directory where output should be saved
#' @param ggir_path full file path to GGIR derivative directory
#' @param mMARCH_path full file path to mMARCH.AC derivative directory
#' @param metrics list of metrics. Default: c('SL', 'PA', 'CR'). Options include: \itemize{
#'  \item{SL - sleep}
#'  \item{PA - physical activity}
#'  \item{CR - circadian rhythem}
#'  }
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_redcap_prepost
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' sleep_dat <- util_actigraph_clean(save_path, ggir_path, mMARCH_path, metrics = c('SL') return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_actigraph_clean <- function(save_path, ggir_path, mMARCH_path, metrics = c('SL', 'PA', 'CR'), overwrite = FALSE, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  save_arg <- methods::hasArg(save_path)

  if (isTRUE(save_arg)) {
    if (!is.character(save_path)) {
      stop("save_path must be entered as a string")
    } else if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
  } else if (isFALSE(save_arg)) {
    stop("save_path must be entered as a string")
  }

  ggir_arg <- methods::hasArg(ggir_path)

  if (isTRUE(ggir_arg)) {
    if (!is.character(ggir_path)) {
      stop("ggir_path must be entered as a string")
    } else if (!file.exists(ggir_path)) {
      stop("ggir_path entered, but file does not exist. Check ggir_path string.")
    }
  } else if (isFALSE(ggir_arg)) {
    stop("ggir_path must be entered as a string")
  }

  mMARCH_arg <- methods::hasArg(mMARCH_path)

  if (isTRUE(mMARCH_arg)) {
    if (!is.character(mMARCH_path)) {
      stop("mMARCH_path must be entered as a string")
    } else if (!file.exists(mMARCH_path)) {
      stop("mMARCH_path entered, but file does not exist. Check mMARCH_path string.")
    }
  } else if (isFALSE(mMARCH_arg)) {
    stop("mMARCH_path must be entered as a string")
  }

  # check that metric exist and is a list
  metric_arg <- methods::hasArg(metrics)

  if (isTRUE(metric_arg)) {
    if (!is.character(metrics)) {
      stop("metrics must be entered as an array of strings: c('SL', 'PA', 'CR')")
    } else {
      metrics <- toupper(metrics)
    }
  }


  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The proc_actigraph.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }

  #### Load Data #####

  # ggir
  if (sum(grepl('SL', metrics)) == 1){
    sleep_ggir4_night_path <- paste0(ggir_path, slash, 'results', slash, 'part4_nightsummary_sleep_cleaned.csv')
    sleep_ggir4_sum_path <- paste0(ggir_path, slash, 'results', slash, 'part4_summary_sleep_cleaned.csv')
    sleep_ggir6_sum_path <- paste0(ggir_path, slash, 'results', slash, 'part6_summary.csv')

    if (!file.exists(sleep_ggir4_night_path)) {
      stop ('entered part4_nightsummary_sleep_cleaned.csv is not an existing file - be sure it exists and ggir_path is correct')
    }

    if (!file.exists(sleep_ggir4_sum_path)) {
      stop ('entered part4_summary_sleep_cleaned.csv is not an existing file - be sure it exists and ggir_path is correct')
    }

    if (!file.exists(sleep_ggir6_sum_path)) {
      stop ('entered part6_summary.csv is not an existing file - be sure it exists and ggir_path is correct')
    }

    sleep_ggir4_night_data <- read.csv(sleep_ggir4_night_path, header = TRUE)
    sleep_ggir4_sum_data <- read.csv(sleep_ggir4_sum_path, header = TRUE)
    sleep_ggir6_sum_data <- read.csv(sleep_ggir6_sum_path, header = TRUE)

    # update names for nightly data
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'ID'] <- 'participant_id'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'SptDuration'] <- 'spt_duration'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'guider_SptDuration'] <- 'guider_spt_duration'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'SleepDurationInSpt'] <- 'sleep_duration_in_spt'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'WASO'] <- 'waso'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'sleeplog_ID'] <- 'sleeplog_id'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'SleepRegularityIndex'] <- 'sri'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'SriFractionValid'] <- 'sri_fraction_valid'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'GGIRversion'] <- 'ggir_version'
    names(sleep_ggir4_night_data)[names(sleep_ggir4_night_data) == 'ACC_spt_mg'] <- 'acc_spt_mg'

    # update names g.part4 summary
    names(sleep_ggir4_sum_data) <- tolower(names(sleep_ggir4_sum_data))

    names(sleep_ggir4_sum_data) <- gsub('sptduration', 'spt_dur', names(sleep_ggir4_sum_data))
    names(sleep_ggir4_sum_data) <- gsub('sleepdurationinspt', 'sleep_dur_in_spt', names(sleep_ggir4_sum_data))
    names(sleep_ggir4_sum_data) <- gsub('sleepregularityindex', 'sri', names(sleep_ggir4_sum_data))
    names(sleep_ggir4_sum_data) <- gsub('srifractionvalid', 'sri_frac_valid', names(sleep_ggir4_sum_data))
    names(sleep_ggir4_sum_data) <- gsub('srifractionvalid', 'sri_frac_valid', names(sleep_ggir4_sum_data))

    names(sleep_ggir4_sum_data)[names(sleep_ggir4_sum_data) == 'id'] <- 'participant_id'
    names(sleep_ggir4_sum_data)[names(sleep_ggir4_sum_data) == 'ggirversion'] <- 'ggir_version'

    sleep_ggir4_sum_data[['participant_id']] <- substr(sleep_ggir4_sum_data[['participant_id']], 1, unlist(gregexpr('_', sleep_ggir4_sum_data[['participant_id']]))-1)

    # update names g.part2 summary
    names(sleep_ggir6_sum_data) <- tolower(names(sleep_ggir6_sum_data))

    names(sleep_ggir6_sum_data)[names(sleep_ggir6_sum_data) == 'id'] <- 'participant_id'
    names(sleep_ggir6_sum_data)[names(sleep_ggir6_sum_data) == 'ggirversion'] <- 'ggir_version'

    # merge data
    sleep_ggir_sum <- merge(sleep_ggir4_sum_data, sleep_ggir6_sum_data[, !(grepl('ggir_version|filename', names(sleep_ggir6_sum_data)))], by = 'participant_id', all = TRUE)

    ## Save

    if (!file.exists(paste0(save_path, slash, 'ggir_actigraph_sleep.tsv')) | isTRUE(overwrite)) {
      write.table(sleep_ggir_sum, paste0(save_path, slash, 'ggir_actigraph_sleep.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    }

    if (!file.exists(paste0(save_path, slash, 'ggir_actigraph_sleepbynight.tsv')) | isTRUE(overwrite)) {
      write.table(sleep_ggir4_night_data, paste0(save_path, slash, 'ggir_actigraph_sleepbynight.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    }
  }



  if (isTRUE(return_data)){
    return(list(data = sleep_ggir_sum))
  }
}

