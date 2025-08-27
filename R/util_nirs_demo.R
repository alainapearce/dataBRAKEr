#' util_nirs_demo: Compile and save demographic information of interest for fNIRS analyses 
#'
#' This function extracts demographic information of interest for fNIRS analyses.
#' 
#' To use this function, util_redcap_*.R must be completed so all REDCap demographic information is available.
#'
#'
#' @param demo_rurality demo data from generated from util_redcap_prepost1.R
#' @param baseline_fnirs_cap fnirs task information generated from util_redcap_child1.R
#' @param baseline_tasks fnirs task information generated from util_redcap_child1.R
#' @param anthro_data anthropometric data from visit 1 generated from util_redcap_child1.R
#' @param followup_fnirs_cap fnirs task information generated from util_redcap_child1.R
#' @param followup_tasks fnirs task information generated from util_redcap_child1.R
#' @param followup_anthro_data anthropometric data from visit 3 generated from util_redcap_child3.R
#' @param demographics demographic data from visit 1 survey generated from util_redcap_parent1.R
#' @param puberty parent-report puberty data visit 1 survey generated from util_redcap_parent1.R
#' @param bodpod double-entered bodpod generated from util_redcap_de.R
#' @param baseline_cams double-entered baseline CAMS generated from util_redcap_de.R
#' @param followup_cams double-entered follow-up CAMS generated from util_redcap_de.R
#' @param fullness_tastetest double-entered freddy fullness for taste-test generated from util_redcap_de.R
#' @param v3_date date from prepost_v3_data
#' @inheritParams util_redcap_prepost
#' 
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' nirs_demo <- util_nirs_demo(sub_str, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_nirs_demo <- function(v1_demo_homeloc, fnirs_info, anthro_data, followup_anthro_data, demographics, puberty, bodpod, baseline_cams, followup_cams, fullness_tastetest, v3_date, return_data = TRUE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  demo_homeloc_arg <- methods::hasArg(v1_demo_homeloc)
  fnirs_arg <- methods::hasArg(fnirs_info)
  anthro_arg <- methods::hasArg(anthro_data)
  followup_anthro_arg <- methods::hasArg(followup_anthro_data)
  demo_arg <- methods::hasArg(demographics)
  puberty_arg <- methods::hasArg(puberty)
  bodpod_arg <- methods::hasArg(bodpod)
  bcams_arg <- methods::hasArg(baseline_cams)
  fcams_arg <- methods::hasArg(followup_cams)
  tt_arg <- methods::hasArg(fullness_tastetest)
  v3_date_arg <- methods::hasArg(v3_date)
  
  if (isTRUE(demo_homeloc_arg)) {
    if (!is.data.frame(v1_demo_homeloc)) {
      stop("v1_demo_homeloc must be a data.frame")
    } 
  } else if (isFALSE(demo_homeloc_arg)) {
    stop("demo data from generated from util_redcap_prepost1 must be entered as a data.frame")
  }
  
  if (isTRUE(fnirs_arg)) {
    if (!is.data.frame(fnirs_info)) {
      stop("fnirs_info must be a data.frame")
    } 
  } else if (isFALSE(fnirs_arg)) {
    stop("fnirs task information generated from util_redcap_child1 must be entered as a data.frame")
  }
  
  if (isTRUE(followup_anthro_arg)) {
    if (!is.data.frame(followup_anthro_data)) {
      stop("followup_anthro_data must be a data.frame")
    } 
  } else if (isFALSE(followup_anthro_arg)) {
    stop("anthropometric data from visit 3 generated from util_redcap_child3 must be entered as a data.frame")
  }
  
  if (isTRUE(anthro_arg)) {
    if (!is.data.frame(anthro_data)) {
      stop("anthro_data must be a data.frame")
    } 
  } else if (isFALSE(anthro_arg)) {
    stop("anthropometric data from visit 1 generated from util_redcap_child1 must be entered as a data.frame")
  }
  
  if (isTRUE(demo_arg)) {
    if (!is.data.frame(demographics)) {
      stop("demographics must be a data.frame")
    } 
  } else if (isFALSE(demo_arg)) {
    stop("demographic data from visit 1 survey generated from util_redcap_parent1 must be entered as a data.frame")
  }
  
  if (isTRUE(puberty_arg)) {
    if (!is.data.frame(puberty)) {
      stop("puberty must be a data.frame")
    } 
  } else if (isFALSE(puberty_arg)) {
    stop("parent-report puberty data visit 1 survey generated from util_redcap_parent1 must be entered as a data.frame")
  }
  
  if (isTRUE(bodpod_arg)) {
    if (!is.data.frame(bodpod)) {
      stop("bodpod must be a data.frame")
    } 
  } else if (isFALSE(bodpod_arg)) {
    stop("double-entered bodpod generated from util_redcap_de must be entered as a data.frame")
  }
  
  if (isTRUE(bcams_arg)) {
    if (!is.data.frame(baseline_cams)) {
      stop("baseline_cams must be a data.frame")
    } 
  } else if (isFALSE(bcams_arg)) {
    stop("double-entered baseline CAMS generated from util_redcap_de must be entered as a data.frame")
  }
  
  if (isTRUE(fcams_arg)) {
    if (!is.data.frame(followup_cams)) {
      stop("followup_cams must be a data.frame")
    } 
  } else if (isFALSE(fcams_arg)) {
    stop("double-entered follow-up CAMS generated from util_redcap_de must be entered as a data.frame")
  }
  
  if (isTRUE(tt_arg)) {
    if (!is.data.frame(fullness_tastetest)) {
      stop("fullness_tastetest must be a data.frame")
    } 
  } else if (isFALSE(tt_arg)) {
    stop("double-entered freddy fullness for taste-test generated from util_redcap_de must be entered as a data.frame")
  }
  
  if (isTRUE(v3_date_arg)) {
    if (!is.data.frame(v3_date)) {
      stop("v3_date must be a data.frame")
    } 
  } else if (isFALSE(tt_arg)) {
    stop("v3_date for taste-test generated from prepost_v3_data must be entered as a data.frame")
  }
  
  
  #### Organize Data #####
  anthro_data <- anthro_data[, !(grepl('v1', names(anthro_data))) & !(grepl('notes', names(anthro_data)))]
  anthro_data['ses'] <- 1
  
  followup_anthro_data <- anthro_data[!grepl('notes', names(anthro_data))]
  followup_anthro_data['ses'] <- 2
  
  anthro_merge <- rbind.data.frame(anthro_data, followup_anthro_data)
  
  v1_date <- v1_demo_homeloc[c('participant_id', 'v1_date')]
  names(v1_date) <- gsub('v1_', '', names(v1_date))
  v1_date['ses'] <- 1
  
  names(v3_date) <- gsub('v3_', '', names(v3_date))
  v3_date['ses'] <- 2
  
  dates_merge <- rbind.data.frame(v1_date[c('participant_id', 'ses', 'date')], v3_date[c('participant_id', 'ses', 'date')])
  
  bodpod[['participant_id']] <- as.numeric(bodpod[['participant_id']])
  bodpod_v1 <- bodpod[c('participant_id', names(bodpod)[grepl('baseline', names(bodpod))])]
  names(bodpod_v1) <- gsub('baseline_', '', names(bodpod_v1))
  bodpod_v1['ses'] <- 1
  
  bodpod_v3 <- bodpod[c('participant_id', names(bodpod)[grepl('followup', names(bodpod))])]
  
  names(bodpod_v3)[names(bodpod_v3) == 'followup_bodpod_no3'] <- 'followup_bodpod_no'
  bodpod_v3 <- bodpod_v3[!grepl('fatfree_p', names(bodpod_v3))]
  names(bodpod_v3) <- gsub('followup_', '', names(bodpod_v3))
  bodpod_v3['ses'] <- 2
  
  bodpod_merge <- rbind.data.frame(bodpod_v1, bodpod_v3)
  
  baseline_cams[['participant_id']] <- as.numeric(baseline_cams[['participant_id']])
  followup_cams[['participant_id']] <- as.numeric(followup_cams[['participant_id']])
  fullness_tastetest[['participant_id']] <- as.numeric(fullness_tastetest[['participant_id']])
  
  nirs_dat <- merge(dates_merge, v1_demo_homeloc[c('participant_id', 'home_locale', 'home_rural', 'school_locale', 'school_rural')], by = 'participant_id', all = TRUE)
  nirs_dat <- merge(nirs_dat, demographics, by = 'participant_id', all = TRUE)
  nirs_dat <- merge(nirs_dat, puberty, by = 'participant_id', all = TRUE)
  nirs_dat <- merge(nirs_dat, anthro_merge, by = c('participant_id', 'ses'), all = TRUE)
  nirs_dat <- merge(nirs_dat, bodpod_merge, by = c('participant_id', 'ses'), all = TRUE)
  nirs_dat <- merge(nirs_dat, fnirs_info, by = 'participant_id', all = TRUE)
  nirs_dat <- merge(nirs_dat, baseline_cams, by = 'participant_id', all = TRUE)
  nirs_dat <- merge(nirs_dat, followup_cams, by = 'participant_id', all = TRUE)
  nirs_dat <- merge(nirs_dat, fullness_tastetest, by = 'participant_id', all = TRUE)
  
  
  if (isTRUE(return_data)){
    return(list(data = nirs_dat))
  }
}

