#' util_redcap_de: Organize double-entry data from REDCap
#'
#' This function organizes REDCap double entry data data
#'
#' @param redcap_api (logical) execute REDCap API. Default = FALSE.
#' @param redcap_de_data REDCap double-entry data from a prior API call
#' @inheritParams util_redcap_parent_v1
#'
#' @return Will return a list including data that has been double-entered and checked along with the metadata for:
#' \itemize{
#'  \item{bodpod}
#'  \item{wasi}
#'  \item{dkefs}
#'  \item{intake}
#'  \item{fullness_baseline}
#'  \item{fullness_followup}
#'  \item{taste_test}
#'  \item{cams}
#' }
#'
#'
#' @export

util_redcap_de <- function(redcap_api = FALSE, redcap_de_data, date_data) {
  
  #### Set up/initial checks #####
  
  # check that data is passed if redcap_api = FALSE
  if (isFALSE(redcap_api)){
    
    # check that redcap_de_data exist and is a data.frame
    de_data_arg <- methods::hasArg(redcap_de_data)
    
    if (isTRUE(de_data_arg)) {
      if (!is.data.frame(redcap_de_data)) {
        stop('redcap_de_data must be a data.frame with recap_api = FALSE')
      }
    } else if (isFALSE(de_data_arg)) {
      stop('redcap_de_data must be a data.frame with recap_api = FALSE')
    }
    
  } else {
    # get data from REDCap directly (only will work if have access and keys setup)
    Sys.setenv(reach_de_redcap_key = keyring::key_get('reach-de_redcap_key'))
    redcap_de <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/',
                                       token = Sys.getenv('reach_de_redcap_key'))
    
    redcap_de_data <- redcap_de[['data']]
    redcap_de_dict <- redcap_de[['dictionary']]
    
    # remove '.factor'
    redcap_de_data <- redcap_de_data[, !grepl('.factor', names(redcap_de_data))]
  }
  
  date_data_arg <- methods::hasArg(date_data)
  
  if (isTRUE(date_data_arg)) {
    if (!is.data.frame(date_data)) {
      stop('date_data must be a data.frame')
    }
  } else if (isFALSE(date_data_arg)) {
    stop('date_data from util_redcap_dates() must be entered as a data.frame')
  }
  
  # update name of participant ID column
  names(redcap_de_data)[names(redcap_de_data) == 'record_id'] <- 'participant_id'
  
  ## Extract data ####
  checked_data <- redcap_de_data[!grepl('--', redcap_de_data$participant_id), ]
  
  # make a grepl string including all merged ids separate by '|'
  merged_ids_grepl <- paste0(checked_data[['participant_id']], collapse = '|')
  
  # get vector indicator of unmerged ids
  unmerged_ids <- sapply(redcap_de_data[['participant_id']], function(x) !grepl(merged_ids_grepl, x))
  
  # if there are unmerged participants
  if (sum(unmerged_ids) > 0) {
    unmerged_data <- redcap_de_data[unmerged_ids, ]
  }
  
  # Make ID column bids compliant: add 'sub_'
  checked_data$participant_id <- paste0('sub-', checked_data$participant_id)
  
  
  ## bod pod ####
  bodpod_data <- checked_data[grepl('_id|bodpod', names(checked_data))]
  
  #reduce columns and reorder
  bodpod_data <- bodpod_data[!grepl('visit_3|complete', names(bodpod_data))]
  
  #split by visit
  bodpod_data_v1 <- bodpod_data[!grepl('v3', names(bodpod_data))]
  
  bodpod_data_v3 <- bodpod_data[grepl('_id|^visit|v3', names(bodpod_data))]
  names(bodpod_data_v3) <- gsub('_v3', '', names(bodpod_data_v3))
  
  #add session
  bodpod_data_v1['session_id'] <- 'baseline'
  bodpod_data_v3['session_id'] <- 'followup'
  
  names(bodpod_data)[names(bodpod_data) == 'bodpod_date'] <- 'visit_date'
  
  bodpod_data <- rbind.data.frame(bodpod_data_v1, bodpod_data_v3)
  
  bodpod_data <- bodpod_data[c('participant_id', 'session_id', 'visit_date', names(bodpod_data)[grepl('bodpod', names(bodpod_data))])]
  
  bodpod_json <- bodpod_json()
  
  ## wasi ####
  wasi_data <- checked_data[grepl('_id|wasi', names(checked_data))]
  wasi_data <- wasi_data[!grepl('other|complete', names(wasi_data))]
  wasi_data['session_id'] <- 'baseline'
  
  names(wasi_data) <- gsub('pcentile', 'p', names(wasi_data))
  
  wasi_data <- wasi_data[c('participant_id', 'session_id', names(wasi_data)[grepl('wasi', names(wasi_data))])]
  
  wasi_json <- json_wasi()
  
  ## dkefs ####
  dkefs_data <- checked_data[grepl('_id|dkefs', names(checked_data))]
  
  dkefs_data <- dkefs_data[!grepl('complete', names(dkefs_data))]
  dkefs_data['session_id'] <- 'baseline'
  
  dkefs_data <- dkefs_data[c('participant_id', 'session_id', names(dkefs_data)[grepl('dkefs', names(dkefs_data))])]

  dkefs_json <- json_dkefs()
  
  ## intake data ####
  intake_data <- checked_data[grepl('_id|pre|post|plate|bowl', names(checked_data))]
  
  #reduce columns and reorder
  intake_data <- intake_data[!grepl('ff|cams|^tt|intake', names(intake_data))]
  
  #split by visit
  intake_data_v1 <- intake_data[!grepl('v3', names(intake_data))]
  intake_data_v1 <- merge(intake_data_v1, date_data[c('participant_id', 'v1_date')], by = 'participant_id')
  names(intake_data_v1)[names(intake_data_v1) == 'v1_date'] <- 'visit_date'
  intake_data_v1['visit_protocol'] <- 1
  
  
  intake_data_v3 <- intake_data[grepl('_id|^visit|v3', names(intake_data))]
  intake_data_v3 <- merge(intake_data_v3, date_data[c('participant_id', 'v3_date')], by = 'participant_id')
  
  names(intake_data_v3)[names(intake_data_v3) == 'v3_date'] <- 'visit_date'
  names(intake_data_v3) <- gsub('_v3', '', names(intake_data_v3))
  intake_data_v3['visit_protocol'] <- 3
  
  #split visit 1 by meal/eah
  intake_data_v1_meal <- intake_data_v1[grepl('_id|^visit|mac|cknug|grapes|carrot|ketchup|water|preweight_notes|postweight_notes', names(intake_data_v1))]
  intake_data_v1_meal <- intake_data_v1_meal[!grepl('eah', names(intake_data_v1_meal))]
    
  intake_data_v1_eah <- intake_data_v1[c('participant_id', names(intake_data_v1)[!names(intake_data_v1) %in% names(intake_data_v1_meal)])]
  
  #add session
  intake_data_v1_meal['session_id'] <- 'baseline'
  intake_data_v1_eah['session_id'] <- 'baseline'
  intake_data_v3['session_id'] <- 'followup'
  
  # merge all together
  intake_data <- rbind.data.frame(intake_data_v1_meal, intake_data_v3)
  intake_data <- merge(intake_data, intake_data_v1_eah, by = c('participant_id', 'session_id'))
  
  # update/harmonize names
  names(intake_data) <- gsub('_pre_', '_pre_w_o_plate_', names(intake_data))
  names(intake_data) <- gsub('prebowl', 'pre_w_plate', names(intake_data))
  names(intake_data) <- gsub('preplate', 'pre_w_plate', names(intake_data))
  names(intake_data) <- gsub('precup', 'pre_w_plate', names(intake_data))
  names(intake_data) <- gsub('post_g', 'post_w_plate_g', names(intake_data))
  names(intake_data) <- gsub('_g', '', names(intake_data))
  
  intake_data <- intake_data[c('participant_id', 'session_id', 'visit_date', names(intake_data)[grepl('plate', names(intake_data))], names(intake_data)[grepl('notes', names(intake_data))])]
  
  intake_json <- json_intake_de()
  
  ## Freddy fullness data ####
  ff_data <- checked_data[grepl('_id|ff', names(checked_data))]
  
  #reduce columns and reorder
  ff_data <- ff_data[!grepl('timeoff|box', names(ff_data))]
  
  #update names
  names(ff_data) <- gsub('ff', 'fullness', names(ff_data))
  
  names(ff_data)[names(ff_data) == 'fullness_meal_liking'] <- 'fullness_preliking_meal'
  names(ff_data)[names(ff_data) == 'fullness_eahliking'] <- 'fullness_preliking_eah'
  names(ff_data)[names(ff_data) == 'fullness_preeah'] <- 'fullness_pre_eah'
  names(ff_data)[names(ff_data) == 'fullness_posteah'] <- 'fullness_post_eah'
  
  names(ff_data)[names(ff_data) == 'fullness_preshape_snack'] <- 'preshape_hungry'
  names(ff_data)[names(ff_data) == 'fullness_preshape_snack2'] <- 'preshape_hungry_postsnack'
  names(ff_data)[names(ff_data) == 'fullness_prefnirs_snack'] <- 'prefnirs_hungry'
  names(ff_data)[names(ff_data) == 'fullness_prefnirs_snack2'] <- 'prefnirs_hungry_postsnack'
  
  #split by visit
  ff_data_baseline <- ff_data[!grepl('v3', names(ff_data))]
  ff_data_baseline <- merge(ff_data_baseline, date_data[c('participant_id', 'v1_date', 'v2_date')], by = 'participant_id')
  ff_data_baseline['session_id'] <- 'baseline'
  
  ff_data_v3 <- ff_data[grepl('_id|v3', names(ff_data))]
  ff_data_v3 <- merge(ff_data_v3, date_data[c('participant_id', 'v3_date')], by = 'participant_id')
  
  names(ff_data_v3)[names(ff_data_v3) == 'v3_date'] <- 'visit_date'
  names(ff_data_v3) <- gsub('_v3', '', names(ff_data_v3))
  ff_data_v3['session_id'] <- 'followup'
  ff_data_v3['visit_protocol'] <- 3
  
  # reorder
  ff_data_baseline <- ff_data_baseline[c('participant_id', 'session_id', 'v1_date', 'v2_date', names(ff_data_baseline)[grepl('fullness|hungry', names(ff_data_baseline))])]
  
  ff_data_v3 <- ff_data_v3[c('participant_id', 'session_id', 'visit_date', names(ff_data_v3)[grepl('fullness', names(ff_data_v3))])]
  
  ff_baseline_json <- json_ff_baseline()
  ff_v3_json <- json_ff_v3()
  
  ## taste-test data ####
  tt_data <- checked_data[grepl('_id|tt', names(checked_data))]
  
  tt_data <- tt_data[!grepl('post|dkefs|skittles|kcal', names(tt_data))]
  tt_data <- merge(tt_data, date_data[c('participant_id', 'v3_date')], by = 'participant_id')
  tt_data['session_id'] <- 'followup'
  
  # update names
  names(tt_data)[names(tt_data) == 'v3_date'] <- 'visit_date'
  names(tt_data) <- gsub('_g_pre', '',  names(tt_data))
  
  tt_data <- tt_data[c('participant_id', 'session_id', 'visit_date', names(tt_data)[grepl('tt', names(tt_data))])]
  
  tt_json <- json_tt_food()
  
  ## cams data ####
  cams_data <- checked_data[grepl('_id|cams', names(checked_data))]
  
  cams_data <- cams_data[!grepl('complete|collect|reason', names(cams_data))]
  
  # update names
  names(cams_data)[names(cams_data) == 'cams_pre'] <- 'cams_pre_baseline'
  names(cams_data)[names(cams_data) == 'cams_post'] <- 'cams_post_baseline'
  
  names(cams_data) <- gsub('_v3', 'followup',  names(cams_data))
  
  cams_json <- json_cams()
  
  ## NonWear Log data
  nonwear_log_data <- data[, grepl('participant_id', names(data)) | grepl('nonwear', names(data))] 
  nonwear_log_data <- nonwear_log_data[, !(grepl('complete', names(nonwear_log_data)))]
  
  names(nonwear_log_data)[1] <- 'participant_id'

    return(list(
      bodpod_data = list(data = bodpod_data, bodpod_json), 
      wasi_data = list(data = wasi_data, meta = wasi_json),
      dkefs_data = list(data = dkefs_data, meta = dkefs_json), 
      intake_data = list(data = intake_data, intake_json), 
      fullness_baseline_data = list(data = ff_data_baseline, ff_baseline_json), 
      fullness_followup_data = list(data = ff_data_v3, ff_v3_json), 
      taste_test_data = list(data = tt_data, tt_json),
      cams_data = list(data = cams_data, meta = cams_json)
      ))
}

