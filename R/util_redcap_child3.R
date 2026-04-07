#' util_redcap_child3: Organize child visit 3 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_3_arm_1
#'
#' @param data data from REDCap event child_visit_3_arm_1
#' @inheritParams util_redcap_child1
#' 
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 3 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{fnirs_cap}
#'    \item{anthro_data}
#'    \item{meal_info}
#'    \item{fnirs_tasks_info}
#'    \item{pref_rank}
#'    \item{loc_data}
#'    \item{sic_data}
#'    \item{cwc_data}
#'    \item{body_es_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' child_visit3_list <- util_redcap_child3(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap(), util_redcap_dates()]
#'
#'
#' @export

util_redcap_child3 <- function(data, date_data) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)
  
  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    } 
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_3_arm_1 must be entered as a data.frame")
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
  names(data)[names(data) == 'record_id'] <- 'participant_id'
  
  # add session column
  data['session_id'] <- 'ses-followup'
  
  # add visit number
  data['visit_protocol'] <- 3
  
  # merge with date_data
  data <- merge(data, date_data[c('participant_id', 'v3_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v3_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])
  
  # get non-repeate instrument data
  data_all <- data
  data <- data_all[is.na(data_all['redcap_repeat_instrument']), ]
  
  
  ## fNIRS fit ####
  fnirs_cap <- data[grepl('_id|^visit|capfit', names(data))]
  
  #reduce columns and reorder
  fnirs_cap <- fnirs_cap[!grepl('hfi|mid', names(fnirs_cap))]
  
  fnirs_cap <- fnirs_cap[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(fnirs_cap)[grepl('capfit', names(fnirs_cap))])]
  
  #update names
  names(fnirs_cap) <- gsub('capfit', 'fnirs', names(fnirs_cap))
  names(fnirs_cap)[names(fnirs_cap) == 'fnirs_frontback'] <- 'fnirs_frontback_cm'
  names(fnirs_cap)[names(fnirs_cap) == 'fnirs_ears'] <- 'fnirs_ears_cm'
  names(fnirs_cap)[names(fnirs_cap) == 'fnirs_circ'] <- 'fnirs_circ_cm'
  
  fnirs_cap_json <- json_fnirscap()
  
  ## anthro ####
  child_anthro <- data[grepl('_id|^visit|weight|height|notes|relationship', names(data))]
  
  #reduce columns and reorder
  child_anthro <- child_anthro[!grepl('grapes|cknug|intake|meal|game|general|status', names(child_anthro))]
  
  names(child_anthro) <- gsub('v3_|_v3', '', names(child_anthro))
  
  child_anthro <- child_anthro[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', 'relationship', names(child_anthro)[grepl('c_', names(child_anthro))], names(child_anthro)[grepl('p_', names(child_anthro))], 'heightweight_notes')]
  
  #update names
  names(child_anthro) <- gsub('avg', 'mean', names(child_anthro))
  names(child_anthro) <- gsub('c_', '', names(child_anthro))
  names(child_anthro) <- gsub('p_', 'parent1_', names(child_anthro))
  names(child_anthro)[names(child_anthro) == 'relationship'] <- 'parent1_relationship'
  
  child_anthro_json <- json_child_anthro()
  
  ## meal information - NOTE: meal intake and freddy data are double entered ####
  meal_info <- data[grepl('_id|^visit|ff|test_meal', names(data))]
  
  meal_info <- meal_info[!grepl('fnirs|ff_check|min_check|record', names(meal_info))]
  
  # update names
  names(meal_info) <- gsub('ff', 'fullness', names(meal_info))
  names(meal_info) <- gsub('freddy', 'fullness', names(meal_info))
  
  meal_info <- meal_info[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(meal_info)[grepl('fullness', names(meal_info))], names(meal_info)[grepl('test_meal', names(meal_info))])]
  
  names(meal_info) <- gsub('^want_', 'eah_wanting_', names(meal_info))
  
  
  meal_info_json <- json_meal_info_v3()
  
  ## fNIRS information - NOTE: fready and CAMS values double entered ####
  tastetest_data <- data_all[!is.na(data_all[['redcap_repeat_instrument']]) & data_all[['redcap_repeat_instrument']] == 'tastetest_game', grepl('_id|^visit|tastetest', names(data_all))]
  
  #update names
  names(tastetest_data) <- gsub('check', 'complete', names(tastetest_data))
  
  #premeal
  fnirs_info <- tastetest_data[tastetest_data[['tastetest_timing']] == 0, ]
  
  names(fnirs_info) <- gsub('tastetest', 'premeal_tastetest', names(fnirs_info))
  
  #postmeal
  post_fnirs_info <- tastetest_data[tastetest_data[['tastetest_timing']] == 1, ]
  names(post_fnirs_info) <- gsub('tastetest', 'postmeal_tastetest', names(post_fnirs_info))
  
  # merge
  fnirs_info <- merge(fnirs_info, post_fnirs_info[grepl('participant_id|post', names(post_fnirs_info))], by = 'participant_id')
  
  # organize
  fnirs_info <- fnirs_info[!grepl('timing', names(fnirs_info))]
  
  fnirs_info <- fnirs_info[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(fnirs_info)[grepl('pre', names(fnirs_info))], names(fnirs_info)[grepl('post', names(fnirs_info))])]
  
  fnirs_info_json <- json_fnirsinfo_v3()
  
  ## preference rank information - NOTE: need to get liking from task ####
  prefrank_data <- data_all[!is.na(data_all[['redcap_repeat_instrument']]) & data_all[['redcap_repeat_instrument']] == 'preferencerank', grepl('_id|^visit|pref_rank', names(data_all))]
  
   #premeal
  prefrank <- prefrank_data[prefrank_data[['pref_rank_timing']] == 0, ]
  
  names(prefrank) <- gsub('pref_rank', 'premeal_pref_rank', names(prefrank))
  
  #postmeal
  post_prefrank <- prefrank_data[prefrank_data[['pref_rank_timing']] == 1, ]
  names(post_prefrank) <- gsub('pref_rank', 'postmeal_pref_rank', names(post_prefrank))
  
  # merge
  prefrank <- merge(prefrank, post_prefrank[grepl('participant_id|post', names(post_prefrank))], by = 'participant_id')
  
  # organize
  prefrank <- prefrank[!grepl('timing', names(prefrank))]
  
  prefrank <- prefrank[c('participant_id', 'session_id', 'visit_protocol', 'visit_date', names(prefrank)[grepl('premeal', names(prefrank))], names(prefrank)[grepl('postmeal', names(prefrank))])]
  
  prefrank_json <- json_pref_rank()
  
  ## LOC data ####
  loc_data <- data[grepl('_id|^visit|loc', names(data))]
  loc_data <- loc_data[!grepl('share', names(loc_data))]
  
  loc_data <- loc_data[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(loc_data)[grepl('loc', names(loc_data))])]
  
  loc_json <- json_loc()
  
  ## SIC data ####
  sic_data <- data[grepl('_id|^visit|sic', names(data))]
  
  sic_data <- sic_data[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(sic_data)[grepl('sic', names(sic_data))])]
  
  #score 
  sic_scored <- dataprepr::score_sic(sic_data, base_zero = TRUE, id = 'participant_id')
  
  sic_json <- json_sic()
  
  ## cwc data ####
  cwc_data <- data[grepl('_id|^visit|wcs', names(data))]
  
  #update names
  names(cwc_data) <- gsub('wcs', 'cwc', names(cwc_data))
  
  cwc_data <- util_format_cwc_data(cwc_data)
  
  cwc_data <- cwc_data[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(cwc_data)[grepl('cwc', names(cwc_data))])]
  
  #score 
  cwc_scored <- dataprepr::score_cwc(cwc_data, base_zero = FALSE, id = 'participant_id', pna_value = 99)
  
  cwc_json <- json_cwc()
  
  ## bis data ####
  bis_data <- data[grepl('_id|^visit|bis', names(data))]
  
  bis_data <- bis_data[c('participant_id','session_id', 'visit_date', 'visit_protocol', names(bis_data)[grepl('bis', names(bis_data))])]
  
  #score 
  bis_data['bis_score'] <- bis_data[['bis_1']] - bis_data[['bis_2']]
  
  bis_scored <- list(score_dat = bis_data[grepl('_id|^visit|bis_score', names(bis_data))],
                     bids_phenotype = bis_data)
  
  bis_json <- json_body_es()
  
  return(list(
    fnirs_cap = list(data = fnirs_cap, meta = fnirs_cap_json),
    anthro_data = list(data = child_anthro, meta = child_anthro_json),
    meal_info = list(data = meal_info, meta = meal_info_json), 
    fnirs_tasks_info = list(data = fnirs_info, meta = fnirs_info_json),
    pref_rank = list(data = prefrank, meta = prefrank_json),
    loc_data = list(data = loc_data, meta = loc_json),
    sic_data = list(data = sic_scored, meta = sic_json),
    cwc_data = list(data = cwc_scored, meta = cwc_json),
    body_es_data = list(data = bis_scored, meta = bis_json)))
  
}

