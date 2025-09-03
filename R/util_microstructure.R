#' util_microstructure: Process raw microstructure coding data
#'
#' This function loads the .txt raw data files from ObserverXT. Cleaning the data involves:
#' \itemize{
#'    \item{ 1) loading data and cleaning data (fixing names, getting coder info, unique food bites/food combos)}
#'    \item{ 2) splitting coders and generating a wide summary microstructure data frame}
#'    \item{ 3) coding switches and foods tried}
#'    \item{ 4) computing all summary data}
#' }
#'
#'
#'
#' @inheritParams proc_tasks
#' @param file_str full path to exported ObserverXT coded data
#' @inheritParams util_micro_wide
#' @inheritParams util_task_org_sourcedata
#'
#'
#' @return Will return a list including data and metadata for
#' #' \itemize{
#'  \item{'beh_wide' - microstructure summary metrics}
#'  \item{'event' - full event-based data for each participant (long format)}
#' }
#'
#' @examples
#' #if in same working directory as data:
#' microstructure_list <- util_microstructure(base_wd_path)
#'
#' \dontrun{
#'
#' }
#'
#'
#' @export
#'
util_microstructure <- function(base_wd, file_str, paradigm, ses_str) {

  #### 1. Set up/initial checks #####

  # check that base_wd exist and is a string
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  # check that base_wd exist and is a string
  file_arg <- methods::hasArg(file_str)

  if (isTRUE(file_arg)) {
    if (!is.character(file_str)) {
      stop('file_str must be entered as a string')
    }
  } else if (isFALSE(data_arg)) {
    stop('file_str must be entered as a string')
  }

  # check that paradigm exist and is a string
  paradigm_arg <- methods::hasArg(paradigm)

  if (isTRUE(paradigm_arg)) {
    if (!is.character(paradigm)) {
      stop('paradigm must be entered as a string')
    } else {
      paradigm <- tolower(paradigm)

      if (!(paradigm %in% c('meal', 'eah'))) {
        stop('paradigm must be entered as either \'meal\' or \'eah\'')
      }
    }

  } else if (isFALSE(paradigm_arg)) {
    stop('paradigm must be entered as a string')
  }

  # check that ses_str exist and is a string
  ses_arg <- methods::hasArg(ses_str)

  if (isTRUE(file_arg)) {
    if (!is.character(ses_str)) {
      stop('ses_str must be entered as a string')
    } else {
      ses_str <- tolower(ses_str)
    }
  } else if (isFALSE(ses_arg)) {
    stop('ses_str must be entered as a string')
  }


  # 2. load and process data file ####s
  micro_data <- read.table(file_str, sep = ',', fileEncoding = 'utf-16le', header = TRUE)
  micro_data <- micro_data[!grepl('X', names(micro_data))]

  # fix one-off errors
  micro_data[micro_data['Observation'] == 'sub_009_DB_AC', 'Observation'] <- 'sub-009_DB_AC'
  
  #fix naming
  names(micro_data) <- tolower(names(micro_data))

  names(micro_data)[names(micro_data) == 'date_time_absolute_dmy_hmsf'] <- 'date_time'
  names(micro_data)[names(micro_data) == 'date_dmy'] <- 'date'
  names(micro_data)[names(micro_data) == 'time_absolute_hms'] <- 'time'
  names(micro_data)[names(micro_data) == 'time_absolute_f'] <- 'time_frames'
  names(micro_data)[names(micro_data) == 'time_relative_hmsf'] <- 'time_hmsf'
  names(micro_data)[names(micro_data) == 'time_relative_hms'] <- 'time_hms'
  names(micro_data)[names(micro_data) == 'time_relative_f'] <- 'time_relative_frames'
  names(micro_data)[names(micro_data) == 'time_relative_sf'] <- 'time_relative'
  names(micro_data)[names(micro_data) == 'duration_sf'] <- 'duration'

  if (paradigm == 'meal') {
    names(micro_data)[names(micro_data) == 'modifier_1'] <- 'grape'
    names(micro_data)[names(micro_data) == 'modifier_2'] <- 'carrot'
    names(micro_data)[names(micro_data) == 'modifier_3'] <- 'cknug'
    names(micro_data)[names(micro_data) == 'modifier_4'] <- 'mac'
    names(micro_data)[names(micro_data) == 'modifier_5'] <- 'ketchup'
    names(micro_data)[names(micro_data) == 'modifier_6'] <- 'hand'
    names(micro_data)[names(micro_data) == 'modifier_7'] <- 'fork'
    names(micro_data)[names(micro_data) == 'modifier_8'] <- 'other'
    names(micro_data)[names(micro_data) == 'modifier_9'] <- 'child_activity'

  } else {

  }

  # replace '.' with '_'
  names(micro_data) <- gsub('\\.', '_', names(micro_data))

  # parse observation string - need to use data.table and then convert back in case there is missing information in observation string
  obsv_data <- data.frame(data.table::rbindlist(sapply(micro_data[['observation']], function(x) data.table::transpose(data.table::setDT(strsplit(x, '_'))), USE.NAMES = FALSE, simplify = FALSE), fill = TRUE))

  names(obsv_data) <- c('participant_id', 'ph1_coder', 'ph2_coder')
  
  # add to micro_dat
  micro_data['participant_id'] <- obsv_data['participant_id']
  micro_data['ph1_coder'] <- obsv_data['ph1_coder']
  micro_data['ph2_coder'] <- obsv_data['ph2_coder']

  # get n coders
  micro_data['n_coders'] <- sapply(micro_data[['participant_id']], function(x) length(unique(micro_data[micro_data['participant_id'] == x, 'ph1_coder'])))

  coder_info <- function(id){
    data <- micro_data[micro_data['participant_id'] == id, ]

    obs_unique <- unique(data['ph1_coder'])
    
    if(nrow(obs_unique) == 1) {
      return(rep(1, nrow(data)))
      
    } else {

      coder_order <- c(rep(1, nrow(data[data['ph1_coder'] == obs_unique[1, ], ])), rep(2, nrow(data[data['ph1_coder'] == obs_unique[2, ], ])))

      return(coder_order)
    }
  }

  #order by participant_id
  micro_data <- micro_data[order(micro_data[['participant_id']]), ]
  
  micro_data['coder_order'] <- unlist(sapply(unique(micro_data[['participant_id']]), function(x) coder_info(x), USE.NAMES = FALSE))

  ## concatenate foods
  if (paradigm == 'meal') {
    micro_data['cknug'] <- gsub('chicken nugget', 'cknug', micro_data[['cknug']])
    micro_data['mac'] <- gsub('mac and cheese', 'mac', micro_data[['mac']])
    micro_data['ketchup'] <- gsub('katchup', 'ketchup', micro_data[['ketchup']])

    micro_data['food'] <- paste0(micro_data[['cknug']], micro_data[['mac']], micro_data[['grape']], micro_data[['carrot']], micro_data[['ketchup']])

    micro_data['food'] <- gsub('cknugketchup', 'cknug_ketchup', micro_data[['food']])
    micro_data['food'] <- gsub('macketchup', 'mac_ketchup', micro_data[['food']])
    micro_data['food'] <- gsub('carrotketchup', 'carrot_ketchup', micro_data[['food']])
    micro_data['food'] <- gsub('cknugmac', 'cknug_mac', micro_data[['food']])
    micro_data['food'] <- gsub('cknugcarrot_ketchup', 'cknug_carrot_ketchup', micro_data[['food']])
    micro_data['food'] <- gsub('maccarrot', 'mac_carrot', micro_data[['food']])
    micro_data['food'] <- gsub('maccarrotketchup', 'mac_carrot_ketchup', micro_data[['food']])
    micro_data['food'] <- gsub('macgrape', 'mac_grape', micro_data[['food']])
    
    
    hed_foods <- c('mac', 'cknug', 'ketchup')
    led_foods <- c('grape', 'carrot', 'carrot_ketchup')

    # food ed
    micro_data[['food_ed']] <- ifelse(is.na(micro_data[['food']]), NA, ifelse(micro_data[['food']] == '', NA, ifelse(micro_data[['food']] %in% hed_foods | gsub('_ranch|ranch_|_ketchup|ketchup_', '', micro_data[['food']]) %in% hed_foods, 'h_ed', ifelse(micro_data[['food']] %in% led_foods | gsub('_ranch|ranch_|_ketchup|ketchup_', '', micro_data[['food']]) %in% led_foods, 'l_ed', ifelse(grepl('mac', micro_data[['food']]) & grepl('cknug', micro_data[['food']]), 'h_ed', ifelse(grepl('grape', micro_data[['food']]) & grepl('carrot', micro_data[['food']]), 'l_ed', 'mixed_ed'))))))

  }

  # food/sip
  micro_data[['food_sip']] <- ifelse(is.na(micro_data[['food']]) | micro_data[['food']] == '', ifelse(micro_data[['behavior']] == 'Sips', 'sip', micro_data[['food']]), micro_data[['food']])

  # food/sip/beh
  micro_data[['food_sip_distract']] <- ifelse(is.na(micro_data[['food']]) | micro_data[['food']] == '', ifelse(micro_data[['behavior']] == 'Sips', 'sip', ifelse(micro_data[['behavior']] == 'Leaving Chair', 'distract_beh', micro_data[['food']])), micro_data[['food']])

  # 3. Switching Data ####

  micro_data_switch <- data.frame(sapply(c('food', 'food_ed', 'food_sip', 'food_sip_distract'), function(x) util_micro_switch(id_var = 'participant_id', micro_data = micro_data, switch_var = x, coder_var = 'coder_order')))
  names(micro_data_switch) <- paste0(names(micro_data_switch), '_switch')

  micro_data <- cbind.data.frame(micro_data, micro_data_switch)

  # fix order and update names
  micro_data['session_id'] <- ses_str
  micro_data <- micro_data[c('participant_id', 'session_id', names(micro_data)[grepl('coder', names(micro_data))], 'observation', names(micro_data)[grepl('date|time|affect', names(micro_data))], names(micro_data)[!grepl('id|coder|observation|date|time|affect', names(micro_data))])]

  # json
  micro_events_json <- json_micro_events()
  
  ## 4. Make Summary Data ####
  micro_data_wide <- util_micro_wide(id_var = 'participant_id', micro_data, ses_str = ses_str, paradigm = paradigm, coder_var = 'coder_order')

  # json
  micro_beh_json <- json_micro_beh()
  
  ## make list of data frame and associated labels
  meal_micro <- list(
    beh_wide_data = list(data = micro_data_wide, meta = micro_beh_json),
    event_data = list(data = micro_data, meta = micro_events_json))

  ## want an export options??

  return(meal_micro)
}
