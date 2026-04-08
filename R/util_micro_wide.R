#' util_micro_wide: Separate microstructure summary behavior by coder for a single participant
#'
#' This function extract microstructure summary behavior by coder
#'
#'
#'
#' @inheritParams util_micro_switch
#' @inheritParams util_micro_switch
#' @inheritParams util_task_org_sourcedata
#' @param paradigm eating paradigm entered as string. Options include: 'meal' or 'eah'
#' @inheritParams util_micro_switch
#'
#'
#'
#' @return a data frame with 1 row and with data by coder for the following variables:
#' \itemize{
#'  \item{nbites}
#'  \item{nsips}
#'  \item{meal_dur}
#'  \item{bite_latency}
#'  \item{total_active_eating}
#'  \item{nbeh_distract}
#'  \item{start_meal_affect}
#'  \item{end_meal_affect}
#'  \item{nswitch}
#'  \item{nswitch_ed}
#'  \item{nswitch_wsip}
#'  \item{nswitch_wsip_distract}
#'  \item{bite_rate}
#'  \item{bite_rate_active}
#'  \item{sip_rate}
#'  \item{sip_rate_active}
#'  \item{switch_rate}
#'  \item{switch_ed_rate}
#'  \item{switch_wsip_rate}
#'  }
#'
#' @examples
#' #if in same working directory as data:
#' beh_wide <- util_micro_wide('participant_id', micro_data, ses_str = 1, paradigm = 'meal', coder_var = 'coder_order')
#'
#' \dontrun{
#'
#' }
#'
#'
#' @export
#'
util_micro_wide <- function(id_var, micro_data, ses_str, paradigm, coder_var) {

  #### 1. Set up/initial checks #####

  # check that base_wd exist and is a string
  data_arg <- methods::hasArg(micro_data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(micro_data)) {
      stop('micro_data must be entered as a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('micro_data must be entered as a data.frame')
  }

  # check that id exist in micro_data
  id_var_arg <- methods::hasArg(id)

  if (isTRUE(id_var_arg)) {
    if (!is.character(id_var)) {
      stop('id_var must be entered as a string')
    } else if (!(id_var %in% names(micro_data))){
      stop('id_var is not a variable name in micro_data')
    } else {
      if (!(id %in% micro_data[[id_var]])) {
        stop('id is not in micro_data[id_var]')
      }
    }
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

  if (isTRUE(ses_arg)) {
    if (!is.character(ses_str)) {
      stop('ses_str must be entered as a number')
    }
  } else if (isFALSE(ses_arg)) {
    stop('ses_str must be entered as a string')
  }

  # check that coder_var exist and is a string
  coder_var_arg <- methods::hasArg(coder_var)

  if (isTRUE(coder_var_arg)) {
    if (!is.character(coder_var)) {
      stop('coder_var must be entered as a string')
    } else if (!(coder_var %in% names(micro_data))) {
      stop('coder_var is not a variable in micro_data')
    }
  }

  # 2. Micro Wide Functions ####
  micro_wide_fn <- function(id_dat, coder_var, coder_val){

    if (!is.na(coder_var)) {
      id_dat_coder <- id_dat[id_dat[[coder_var]] == coder_val, ]
    } else {
      id_dat_coder <- id_dat
    }

    # compute values
    start_affect <- id_dat_coder[1, 'start_of_meal_affect']
    end_affect <- id_dat_coder[1, 'end_of_meal_affect']

    if (nrow(id_dat_coder[id_dat_coder[['behavior']] == 'Meal Duration', ]) > 0) {
      meal_dur <- max(id_dat_coder[id_dat_coder[['behavior']] == 'Meal Duration', 'duration'])
    } else {
      meal_dur <- NA
    }

    if (nrow(id_dat_coder[id_dat_coder[['behavior']] == 'Latency to First Bite', ]) > 0) {
      latency <- max(id_dat_coder[id_dat_coder[['behavior']] == 'Latency to First Bite', 'duration'])
    } else {
      latency <- NA
    }

    if (nrow(id_dat_coder[id_dat_coder[['behavior']] == 'Active Eating Time', ]) > 0) {
      active_dur <- max(id_dat_coder[id_dat_coder[['behavior']] == 'Active Eating Time', 'duration'])
    } else {
      active_dur <- NA
    }

    #first bite
    bites_dat <- id_dat_coder[id_dat_coder[['behavior']] == 'Bite', ]
    bite1_food <- bites_dat[1, 'food']

    nbites <- nrow(id_dat_coder[id_dat_coder[['behavior']] == 'Bite' & !is.na(id_dat_coder[['time_relative']]), ])
    nsips <- nrow(id_dat_coder[id_dat_coder[['behavior']] == 'Sips' & !is.na(id_dat_coder[['time_relative']]), ])
    n_distract <- nrow(id_dat_coder[id_dat_coder[['behavior']] == 'Leaving Chair' & !is.na(id_dat_coder[['time_relative']]), ])


    # get data.frame
    wide_data <- cbind.data.frame(start_affect, end_affect, bite1_food, nbites, nsips, meal_dur, latency, active_dur, n_distract)

    names(wide_data) <- c('start_meal_affect', 'end_meal_affect', 'bite1_food', 'nbites', 'nsips', 'meal_dur', 'bite_latency', 'total_active_eating', 'nbeh_distract')

    # switching count
    switch_var_list <- c('food_switch', 'food_ed_switch', 'food_sip_switch', 'food_sip_distract_switch')
    id_dat_coder[switch_var_list] <- sapply(switch_var_list, function(x) as.numeric(id_dat_coder[[x]]))

    wide_data[c('nswitch', 'nswitch_ed', 'nswitch_wsip', 'nswitch_wsip_distract')] <- colSums(id_dat_coder[c('food_switch', 'food_ed_switch', 'food_sip_switch', 'food_sip_distract_switch')], na.rm = TRUE)

    # make numeric
    wide_data[!grepl('bite1', names(wide_data))] <- sapply(wide_data[!grepl('bite1', names(wide_data))], function(x) as.numeric(x))

    #convert to minutes
    wide_data['meal_dur'] <- wide_data[['meal_dur']]/60
    wide_data['bite_latency'] <- wide_data[['bite_latency']]/60
    wide_data['total_active_eating'] <- wide_data[['total_active_eating']]/60
    
    #generate other data by coder
    wide_data[['bite_rate']] <- wide_data[['nbites']]/wide_data[['meal_dur']]

    wide_data[['bite_rate_active']] <- wide_data[['nbites']]/wide_data[['total_active_eating']]

    wide_data[['sip_rate']] <- wide_data[['nsips']]/wide_data[['meal_dur']]

    wide_data[['sip_rate_active']] <- wide_data[['nsips']]/wide_data[['total_active_eating']]

    wide_data[['switch_rate']] <- wide_data[['nswitch']]/wide_data[['nbites']]

    wide_data[['switch_ed_rate']] <- wide_data[['nswitch_ed']]/wide_data[['nbites']]

    wide_data[['switch_wsip_rate']] <- wide_data[['nswitch_ed']]/(wide_data[['nbites']] + wide_data[['nsips']])

    ## round
    wide_data[!grepl('bite1', names(wide_data))] <- sapply(names(wide_data)[!grepl('bite1', names(wide_data))], function(x) round(wide_data[[x]], 2))

    ## add coder_val to names
    if (!is.na(coder_var)) {
      names(wide_data) <- paste0(names(wide_data), '_', coder_val)
    }

    ## switching
    return(wide_data)
  }

  # wide beh wrapper
  wide_beh_wrapper <- function(id, id_var, dat, coder_var, coder_val){
    id_dat <- dat[dat[[id_var]] == id, ]

    if (isTRUE(coder_var_arg)){
      wide_data_beh <- as.data.frame(t(unlist(sapply(unique(id_dat[[coder_var]]), function(x) micro_wide_fn(id_dat, coder_var, x), simplify = FALSE))))
    } else {
      wide_data_beh <- as.data.frame(t(unlist(sapply(NA, function(x) micro_wide_fn(id_dat, coder_var, x)))))
    }

    wide_data <- cbind.data.frame(id, wide_data_beh)
    names(wide_data)[names(wide_data) == 'id'] <- id_var

    return(wide_data)
  }


  ## 3. compile database ####
  micro_wide_data <- data.frame(data.table::rbindlist(sapply(unique(micro_data[[id_var]]), function(x) wide_beh_wrapper(x, id_var, micro_data, coder_var, coder_val), simplify = FALSE, USE.NAMES = FALSE), fill = TRUE))

  names(micro_wide_data) <- gsub('X1.', '', names(micro_wide_data))
  names(micro_wide_data) <- gsub('X2.', '', names(micro_wide_data))

  micro_wide_data['session_id'] <- ses_str
  micro_wide_data['paradigm'] <- paradigm

  # organize data
  micro_wide_data <- micro_wide_data[c('participant_id', 'session_id', 'paradigm', names(micro_wide_data)[!grepl('_id|ses|visit|paradigm', names(micro_wide_data))])]

  return(micro_wide_data)
}
