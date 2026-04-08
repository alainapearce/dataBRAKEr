#' util_micro_switch: Code switching between microstructure behaviors
#'
#' This function code switches between microstructure behaviors for all participants in micro_data
#'
#'
#' @param id_var (string) name of participant ID variable
#' @param micro_data data frame that contains ObserverXT event data where column id_var has values matching id
#' @param switch_var (string) name of variable in micro_data to code switching on - must match variable name in micro_data
#' @param coder_var (string) name of variable that indicates coder order (e.g., 1, 2 or 'coder 1', 'coder 2') if some participants were coded by multiple coders. micro_data should be in long format with coded events for each coder stacked rather than organized in separate columns
#'
#'
#' @return Will return a data.frame with the following variable:
#' \itemize{
#'  \item{switch_var: coded switches}
#' }
#'
#' @examples
#' #if in same working directory as data:
#' switching <- util_micro_switch('participant_id', micro_data, 'food', 'coder_order')
#'
#' \dontrun{
#'
#' }
#'
#'
#' @export
#'
util_micro_switch <- function(id_var, micro_data, switch_var, coder_var) {

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

  # check that switch_var exist and is a string
  switch_arg <- methods::hasArg(switch_var)
  
  if (isTRUE(switch_arg)) {
    if (!is.character(switch_var)) {
      stop('switch_var must be entered as a string')
    } 
    
    switch_var <- tolower(switch_var)
    
    if (!(switch_var %in% names(micro_data))) {
      stop('coder_var is not a variable in micro_data')
    }
    
  } else if (isFALSE(switch_arg)) {
    stop('switch_var must be entered as a string')
  }

  # check that coder_var exist and is a string
  coder_var_arg <- methods::hasArg(coder_var)

  if (isTRUE(coder_var_arg)) {
    if (!is.character(coder_var)) {
      stop('coder_var must be entered as a string')
    } else if (!(coder_var %in% names(micro_data))) {
      stop('coder_var is not a variable in micro_data')
    }
  } else if (isFALSE(coder_var_arg)) {
    stop('coder_var must be entered as a string')
  }


  # 2. Define Switching Functions ####

  # code switches by event
  switch_fn <- function(food, event){
    if (event == 1 | is.na(food[[event]])) {
      switch <- NA
    } else if (food[[event]] != food[[event - 1]]){
      switch <- 1
    } else {
      switch <- 0
    }
  }

  switch_code_var <- function(id_dat, switch_var, coder_var, coder_val){

    if (!is.na(coder_var)) {
      id_dat_coder <- id_dat[id_dat[[coder_var]] == coder_val, ]
    } else {
      id_dat_coder <- id_dat
    }

    # variable strings
    switch_event <- paste0(switch_var, '_event')
    switch_varname <- paste0(switch_var, '_switch')

    # get event num and foods for switch_var != NA or '' only
    id_dat_coder[!(id_dat_coder[[switch_var]] %in% c(NA, '')), switch_event] <- seq(1, nrow(id_dat_coder[!(id_dat_coder[[switch_var]] %in% c(NA, '')), ]), 1)

    food_list <- id_dat_coder[!(id_dat_coder[[switch_var]] %in% c(NA, '')), switch_var]

    # code switches
    id_dat_coder[!(id_dat_coder[[switch_var]] %in% c(NA, '')), switch_varname] <- sapply(seq(1, max(id_dat_coder[[switch_event]], na.rm = TRUE), 1), function(x) switch_fn(food_list, x))

    return(id_dat_coder[[switch_varname]])

  }

  # 3. get switching ####
  # switch wrapper
  switch_wrapper_food <- function(id, switch_var, dat, coder_var_arg){
    id_dat <- dat[dat[[id_var]] == id, ]

    switch_varname <- paste0(switch_var, '_switch')

    if (isTRUE(coder_var_arg)){
      id_dat[switch_varname] <- unlist(sapply(unique(id_dat[[coder_var]]), function(x) switch_code_var(id_dat, switch_var, coder_var, x), simplify = FALSE))
    } else {
      id_dat[switch_varname] <- unlist(sapply(NA, function(x) switch_code_var(id_dat, switch_var, NA, x)))
    }

    return(id_dat[[switch_varname]])
  }

  # execute
  switch_varname <- paste0(switch_var, '_switch')

  micro_data[[switch_varname]] <- unlist(sapply(unique(micro_data[[id_var]]), function(x) switch_wrapper_food(x, switch_var, micro_data, coder_var_arg), USE.NAMES = FALSE))

  return(micro_data[[switch_varname]])

}
