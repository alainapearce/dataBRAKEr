#' score_demo_restricted: Score restricted demographic data from Study BRAKE 
#'
#' This function scores restricted demographic data from Study BRAKE and provides summary information that is less specific/restricted
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @param demo_data a data.frame with restricted PPI information from Study BRAKE (from util_redcap_prepost1.R)
#' @param score_base the smallest value assigned to a choice is 0 (i.e., range 0-3). Default = TRUE.
#' @param id (optional) name of participant ID column in bes_data. If included the output dataset will be matched by id, if not included the output dataset will be in the order of bes_data but will have no participant identifier. Required to get the phenotype dataset (raw data merged with scores.)
#'
#' @return A dataset with summary demographic information for each participant
#' @examples
#'
#' # scoring for the hfi with IDs
#' demo_data <- score_demo_restricted(demo_data, id = 'ID')
#'
#'
#' @export

score_demo_restricted <- function(demo_data, score_base = TRUE, id) {

    #### 1. Set up/initial checks #####

    # check that demo_data exist and is a data.frame
    data_arg <- methods::hasArg(demo_data)

    if (isTRUE(data_arg) & !is.data.frame(demo_data)) {
        stop('demo_data must be entered as a data.frame')
    } else if (isFALSE(data_arg)) {
        stop('demo_data must set to the data.frame')
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(demo_data))) {
            stop('variable name entered as id is not in demo_data')
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    demo_score_dat <- data.frame(county_pop_denisty = rep(NA, nrow(demo_data)), zmunicipality_pop_denisty = rep(NA, nrow(demo_data)), zip_pop_denisty = rep(NA, nrow(demo_data)), censustrack_pop_denisty = rep(NA, nrow(demo_data)))

    if (isTRUE(ID_arg)) {
        demo_score_dat <- data.frame(demo_data[[id]], demo_score_dat)
        names(demo_score_dat)[1] <- id
    }
    
    # re-scale data
    demo_data_edit <- demo_data
    
    
    #### 3. Clean Export/Scored Data #####
    ## merge raw responses with scored data
    if (isTRUE(ID_arg)){
      hfi_phenotype <- merge(demo_data, demo_score_dat, by = id)
      
      return(list(score_dat = as.data.frame(demo_score_dat),
                  bids_phenotype = as.data.frame(hfi_phenotype)))
    } else {
      return(list(score_dat = as.data.frame(demo_score_dat)))
    }

}

