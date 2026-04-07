#' write_microstructure: Write selected data and json files from processed microstructure data
#'
#' This function:
#' \itemize{
#'    \item{1) Calls proc_microstructure function to get clean and compiled data and metadata}
#'    \item{2) Exports all or select BIDS-compliant .tsv and .json files into bids/phenotype and/or bids/rawdata}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @inheritParams proc_tasks
#' @inheritParams proc_microstructure
#' @inheritParams util_task_org_sourcedata
#' @param micro_protocols list of strings matching the notes below to indicate the which microstructure data. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'meal-baseline' - meal microstructure behavior at baseline}
#'  \item{'meal-followup' - meal microstructure behavior at followup}
#'  \item{'eah' - EAH microstructure behavior}
#' }
#' @param data_type Type of data to process for meal microstructure - list of strings matching the data types listed below. Default = 'all' to export both:
#'  \itemize{
#'    \item{'beh_wide' - summary behavioral measures in wide formate by coder. Note: this will write out a summary dataset in bids/phenotype.}
#'    \item{'events_long' - event level data in log format by coder. Note: this writes out a file per participant into bids/rawdata.}
#'  }
#' @inheritParams write_tasks
#'
#' @return Does not return anything
#'
#'
#' @examples
#'
#' \dontrun{
#' write_microstructure(base_wd, intake_data, overwrite = FALSE, micro_protocols = 'all', micro_data_type = 'all')
#'
#' }
#'
#' @export

write_microstructure <- function(base_wd, intake_data, overwrite = FALSE, micro_protocols = 'all', data_type = 'all', return_data = FALSE) {
  
  #### Set up/initial checks #####
  
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
  
  # set paths for other directories
  bids_wd <- file.path(base_wd, 'bids')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  micro_raw_dir <- file.path(base_wd, 'bids', 'rawdata')
  
  # get microstructure data
  proc_micro_data <- proc_microstructure(base_wd, intake_data)
  
  # get 'all' list
  micro_protocols_options <- c('meal-baseline', 'meal-followup', 'eah')
  micro_data_type_options <- c('beh_wide', 'events_long')
  
  if (length(micro_protocols) == 1){
    if (micro_protocols == 'all'){
      micro_protocols_use = micro_protocols_options
    } else {
      micro_protocols_use = micro_protocols
    }
  } else {
    micro_protocols_use = micro_protocols
  }
  
  if (length(micro_data_type) == 1){
    if (micro_data_type == 'all'){
      micro_data_type_use = micro_data_type_options
    } else {
      micro_data_type_use = micro_data_type
    }
  } else {
    micro_data_type_use = micro_data_type
  }
  
  # 2. process/save behavioral data ####
  if ('beh_wide' %in% micro_data_type_use){
    
    # meal
    if (sum(grepl('meal', micro_protocols_use)) > 0){
      # if both meals, merge
      if (sum(grepl('meal', micro_protocols_use)) == 2){
        
        meal_dat_name1 <- micro_protocols_use[grepl('meal', micro_protocols_use)][1]
        meal_dat_name1 <- paste0(meal_dat_name1, '_beh')
        
        meal_dat_name2 <- micro_protocols_use[grepl('meal', micro_protocols_use)][2]
        meal_dat_name2 <- paste0(meal_dat_name2, '_beh')
        
        meal_dat_save <- rbind.data.frame(proc_micro_data[[meal_dat_name1]]$data, proc_micro_data[[meal_dat_name2]]$data)
        
      } else if (sum(grepl('meal', micro_protocols_use)) == 1){
        meal_dat_name1 <- micro_protocols_use[grepl('meal', micro_protocols_use)]
        meal_dat_name1 <- paste0(meal_dat_name1, '_beh')
        
        meal_dat_save <- proc_micro_data[[meal_dat_name1]]$data
      } 
      
      s <- proc_micro_data[[meal_dat_name1]]$meta
      
      meal_filename_tsv <- file.path(phenotype_wd, 'microstructure.tsv')
      meal_filename_json <- file.path(phenotype_wd, 'microstructure.json')
      
      # write tsv
      if ( isTRUE(overwrite) | !file.exists(meal_filename_tsv) ) {
        # use 'n/a' for missing values for BIDS compliance
        
        write.table(meal_dat_save, meal_filename_tsv, quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE, na = 'n/a')
      }
      
      # write json
      if ( isTRUE(overwrite) | !file.exists(meal_filename_json) ) {
        write(meal_json_beh, meal_filename_json)
      }
    }
    
    if (sum(grepl('eah', micro_protocols_use)) == 1){
      eah_dat_name1 <- micro_protocols_use[grepl('eah', micro_protocols_use)]
      eah_dat_name1 <- paste0(eah_dat_name1, '_beh')
      
      eah_dat_save <- proc_micro_data[[eah_dat_name1]]$data
      
      eah_json_beh <- proc_micro_data[[eah_dat_name1]]$meta
      
      eah_filename_tsv <- file.path(phenotype_wd, 'eah_bites.tsv')
      eah_filename_json <- file.path(phenotype_wd, 'eah_bites.json')
      
      # write tsv
      if ( isTRUE(overwrite) | !file.exists(eah_filename_tsv) ) {
        # use 'n/a' for missing values for BIDS compliance
        
        write.table(eah_dat_save, eah_filename_tsv, quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE, na = 'n/a')
      }
      
      # write json
      if ( isTRUE(overwrite) | !file.exists(eah_filename_json) ) {
        write(eah_json_beh, eah_filename_json)
      }
      
    }
  }
  
  
  # 3. process/save in events data for each participant ####
  if ('events_long' %in% micro_data_type_use){
    
    # add if statement to check for file/overwrite option
    raw_save <- function(micro_data, id, micro_raw_dir, ses_str, name_str, paradigm, overwrite){
      
      # get directory and check it existis
      micro_sub_dir <- file.path(micro_raw_dir, id, ses_str, 'videos')
      
      if (!dir.exists(micro_sub_dir)) {
        dir.create(micro_sub_dir, recursive = TRUE)
      }
      
      # save file path
      save_file_path <- file.path(micro_sub_dir, paste0(id, '_', ses_str, '_', paradigm, '-', name_str, '_events.tsv'))
      
      # check if file exists or should overwrite
      
      file_exists <- file.exists(save_file_path)
      
      if (isFALSE(file_exists) || isTRUE(overwrite)){
        data <- micro_data[micro_data['participant_id'] == id, ]
        print(save_file_path)
        write.table(data, file = save_file_path, sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
        
        return(paste0(id, ' raw saved'))
        
      } else {
        return(paste0(id, ' raw not overwritten'))
      }
    }
    
    
    # loop through micro_protocols and to run save function for events
    for (d in micro_protocols_use) {
      data_str <- paste0(d, '_events')
      
      micro_protocols_subset <- proc_micro_data[grepl(data_str, names(proc_micro_data))]
      
      #get info
      ses_str <- ifelse(grepl('baseline', data_str), 'ses-baseline', 'ses-followup')
      paradigm <- ifelse(grepl('meal', data_str), 'meal', 'eah')
      
      # export dataset_description.json
      name_str <- ifelse(grepl('meal', data_str), 'micro', 'bites')
      
      filename_json <- file.path(bids_wd, paste0(paradigm, '-', name_str, '_events.json'))
      
      json_events <- micro_protocols_subset[[data_str]]$meta
      
      if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
        write(json_events, filename_json)
      }
      
      micro_data_long <- proc_micro_data[[data_str]]$data
      
      save_msg <- sapply(unique(micro_data_long[['participant_id']]), function(x) raw_save(micro_data = micro_data_long, id = x, micro_raw_dir, ses_str, paradigm = paradigm, name_str = name_str, overwrite))
    }
  }
  
  
  #### Return Data ####
  if (isTRUE(return_data)) {
    return(proc_micro_data)
  }
}

