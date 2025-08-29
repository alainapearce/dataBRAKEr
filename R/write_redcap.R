#' write_redcap: Write selected data and json files from processed REDCap data
#'
#' This function:
#' \itemize{
#'    \item{1) Calls proc_redcap function to get clean and compiled data and metadata}
#'    \item{2) Exports all or select BIDS-compliant .tsv and .json files into bids/phenotype}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @inheritParams proc_tasks
#' @inheritParams util_task_org_sourcedata
#' @param data_list list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'paticipants' - BIDS specified participants.tsv file}
#'  \item{'anthropometrics' - height, weight, and computed anthropometric data}
#'  \item{'demographics' - compiled demographic data}
#'  \item{'bodpod' - verified BodPod data}
#'  \item{'fnirs_info' - compiled fNIRS task-related information}
#'  \item{'dkefs' - verified D-KEFS data}
#'  \item{'wasi' - verified WASI data}
#'  \item{'intake' - compiled verified intake data with computed intake values}
#'  \item{'tasttest_samples' - verified Taste-Test sample weights}
#'  \item{'household' - compiled demographicinformation about houshold}
#'  \item{'infancy' - compiled demographic information related to infancy}
#'  \item{'bes' - Binge Eating Scale}
#'  \item{'brief2' - Behavioral Rating Inventory of Executive Function-2}
#'  \item{'cbq' - Child Behavior Questionnaire}
#'  \item{'cebq' - Children's Eating Behavior Questionnaire}
#'  \item{'cfq' - Child Feeding Questionnaire}
#'  \item{'cshq' - Children Sleep Habits Questionnaire}
#'  \item{'efcr' - External Food Cue Responsiveness Scale}
#'  \item{'ffbs' - Family Food Behavior Survey}
#'  \item{'ffq' - HELIX cohort Food Frequency Questionnaire}
#'  \item{'fmcb' - Feeding to Manage Child Behavior Questionnaire}
#'  \item{'hfe' - Home Food Environment}
#'  \item{'hfi' - Fulkerson Home Food Inventory}
#'  \item{'lbc' - Lifestyle Behavior Checklist}
#'  \item{'loc' - Loss of Control-Eating Questionnaire}
#'  \item{'puberty' - combination of Tanner and Pubertal Rating Scale}
#'  \item{'pwlb' - Parent Weight-Loss Behavior Questionnaire}
#'  \item{'sic' - Stress in Children Questionnaire}
#'  \item{'sleeplog' - Week long sleep log}
#'  \item{'spsrq' - Sensitivity to Punishment and Sensitivity to Reward Questionnaire}
#'  \item{'tfeq' - Three Factor Eating Questionnaire}
#' }
#' @inheritParams util_merged_intake
#' @inheritParams util_group_foodrating
#'
#' @return Does not return anything
#'
#'
#' @examples
#'
#' \dontrun{
#' write_redcap(base_wd, overwrite = FALSE, data_list = 'all')
#'
#' }
#'
#'
#' @export

write_redcap <- function(base_wd, overwrite = FALSE, data_list = 'all', tastetest_data, return_data = FALSE) {

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

  #### Get REDCap Data ####
  
  print('-- loading REDCap data from the API')
  
  # get data from REDCap directly (only will work if have access and keys setup)
  Sys.setenv(brake_redcap_key = keyring::key_get('brake_redcap_key'))
  redcap_visit <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/', token = Sys.getenv('brake_redcap_key'))
  
  
  Sys.setenv(brake_de_redcap_key = keyring::key_get('brake-de_redcap_key'))
  redcap_de <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/',
                                     token = Sys.getenv('brake_de_redcap_key'))
  
  redcap_visit_data <- redcap_visit[['data']]
  redcap_visit_dict <- redcap_visit[['dictionary']]

  redcap_de_data <- redcap_de[['data']]
  redcap_de_dict <- redcap_de[['dictionary']]

  # remove '.factor'
  redcap_visit_data <- redcap_visit_data[, !grepl('.factor', names(redcap_visit_data))]
  redcap_de_data <- redcap_de_data[, !grepl('.factor', names(redcap_de_data))]

  # Make ID column bids compliant: Convert record_id to strings padded with zeros and add 'sub_'
  redcap_visit_data <- redcap_visit_data[!grepl('PILOT|pilot-6', redcap_visit_data[['record_id']]), ]
  redcap_visit_data['record_id'] <- sprintf('sub-%03d', as.numeric(redcap_visit_data[['record_id']]))

  # set paths for other directories
  bids_wd <- file.path(base_wd, 'bids')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')

  #### Process REDCap data ####
  proc_redcap_data <- proc_redcap(redcap_api = FALSE, redcap_visit_data, redcap_de_data, tastetest_data)

  # quick fixes for notes where /n formatting got saved
  proc_redcap_data$intake$data[grepl('notes', names(proc_redcap_data$intake$data))] <- sapply(names(proc_redcap_data$intake$data)[grepl('notes', names(proc_redcap_data$intake$data))], function(x) gsub('\n', '', proc_redcap_data$intake$data[[x]]))

  proc_redcap_data$mri_visit$data[grepl('notes', names(proc_redcap_data$mri_visit$data))] <- sapply(names(proc_redcap_data$mri_visit$data)[grepl('notes', names(proc_redcap_data$mri_visit$data))], function(x) gsub('\n', '', proc_redcap_data$mri_visit$data[[x]]))

  proc_redcap_data$pstca$data[grepl('response|pstca_29i', names(proc_redcap_data$pstca$data))] <- sapply(names(proc_redcap_data$pstca$data)[grepl('response|pstca_29i', names(proc_redcap_data$pstca$data))], function(x) gsub('\n|- ', ' ', proc_redcap_data$pstca$data[[x]]))

  proc_redcap_data$fsq$data[grepl('resources|fsq_12', names(proc_redcap_data$fsq$data))] <- sapply(names(proc_redcap_data$fsq$data)[grepl('resources|fsq_12', names(proc_redcap_data$fsq$data))], function(x) gsub('\n|- ', ' ', proc_redcap_data$fsq$data[[x]]))

  #### function to export data and metadata ####

  data_list_options <- c('participants', 'anthropometrics', 'demographics', 'bodpod', 'fnirs_info', 'dkefs', 'wasi', 'intake', 'tasttest_samples', 'household', 'infancy',  'bes', 'brief2', 'cbq', 'cebq', 'cfq', 'cshq', 'efcr', 'ffbs', 'ffq', 'fmcb', 'hfe', 'hfi', 'lbc', 'loc', 'puberty', 'pwlb', 'sic', 'sleeplog', 'spsrq', 'tfeq')

  if (length(data_list) == 1) {
    if (data_list == 'all'){
      data_list <- data_list_options
    }
  }

  # loop through data_to_export and export data and meta-data
  redcap_export <- function(data_str, overwrite){

    if (data_str %in% data_list_options){

      if (data_str == 'participants'){
        filename_tsv <- file.path(bids_wd, paste0(data_str, '.tsv'))
        filename_json <- file.path(bids_wd, paste0(data_str, '.json'))
      } else {
        filename_tsv <- file.path(phenotype_wd, paste0(data_str, '.tsv'))
        filename_json <- file.path(phenotype_wd, paste0(data_str, '.json'))
      }

      # write tsv
      if ( isTRUE(overwrite) | !file.exists(filename_tsv) ) {
        # use 'n/a' for missing values for BIDS compliance

        write.table(proc_redcap_data[[data_str]]$data, filename_tsv, quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE, na = 'n/a')
      }

      # write json
      if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
        write(proc_redcap_data[[data_str]]$meta, filename_json)
      }

    } else {
      print(paste0(data_str, ' is not one of the available data set options to print in write_redcap(). Please see help(write_redcap)'))
    }
  }

  print('-- writing REDCap data')
  
  write_redcap_output <- sapply(data_list, function(x) redcap_export(x, overwrite))

  #### Return Data ####
  if (isTRUE(return_data)) {
    return(proc_redcap_data)
  }
}

