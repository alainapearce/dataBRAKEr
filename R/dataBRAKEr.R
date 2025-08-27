#' dataBRAKEr: Process Study BRAKE data
#'
#' This function:
#' \itemize{Calls core proc_\* and write_\* functions and writes BIDS-compliant .tsv and .json files into bids/phenotype:
#'    \item{write_redcap: processes and writes out specified REDCap data}
#'    \item{proc_microstructure: processes and writes out microstructure data}
#'    \item{proc_tasks:}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @inheritParams proc_tasks
#' @inheritParams util_task_org_sourcedata
#' @param data_list list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'participants' - BIDS specified participants.tsv file}
#'  \item{'actigraph' - activity and sleep data generated from GGIR and mMARCH.AC}
#'  \item{'microstructure' - coded meal microstructure data}
#'  \item{'foodview' - fMRI Food Viewing task}
#'  \item{'nihtoolbox' - NIH Toolbox data}
#'  \item{'pit' - Pavlovian Instrumental Transfer task data}
#'  \item{'rrv' - Relative Reinforcing Value of Food task}
#'  \item{'spacegame' - Space Game data (need to finish processing in Matlab)}
#'  \item{'sst' - fMRI Stop-Signal Task data}
#'  \item{'anthropometrics' - height, weight, and computed anthropometric data}
#'  \item{'demographics' - compiled demographic data}
#'  \item{'dxa' - verified DXA data}
#'  \item{'household' - compiled demographicinformation about houshold}
#'  \item{'infancy' - compiled demographic information related to infancy}
#'  \item{'intake' - compiled intake data with computed intake values}
#'  \item{'mri_visit' - MRI visit information including Freddy and CAMS}
#'  \item{'parent_updates' - all visit updates}
#'  \item{'researcher_notes' - all visit notes}
#'  \item{'audit' - Alcohol Use Disorders Identification Test}
#'  \item{'bes' - Binge Eating Scale}
#'  \item{'bisbas' - Behavioral Inhibition System/Behavioral Activation System}
#'  \item{'brief2' - Behavioral Rating Inventory of Executive Function-2}
#'  \item{'cbq' - Child Behavior Questionnaire}
#'  \item{'cchip' - Community Childhood Hunger Identification Project}
#'  \item{'cebq' - Children's Eating Behavior Questionnaire}
#'  \item{'cfpq' - Comprehensive Feeding Practices Questionnaire}
#'  \item{'cfq' - Child Feeding Questionnaire}
#'  \item{'chaos' - Confusion, Hubbub, and Order Scale}
#'  \item{'class' - *need*}
#'  \item{'cshq' - Children Sleep Habits Questionnaire}
#'  \item{'debq' - Dutch Eating Behavior Questionnaire}
#'  \item{'efcr' - External Food Cue Responsiveness Scale}
#'  \item{'ffbs' - Family Food Behavior Survey}
#'  \item{'fsq' - *need*}
#'  \item{'hfi' - Fulkerson Home Food Inventory}
#'  \item{'hfias' - Household Food Insecurity Access Scale}
#'  \item{'hfssm' - U.S. Household Food Security Survey Module}
#'  \item{'kbas' - Kid's Brand Awareness Scale}
#'  \item{'lbc' - Lifestyle Behavior Checklist}
#'  \item{'loc' - Loss of Control-Eating Questionnaire}
#'  \item{'pmum' - Problematic Media Use Measure *need*}
#'  \item{'pptq' - Pictorial Personality Traits Questionnaire for Children}
#'  \item{'pss' - Perceived Stress Scale}
#'  \item{'pstca' - *need*}
#'  \item{'puberty' - combination of Tanner and Pubertal Rating Scale}
#'  \item{'pwlb' - Parent Weight-Loss Behavior Questionnaire}
#'  \item{'rank' - Parent ranking of foods sources? *need*}
#'  \item{'scpf' - tructure and Control in Parent Feeding Questionnaire}
#'  \item{'sic' - Stress in Children Questionnaire *need*}
#'  \item{'sleeplog' - Week long sleep log}
#'  \item{'spsrq' - Sensitivity to Punishment and Sensitivity to Reward Questionnaire}
#'  \item{'stq' - Screen Time Questionnaire *need*}
#'  \item{'tfeq' - Three Factor Eating Questionnaire}
#' }
#' @param micro_protocols (optional) list of strings matching the notes below to indicate the which microstructure data. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'meal-boring' - meal microstructure behavior after boring commercials}
#'  \item{'meal-food' - meal microstructure behavior after food commercials}
#'  \item{'meal-toy' - meal microstructure behavior after toy commercials}
#'  \item{'eah-boring' - EAH microstructure behavior after boring commercials}
#'  \item{'eah-food' - EAH microstructure behavior after food commercials}
#'  \item{'eah-toy' - EAH microstructure behavior after toy commercials}
#' }
#' @param micro_data_type (optional) Type of data to process for meal microstructure - list of strings matching the data types listed below. Default = 'all' to export both:
#'  \itemize{
#'    \item{'beh_wide' - summary behavioral measures in wide formate by coder. Note: this will write out a summary dataset in bids/phenotype.}
#'    \item{'events_long' - event level data in log format by coder. Note: this writes out a file per participant into bids/rawdata.}
#'  }
#' @inheritParams write_tasks
#' @inheritParams proc_actigraph
#'
#'
#' @return Does not return anything
#'
#'
#' @examples
#'
#' \dontrun{
#' dataBRAKEr(base_wd, overwrite = FALSE, data_list = 'all')
#'
#' }
#'
#' @importFrom utils tail write.table read.csv head
#'
#' @export

dataBRAKEr <- function(base_wd, overwrite = FALSE, data_list = 'all', data_type = 'all', micro_protocols = 'all', micro_data_type = 'all', overwrite_ggir_derivs = FALSE, return_data = FALSE) {

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

  ## dataset description - requried by BIDS ####
  # export dataset_description.json
  # json_dataset_desc <- json_phe_dataset_desc(visit_data_path, data_de_path)
  # filename_dataset_json <- file.path(phenotype_wd, 'dataset_description.json')

  # if ( isTRUE(overwrite) | !file.exists(filename_dataset_json) ) {
  #   write(json_dataset_desc, filename_json)
  # }

  #### function to export data and metadata ####

  # data for 'all' option - need to add actigraph eventually

  # data from redcap
  redcap_data_options <- c('participants', 'anthropometrics', 'demographics', 'dxa', 'household', 'infancy', 'intake', 'mri_visit', 'parent_updates', 'researcher_notes', 'audit', 'bes', 'bisbas', 'brief2', 'cbq', 'cchip', 'cebq', 'cfpq', 'cfq', 'chaos', 'class', 'cshq', 'debq', 'efcr', 'ffbs', 'fsq', 'hfi', 'hfias', 'hfssm', 'kbas', 'lbc', 'loc', 'pmum', 'pptq', 'pss', 'pstca', 'puberty', 'pwlb', 'rank', 'scpf', 'sic', 'sleeplog', 'spsrq', 'stq', 'tfeq')

  # task data
  task_data_options <- c('sst','foodview','spacegame','nih_toolbox','rrv','pit')

  if (length(data_list) == 1) {
    if (data_list == 'all') {
      data_list = c(redcap_data_options, 'microstructure', task_data_options)
    }
  }

  # ensure that intake data is processed if microstructure data is requested
  if (('microstructure' %in% data_list) & !('intake' %in% data_list)) {
    data_list <- c(data_list, 'intake')
  }

  #process redcap data
  if (sum(data_list %in% redcap_data_options) > 0) {
    data_list_redcap = data_list[(data_list %in% redcap_data_options)]

    # return data?
    if (('microstructure' %in% data_list) | isTRUE(return_data)) {
      proc_redcap_data <- write_redcap(base_wd, overwrite = overwrite, data_list = data_list_redcap, return_data = TRUE)

      #get intake data
      intake_data <- proc_redcap_data$intake$data
    } else {
      write_redcap(base_wd, overwrite = overwrite, data_list = data_list_redcap, return_data = FALSE)
    }
  }

  #process microstructure data
  if ('microstructure' %in% data_list) {
    micro_data <- write_microstructure(base_wd, intake_data = intake_data, data_list = micro_protocols, data_type = micro_data_type, overwrite = overwrite, return_data = return_data)
  }

  #process task data
  if (sum(data_list %in% task_data_options) > 0) {

    data_list_tasks = data_list[(data_list %in% task_data_options)]

    task_data <- write_tasks(base_wd, overwrite = overwrite, data_list = data_list_tasks, return_data = return_data)

  }

  #process actigraph data
  if ('actigraph' %in% data_list) {
    proc_actigraph(base_wd, overwrite = overwrite, overwrite_ggir_derivs = overwrite_ggir_derivs)
  }

  #### Return Data ####
  if (isTRUE(return_data)) {
    return(list = c(redcap_data = proc_redcap_data,
                    microstructure_data = micro_data,
                    task_data = task_data))
  }
}

