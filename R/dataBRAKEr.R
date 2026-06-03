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
#' @inheritParams proc_tasks
#' @inheritParams proc_task_derivs
#' @inheritParams proc_task_derivs
#' @param data_list list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#' \item{'participants' - BIDS specified participants.tsv file}
#'   \itemize{tasks:
#'    \item{'foodrating' - fNIRS Food Rating task}
#'    \item{'foodchoice' - fNIRS Food Choice task}
#'    \item{'shapegame' - Shape Game data}
#'    \item{'spacegame' - Space Game data (need to finish processing in Matlab)}
#'    \item{'nihtoolbox' - NIH Toolbox data}
#'    \item{'tastetest' - fNIRS Taste-Test task}
#'    \item{'pit' - Pavlovian Instrumental Transfer task data}
#'  }
#'  \item{'actigraph' - activity and sleep data generated from GGIR and mMARCH.AC}
#'  \item{'microstructure' - coded meal microstructure data}
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
#'  \item{'cwc' - Child Weight Concerns Questionnaire}
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
#'  \item{'scpf' - Structure and Control of Parent Feeding Questionnaire}
#'  \item{'sic' - Stress in Children Questionnaire}
#'  \item{'sleeplog' - Week long sleep log}
#'  \item{'spsrq' - Sensitivity to Punishment and Sensitivity to Reward Questionnaire}
#'  \item{'tfeq' - Three Factor Eating Questionnaire}
#' }
#' @param micro_protocols (optional) list of strings matching the notes below to indicate the which microstructure data. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'meal-baseline' - meal microstructure behavior at baseline}
#'  \item{'meal-followup' - meal microstructure behavior at followup}
#'  \item{'eah' - EAH microstructure behavior}
#' }
#' @param micro_data_type (optional) Type of data to process for meal microstructure - list of strings matching the data types listed below. Default = 'all' to export both:
#'  \itemize{
#'    \item{'beh_wide' - summary behavioral measures in wide formate by coder. Note: this will write out a summary dataset in bids/phenotype.}
#'    \item{'events_long' - event level data in log format by coder. Note: this writes out a file per participant into bids/rawdata.}
#'  }
#' @inheritParams proc_actigraph
#' @inheritParams proc_actigraph
#' @inheritParams util_task_foodrating
#' @param deid (logical) generate de-identified data into scholar-sphere directory. Do not include data_list argument to run this funciton.
#' @param deid_list data to process. Options include 'all' to process all data that will be put on ScholarSphere or a list of the following:
#' \itemize{
#'  \item{'phenotype' - all phenotype informaiton}
#'  \item{'derivatives' - all derivative and processed individual task data}
#' }
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

dataBRAKEr <- function(base_wd, overwrite = FALSE, fnirs_overwrite = FALSE, proc_source = FALSE, data_list = 'all', data_type = 'all', micro_protocols = 'all', micro_data_type = 'all', proc_ggir = FALSE, overwrite_ggir_derivs = FALSE, return_data = FALSE, deid = FALSE, deid_list = 'all') {

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
  #### function to export data and metadata ####

  # data for 'all' option - need to add actigraph eventually

  # task data
  task_data_options <- c('foodrating', 'foodchoice','shapegame','spacegame','nihtoolbox','tastetest','pit')
  
  # data from redcap
  redcap_data_options <- c('participants', 'anthropometrics', 'demographics', 'bodpod', 'fnirs_info', 'dkefs', 'wasi', 'intake', 'tasttest_samples', 'household', 'infancy',  'bes', 'brief2', 'cbq', 'cebq', 'cfq', 'cshq', 'cwc', 'efcr', 'ffbs', 'ffq', 'fmcb', 'hfe', 'hfi', 'lbc', 'loc', 'puberty', 'pwlb', 'scpf', 'sic', 'sleeplog', 'spsrq', 'tfeq')

  if (isFALSE(hasArg(data_list))){
    ## ScholarSphere de-id data ####
    if (isTRUE(deid)){
      util_scholarsphere(base_wd, overwrite = overwrite, data_list = deid_list)
      return('de-identified data creatd')
    }
    return('no data list')
  } else if (length(data_list) == 1) {
    if (data_list == 'all') {
      data_list = c(redcap_data_options, 'microstructure', task_data_options, 'actigraph')
    }
  } 
  
  # ensure that intake data is processed if microstructure data is requested
  if (('microstructure' %in% data_list) & !('intake' %in% data_list)) {
    data_list <- c(data_list, 'intake')
  }
  
  # ensure that taste-test data is processed if intake data is processed (to get followup meal VAS)
  if (('intake' %in% data_list) & !('tastetest' %in% data_list)) {
    data_list <- c(data_list, 'tastetest')
  }
  
  #### process task data ####
  if (sum(data_list %in% task_data_options) > 0) {
    
    data_list_tasks = data_list[(data_list %in% task_data_options)]
    
    if ('intake' %in% data_list) {
      
      task_data <- proc_task_derivs(base_wd = base_wd, overwrite = overwrite, proc_source = proc_source, fnirs_overwrite = fnirs_overwrite, task_list = data_list_tasks, return_data = TRUE)
      
      tastetest_data <- task_data$tastetest_database$tastetest_beh$data
      
    } else {
      task_data <- proc_task_derivs(base_wd = base_wd, overwrite = overwrite, proc_source = proc_source, fnirs_overwrite = fnirs_overwrite, task_list = data_list_tasks, return_data = return_data)
    }
  }
  
  # process redcap data ####
  if (sum(data_list %in% redcap_data_options) > 0) {
    data_list_redcap = data_list[(data_list %in% redcap_data_options)]

    # return data?
    if (sum(grepl('intake|micro',data_list) > 0) | isTRUE(return_data)) {
      proc_redcap_data <- write_redcap(base_wd, overwrite = overwrite, data_list = data_list_redcap, tastetest_data = tastetest_data, return_data = TRUE)

      #get intake data
      intake_data <- proc_redcap_data$intake$data
    } else {
      write_redcap(base_wd, overwrite = overwrite, data_list = data_list_redcap, return_data = FALSE)
    }
  }

  # process microstructure data ####
  if ('microstructure' %in% data_list) {
    micro_data <- write_microstructure(base_wd, intake_data = intake_data, micro_protocols = micro_protocols, micro_data_type = micro_data_type, overwrite = overwrite, return_data = return_data)
  }

  
  # process actigraph data ####
  if ('actigraph' %in% data_list) {
    proc_actigraph(base_wd, overwrite = overwrite, proc_ggir = proc_ggir, overwrite_ggir_derivs = overwrite_ggir_derivs)
  }

  #### Return Data ####
  if (isTRUE(return_data)) {
    return(list = c(redcap_data = proc_redcap_data,
                    microstructure_data = micro_data,
                    task_data = task_data))
  }
}

