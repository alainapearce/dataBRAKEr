#' proc_redcap: Process raw data downloaded from Study BRAKE REDCap
#' 
#' This function:
#' \itemize{
#'    \item{1) Reads REDCap data (visit and double-entry) using the REDCap API}
#'    \item{2) Calls util_ functions to clean and compile data in dataframes}
#'    \item{3) Calls json_ functions to create strings with meta-data stored in JSON format for each dataframe}
#'    \item{4) Compiles data repeated across visits and sessions}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @param redcap_api (logical) execute REDCap API. Default = FALSE.
#' @param redcap_visit_data REDCap visit data from a prior API call
#' @param redcap_de_data REDCap double-entry data from a prior API call
#' @inheritParams util_merged_intake
#'
#' @return Will return a list including data and metadata for:
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
#'
#' @examples
#'
#' \dontrun{
#' redcap_data <- proc_redcap(base_wd, overwrite = FALSE, overwrite_jsons = FALSE)
#'
#' }
#'
#' @seealso [write_redcap()]
#'
#' @export

proc_redcap <- function(redcap_api = FALSE, redcap_visit_data, redcap_de_data, tastetest_data) {
  
  #### Set up/initial checks #####
  
  # check that data is passed if redcap_api = FALSE
  if (isFALSE(redcap_api)){
    
    # check that redcap_visit_data exist and is a data.frame
    visit_data_arg <- methods::hasArg(redcap_visit_data)
    
    if (isTRUE(visit_data_arg)) {
      if (!is.data.frame(redcap_visit_data)) {
        stop('redcap_visit_data must be a data.frame with recap_api = FALSE')
      }
    } else if (isFALSE(visit_data_arg)) {
      stop('redcap_visit_data must be a data.frame with recap_api = FALSE')
    }
    
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
    
  }
  
  #### Extract visit data ####
  
  print('-- processing REDCap data')
  
  # subset events and remove unnecessary columns
  redcap_long_wide <- function(event_name, data){
    
    #subset
    sub_dat <- data[data[['redcap_event_name']] == event_name, ]
    
    #remove empty columns
    sub_dat <- sub_dat[, !colSums(is.na(sub_dat)) == nrow(sub_dat)]
    
    #return
    return(sub_dat)
  }
  
  # process visit data ####
  visit_1_prepost_arm_1 <- redcap_long_wide('visit_1_prepost_arm_1', redcap_visit_data)
  visit_2_prepost_arm_1 <- redcap_long_wide('visit_2_prepost_arm_1', redcap_visit_data)
  visit_3_prepost_arm_1 <- redcap_long_wide('visit_3_prepost_arm_1', redcap_visit_data)
  child_visit_1_arm_1 <- redcap_long_wide('child_visit_1_arm_1', redcap_visit_data)
  parent_visit_1_arm_1 <- redcap_long_wide('parent_visit_1_arm_1', redcap_visit_data)
  child_visit_2_arm_1 <- redcap_long_wide('child_visit_2_arm_1', redcap_visit_data)
  parent_visit_2_arm_1 <- redcap_long_wide('parent_visit_2_arm_1', redcap_visit_data)
  child_visit_3_arm_1 <- redcap_long_wide('child_visit_3_arm_1', redcap_visit_data)
  parent_visit_3_arm_1 <- redcap_long_wide('parent_visit_3_arm_1', redcap_visit_data)
  
  
  #### Process visit data ####
  
  # make data.frame of dates, ages, and sex
  date_data <- util_redcap_dates(child_v1 = visit_1_prepost_arm_1, child_v2 = visit_2_prepost_arm_1, child_v3 = visit_3_prepost_arm_1, parent_v1 = parent_visit_1_arm_1)
  
  # get pre/post data information
  prepost_data <- util_redcap_prepost(v1_data = visit_1_prepost_arm_1, v2_data = visit_2_prepost_arm_1, v3_data = child_visit_3_arm_1)
  
  # organize event data
  child_v1_data <- util_redcap_child1(child_visit_1_arm_1, date_data)
  parent_v1_data <- util_redcap_parent1(parent_visit_1_arm_1, date_data)
  child_v2_data <- util_redcap_child2(child_visit_2_arm_1, date_data)
  parent_v2_data <- util_redcap_parent2(parent_visit_2_arm_1, date_data)
  child_v3_data <- util_redcap_child3(child_visit_3_arm_1, date_data)
  parent_v3_data <- util_redcap_parent3(parent_visit_3_arm_1, date_data)
  
  #### Process double-entry data ####
  proc_de_data <- util_redcap_de(redcap_api = FALSE, redcap_de_data, date_data)
  
  #### Combine data across visits ####
 
  ## Merge intake-related data
  # merge intake-related data (paradigm info, liking data, wanting data, intake data, fullness data)
  merged_intake <- util_merged_intake(child_v1_data, child_v3_data, proc_de_data, tastetest_data)
  
  
  ## questionnaires
  merged_qs <- util_merge_questionnaires(child_v1_data, child_v2_data, child_v3_data, parent_v1_data, parent_v2_data, parent_v3_data)
  
  ## fNIRS 
  merged_fnirs <- util_merged_fnirs(child_v1_data, child_v3_data, proc_de_data)
  
  ## anthro
  merged_anthro <- util_merged_anthro(visit1_anthro = child_v1_data$anthro_data$data, visit3_anthro = child_v3_data$anthro_data$data, household_all = merged_qs$household_all$data, date_data = date_data)
  
  #### Generate demographics dataframe  ####
  merged_demo <- util_merged_demo(visit1_demo = parent_v1_data$demo_data$data, household_all = merged_qs$household_all$data, merged_anthro = merged_anthro$data, date_data = date_data)
  
  #### Generate participants dataframe ####
  participants_data <- util_merged_participants(merged_demo = merged_demo$data, date_data)
  
  #### Data to return ####
  
  # list dataframes to return, where the name is the corresponding json function without 'json_'
  return(list(
    participants = participants_data,
    anthropometrics = merged_anthro,
    demographics = merged_demo,
    bodpod = proc_de_data$bodpod_data,
    fnirs_info = merged_fnirs,
    wasi = proc_de_data$wasi_data,
    dkefs = proc_de_data$dkefs_data,
    intake = merged_intake,
    tasttest_samples = proc_de_data[['taste_test_data']],
    household = merged_qs[['household_all']],
    infancy = parent_v1_data[['infancy_data']],
    bes = merged_qs[['bes_all']],
    brief2 = list(data = parent_v1_data[['brief_data']]$data$bids_phenotype,
                  meta = parent_v1_data[['brief_data']]$meta),
    cbq = list(data = parent_v2_data[['cbq_data']]$data$bids_phenotype,
               meta = parent_v2_data[['cbq_data']]$meta),
    cebq = list(data = parent_v1_data[['cebq_data']]$data$bids_phenotype,
                meta = parent_v1_data[['cebq_data']]$meta),
    cfq = merged_qs[['cfq_all']],
    cshq = merged_qs[['cshq_all']],
    efcr = merged_qs[['efcr_all']],
    ffbs = list(data = parent_v2_data[['ffbs_data']]$data$bids_phenotype,
                meta = parent_v2_data[['ffbs_data']]$meta),
    ffq = merged_qs[['ffq_all']],
    fmcb = list(data = parent_v2_data[['fmcb_data']]$data$bids_phenotype,
                meta = parent_v2_data[['fmcb_data']]$meta),
    hfe = merged_qs[['hfe_all']],
    hfi = merged_qs[['hfi_all']],
    lbc = list(data = parent_v1_data[['lbc_data']]$data$bids_phenotype,
               meta = parent_v1_data[['lbc_data']]$meta),
    loc = merged_qs[['loc_all']],
    puberty = merged_qs[['puberty_all']],
    pwlb =  merged_qs[['pwlb_all']],
    scpf = list(data = parent_v2_data[['scpf_data']]$data$bids_phenotype,
                meta = parent_v2_data[['scpf_data']]$meta),
    sic = merged_qs[['sic_all']],
    sleeplog = list(data = child_v1_data[['sleep_wk_data']]$data$bids_phenotype,
                    meta = child_v1_data[['sleep_wk_data']]$meta),
    spsrq = list(data = parent_v2_data[['spsrq_data']]$data$bids_phenotype,
                 meta = parent_v2_data[['spsrq_data']]$meta),
    tfeq = list(data = parent_v2_data[['tfeq_data']]$data$bids_phenotype,
                meta = parent_v2_data[['tfeq_data']]$meta)
  ))
  
}

