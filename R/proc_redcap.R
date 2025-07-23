#' proc_redcap: Process raw data downloaded from Study BRAKE REDCap
#'
#' This function:
#' 1) reads REDCap data from sourcedata
#' 2) cleans data to save in BIDS format in phenotype. Produces the following files:
#'    *
#' 3) calls functions to create .json files for each phoneypte/x.tsv file
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#'
#' @param sourcedata_wd full path to the sourcedata directory
#' @param overwrite overwrite existing files (default = FALSE)
#' @param return_data return phenotype to console (default = FLASE)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw phenotype datasets for each task
#'  2) meta-data/.json inforamtion for each task
#'
#' @examples
#'
#' # process REDCap data
#' phenotype_data <- proc_redcap(visit_data_path, data_de_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export
#' 

# look up redcap_read_oneshot() and redcap_read, readcap_metadata_read

proc_redcap <- function(sourcedata_wd, overwrite = FALSE, return_data = FALSE) {

  
  #### 1. Set up/initial checks #####

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
    # get data from REDCap directly (only will work if have access and keys setup)
    Sys.setenv(reach_redcap_key = keyring::key_get('brake_redcap_key'))
    redcap_visit <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/', token = Sys.getenv('reach_redcap_key'))
    
    
    Sys.setenv(reach_de_redcap_key = keyring::key_get('brake-de_redcap_key'))
    redcap_de <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/', token = Sys.getenv('reach_de_redcap_key'))
    
    redcap_visit_data <- redcap_visit[['data']]
    redcap_visit_dict <- redcap_visit[['dictionary']]
    
    redcap_de_data <- redcap_de[['data']]
    redcap_de_dict <- redcap_de[['dictionary']]
    
    # remove '.factor'
    redcap_visit_data <- redcap_visit_data[, !grepl('.factor', names(redcap_visit_data))]
    redcap_de_data <- redcap_de_data[, !grepl('.factor', names(redcap_de_data))]
  }
  
  
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
  
  # organize event data
  prepost_v1_data <- util_redcap_prepost1(visit_1_prepost_arm_1)
  prepost_v2_data <- util_redcap_prepost2(visit_2_prepost_arm_1)
  prepost_v3_data <- util_redcap_prepost3(visit_3_prepost_arm_1)
  
  child_v1_data <- util_redcap_child1(child_visit_1_arm_1)
  parent_v1_data <- util_redcap_parent1(parent_visit_1_arm_1, prepost_v1_data$demo[c('participant_id', 'v1_date')])
  child_v2_data <- util_redcap_child2(child_visit_2_arm_1)
  parent_v2_data <- util_redcap_parent2(parent_visit_2_arm_1)
  child_v3_data <- util_redcap_child3(child_visit_3_arm_1)
  parent_v3_data <- util_redcap_parent3(parent_visit_3_arm_1)
  
  #### Load and organize double-entry data ####
  
  # all validated so can just take reviewer 1 data
  redcap_de_data <- redcap_de_data[grepl('--2', redcap_de_data[['record_id']]), ]
  redcap_de_data <- redcap_de_data[!(grepl('pilot', redcap_de_data[['record_id']])), ]
  
  redcap_de_data['record_id'] <- sapply(redcap_de_data['record_id'], function(x) gsub('--2', '', x))
  
  de_data_clean <- util_redcap_de(redcap_de_data)
  

  # create necessary files for fNIRS processing ####
  nirs_demo_data <- util_nirs_demo(v1_demo_homeloc = prepost_v1_data$demo, fnirs_info = child_v1_data$fnirs_info, anthro_data = child_v1_data$anthro_data, followup_anthro_data = child_v3_data$anthro_data, demographics = parent_v1_data$demo_data$data, puberty = parent_v1_data$puberty_data$data$score_dat, bodpod = de_data_clean$bodpod$data, baseline_cams = de_data_clean$baseline_cams$data, followup_cams = de_data_clean$followup_cams$data, fullness_tastetest = de_data_clean$taste_test$data, v3_date = prepost_v3_data)
  
  # quick fixes for notes where /n formatting got saved
  nirs_demo_data$data[grepl('notes', names(nirs_demo_data$data))] <- sapply(names(nirs_demo_data$data)[grepl('notes', names(nirs_demo_data$data))], function(x) gsub('\n', '', nirs_demo_data$data[[x]]))
  
   
  write.table(nirs_demo_data$data, file.path(bids_wd, 'sourcedata', 'phenotype', 'nirs_demo_data.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
  
  baseline_fitcap <- child_v1_data$fnirs_cap
  
  write.table(baseline_fitcap, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ses-baseline_nirs-fitcap.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
  
  followup_fitcap <- child_v3_data$fnirs_cap
  
  write.table(followup_fitcap, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ses-followup_nirs-fitcap.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
  
  #--------------------------------#
  
  ## REDCap data for metabolite analyses - ffq
  #baseline
  v1_bodpod <- de_data_clean$bodpod$data[, grepl('participant_id|baseline', names(de_data_clean$bodpod$data))]
  names(v1_bodpod) <- gsub('baseline_', '', names(v1_bodpod))
  v1_bodpod$participant_id <- as.numeric(v1_bodpod$participant_id )
  
  names(child_v1_data$anthro_data) <- gsub('v1_', '', names(child_v1_data$anthro_data))
  
  baseline_dat <- merge(parent_v1_data$demo_data$data[c('participant_id', 'demo_income','demo_mod_ed', 'demo_dad_ed')], v1_bodpod[c('participant_id', 'bodpod_date', 'fat_p')], by = 'participant_id', all = TRUE)
  
  baseline_dat <- merge(baseline_dat, child_v1_data$anthro_data[, !grepl('heightweight_notes', names(child_v1_data$anthro_data))], by = 'participant_id', all = TRUE)
  
  baseline_dat <- merge(baseline_dat, child_v1_data$hfi_data$data$score_dat, by = 'participant_id', all = TRUE)
  
  baseline_dat <- merge(baseline_dat, parent_v1_data$ffq_data$data$bids_phenotype, by = 'participant_id', all = TRUE)
  
  baseline_dat[['ses']] <- 'baseline'
  
  #followup
  v3_bodpod <- de_data_clean$bodpod$data[, grepl('participant_id|followup', names(de_data_clean$bodpod$data))]
  names(v3_bodpod) <- gsub('followup_|3', '', names(v3_bodpod))
  
  v3_bodpod$participant_id <- as.numeric(v3_bodpod$participant_id )
  
  followup_dat <- merge(parent_v3_data$demo_data$data, v3_bodpod[c('participant_id', 'bodpod_date', 'fat_p')], by = 'participant_id', all = TRUE)
  
  followup_dat <- merge(followup_dat, child_v3_data$anthro_data, by = 'participant_id', all = TRUE)
  
  followup_dat <- merge(followup_dat, parent_v3_data$hfi_data$data$score_dat, by = 'participant_id', all = TRUE)
  
  followup_dat <- merge(followup_dat, parent_v3_data$ffq_data$data$bids_phenotype, by = 'participant_id', all = TRUE)
  
  followup_dat[['ses']] <- 'followup'
  
  #merge 
  ffq_long <- rbind.data.frame(baseline_dat, followup_dat)
  
  baseline_demo <- merge(prepost_v1_data$demo[c('participant_id', 'v1_date')], parent_v1_data$demo_data$data[c('participant_id', 'demo_c_dob', 'demo_c_sex', 'demo_race', 'demo_ethnicity', 'demo_child_other_race')], by = 'participant_id', all = TRUE)
  
  baseline_demo <- merge(baseline_demo, prepost_v3_data[c('participant_id', 'v3_date')], by = 'participant_id', all = TRUE)
  
  ffq_long <- merge(baseline_demo, ffq_long, by = 'participant_id', all = TRUE)

  write.csv(ffq_long, file.path(sourcedata_wd, 'phenotype', 'marissa_brake_phenotype.csv'), row.names = FALSE)
  
  
  ## REDCap data for metabolite analyses - sleep
  de_data_clean$bodpod$data$participant_id <- as.numeric(de_data_clean$bodpod$data$participant_id)
  
  ure_dat_metab_sleep <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id', all = TRUE)
  ure_dat_metab_sleep <- merge(ure_dat_metab_sleep, child_v1_data$anthro_data, by = 'participant_id', all = TRUE)
  ure_dat_metab_sleep <- merge(ure_dat_metab_sleep, de_data_clean$bodpod$data, by = 'participant_id', all = TRUE)
  ure_dat_metab_sleep <- merge(ure_dat_metab_sleep, parent_v2_data$cshq_data$data$bids_phenotype, by = 'participant_id', all = TRUE)
  ure_dat_metab_sleep <- merge(ure_dat_metab_sleep, child_v1_data$sleep_wk_data$data$bids_phenotype, by = 'participant_id', all = TRUE)
  ure_dat_metab_sleep <- merge(ure_dat_metab_sleep, parent_v1_data$ffq_data$data$bids_phenotype, by = 'participant_id', all = TRUE)

  
  write.csv(ure_dat_metab_sleep, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ure_metab-sleep_demo.csv'), row.names = FALSE)
  
  ## ure_dat -- metabolites x obesity
  ure_dat_metabolite <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  ure_dat_metabolite <- merge(ure_dat_metabolite, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  ure_dat_metabolite <- merge(ure_dat_metabolite, bod_pod_data, by = 'participant_id')
  ure_dat_metabolite <- merge(ure_dat_metabolite, child_v1_data$hfi_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  ure_dat_metabolite <- merge(ure_dat_metabolite, parent_v1_data$ffq_data$data$bids_phenotype, by = 'participant_id', all.x = TRUE)
  
  write.csv(ure_dat_metabolite, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'mattheisen_honors_data.csv'), row.names = FALSE)
  
  ## ure_dat -- restriction and EAH
  de_data_clean$baseline_intake$data$participant_id <- as.numeric(de_data_clean$baseline_intake$data$participant_id)
  
  ure_dat_eah<- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  ure_dat_eah <- merge(ure_dat_eah, child_v1_data$anthro_data, by = 'participant_id')
  ure_dat_eah <- merge(ure_dat_eah, parent_v1_data$cfq_data$data$bids_phenotype, by = 'participant_id')
  ure_dat_eah <- merge(ure_dat_eah, de_data_clean$baseline_intake$data, by = 'participant_id', all.x = TRUE)
  
  write.csv(ure_dat_eah, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ure_cfq-eah.csv'), row.names = FALSE)
  
  ## ssib dat Kyle -- EF and CFQ
  
  tanner_dat <- parent_visit_1_arm_1[, grepl('record_id', names(parent_visit_1_arm_1)) | grepl('demo_c_sex', names(parent_visit_1_arm_1)) | grepl('tanner', names(parent_visit_1_arm_1))]
  names(tanner_dat)[1] <- 'participant_id'
  tanner_dat[['tanner_choice']] <- ifelse(tanner_dat[['demo_c_sex']] == 0, tanner_dat[['tanner_m']], tanner_dat[['tanner_f']])

  tanner_datv3 <- parent_visit_3_arm_1[, grepl('record_id', names(parent_visit_3_arm_1)) | grepl('pds_sex', names(parent_visit_3_arm_1)) | grepl('tanner', names(parent_visit_3_arm_1))]
  names(tanner_datv3)[1] <- 'participant_id'
  tanner_datv3[['tanner_choice']] <- ifelse(tanner_datv3[['pds_sex']] == 1, tanner_datv3[['tanner_m']], tanner_datv3[['tanner_f']]) 
  
  #baseline
  v1_bodpod <- de_data_clean$bodpod$data[, grepl('participant_id|baseline', names(de_data_clean$bodpod$data))]
  names(v1_bodpod) <- gsub('baseline_', '', names(v1_bodpod))
  
  names(child_v1_data$anthro_data) <- gsub('v1_', '', names(child_v1_data$anthro_data))

  baseline_dat <- merge(parent_v1_data$demo_data$data[c('participant_id', 'demo_income','demo_mod_ed', 'demo_dad_ed')], v1_bodpod[c('participant_id', 'bodpod_date', 'fat_p')], by = 'participant_id', all = TRUE)
  baseline_dat <- merge(baseline_dat, child_v1_data$anthro_data[, !grepl('heightweight_notes', names(child_v1_data$anthro_data))], by = 'participant_id', all = TRUE)
  
  baseline_dat <- merge(baseline_dat, parent_v1_data$puberty_data$data$score_dat, by = 'participant_id', all = TRUE)
  
  baseline_dat <- merge(baseline_dat, tanner_dat[c('participant_id', 'tanner_choice')], by = 'participant_id', all = TRUE)
  
  baseline_dat <- merge(baseline_dat, parent_v1_data$cfq_data$data$score_dat, by = 'participant_id', all = TRUE)
  
  baseline_dat[['ses']] <- 'baseline'

  #followup
  v2_bodpod <- de_data_clean$bodpod$data[, grepl('participant_id|followup', names(de_data_clean$bodpod$data))]
  names(v2_bodpod) <- gsub('followup_|3', '', names(v2_bodpod))
  
  followup_dat <- merge(parent_v3_data$demo_data$data, v2_bodpod[c('participant_id', 'bodpod_date', 'fat_p')], by = 'participant_id', all = TRUE)
  
  followup_dat <- merge(followup_dat, child_v3_data$anthro_data, by = 'participant_id', all = TRUE)
  
  followup_dat <- merge(followup_dat, parent_v3_data$puberty_data$data$score_dat, by = 'participant_id', all = TRUE)
  
  followup_dat <- merge(followup_dat, tanner_datv3[c('participant_id', 'tanner_choice')], by = 'participant_id', all = TRUE)
  
  followup_dat <- merge(followup_dat, parent_v3_data$cfq_data$data$score_dat, by = 'participant_id', all = TRUE)
  
  followup_dat[['ses']] <- 'followup'
  
  #merge 
  cfq_ef_long <- rbind.data.frame(baseline_dat, followup_dat)
  
  baseline_demo <- merge(prepost_v1_data$demo[c('participant_id', 'v1_date')], parent_v1_data$demo_data$data[c('participant_id', 'demo_c_dob', 'demo_c_sex', 'demo_race', 'demo_ethnicity', 'demo_child_other_race')], by = 'participant_id', all = TRUE)
  
  cfq_ef_long <- merge(baseline_demo, cfq_ef_long, by = 'participant_id', all = TRUE)
  
  write.csv(cfq_ef_long, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'kyle_brake_phenotype.csv'), row.names = FALSE)
  
  
  ## interview quick work
  
  
  ## ure_dat -- wasi
  ure_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  ure_dat <- merge(ure_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  ure_dat <- merge(ure_dat, bod_pod_data, by = 'participant_id')
  ure_dat <- merge(ure_dat, wasi_data, by = 'participant_id', all = TRUE)
  write.csv(ure_dat, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ure_pilot_data.csv'), row.names = FALSE)
  
  ## R01 pilot data -- intake
  r01_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  r01_dat <- merge(r01_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  r01_dat <- merge(r01_dat, bod_pod_data, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, wasi_data, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, intake_data, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, parent_v1_data$efcr_data$data$score_dat, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, parent_v1_data$cebq_data$data$score_dat, by = 'participant_id', all = TRUE)
  r01_dat <- merge(r01_dat, child_v2_data$loc_data$data, by = 'participant_id', all = TRUE)
  write.csv(r01_dat, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'r01_pilot_data.csv'), row.names = FALSE)
  
  
  
  
  ## dairy grant pilot data
  dairy_pilot <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, parent_v1_data$brief_data$data$score_dat, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, parent_v1_data$ffq_data$data$score_dat, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, parent_v1_data$ffq_data$data$bids_phenotype[c(1, 3:4)], by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, child_v1_data$hfi_data$data$score_dat, by = 'participant_id', all = TRUE)
  dairy_pilot <- merge(dairy_pilot, child_v1_data$hfi_data$data$bids_phenotype[c(1, 21:23)], by = 'participant_id', all = TRUE)
  write.csv(dairy_pilot, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'dairy_pilot.csv'), row.names = FALSE)
  
  interview_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  interview_dat <- merge(interview_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  interview_dat <- merge(interview_dat, bod_pod_data, by = 'participant_id')
  interview_dat <- merge(interview_dat, parent_v2_data$hfe_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$cfq_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v2_data$ffbs_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, child_v1_data$hfi_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$ffq_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$efcr_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v1_data$cebq_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, child_v2_data$loc_data$data, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, child_v1_data$sleep_wk_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  interview_dat <- merge(interview_dat, parent_v2_data$cshq_data$data$score_dat, by = 'participant_id', all.x = TRUE)
  
  write.csv(interview_dat, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'interview_pilot_data.csv'), row.names = FALSE)
  
  # nasa presentation
  nasa_dat <- merge(prepost_v1_data$demo, parent_v1_data$demo_data$data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, child_v1_data$demo_data$child_v1demo_data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, bod_pod_data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, intake_data, by = 'participant_id')
  nasa_dat <- merge(nasa_dat, child_v2_data$loc_data$data, by = 'participant_id', all = TRUE)
  nasa_dat <- merge(nasa_dat, parent_v1_data$cebq_data$data$score_dat, by = 'participant_id', all = TRUE)
  write.csv(nasa_dat, paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'nasa_pilot_data.csv'), row.names = FALSE)
  
  # export data
  
  ## interview data
  write.table(child_v1_data[['otherdata']][['fnirs_cap']], paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ses-baseline_nirs-fitcap.tsv'), sep='\t', quote = FALSE, row.names = FALSE)
  
  ## fNIRS - raw_untouched
  write.table(child_v1_data[['otherdata']][['fnirs_cap']], paste0(bids_wd, slash, 'sourcedata', slash, 'phenotype', slash, 'ses-baseline_nirs-fitcap.tsv'), sep='\t', quote = FALSE, row.names = FALSE)
  
  
  
  
  
  
  # merge
  participant_data <- merge(prepost_v1_data$prepost_data$data, prepost_v2_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, child_v1_data$child_visit1_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, child_v2_data$child_visit2_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, child_v2_data$loc_data$data, by = 'participant_id')
  participant_data <- merge(participant_data, parent_v1_data$demo_data$data, by = 'participant_id')
  
  participant_data$participant_id <- as.numeric(participant_data$participant_id)
  
  double_enter_data$bodpod_data$data$participant_id <- as.numeric(double_enter_data$bodpod_data$data$participant_id)
  #### Load and organize data double entry ####
  redcap_de_data <- read.csv(data_de_path, header = TRUE)
  
  double_enter_data <- util_redcap_de(redcap_de_data)
  participant_data <- merge(participant_data, double_enter_data$bodpod_data$data, by = 'participant_id')
  
  
  #### Merge visit data/notes ###
  
  #### Export Phenotype Data ####
  
  ## child visit 1
  
  #sleep log
  write.csv(child_v1_data$sleep_wk_data$data, paste0(phenotype_wd, slash, 'sleep_log.tsv'), row.names = FALSE)
  #write(child_v1_data$sleep_wk_data$meta, paste0(phenotype_wd, slash, 'sleep_log.json'))
  
  #hfi
  write.csv(child_v1_data$hfi_data$data, paste0(phenotype_wd, slash, 'hfi.tsv'), row.names = FALSE)
  #write(child_v1_data$hfi_data$meta, paste0(phenotype_wd, slash, 'home_food_inventory.json'))
  
  ## child visit 2
  
  #loc
  write.csv(child_v2_data$loc_data$data, paste0(phenotype_wd, slash, 'loc.tsv'), row.names = FALSE)
  #write(child_v2_data$loc_data$meta, paste0(phenotype_wd, slash, 'loc.json'))
  
  #sic
  write.csv(child_v2_data$sic_data$data, paste0(phenotype_wd, slash, 'stess_children.tsv'), row.names = FALSE)
  #write(child_v2_data$loc_data$meta, paste0(phenotype_wd, slash, 'stress_children.json'))
  
  ## parent visit 1
  
  #cfq
  write.csv(parent_v1_data$cfq_data$data, paste0(phenotype_wd, slash, 'cfq.tsv'), row.names = FALSE)
  #write(parent_v1_data$cfq_data$meta, paste0(phenotype_wd, slash, 'cfq.json'))
  
  #cebq
  write.csv(parent_v1_data$cebq_data$data, paste0(phenotype_wd, slash, 'cebq.tsv'), row.names = FALSE)
  #write(parent_v1_data$cebq_data$meta, paste0(phenotype_wd, slash, 'cebq.json'))
  
  #efcr
  write.csv(parent_v1_data$efcr_data$data, paste0(phenotype_wd, slash, 'efcr.tsv'), row.names = FALSE)
  #write(parent_v1_data$efcr_data$meta, paste0(phenotype_wd, slash, 'efcr.json'))
  
  #lbc
  write.csv(parent_v1_data$lbc_data$data, paste0(phenotype_wd, slash, 'lbc.tsv'), row.names = FALSE)
  #write(parent_v1_data$lbc_data$meta, paste0(phenotype_wd, slash, 'lbc.json'))
  
  #brief
  write.csv(parent_v1_data$brief_data$data, paste0(phenotype_wd, slash, 'brief.tsv'), row.names = FALSE)
  #write(parent_v1_data$brief_data$meta, paste0(phenotype_wd, slash, 'brief.json'))
  
  #ffq
  write.csv(parent_v1_data$ffq_data$data, paste0(phenotype_wd, slash, 'ffq.tsv'), row.names = FALSE)
  #write(parent_v1_data$ffq_data$meta, paste0(phenotype_wd, slash, 'ffq.json'))
  
  if (isTRUE(return_data)){
    return(list( foodchoice_dat = dat,
                 foodchoice_labels = meta_json))
  }
}

