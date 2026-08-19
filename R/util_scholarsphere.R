#' util_scholarsphere: Copy BIDS data files and de-identify for the purpose of sharing publicly.
#'
#' This function: \itemize{
#' \item{1) copies data from data/bids to data/scholarsphere}
#' \item{2) ensures data are de-identified and json files are updated}
#' }
#'
#' @param base_wd full path to directory containing both raw_untouched and bids directories
#' @inheritParams util_task_org_sourcedata
#' @param data_list data to process. Options include 'all' to process all data that will be put on ScholarSphere or a list of the following:
#' \itemize{
#'  \item{'phenotype' - all phenotype informaiton}
#'  \item{'derivatives' - all derivative and processed individual task data}
#' }
#'
#'
#' @examples
#'
#' \dontrun{
#' # organize task data for space game and NIH toolbox in untouchedRaw into sourcedata and raw data
#' util_scholarsphere(base_wd = base_wd, task_list = c('spacegame', 'pit'))
#'
#' }
#'
#'
#' @export
#'

util_scholarsphere <- function(base_wd, overwrite = FALSE, data_list = 'all') {
  
  #### 1. Set up/initial checks #####
  
  # check that base_wd exist and is a data.frame
  path_arg <- methods::hasArg(base_wd)
  
  if (isTRUE(path_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(path_arg)) {
    stop("base_wd must be entered as a string")
  }
  
  # check that task options correctly specified
  data_list_arg <- methods::hasArg(data_list)
  
  if (isTRUE(data_list_arg)) {
    if (data_list != 'all' & !is.vector(data_list)) {
      stop('Input to data_list must entered as a \'all\' or be vector (e.g., task_list = c("phenotype")')
    } else {
      if (sum(!data_list %in% c('all', 'phenotype','derivatives')) > 0) {
        stop(paste0('at least 1 item in tasks is not an option: ', task_list))
      }
    }
  } else {
    stop('Must provide at least 1 option in tasks argument')
  }
  
  #### Define paths ####
  bids_wd <- file.path(base_wd, 'bids')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives')
  scholarsphere_wd <- file.path(base_wd, 'scholar-sphere')
  openneuro_wd <- file.path(base_wd, 'open-neuro')
  
  
  de_id_basic <- function(data_name, data_wd) {
    data_file <- file.path(data_wd, data_name)
    dat <- read.csv(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
    
    # remove exact visit date (only keep date for data with multiple assessments)
    if(grepl('anthro|bes|brief|bodpod|cfq|demo|efcr|fnirs|hfe|house|intake|loc|puberty|pwlb|sic', data_name)){
      dat['visit_date'] <- format(ymd(dat[['visit_date']]), "%Y-%m")
    } else if (grepl('participant', data_name)){
      dat['child_protocol_1_date'] <- format(ymd(dat[['child_protocol_1_date']]), "%Y-%m")
      dat['child_protocol_2_date'] <- format(ymd(dat[['child_protocol_2_date']]), "%Y-%m")
      dat['child_protocol_3_date'] <- format(ymd(dat[['child_protocol_3_date']]), "%Y-%m")
    } else {
      dat <- dat[!grepl('visit_date', names(dat))]
    }
    
    if(grepl('hfi', data_name)){
      dat['hfi_date'] <- format(ymd(dat[['hfi_date']]), "%Y-%m")
    } 
    
    # for BREIF - dont need age and sex once scored
    if(grepl('brief|anthro|puberty', data_name)){
      dat <- dat[!grepl('age|sex', names(dat))]
    }
    
    # round age down to the half age 
    if ('age' %in% names(dat)){
      dat['age'] <- ifelse(dat[['age']] == 7.9, 8, ifelse(dat[['age']] < 8.5, 8, ifelse(dat[['age']] < 9, 8.5, ifelse(dat[['age']] < 9.5, 9, ifelse(dat[['age']] < 10, 9.5, ifelse(dat[['age']] < 10.5, 10, ifelse(dat[['age']] < 11, 10.5, ifelse(dat[['age']] < 11.5, 11, ifelse(dat[['age']] < 12, 11.5, 12)))))))))
    } else if (grepl('participant', data_name)){
      dat['child_protocol_1_age'] <- ifelse(dat[['child_protocol_1_age']] == 7.9, 8, ifelse(dat[['child_protocol_1_age']] < 8.5, 8, ifelse(dat[['child_protocol_1_age']] < 9, 8.5, ifelse(dat[['child_protocol_1_age']] < 9.5, 9, ifelse(dat[['child_protocol_1_age']] < 10, 9.5, ifelse(dat[['child_protocol_1_age']] < 10.5, 10, ifelse(dat[['child_protocol_1_age']] < 11, 10.5, ifelse(dat[['child_protocol_1_age']] < 11.5, 11, ifelse(dat[['child_protocol_1_age']] < 12, 11.5, 12)))))))))
      
      dat['child_protocol_2_age'] <- ifelse(dat[['child_protocol_2_age']] == 7.9, 8, ifelse(dat[['child_protocol_2_age']] < 8.5, 8, ifelse(dat[['child_protocol_2_age']] < 9, 8.5, ifelse(dat[['child_protocol_2_age']] < 9.5, 9, ifelse(dat[['child_protocol_2_age']] < 10, 9.5, ifelse(dat[['child_protocol_2_age']] < 10.5, 10, ifelse(dat[['child_protocol_2_age']] < 11, 10.5, ifelse(dat[['child_protocol_2_age']] < 11.5, 11, ifelse(dat[['child_protocol_2_age']] < 12, 11.5, 12)))))))))
      
      dat['child_protocol_3_age'] <- ifelse(dat[['child_protocol_3_age']] == 7.9, 8, ifelse(dat[['child_protocol_3_age']] < 8.5, 8, ifelse(dat[['child_protocol_3_age']] < 9, 8.5, ifelse(dat[['child_protocol_3_age']] < 9.5, 9, ifelse(dat[['child_protocol_3_age']] < 10, 9.5, ifelse(dat[['child_protocol_3_age']] < 10.5, 10, ifelse(dat[['child_protocol_3_age']] < 11, 10.5, ifelse(dat[['child_protocol_3_age']] < 11.5, 11, ifelse(dat[['child_protocol_3_age']] < 12, 11.5, 12)))))))))
    } 
    
    # remove height and weight - rely on computed bmi
    dat <- dat[!(grepl('height1|height2|weight1|weight2|height_|weight_mean|weight_l|weight_k', names(dat)))]
    
    if(grepl('demo|participant', data_name)){
      dat <- dat[!(grepl('child_other_race', names(dat)))]
      
      # reduce race categories
      dat['race'] <- ifelse(dat[['race']] != 3, 0, 1)
    }
    
    if(grepl('demo', data_name)){
      
      # reduce income categories
      dat['income'] <- ifelse(dat[['income']] < 3, 0, ifelse(dat[['income']] < 5, 1, 2))
      
      # reduce parent education categories
      dat['mom_ed'] <- ifelse(dat[['mom_ed']] < 16, 0, ifelse(dat[['mom_ed']] == 16, 1, 2))
      
      dat['dad_ed'] <- ifelse(dat[['dad_ed']] < 16, 0, ifelse(dat[['dad_ed']] == 16, 1, 2))
    }
    
    if(grepl('household', data_name)){
      dat <- dat[!(grepl('demo_parent_other_race|income|mom_ed|dad_ed|demo_education_partner|marital_status|parent_age|parent2_rep_bmi|snap|wic|tnaf|medicaid|liheap|_lunch$|program_other|programs_other|demo_parents_together|_relationship_other|retired', names(dat)))]
      
      # reduce race categories
      dat['demo_parent_race'] <- ifelse(dat[['demo_parent_race']] != 3, 0, 1)
      
      # reduce # children categories
      dat['demo_nchildren'] <- dat[['demo_nchildren']] - 1
      dat['demo_nchildren'] <- ifelse(dat[['demo_nchildren']] >= 3, 3, dat[['demo_nchildren']])
      
      # reduce birth order categories
      dat['demo_birth_order'] <- dat[['demo_birth_order']] - 1
      dat['demo_birth_order'] <- ifelse(dat[['demo_birth_order']] >= 2, 2, dat[['demo_birth_order']])
      
      # reduce grade categories
      dat['demo_grade'] <- dat[['demo_grade']] - 1
      dat['demo_grade'] <- ifelse(is.na(dat[['demo_grade']]), NA, ifelse(dat[['demo_grade']] == 0, 1, ifelse( dat[['demo_grade']] >= 4, 4, dat[['demo_grade']])))
      
      # reduce relationship categories
      dat['demo_partner_relationship'] <- ifelse(!is.na(dat[['demo_partner_relationship']]) & dat[['demo_partner_relationship']] >= 2, 2, dat[['demo_partner_relationship']])
      
      # merge food pantry items
      dat['demo_assist_program_no'] <- ifelse(dat[['demo_assist_program_no']] == 1, ifelse(!is.na(dat[['demo_food_pantry']]) & dat[['demo_food_pantry']] == 1, 0, dat[['demo_assist_program_no']]), dat[['demo_assist_program_no']])
      
      dat <- dat[!(grepl('pantry', names(dat)))]
    }
    
    if(grepl('infacny', data_name)){
      dat <- dat[!(grepl('weeks', names(dat)))]
    }
    
    if(grepl('sleeplog', data_name)){
      dat <- dat[!(grepl('date', names(dat)))]
      
    }
    
    if(grepl('loc', data_name)){
      dat <- dat[(grepl('_id$|visit|loc_1$', names(dat)))]
      
    }
    
    if(grepl('puberty', data_name)){
      dat <- dat[(grepl('_id$|visit|pds_score|tanner', names(dat)))]
    }
    
    if(grepl('sleeplog', data_name)){
      dat <- dat[(!grepl('date', names(dat)))]
    }
    
    
    ## write json files
    data_name_str <- strsplit(data_name,'.tsv')[[1]] 
    
    if(grepl('anthro', data_name)){
      # anthropometrics
      deid_json <- json_anthro_deid()
    } else if(grepl('brief', data_name)){
      #brief
      deid_json <- json_brief2_deid()
    } else if(grepl('demo', data_name)){
      #demographics
      deid_json <- json_demo_deid()
    } else if(grepl('fnirs', data_name)){
      #fnirs_info
      deid_json <- json_fnirs_deid()
    } else if(grepl('micro', data_name)){
      #microstructure
      deid_json <- json_micro_beh_deid()
    } else if(grepl('nih', data_name)){
      #nihtoolbox_desc-scores
      deid_json <- json_nihtoolbox_scores_deid()
    } else if(grepl('puberty', data_name)){
      #puberty
      deid_json <- json_pds_deid()
    } else if(grepl('tasttest_sample', data_name)){
      #tastetest_sample
      deid_json <- json_tt_food_deid()
    } else if(grepl('pit', data_name)){
      #pit
      deid_json <- json_pit_summary()
    } else if(grepl('shapegame_beh', data_name)){
      #shapegame
      deid_json <- json_shapegame_deid()
    } else if(grepl('shapegame_desc', data_name)){
      #shapegame
      deid_json <- json_shapegame_long_deid()
    } else if(grepl('spacegame', data_name)){
      #spacegame
      deid_json <- json_spacegame_deid()
    } else if(grepl('foodchoice_beh', data_name)){
      #foodchoice
      deid_json <- json_foodchoice_deid()
    } else if(grepl('foodrating_beh', data_name)){
      #foodrating
      deid_json <- json_foodrating_deid()
    } else if(grepl('tastetest_beh', data_name)){
      #tastetest_beh
      deid_json <- json_tastetest_deid()
    } else if(grepl('tastetest_desc', data_name)){
      #tastetest_desc-long
      deid_json <- json_tastetest_long_deid()
    } else {
      # json function mates name of data_name
      json_fn_str <- paste0('json_', data_name_str, '_deid')
      deid_json <- eval(call(json_fn_str))
    }
    
    # write data to scholarsphere directory
    if (grepl('phenotype', data_wd)){
      deid_filename_tsv <- file.path(scholarsphere_wd, 'phenotype', data_name)
      deid_filename_json <- file.path(scholarsphere_wd, 'phenotype', paste0(data_name_str, '.json'))
    } else if (grepl('participants', data_name)){
      deid_filename_tsv <- file.path(scholarsphere_wd, data_name)
      deid_filename_json <- file.path(scholarsphere_wd, paste0(data_name_str, '.json'))
      
      #save to open-neuro too
      write.table(dat, file.path(openneuro_wd, data_name), quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE, na = 'n/a')
      #write json
      write(openneuro_wd, deid_filename_json)
      
    } else {
      deid_filename_tsv <- file.path(scholarsphere_wd, 'derivatives', data_name)
      deid_filename_json <- file.path(scholarsphere_wd, 'derivatives', paste0(data_name_str, '.json'))
    }
    
    #write data
    write.table(dat, deid_filename_tsv, quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE, na = 'n/a')
    
    #write json
    write(deid_json, deid_filename_json)
    
  }
  
  # Phenotype ####
  
  if (data_list == 'all' | 'phenotype' %in% data_list) {
    print('-- de-identifying all phenotype data')
    
    # participants.tsv
    de_id_basic(data_name = 'participants.tsv', data_wd = bids_wd)
    
    # get list of phenotype data 
    phenotype_list <- as.data.frame(list.files(path = file.path(phenotype_wd), pattern = '.tsv'))
    names(phenotype_list) <- 'filename'
    
    sapply(phenotype_list[['filename']], function(x) de_id_basic(data_name = x, data_wd = phenotype_wd))
  }
  
  if (data_list == 'all' | 'derivatives' %in% data_list) {
    print('-- de-identifying all derivative data')
    
    # get list of derivative data
    deriv_list_beh <- as.data.frame(list.files(path = file.path(deriv_wd, 'beh'), pattern = '.tsv'))
    names(deriv_list_beh) <- 'filename'
    
    sapply(deriv_list_beh[['filename']], function(x) de_id_basic(data_name = x, data_wd = file.path(deriv_wd, 'beh')))
    
    deriv_list_nirsbeh <- as.data.frame(list.files(path = file.path(deriv_wd, 'nirs-beh'), pattern = '.tsv'))
    names(deriv_list_nirsbeh) <- 'filename'
    
    sapply(deriv_list_nirsbeh[['filename']], function(x) de_id_basic(data_name = x, data_wd = file.path(deriv_wd, 'nirs-beh')))
    
    #copy over spacegame dm files
    deriv_list_dm <- list.files(path = file.path(deriv_wd, 'beh', 'spacegame_desc-dm_kool2016'), pattern = 'dm_summary')
    
    file.copy(from = file.path(deriv_wd, 'beh', 'spacegame_desc-dm_kool2016', deriv_list_dm), to = file.path(scholarsphere_wd, 'derivatives', deriv_list_dm), overwrite = overwrite)
    
  }
  
  ## Individual files ####
  cp_scholarsphere <- function(data_path, file_name, meal_desc, overwrite) {
    
    if (grepl('meal', file_name)){
      scholarsphere_path <- file.path(scholarsphere_wd, 'rawdata', data_path, 'beh')
      raw_data_path <- file.path(raw_wd, data_path, 'videos')
    } else {
      scholarsphere_path <- file.path(scholarsphere_wd, 'rawdata', data_path)
      raw_data_path <- file.path(raw_wd, data_path)
    }
    
    #make directory if needed
    
    if (!dir.exists(scholarsphere_path)) {
      dir.create(scholarsphere_path, recursive = TRUE)
    } 
    
    if (!file.exists(file.path(raw_data_path, file_name)) | isTRUE(overwrite)) {
      file.copy(from = file.path(raw_data_path, file_name), to = file.path(scholarsphere_path, file_name), overwrite = overwrite)
    }
  }
  
  #  beh get list of available subjects  ####
  print('-- copying individual task beh files to scholarsphere')
  
  baseline_tsv_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'beh')), pattern = '*.tsv$', recursive = TRUE))
  names(baseline_tsv_list) <- 'filename'
  baseline_tsv_list['ses'] <- 'ses-baseline'
  baseline_tsv_list <- baseline_tsv_list[!grepl('toolbox', baseline_tsv_list[['filename']]), ]
  
  followup_tsv_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-followup', 'beh')), pattern = '*.tsv$', recursive = TRUE))
  names(followup_tsv_list) <- 'filename'
  followup_tsv_list['ses'] <- 'ses-followup'
  
  beh_tsv_list <- rbind(baseline_tsv_list, followup_tsv_list)
  
  #get list of subject IDs
  beh_tsv_list[['sub_str']] <- sapply(beh_tsv_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  beh_tsv_list[['data_path']] <- file.path(beh_tsv_list[['sub_str']], beh_tsv_list[['ses']], 'beh')
  
  #organize data into BIDS sourcedata
  mapply(cp_scholarsphere, data_path = beh_tsv_list[['data_path']], file_name = beh_tsv_list[['filename']], MoreArgs = list(overwrite = overwrite))
  
  
  #  beh get list of available subjects  ####
  print('-- copying individual eyetracking files to scholarsphere')
  
  baseline_eye_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'eyetrack')), pattern = '*.tsv.gz$', recursive = TRUE))
  names(baseline_eye_list) <- 'filename'
  
  #get list of subject IDs
  baseline_eye_list[['sub_str']] <- sapply(baseline_eye_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  baseline_eye_list[['data_path']] <- file.path(baseline_eye_list[['sub_str']], 'ses-baseline', 'eyetrack')
  
  #organize data into BIDS sourcedata
  mapply(cp_scholarsphere, data_path = baseline_eye_list[['data_path']], file_name = baseline_eye_list[['filename']], MoreArgs = list(overwrite = overwrite))
  
  #  beh get list of available subjects  ####
  print('-- copying individual microstrucutre meal files to scholarsphere')
  
  micro_baseline_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'videos')), pattern = '*.tsv$', recursive = TRUE))
  names(micro_baseline_list) <- 'filename'
  micro_baseline_list['ses'] <- 'ses-baseline'
  
  
  micro_followup_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-followup', 'videos')), pattern = '*.tsv$', recursive = TRUE))
  names(micro_followup_list) <- 'filename'
  micro_followup_list['ses'] <- 'ses-followup'
  
  micro_list <- rbind(micro_baseline_list, micro_followup_list)
  
  #get list of subject IDs
  micro_list[['sub_str']] <- sapply(micro_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  micro_list[['data_path']] <- file.path(micro_list[['sub_str']], micro_list[['ses']])
  
  #organize data into BIDS sourcedata
  mapply(cp_scholarsphere, data_path = micro_list[['data_path']], file_name = micro_list[['filename']], MoreArgs = list(overwrite = overwrite))
  
  #copy over individual *_event.json files
  print('-- copying global *events.json files')
  
  event_json_list <- list.files(path = bids_wd, pattern = 'events.json', recursive = FALSE)

  file.copy(from = file.path(bids_wd, event_json_list), to = file.path(scholarsphere_wd, event_json_list), overwrite = overwrite)
  
  ## copy actigraph data ####
  print('-- copying individual actigraph files')
  
  motion_baseline_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-*', 'motion')), pattern = '*.tsv.gz', recursive = TRUE))
  names(motion_baseline_list) <- 'filename'
  motion_baseline_list['ses'] <- 'ses-baseline'
  
  motion_followup_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-followup', 'motion')), pattern = '*.tsv.gz', recursive = TRUE))
  names(motion_followup_list) <- 'filename'
  motion_followup_list['ses'] <- 'ses-followup'
  
  
  motion_list <- rbind(motion_baseline_list, motion_followup_list)
  
  #get list of subject IDs
  motion_list[['sub_str']] <- sapply(motion_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
  
  motion_list[['data_path']] <- file.path(motion_list[['sub_str']], motion_list[['ses']], 'motion')
  
  #organize data into BIDS sourcedata
  mapply(cp_scholarsphere, data_path = motion_list[['data_path']], file_name = motion_list[['filename']], MoreArgs = list(overwrite = overwrite))
  
}

