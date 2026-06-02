#' util_scholarsphere: Copy BIDS data files and de-identify for the purpose of sharing publically.
#'
#' This function: \itemize{
#' \item{1) copies data from data/bids to data/scholarsphere
#' \item{2) ensures data are de-identified and json files are updated
#' }
#'
#' @param base_wd full path to directory containing both raw_untouched and bids directories
#' @inheritParams util_task_org_sourcedata
#' @param data_list data to process. Options include 'all' to process all data that will be put on ScholarSphere or a list of the following:\itemize{
#'  \item{'phenotype' - all phenotype informaiton}
#'  \item{'shapegame' - Shape Game data}
#'  \item{'spacegame' - Space Game data (need to finish processing in Matlab)}
#'  \item{'pit' - Pavlovian Instrumental Transfer task data}
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
  task_list_arg <- methods::hasArg(task_list)
  
  if (isTRUE(task_list_arg)) {
    if (task_list != 'all' & !is.vector(task_list)) {
      stop('Input to task_list must entered as a \'all\' or be vector (e.g., task_list = c("foodrating"")')
    } else {
      if (sum(!task_list %in% c('all', 'phenotype','shapegame', 'spacegame','nihtoolbox', 'pit')) > 0) {
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
  
  de_id_basic <- function(data_name) {
    data_file <- file.path(phenotype_wd, data_name)
    dat <- read.csv(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
    names(dat)
    
    # remove exact visit dat
    dat <- dat[!grepl('visit_date', names(dat))]
    
    # for BREIF - dont need age and sex once scored
    if(grepl('brief|anthro|puberty', data_name)){
      dat <- dat[!grepl('age|sex', names(dat))]
    }
    
    # round age down to the half age 
    if ('age' %in% names(dat)){
      dat['age'] <- ifelse(dat[['age']] == 7.9, 8, ifelse(dat[['age']] < 8.5, 8, ifelse(dat[['age']] < 9, 8.5, ifelse(dat[['age']] < 9.5, 9, ifelse(dat[['age']] < 10, 9.5, ifelse(dat[['age']] < 10.5, 10, ifelse(dat[['age']] < 11, 10.5, ifelse(dat[['age']] < 11.5, 11, ifelse(dat[['age']] < 12, 11.5, 12)))))))))
    }
    
    # remove height and weight - rely on computed bmi
    dat <- dat[!(grepl('height|weight', names(dat)))]
    
    if(grepl('demo', data_name)){
      dat <- dat[!(grepl('child_other_race', names(dat)))]
      
      # reduce race categories
      dat['race'] <- ifelse(dat[['race']] != 3, 0, 1)
      
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
  }
  
  # Phenotype ####
  
  if (data_list == 'all' | 'phenotype' %in% data_list) {
    print('-- de-identifying all phenotype data')
    
    # get list of available subjects 
    phenotype_list <- as.data.frame(list.files(path = file.path(phenotype_wd), pattern = '.tsv'))
    names(phenotype_list) <- 'filename'
    
    #organize data into BIDS sourcedata
    foodrating_list[['sourcedata_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodrating', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = fnirs_overwrite), simplify = TRUE)
    
    #process raw data
    foodrating_list[['rawproc_done']] <- sapply(foodrating_list[['sub_str']], function(x) util_task_foodrating(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = fnirs_overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    foodrating_json <- json_foodrating_events()
    
    foodrating_filename_json <- file.path(bids_wd, 'ses-baseline_task-foodrating_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(foodrating_filename_json) ) {
      write(foodrating_json, foodrating_filename_json)
    }
    
  }
  
  # Food Choice ####
  
  if (task_list == 'all' | 'foodchoice' %in% task_list) {
    print('-- processing Food Choice')
    
    # get list of available subjects 
    foodchoice_list <- as.data.frame(list.files(path = file.path(data_path, 'foodchoice_game'), pattern = '.csv'))
    names(foodchoice_list) <- 'filename'
    
    #get list of subject IDs
    foodchoice_list[['sub_str']] <- sapply(foodchoice_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #valid choice-pairing assignment
    foodchoice_list[['choice_pairing']] <- sapply(foodchoice_list[['filename']], function(x) ifelse(grepl('999', x), '999', 'rating'), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    foodchoice_list[['sourcedata_done']] <- sapply(foodchoice_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'foodchoice', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'nirs', overwrite = fnirs_overwrite), simplify = TRUE)
    
    #process raw data
    foodchoice_list['rawproc_done'] <- sapply(foodchoice_list[['sub_str']], function(x) util_task_foodchoice(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = fnirs_overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    foodchoice_json <- json_foodchoice_events()
    
    foodchoice_filename_json <- file.path(bids_wd, 'ses-baseline_task-foodchoice_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(foodchoice_filename_json) ) {
      write(foodchoice_json, foodchoice_filename_json)
    }
  }
  
  # Shape Game ####
  
  if (task_list == 'all' | 'shapegame' %in% task_list) {
    print('-- processing Shape Game')
    
    # get list of available subjects 
    shape_list <- as.data.frame(list.files(path = file.path(data_path, 'shape_game'), pattern = '.csv'))
    names(shape_list) <- 'filename'
    
    #get list of subject IDs
    shape_list[['sub_str']] <- sapply(shape_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    shape_list[['sourcedata_done']] <- sapply(shape_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'shape', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    #process raw data
    shape_list['rawproc_done'] <- sapply(shape_list[['sub_str']], function(x) util_task_shapegame(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    shapegame_json <- json_shapegame_events()
    
    shapegame_filename_json <- file.path(bids_wd, 'ses-baseline_task-shapegame_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(shapegame_filename_json) ) {
      write(shapegame_json, shapegame_filename_json)
    }
    
  }
  
  
  # Space Game ####
  
  if (task_list == 'all' | 'spacegame' %in% task_list) {
    print('-- processing Space Game')
    
    # get list of available subjects 
    space_list <- as.data.frame(list.files(path = file.path(data_path, 'space_game'), pattern = '.mat'))
    names(space_list) <- 'filename'
    
    #get list of subject IDs
    space_list[['sub_str']] <- sapply(space_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    space_list[['sourcedata_done']] <- sapply(space_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'space', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    #process raw data
    space_list['rawproc_done'] <- sapply(space_list[['sub_str']], function(x) util_task_spacegame(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    spacegame_json <- json_spacegame_events()
    
    spacegame_filename_json <- file.path(bids_wd, 'ses-baseline_task-spacegame_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(spacegame_filename_json) ) {
      write(spacegame_json, spacegame_filename_json)
    }
    
  }
  
  # NIH Toolbox - raw data ####
  
  if (task_list == 'all' | 'nihtoolbox' %in% task_list) {
    print('-- processing NIH Toolbox')
    
    # get list of available subjects 
    nih_list <- as.data.frame(list.files(path = file.path(data_path, 'nih_toolbox'), pattern = 'events.csv'))
    names(nih_list) <- 'filename'
    
    nih_list_flanker <- as.data.frame(nih_list[grepl('flanker', nih_list[['filename']]), ])
    nih_list_listsort <- as.data.frame(nih_list[grepl('listsort', nih_list[['filename']]), ])
    
    names(nih_list_flanker) <- 'flanker-dccs'
    names(nih_list_listsort) <- 'listsort'
    
    #get list of subject IDs
    nih_list_flanker[['sub_str']] <- sapply(nih_list_flanker[['flanker-dccs']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    nih_list_listsort[['sub_str']] <- sapply(nih_list_listsort[['listsort']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #merge to get 1 set of sub-str
    nih_list <- merge(nih_list_listsort, nih_list_flanker, id = 'sub_str', all = TRUE)
    
    # org
    nih_list[['sourcedata_done']] <- sapply(nih_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'nih', sub_str = x, ses = 'baseline', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    # process raw data
    nih_list[['rawdata_done']] <- sapply(nih_list[['sub_str']], function(x) util_task_nihtoolbox(sub_str = x, ses = 'baseline', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)
    
    #generate json file for rawdata
    nihtoolbox_json <- json_nihtoolbox_events()
    
    nihtoolbox_filename_json <- file.path(bids_wd, 'ses-baseline_task-nih_toolbox_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(nihtoolbox_filename_json) ) {
      write(nihtoolbox_json, nihtoolbox_filename_json)
    }
    
  }
  
  # Food Taste-Test ####
  
  if (task_list == 'all' | 'tastetest' %in% task_list) {
    print('-- processing Taste-Test')
    
    # get list of available subjects 
    tastetest_list <- as.data.frame(list.files(path = file.path(data_path, 'tastetest_game'), pattern = '.csv'))
    names(tastetest_list) <- 'filename'
    
    #get list of subject IDs
    tastetest_list[['sub_str']] <- sapply(tastetest_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    tastetest_list[['sourcedata_done']] <- sapply(tastetest_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'tastetest', sub_str = x, ses = 'followup', base_wd = base_wd, task_cat = 'nirs', overwrite = fnirs_overwrite), simplify = TRUE)
    
    #process raw data
    tastetest_list[['rawproc_done']] <- sapply(tastetest_list[['sub_str']], function(x) util_task_tastetest(sub_str = x, ses = 'followup', base_wd = base_wd, overwrite = fnirs_overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    tastetest_json <- json_tastetest_events()
    
    tastetest_pre_filename_json <- file.path(bids_wd, 'ses-baseline_task-tastetest_desc-pre_events.json')
    tastetest_post_filename_json <- file.path(bids_wd, 'ses-baseline_task-tastetest_desc-post_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(tastetest_pre_filename_json) ) {
      write(tastetest_json, tastetest_pre_filename_json)
      write(tastetest_json, tastetest_post_filename_json)
      
    }
    
  }
  
  # Food PIT task ####
  if (task_list == 'all' | 'pit' %in% task_list) {
    print('-- processing PIT Task')
    
    # get list of available subjects 
    pit_list <- as.data.frame(list.files(path = file.path(data_path, 'friendsgame_pit'), pattern = '.csv'))
    names(pit_list) <- 'filename'
    
    #get list of subject IDs
    pit_list[['sub_str']] <- sapply(pit_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    
    #organize data into BIDS sourcedata
    pit_list[['sourcedata_done']] <- sapply(pit_list[['sub_str']], function(x) util_task_org_sourcedata(task_str = 'pit', sub_str = x, ses = 'followup', base_wd = base_wd, task_cat = 'beh', overwrite = overwrite), simplify = TRUE)
    
    #process raw data
    pit_list[['rawproc_done']] <- sapply(pit_list[['sub_str']], function(x) util_task_pit(sub_str = x, ses = 'followup', base_wd = base_wd, overwrite = overwrite, return = FALSE), simplify = TRUE)
    
    #generate json file for rawdata
    pit_json <- json_pit()
    
    pit_filename_json <- file.path(bids_wd, 'ses-followup_task-pit_events.json')
    
    if ( isTRUE(overwrite) | !file.exists(pit_filename_json) ) {
      write(pit_json, pit_filename_json)
      
    }
    
  }
}

