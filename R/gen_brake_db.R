#' gen_BRAKE_db: Generate databases for the Study of Brain, Reward, and Kids' Eating (BRAKE)
#'
#' This function will generate up-to-date databases for the Study of Brain, Reward, and Kids' Eating (BRAKE).
#'
#' @param data_elements list of strings to indicate which data to include in the database. All strings should be entered as a list. example: c('demo',' cebq', 'cbq'). Options include the following (must match string exactly):
#' \itemize{
#'  \item{Questionnaires:}
#'  \itemize{
#'    \item{bes - Binge Eating Scale}
#'    \item{brief2 - Behavioral Rating Inventory of Executive Functioning}
#'    \item{cams - Child Anxiety Meter Scale}
#'    \item{cbis - Child Body Image Scale}
#'    \item{cbq - Child Behavior Questionnaire}
#'    \item{cebq - Children's Eating Behavior Questionnaire}
#'    \item{cfq - Child Feeding Questionnaire}
#'    \item{cshq - Childrens Sleep Habits Questionnaire}
#'    \item{demo - all demographics related information}
#'    \item{efcr - External Food Cue Responsivenes Scale}
#'    \item{ffbs - Family Food Behavior Survey}
#'    \item{ffq_helix - Food Frequency Questionniare from the HELIX cohort study}
#'    \item{hmcb - Feeding to Manage Child Behavior}
#'    \item{hfi - Home Food Inventory}
#'    \item{lbc - Lifestyle Behavior Checklist}
#'    \item{loc - Loss of Control-Eating Questionnaire}
#'    \item{nik_hfe - Home Food Environment Survey from the Neighborhood Impact on Kids}
#'    \item{pds - Pubertal Development Scale}
#'    \item{sic - Stress in Children Questionnaire}
#'    \item{sleeplog - Week Sleep Log}
#'    \item{spsrq - Sensitivity to Punishment Sensitivity to Reward Questionnaire}
#'    \item{tfeq - Three Factor Eating Questionnaire}
#'    \item{wic - Weight Concerns Scale}
#'  }
#'  \item{Tasks:}
#'  \itemize{
#'    \item{anthro - Measured height and weight, BMI, and CDC BMI percentiles}
#'    \item{dkefs - Behavioral data for the D-KEFS Trail Making, Verbal Fluency, and Design Fluency}
#'    \item{foodchoice - Pre-process and produce behavioral data from the Food Choice fNIRS Task}
#'    \item{foodrating - Pre-process and produce behavioral data from the Food Rating fNIRS Task}
#'    \item{nihtoolbox - Behavioral data for NIH Toolbox tasks: Listsort, Flanker, and Dimensional Card Sort Tasks}
#'    \item{pit - Behavioral data for PIT task}
#'    \item{shapegame - Pre-process and produce behavioral and eye-tracking data for the VMAC Shape Game}
#'    \item{spacegame - Behavioral data for the Space Game}
#'    \itemize{
#'      \item{DM Model not processed: will produce only data needed to process task in Matlab to get DM model parameters}
#'      \item{DM Model exisits: will produce a database with behavioral and process DM model parameters}
#'      }
#'    \item{tasttest - Pre-process and produce behavioral data from the Food Choice fNIRS Task}
#'    \item{wasi - WASI IQ scores}
#'    }
#'  \item{Protocol Elements:}
#'  \itemize{
#'    \item{actigraph: Activity and sleep data}
#'    \item{eah: Intake and fullness data for EAH}
#'    \item{eah_micro: EAH behavioral coding}
#'    \item{meal: Intake and fullness data for meal}
#'    \item{meal_micro: Mictrostructure data for meal}
#'    \item{meal_CIC: Cumultaive intake curves for meal}
#'    \item{bodpod: BodPod metrics}
#'  }
#' }
#' \cr
#' @param write_dat indicate whether to write databases. Default is TRUE.
#' @param write_path (optional) a string with the path indicating where to save the generated databases if write_dat is TRUE (default option). If no path is given, databases will be written to working directory.
#' @inheritParams proc_tasks
#' @param return_data indicate whether to return databases to the environment. Default is FALSE.
#'
#' @return A list containing all databases that were generated
#'
#' @examples
#' #if in same working directory as data, want all databases, and the databases to be written to working directory:
#' fbs_data_proc <- gen_BRAKE_db(model_DD = TRUE)
#'
#' #if only want the Demographics and Intake databases written to working directory:
#' fbs_data_proc <- gen_BRAKE_db(databases = c('demo', 'intake'))
#'
#' #if want to work with the Demographics database but not write it to a file:
#' fbs_data_proc <- gen_BRAKE_db(databases = 'demo', write_dat = FALSE)
#'
#' \dontrun{
#' #databases must be strings. The following will not run:
#' fbs_data_proc <- gen_BRAKE_db(databases = c(demo, intake))
#'
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_merge_v1dat}}, \code{\link{util_fbs_merge_v2dat}}, \code{\link{util_fbs_merge_v3dat}}, \code{\link{util_fbs_merge_v4dat}}, \code{\link{util_fbs_merge_v5dat}}, \code{\link{util_fbs_merge_v6dat}}, \code{\link{util_fbs_merge_v7dat}}
#'
#'
#' @export

gen_BRAKE_db <- function(data_elements, write_dat = TRUE, write_path, data_path, return_data = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check the file patterns
  
  c_filepat_arg <- methods::hasArg(child_file_pattern)
  
  if (isTRUE(c_filepat_arg)) {
    if (!is.character(child_file_pattern)) {
      stop("child_file_pattern must be entered as a string: e.g., 'Child'")
    } else {
      child_fp <- child_file_pattern
    }
  } else if (isFALSE(c_filepat_arg)) {
    child_fp <- 'Child'
  }
  
  p_filepat_arg <- methods::hasArg(parent_file_pattern)
  
  if (isTRUE(p_filepat_arg)) {
    if (!is.character(parent_file_pattern)) {
      stop("parent_file_pattern must be entered as a string: e.g., 'Parent'")
    } else {
      parent_fp <- parent_file_pattern
    }
  } else if (isFALSE(p_filepat_arg)) {
    parent_fp <- 'Parent'
  }
  
  v_filepat_arg <- methods::hasArg(visit_file_pattern)
  
  if (isTRUE(p_filepat_arg)) {
    if (!is.character(visit_file_pattern)) {
      stop("parent_file_pattern must be entered as a string: e.g., 'V' or 'Visit_'")
    } else {
      visit_fp <- visit_file_pattern
    }
  } else if (isFALSE(p_filepat_arg)) {
    visit_fp <- 'V'
  }
  
  # check datapath
  datapath_arg <- methods::hasArg(data_path)
  
  if (isTRUE(datapath_arg)) {
    if (!is.character(data_path)) {
      stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/")
    }
    
    #make universal to 'Untouched_Raw'
    if (grepl('Qualtrics_Raw', data_path, fixed = TRUE)){
      data_path <- gsub('Qualtrics_Raw', '', data_path)
    } else if (grepl('Microstructure_Raw', data_path, fixed = TRUE)){
      data_path <- gsub('Microstructure_Raw', '', data_path)
    }
  }
  
  # check databases argument
  databases_arg <- methods::hasArg(databases)
  
  database_options <- c('demo', 'anthro', 'intake', 'food_qs', 'psych_qs', 'dd', 'intero', 'notes', 'pna', 'micro')
  
  if (isTRUE(databases_arg)) {
    
    databases_string <- sapply(databases, FUN = is.character)
    ndatabases <- length(databases)
    
    if (sum(databases_string) != ndatabases) {
      stop("Not all items listed in databases are strings. All databases must be entered as strings and matach the following options: 'demo', 'anthro', 'intake', 'food_qs', 'psych_qs', 'dd', 'intero', 'notes', 'pna'.")
    }
    
    #check if entered databases match database_options
    if (sum(databases %in% database_options) != ndatabases){
      stop("Not all items listed in databases match available options. Options include: 'demo', 'anthro', 'intake', 'food_qs', 'psych_qs', 'dd', 'intero', 'notes', 'pna', 'micro'.")
    }
  } else {
    databases <- NA
  }
  
  # check write_path
  writepath_arg <- methods::hasArg(write_path)
  
  if (isTRUE(writepath_arg)) {
    if (!is.character(write_path)) {
      stop("write_path must be entered as a string: e.g., '.../Participant_Data/Databases/")
    }
  }
  
  #### 2. Get Visit Databases ####
  
  ## Visit 1 data - need for all databases
  
  if (isTRUE(datapath_arg)){
    v1_data <- util_fbs_merge_v1(child_file_pattern = paste0(child_fp, '_', visit_fp, '1'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '1'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
  } else {
    v1_data <- util_fbs_merge_v1(child_file_pattern = paste0(child_fp, '_', visit_fp, '1'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '1'))
  }
  
  
  ## Visit 2 - need for Anthroprometrics, food/eating behavior, and cog/psych databases
  # requires the V4 parent database so check both v2_datestr_arg and v4_datestr_arg
  if (isFALSE(databases_arg) | 'anthro' %in% databases | 'food_qs' %in% databases | 'psych_qs' %in% databases | 'intake' %in% databases | 'notes' %in% databases | 'pna' %in% databases){
    
    if (isTRUE(datapath_arg)){
      v2_data <- util_fbs_merge_v2(child_file_pattern = paste0(child_fp, '_', visit_fp, '2'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '2'), parentV4_file_pattern = paste0(parent_fp, '_', visit_fp, '4'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
    } else {
      v2_data <- util_fbs_merge_v2(child_file_pattern = paste0(child_fp, '_', visit_fp, '2'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '2'), parentV4_file_pattern = paste0(parent_fp, '_', visit_fp, '4'))
    }
  }
  
  ## Visit 3 data - need for the food/eating behavior, cog/psych, and delay discounting databases
  if (isFALSE(databases_arg) | 'food_qs' %in% databases | 'psych_qs' %in% databases | 'dd' %in% databases | 'intake' %in% databases | 'notes' %in% databases | 'pna' %in% databases){
    
    if (isTRUE(datapath_arg)){
      v3_data <- util_fbs_merge_v3(child_file_pattern = paste0(child_fp, '_', visit_fp, '3'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '3'), data_path = paste0(data_path, '/Qualtrics_Raw/'), model_DD = model_DD)
    } else {
      v3_data <- util_fbs_merge_v3(child_file_pattern = paste0(child_fp, '_', visit_fp, '3'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '3'), model_DD = model_DD)
    }
  }
  
  ## Visit 4 data - need for the demographics, food/eating behavior, and cog/psych databases
  if (isFALSE(databases_arg) | 'demo' %in% databases | 'food_qs' %in% databases | 'psych_qs' %in% databases | 'intake' %in% databases | 'notes' %in% databases | 'pna' %in% databases){
    if (isTRUE(datapath_arg)){
      v4_data <- util_fbs_merge_v4(child_file_pattern = paste0(child_fp, '_', visit_fp, '4'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '4'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
    } else {
      v4_data <- util_fbs_merge_v4(child_file_pattern = paste0(child_fp, '_', visit_fp, '4'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '4'))
    }
  }
  
  ## Visit 5 data - need for the demographics database
  if (isFALSE(databases_arg) | 'demo' %in% databases | 'intake' %in% databases | 'food_qs' %in% databases | 'notes' %in% databases | 'pna' %in% databases){
    if (isTRUE(datapath_arg)){
      v5_data <- util_fbs_merge_v5(child_file_pattern = paste0(child_fp, '_', visit_fp, '5'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '5'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
    } else {
      v5_data <- util_fbs_merge_v5(child_file_pattern = paste0(child_fp, '_', visit_fp, '5'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '5'))
    }
  }
  
  ## Visit 6 data - need for the fMRI database
  if (isFALSE(databases_arg) | 'food_qs' %in% databases | 'notes' %in% databases | 'pna' %in% databases){
    if (isTRUE(datapath_arg)){
      v6_data <- util_fbs_merge_v6(child_file_pattern = paste0(child_fp, '_', visit_fp, '6'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '6'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
    } else {
      v6_data <- util_fbs_merge_v6(child_file_pattern = paste0(child_fp, '_', visit_fp, '6'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '6'))
    }
  }
  
  ## Visit 7 data - need for the demographics, anthroprometrics, food/eating behavior, and cog/psych databases
  if (isFALSE(databases_arg) | 'demo' %in% databases | 'anthro' %in% databases | 'food_qs' %in% databases | 'psych_qs' %in% databases | 'intake' %in% databases | 'notes' %in% databases | 'pna' %in% databases){
    if (isTRUE(datapath_arg)){
      v7_data <- util_fbs_merge_v7(child_file_pattern = paste0(child_fp, '_', visit_fp, '7'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '7'), data_path = paste0(data_path, '/Qualtrics_Raw/'))
    } else {
      v7_data <- util_fbs_merge_v7(child_file_pattern = paste0(child_fp, '_', visit_fp, '7'), parent_file_pattern = paste0(parent_fp, '_', visit_fp, '7'))
    }
  }
  
  ## Microstructure data
  if (isFALSE(databases_arg) | 'micro' %in% databases){
    if (isTRUE(datapath_arg)){
      micro_data <- util_fbs_merge_micro(data_path = paste0(data_path, '/Microstructure_Raw/'))
    } else {
      micro_data <- util_fbs_merge_micro()
    }
  }
  
  #### 3. Make Databases #####
  
  #empty list
  database_return <- list()
  
  #get cross-database variables
  common_demo_data <- v1_data[['data']][c(1:13, 20:21, 60:64, 94:97, 232:233)]
  common_demo_labels <- v1_data[['dict']][c(1:13, 20:21, 60:64, 94:97, 232:233)]
  
  names(common_demo_data)[2] <- 'v1_date'
  names(common_demo_labels)[2] <- 'v1_date'
  
  ## 3b) Demographics ####
  if (isFALSE(databases_arg) | 'demo' %in% databases){
    
    #visit 1
    v1_demo_data <- v1_data[['data']][c(1:13, 20:21, 60:61, 63, 94:97, 232:233, 14:19, 22:59, 65:87)]
    v1_demo_labels <- v1_data[['dict']][c(1:13, 20:21, 60:61, 63, 94:97, 232:233, 14:19, 22:59, 65:87)]
    
    names(v1_demo_data)[2] <- 'v1_date'
    names(v1_demo_labels)[2] <- 'v1_date'
    
    #visit 4
    v4_demo_data <- v4_data[['data']][c(1:2, 6:99)]
    v4_demo_labels <- v4_data[['dict']][c(1:2, 6:99)]
    
    names(v4_demo_data)[2] <- 'v4_date'
    names(v4_demo_labels)[2] <- 'v4_date'
    
    #visit 5
    v5_demo_data <- v5_data[['data']][c(1:34)]
    v5_demo_labels <- v5_data[['dict']][c(1:34)]
    
    names(v5_demo_data)[2] <- 'v5_date'
    names(v5_demo_labels)[2] <- 'v5_date'
    
    #visit 7
    v7_demo_data <- v7_data[['data']][c(1:2, 5:120)]
    v7_demo_labels <- v7_data[['dict']][c(1:2, 5:120)]
    
    names(v7_demo_data)[2] <- 'v7_date'
    names(v7_demo_labels)[2] <- 'v7_date'
    
    #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
    for (v in 3:ncol(v7_demo_data)){
      #names
      var_name <- names(v7_demo_data)[v]
      v7_name <- paste0('v7_', var_name)
      names(v7_demo_data)[v] <- v7_name
      
      #labels
      v7_demo_labels[[var_name]] <- paste0('Visit 7 - ', v7_demo_labels[[var_name]])
    }
    
    #make names match
    names(v7_demo_labels) <- names(v7_demo_data)
    
    ## merge databases
    
    #weird issue - couldn't rbind due to some `labels` to logical issue - brute force fix
    set_attr_hfssm_hh2 <- attributes(v4_demo_data[['hfssm_hh2']])
    set_attr_hfssm_hh3 <- attributes(v4_demo_data[['hfssm_hh3']])
    set_attr_hfssm_hh4 <- attributes(v4_demo_data[['hfssm_hh4']])
    set_attr_hfssm_ch1 <- attributes(v4_demo_data[['hfssm_ch1']])
    set_attr_hfssm_ch2 <- attributes(v4_demo_data[['hfssm_ch2']])
    set_attr_hfssm_ch3 <- attributes(v4_demo_data[['hfssm_ch3']])
    set_attr_hfssm_ch5a <- attributes(v4_demo_data[['hfssm_ch5a']])
    set_attr_hfias_category <- attributes(v4_demo_data[['hfias_category']])
    
    v4_demo_data[c(5:7, 15:17, 20, 50)] <- sapply(v4_demo_data[c(5:7, 15:17, 20, 50)], as.numeric)
    
    
    demo_v1v4_data <- merge(v1_demo_data, v4_demo_data, by = 'id', all = TRUE)
    demo_v1v4v5_data <- merge(demo_v1v4_data, v5_demo_data, by = 'id', all = TRUE)
    demo_v1v4v5v7_data <- merge(demo_v1v4v5_data, v7_demo_data, by = 'id', all = TRUE)
    
    #reset labels due to brute force fix
    attributes(demo_v1v4v5v7_data[['hfssm_hh2']]) <- set_attr_hfssm_hh2
    attributes(demo_v1v4v5v7_data[['hfssm_hh3']]) <- set_attr_hfssm_hh3
    attributes(demo_v1v4v5v7_data[['hfssm_hh4']]) <- set_attr_hfssm_hh4
    attributes(demo_v1v4v5v7_data[['hfssm_ch1']]) <- set_attr_hfssm_ch1
    attributes(demo_v1v4v5v7_data[['hfssm_ch2']]) <- set_attr_hfssm_ch2
    attributes(demo_v1v4v5v7_data[['hfssm_ch3']]) <- set_attr_hfssm_ch3
    attributes(demo_v1v4v5v7_data[['hfssm_ch5a']]) <- set_attr_hfssm_ch5a
    attributes(demo_v1v4v5v7_data[['hfias_category']]) <- set_attr_hfias_category
    
    class(demo_v1v4v5v7_data[["hfssm_hh2"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    class(demo_v1v4v5v7_data[["hfssm_hh3"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    class(demo_v1v4v5v7_data[["hfssm_hh4"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    class(demo_v1v4v5v7_data[["hfssm_ch1"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    class(demo_v1v4v5v7_data[["hfssm_ch2"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    class(demo_v1v4v5v7_data[["hfssm_ch3"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    class(demo_v1v4v5v7_data[["hfssm_ch5a"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    class(demo_v1v4v5v7_data[["hfias_category"]]) <- c("haven_labelled", "vctrs_vctr", "double")
    
    #get labels
    demographic_labels <- c(v1_demo_labels, v4_demo_labels[2:length(v4_demo_labels)], v5_demo_labels[2:length(v5_demo_labels)], v7_demo_labels[2:length(v7_demo_labels)])
    
    # ensure labels are up to date
    demographic_data = sjlabelled::set_label(demo_v1v4v5v7_data, label = matrix(unlist(demographic_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(demo_data = demographic_data, demo_dict = demographic_labels))
    
    # write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      demo_dict <- labelled::generate_dictionary(demographic_data, details = TRUE)
      demo_dict$label <- matrix(unlist(demographic_labels, use.names = FALSE))
      names(demo_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      demo_dict_write <- sapply(demo_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(demographic_data, path = paste0(write_path, 'demographics_data.sav'))
        write.csv(demo_dict_write, file = paste0(write_path, 'dict-demographics_data.csv'), row.names = FALSE)
      } else {
        haven::write_sav(demographic_data, path = 'demographics_data.sav')
        write.csv(demo_dict_write, file = 'dict-demographics_data.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3b) Anthropometrics ####
  if (isFALSE(databases_arg) | 'anthro' %in% databases){
    
    #visit 1
    v1_anthro_data <- v1_data[['data']][c(1, 88:336)]
    v1_anthro_labels <- v1_data[['dict']][c(1, 88:336)]
    
    #visit 2
    v2_anthro_data <- v2_data[['data']][c(1:2, 4:29)]
    v2_anthro_labels <- v2_data[['dict']][c(1:2, 4:29)]
    
    names(v2_anthro_data)[2] <- 'v2_date'
    names(v2_anthro_labels)[2] <- 'v2_date'
    
    #visit 7
    v7_anthro_data <- v7_data[['data']][c(1:2, 121:369, 376:398)]
    v7_anthro_labels <- v7_data[['dict']][c(1:2, 121:369, 376:398)]
    
    names(v7_anthro_data)[2] <- 'v7_date'
    names(v7_anthro_labels)[2] <- 'v7_date'
    
    #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
    for (v in 3:ncol(v7_anthro_data)){
      #names
      var_name <- names(v7_anthro_data)[v]
      v7_name <- paste0('v7_', var_name)
      names(v7_anthro_data)[v] <- v7_name
      
      #labels
      v7_anthro_labels[[var_name]] <- paste0('Visit 7 - ', v7_anthro_labels[[var_name]])
    }
    
    #make names match
    names(v7_anthro_labels) <- names(v7_anthro_data)
    
    ## merge databases from v1
    anthro_demov1_data <- merge(common_demo_data[1:20], v1_anthro_data, by = 'id', all = TRUE)
    
    ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
    anthro_demov1v2_data <- merge(anthro_demov1_data, v2_anthro_data, by = 'id', all.x = FALSE, all.y = TRUE)
    
    ## other merges - set all = TRUE so get all participants in visits 2-7
    anthro_demov1v2v7_data <- merge(anthro_demov1v2_data, v7_anthro_data, by = 'id', all = TRUE)
    
    #get labels
    anthroprometric_labels <- c(common_demo_labels[1:20], v1_anthro_labels[2:length(v1_anthro_labels)], v2_anthro_labels[2:length(v2_anthro_labels)], v7_anthro_labels[2:length(v7_anthro_labels)])
    
    # ensure labels are up to date
    anthroprometric_data = sjlabelled::set_label(anthro_demov1v2v7_data, label = matrix(unlist(anthroprometric_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(anthro_data = anthroprometric_data, anthro_dict = anthroprometric_labels))
    
    # write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      antho_dict <- labelled::generate_dictionary(anthroprometric_data, details = TRUE)
      antho_dict$label <- matrix(unlist(anthroprometric_labels, use.names = FALSE))
      names(antho_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      antho_dict_write <- sapply(antho_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(anthroprometric_data, path = paste0(write_path, 'anthro_data.sav'))
        write.csv(antho_dict_write, file = paste0(write_path, 'dict-anthro_data.csv'), row.names = FALSE)
      } else {
        haven::write_sav(anthroprometric_data, path = 'anthro_data.sav')
        write.csv(antho_dict_write, file = 'dict_anthro_data.csv', row.names = FALSE)
      }
    }
    
  }
  
  ## 3c) Intake ####
  if (isFALSE(databases_arg) | 'intake' %in% databases){
    
    #visit 1
    v1_intake_data <- v1_data[['data']][c(1, 337:505)]
    v1_intake_labels <- v1_data[['dict']][c(1, 337:505)]
    
    #re-name variables with 'v1_' and add 'Visit 1 - ' to labels
    for (v in 2:ncol(v1_intake_data)){
      #names
      var_name <- names(v1_intake_data)[v]
      v1_name <- paste0('v1_', var_name)
      names(v1_intake_data)[v] <- v1_name
      
      #labels
      v1_intake_labels[[var_name]] <- paste0('Visit 1 - ', v1_intake_labels[[var_name]])
    }
    
    names(v1_intake_labels) <- names(v1_intake_data)
    
    #visit 2
    v2_intake_data <- v2_data[['data']][c(1:2, 30:76)]
    v2_intake_labels <- v2_data[['dict']][c(1:2, 30:76)]
    
    names(v2_intake_data)[2] <- 'v2_date'
    names(v2_intake_labels)[2] <- 'v2_date'
    
    #re-name variables with 'v2_' and add 'Visit 2 - ' to labels
    for (v in 3:ncol(v2_intake_data)){
      #names
      var_name <- names(v2_intake_data)[v]
      v2_name <- paste0('v2_', var_name)
      names(v2_intake_data)[v] <- v2_name
      
      #labels
      v2_intake_labels[[var_name]] <- paste0('Visit 2 - ', v2_intake_labels[[var_name]])
    }
    
    names(v2_intake_labels) <- names(v2_intake_data)
    
    #visit 3
    v3_intake_data <- v3_data[['data']][1:49]
    v3_intake_labels <- v3_data[['dict']][1:49]
    
    names(v3_intake_data)[2] <- 'v3_date'
    names(v3_intake_labels)[2] <- 'v3_date'
    
    #re-name variables with 'v3_' and add 'Visit 3 - ' to labels
    for (v in 3:ncol(v3_intake_data)){
      #names
      var_name <- names(v3_intake_data)[v]
      v3_name <- paste0('v3_', var_name)
      names(v3_intake_data)[v] <- v3_name
      
      #labels
      v3_intake_labels[[var_name]] <- paste0('Visit 3 - ', v3_intake_labels[[var_name]])
    }
    
    names(v3_intake_labels) <- names(v3_intake_data)
    
    #visit 4
    v4_intake_data <- v4_data[['data']][c(1:2, 100:146)]
    v4_intake_labels <- v4_data[['dict']][c(1:2, 100:146)]
    
    names(v4_intake_data)[2] <- 'v4_date'
    names(v4_intake_labels)[2] <- 'v4_date'
    
    #re-name variables with 'v4_' and add 'Visit 4 - ' to labels
    for (v in 3:ncol(v4_intake_data)){
      #names
      var_name <- names(v4_intake_data)[v]
      v4_name <- paste0('v4_', var_name)
      names(v4_intake_data)[v] <- v4_name
      
      #labels
      v4_intake_labels[[var_name]] <- paste0('Visit 4 - ', v4_intake_labels[[var_name]])
    }
    
    names(v4_intake_labels) <- names(v4_intake_data)
    
    #visit 5
    v5_intake_data <- v5_data[['data']][c(1:2, 35:81)]
    v5_intake_labels <- v5_data[['dict']][c(1:2, 35:81)]
    
    names(v5_intake_data)[2] <- 'v5_date'
    names(v5_intake_labels)[2] <- 'v5_date'
    
    #re-name variables with 'v5_' and add 'Visit 5 - ' to labels
    for (v in 3:ncol(v5_intake_data)){
      #names
      var_name <- names(v5_intake_data)[v]
      v5_name <- paste0('v5_', var_name)
      names(v5_intake_data)[v] <- v5_name
      
      #labels
      v5_intake_labels[[var_name]] <- paste0('Visit 5 - ', v5_intake_labels[[var_name]])
    }
    
    names(v5_intake_labels) <- names(v5_intake_data)
    
    #visit 7
    v7_intake_data <- v7_data[['data']][c(1:2, 399:571)]
    v7_intake_labels <- v7_data[['dict']][c(1:2, 399:571)]
    
    names(v7_intake_data)[2] <- 'v7_date'
    names(v7_intake_labels)[2] <- 'v7_date'
    
    #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
    for (v in 3:ncol(v7_intake_data)){
      #names
      var_name <- names(v7_intake_data)[v]
      v7_name <- paste0('v7_', var_name)
      names(v7_intake_data)[v] <- v7_name
      
      #labels
      v7_intake_labels[[var_name]] <- paste0('Visit 7 - ', v7_intake_labels[[var_name]])
    }
    
    #make names match
    names(v7_intake_labels) <- names(v7_intake_data)
    
    ## merge databases from v1
    intake_demov1_data <- merge(common_demo_data, v1_intake_data, by = 'id', all = TRUE)
    
    ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
    intake_demov1v2_data <- merge(intake_demov1_data, v2_intake_data, by = 'id', all.x = FALSE, all.y = TRUE)
    
    ## other merges - set all = TRUE so get all participants in visits 2-7
    intake_demov1v2v3_data <- merge(intake_demov1v2_data, v3_intake_data, by = 'id', all = TRUE)
    intake_demov1v2v3v4_data <- merge(intake_demov1v2v3_data, v4_intake_data, by = 'id', all = TRUE)
    intake_demov1v2v3v4v5_data <- merge(intake_demov1v2v3v4_data, v5_intake_data, by = 'id', all = TRUE)
    intake_demov1v23v4v5v7_data <- merge(intake_demov1v2v3v4v5_data, v7_intake_data, by = 'id', all = TRUE)
    
    #get labels
    intake_labels <- c(common_demo_labels, v1_intake_labels[2:length(v1_intake_labels)], v2_intake_labels[2:length(v2_intake_labels)], v3_intake_labels[2:length(v3_intake_labels)], v4_intake_labels[2:length(v4_intake_labels)], v5_intake_labels[2:length(v5_intake_labels)], v7_intake_labels[2:length(v7_intake_labels)])
    
    # ensure labels are up to date
    intake_data = sjlabelled::set_label(intake_demov1v23v4v5v7_data, label = matrix(unlist(intake_labels, use.names = FALSE)))
    
    ## add sort by portion size to database
    
    ps_vars <- c('id', 'date', 'freddy_pre_meal', 'freddy_post_meal', 'vas_mac_cheese', 'vas_chkn_nug', 'vas_broccoli', 'vas_grape', 'vas_water', 'rank_mac_cheese', 'rank_chkn_nug', 'rank_broccoli', 'rank_grape', 'meal_start', 'meal_end', 'meal_dur', 'noplate_chkn_nug_g', 'plate_chkn_nug_g', 'post_chkn_nug_g', 'consumed_chkn_nug_g', 'consumed_chkn_nug_kcal', 'noplate_mac_cheese_g', 'plate_mac_cheese_g', 'post_mac_cheese_g', 'consumed_mac_cheese_g', 'consumed_mac_cheese_kcal', 'noplate_grapes_g', 'plate_grapes_g', 'post_grapes_g', 'consumed_grapes_g', 'consumed_grapes_kcal', 'noplate_broccoli_g', 'plate_broccoli_g', 'post_broccoli_g', 'consumed_broccoli_g', 'consumed_broccoli_kcal', 'noplate_ketchup_g', 'plate_ketchup_g', 'post_ketchup_g', 'consumed_ketchup_g', 'consumed_ketchup_kcal', 'noplate_water_g', 'plate_water_g', 'post_water_g', 'consumed_water_g', 'total_g', 'total_kcal')
    
    ps_convert_fn <- function(intake_data, ps, ps_var) {
      
      if (ps_var == 'id'){
        ps_var_dat <- intake_data[['id']]
      } else {
        
        #visit vars
        v2_var_name <- paste0('v2_', ps_var)
        v3_var_name <- paste0('v3_', ps_var)
        v4_var_name <- paste0('v4_', ps_var)
        v5_var_name <- paste0('v5_', ps_var)
        
        #get new variable added to dataset
        ps_value <- ps -1
        
        if (ps_var == 'date'){
          ps_var_dat <- ifelse(intake_data[['v2_meal_ps']] == ps_value, as.character(intake_data[[v2_var_name]]), ifelse(intake_data[['v3_meal_ps']] == ps_value, as.character(intake_data[[v3_var_name]]), ifelse(intake_data[['v4_meal_ps']] == ps_value, as.character(intake_data[[v4_var_name]]), ifelse(intake_data[['v5_meal_ps']] == ps_value, as.character(intake_data[[v5_var_name]]), NA))))
        } else {
          ps_var_dat <- ifelse(intake_data[['v2_meal_ps']] == ps_value, intake_data[[v2_var_name]], ifelse(intake_data[['v3_meal_ps']] == ps_value, intake_data[[v3_var_name]], ifelse(intake_data[['v4_meal_ps']] == ps_value, intake_data[[v4_var_name]], ifelse(intake_data[['v5_meal_ps']] == ps_value, intake_data[[v5_var_name]], NA))))
        }
      }
      
      return(ps_var_dat)
    }
    
    ## portion size data
    ps1_intake_data <- as.data.frame(sapply(ps_vars, FUN = ps_convert_fn, intake_data = intake_data, ps = 1, simplify = TRUE, USE.NAMES = TRUE))
    names(ps1_intake_data)[2:ncol(ps1_intake_data)] <- paste0('ps1_', ps_vars[2:length(ps_vars)])
    ps1_intake_data[c(1, 3:13, 16:47)] <- sapply(ps1_intake_data[c(1, 3:13, 16:47)], as.numeric)
    intake_data <- merge(intake_data, ps1_intake_data, by = 'id',  all = TRUE)
    
    ps2_intake_data <- as.data.frame(sapply(ps_vars, FUN = ps_convert_fn, intake_data = intake_data, ps = 2, simplify = TRUE, USE.NAMES = TRUE))
    names(ps2_intake_data)[2:ncol(ps2_intake_data)] <- paste0('ps2_', ps_vars[2:length(ps_vars)])
    ps2_intake_data[c(1, 3:13, 16:47)] <- sapply(ps2_intake_data[c(1, 3:13, 16:47)], as.numeric)
    intake_data <- merge(intake_data, ps2_intake_data, by = 'id',  all = TRUE)
    
    ps3_intake_data <- as.data.frame(sapply(ps_vars, FUN = ps_convert_fn, intake_data = intake_data, ps = 3, simplify = TRUE, USE.NAMES = TRUE))
    names(ps3_intake_data)[2:ncol(ps3_intake_data)] <- paste0('ps3_', ps_vars[2:length(ps_vars)])
    ps3_intake_data[c(1, 3:13, 16:47)] <- sapply(ps3_intake_data[c(1, 3:13, 16:47)], as.numeric)
    intake_data <- merge(intake_data, ps3_intake_data, by = 'id',  all = TRUE)
    
    ps4_intake_data <- as.data.frame(sapply(ps_vars, FUN = ps_convert_fn, intake_data = intake_data, ps = 4, simplify = TRUE, USE.NAMES = TRUE))
    names(ps4_intake_data)[2:ncol(ps4_intake_data)] <- paste0('ps4_', ps_vars[2:length(ps_vars)])
    ps4_intake_data[c(1, 3:13, 16:47)] <- sapply(ps4_intake_data[c(1, 3:13, 16:47)], as.numeric)
    intake_data <- merge(intake_data, ps4_intake_data, by = 'id',  all = TRUE)
    
    
    ## update/add portion size labels
    for (ps in 1:4) {
      for (v in 2:length(ps_vars)){
        #new portion size name
        ps_var_name <- paste0('ps', ps, '_', ps_vars[v])
        
        #visit vars
        v2_var_name <- paste0('v2_', ps_vars[v])
        
        #get label information with visit information removed
        novisit_label <- gsub('Visit 2 - ', '', intake_labels[[v2_var_name]], fixed = TRUE)
        
        #add new label with portion size information
        intake_labels[[ps_var_name]] <- paste0('Portion Size ', ps, ': ', novisit_label)
      }
    }
    
    #update and match labels
    names(intake_labels) <- names(intake_data)
    intake_data = sjlabelled::set_label(intake_data, label = matrix(unlist(intake_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(intake_data = intake_data, intake_dict = intake_labels))
    
    # write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      intake_dict <- labelled::generate_dictionary(intake_data, details = TRUE)
      intake_dict$label <- matrix(unlist(intake_labels, use.names = FALSE))
      names(intake_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      intake_dict_write <- sapply(intake_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(intake_data, path = paste0(write_path, 'intake_data.sav'))
        write.csv(intake_dict_write, file = paste0(write_path, 'dict-intake_data.csv'), row.names = FALSE)
      } else {
        haven::write_sav(intake_data, path = 'intake_data.sav')
        write.csv(intake_dict_write, file = 'dict-intake_data.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3d) Eating Behavior/Food Intake Questionnaires ####
  if (isFALSE(databases_arg) | 'food_qs' %in% databases){
    
    #visit 1
    v1_foodqs_data <- v1_data[['data']][c(1, 506:821)]
    v1_foodqs_labels <- v1_data[['dict']][c(1, 506:821)]
    
    #visit 2
    v2_foodqs_data <- v2_data[['data']][c(1:2, 77:299)]
    v2_foodqs_labels <- v2_data[['dict']][c(1:2, 77:299)]
    
    names(v2_foodqs_data)[2] <- 'v2_date'
    names(v2_foodqs_labels)[2] <- 'v2_date'
    
    #visit 3
    v3_foodqs_data <- v3_data[['data']][c(1:2, 50:160)]
    v3_foodqs_labels <- v3_data[['dict']][c(1:2, 50:160)]
    
    names(v3_foodqs_data)[2] <- 'v3_date'
    names(v3_foodqs_labels)[2] <- 'v3_date'
    
    #visit 4
    v4_foodqs_data <- v4_data[['data']][c(1:2, 147:158)]
    v4_foodqs_labels <- v4_data[['dict']][c(1:2, 147:158)]
    
    names(v4_foodqs_data)[2] <- 'v4_date'
    names(v4_foodqs_labels)[2] <- 'v4_date'
    
    #visit 5
    v5_foodqs_data <- v5_data[['data']][c(1:2, 82:104)]
    v5_foodqs_labels <- v5_data[['dict']][c(1:2, 82:104)]
    
    names(v5_foodqs_data)[2] <- 'v5_date'
    names(v5_foodqs_labels)[2] <- 'v5_date'
    
    #visit 6
    v6_foodqs_data <- v6_data[['data']][c(1:2, 330:336)]
    v6_foodqs_labels <- v6_data[['dict']][c(1:2, 330:336)]
    
    names(v6_foodqs_data)[2] <- 'v6_date'
    names(v6_foodqs_labels)[2] <- 'v6_date'
    
    #visit 7
    v7_foodqs_data <- v7_data[['data']][c(1:2, 370:375, 572:887)]
    v7_foodqs_labels <- v7_data[['dict']][c(1:2, 370:375, 572:887)]
    
    names(v7_foodqs_data)[2] <- 'v7_date'
    names(v7_foodqs_labels)[2] <- 'v7_date'
    
    #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
    for (v in 3:ncol(v7_foodqs_data)){
      #names
      var_name <- names(v7_foodqs_data)[v]
      v7_name <- paste0('v7_', var_name)
      names(v7_foodqs_data)[v] <- v7_name
      
      #labels
      v7_foodqs_labels[[var_name]] <- paste0('Visit 7 - ', v7_foodqs_labels[[var_name]])
    }
    
    #make names match
    names(v7_foodqs_labels) <- names(v7_foodqs_data)
    
    ## merge databases from v1
    foodqs_demov1_data <- merge(common_demo_data, v1_foodqs_data, by = 'id', all = TRUE)
    
    ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
    foodqs_demov1v2_data <- merge(foodqs_demov1_data, v2_foodqs_data, by = 'id', all.x = FALSE, all.y = TRUE)
    
    ## other merges - set all = TRUE so get all participants in visits 2-7
    foodqs_demov1v2v3_data <- merge(foodqs_demov1v2_data, v3_foodqs_data, by = 'id', all = TRUE)
    foodqs_demov1v2v3v4_data <- merge(foodqs_demov1v2v3_data, v4_foodqs_data, by = 'id', all = TRUE)
    foodqs_demov1v2v3v4v5_data <- merge(foodqs_demov1v2v3v4_data, v5_foodqs_data, by = 'id', all = TRUE)
    foodqs_demov1v2v3v4v5v6_data <- merge(foodqs_demov1v2v3v4v5_data, v6_foodqs_data, by = 'id', all = TRUE)
    foodqs_demov1v2v3v4v5v6v7_data <- merge(foodqs_demov1v2v3v4v5v6_data, v7_foodqs_data, by = 'id', all = TRUE)
    
    #get labels
    foodqs_labels <- c(common_demo_labels, v1_foodqs_labels[2:length(v1_foodqs_labels)], v2_foodqs_labels[2:length(v2_foodqs_labels)], v3_foodqs_labels[2:length(v3_foodqs_labels)], v4_foodqs_labels[2:length(v4_foodqs_labels)], v5_foodqs_labels[2:length(v5_foodqs_labels)], v6_foodqs_labels[2:length(v6_foodqs_labels)], v7_foodqs_labels[2:length(v7_foodqs_labels)])
    
    # ensure labels are up to date
    foodqs_data = sjlabelled::set_label(foodqs_demov1v2v3v4v5v6v7_data, label = matrix(unlist(foodqs_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(foodqs_data = foodqs_data, foodqs_dict = foodqs_labels))
    
    #write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      foodqs_dict <- labelled::generate_dictionary(foodqs_data, details = TRUE)
      foodqs_dict$label <- matrix(unlist(foodqs_labels, use.names = FALSE))
      names(foodqs_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      foodqs_dict_write <- sapply(foodqs_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(foodqs_data, path = paste0(write_path, 'qs_eatbeh_bodyimage.sav'))
        write.csv(foodqs_dict_write, file = paste0(write_path, 'dict-qs_eatbeh_bodyimage.csv'), row.names = FALSE)
      } else {
        haven::write_sav(foodqs_data, path = 'qs_eatbeh_bodyimage.sav')
        write.csv(foodqs_dict_write, file = 'dict-qs_eatbeh_bodyimage.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3e) Cognitive and Psychosocial Questionnaires/Measures ####
  if (isFALSE(databases_arg) | 'psych_qs' %in% databases){
    
    #visit 2
    v2_psychqs_data <- v2_data[['data']][c(1:2, 300:458)]
    v2_psychqs_labels <- v2_data[['dict']][c(1:2, 300:458)]
    
    names(v2_psychqs_data)[2] <- 'v2_date'
    names(v2_psychqs_labels)[2] <- 'v2_date'
    
    #visit 3
    v3_psychqs_data <- v3_data[['data']][c(1:2, 161:248)]
    v3_psychqs_labels <- v3_data[['dict']][c(1:2, 161:248)]
    
    names(v3_psychqs_data)[2] <- 'v3_date'
    names(v3_psychqs_labels)[2] <- 'v3_date'
    
    #visit 4
    v4_psychqs_data <- v4_data[['data']][c(1:2, 159:280)]
    v4_psychqs_labels <- v4_data[['dict']][c(1:2, 159:280)]
    
    names(v4_psychqs_data)[2] <- 'v4_date'
    names(v4_psychqs_labels)[2] <- 'v4_date'
    
    #visit 7
    v7_psychqs_data <- v7_data[['data']][c(1:2, 888:998)]
    v7_psychqs_labels <- v7_data[['dict']][c(1:2, 888:998)]
    
    names(v7_psychqs_data)[2] <- 'v7_date'
    names(v7_psychqs_labels)[2] <- 'v7_date'
    
    #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
    for (v in 3:ncol(v7_psychqs_data)){
      #names
      var_name <- names(v7_psychqs_data)[v]
      v7_name <- paste0('v7_', var_name)
      names(v7_psychqs_data)[v] <- v7_name
      
      #labels
      v7_psychqs_labels[[var_name]] <- paste0('Visit 7 - ', v7_psychqs_labels[[var_name]])
    }
    
    #make names match
    names(v7_psychqs_labels) <- names(v7_psychqs_data)
    
    ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
    psychqs_demov2_data <- merge(common_demo_data, v2_psychqs_data, by = 'id', all.x = FALSE, all.y = TRUE)
    
    ## other merges - set all = TRUE so get all participants in visits 2-7
    psychqs_demov2v3_data <- merge(psychqs_demov2_data, v3_psychqs_data, by = 'id', all = TRUE)
    psychqs_demov2v3v4_data <- merge(psychqs_demov2v3_data, v4_psychqs_data, by = 'id', all = TRUE)
    psychqs_demov2v3v4v7_data <- merge(psychqs_demov2v3v4_data, v7_psychqs_data, by = 'id', all = TRUE)
    
    #get labels
    psychqs_labels <- c(common_demo_labels, v2_psychqs_labels[2:length(v2_psychqs_labels)], v3_psychqs_labels[2:length(v3_psychqs_labels)], v4_psychqs_labels[2:length(v4_psychqs_labels)], v7_psychqs_labels[2:length(v7_psychqs_labels)])
    
    # ensure labels are up to date
    psychqs_data = sjlabelled::set_label(psychqs_demov2v3v4v7_data, label = matrix(unlist(psychqs_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(psychqs_data = psychqs_data, psychqs_dict = psychqs_labels))
    
    #write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      psychqs_dict <- labelled::generate_dictionary(psychqs_data, details = TRUE)
      psychqs_dict$label <- matrix(unlist(psychqs_labels, use.names = FALSE))
      names(psychqs_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      psychqs_dict_write <- sapply(psychqs_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(psychqs_data, path = paste0(write_path, 'qs_cog_psych_soc.sav'))
        write.csv(psychqs_dict_write, file = paste0(write_path, 'dict-qs_cog_psych_soc.csv'), row.names = FALSE)
      } else {
        haven::write_sav(psychqs_data, path = 'qs_cog_psych_soc.sav')
        write.csv(psychqs_dict_write, file = 'dict-qs_cog_psych_soc.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3f) Delay Discounting ####
  if (isFALSE(databases_arg) | 'dd' %in% databases ){
    
    #visit 3
    v3_dd_data <- v3_data[['data']][c(1:2, 249:325)]
    v3_dd_labels <- v3_data[['dict']][c(1:2, 249:325)]
    
    names(v3_dd_data)[2] <- 'v3_date'
    names(v3_dd_labels)[2] <- 'v3_date'
    
    ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
    dd_demov3_data <- merge(common_demo_data, v3_dd_data, by = 'id', all.x = FALSE, all.y = TRUE)
    
    #get labels
    dd_labels <- c(common_demo_labels, v3_dd_labels[2:length(v3_dd_labels)])
    
    # ensure labels are up to date
    dd_data = sjlabelled::set_label(dd_demov3_data, label = matrix(unlist(dd_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(dd_data = dd_data, dd_dict = dd_labels))
    
    #write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      dd_dict <- labelled::generate_dictionary(dd_data, details = TRUE)
      dd_dict$label <- matrix(unlist(dd_labels, use.names = FALSE))
      names(dd_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      dd_dict_write <- sapply(dd_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(dd_data, path = paste0(write_path, 'delay_discounting.sav'))
        write.csv(dd_dict_write, file = paste0(write_path, 'dict-delay_discounting.csv'), row.names = FALSE)
      } else {
        haven::write_sav(dd_data, path = 'delay_discounting.sav')
        write.csv(dd_dict_write, file = 'dict_delay_discounting.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3g) Interoception ####
  if (isFALSE(databases_arg) | 'intero' %in% databases){
    
    #visit 3
    v5_intero_data <- v5_data[['data']][c(1:2, 105:191)]
    v5_intero_labels <- v5_data[['dict']][c(1:2, 105:191)]
    
    names(v5_intero_data)[2] <- 'v5_date'
    names(v5_intero_labels)[2] <- 'v5_date'
    
    ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
    intero_demov5_data <- merge(common_demo_data, v5_intero_data, by = 'id', all.x = FALSE, all.y = TRUE)
    
    #get labels
    intero_labels <- c(common_demo_labels, v5_intero_labels[2:length(v5_intero_labels)])
    
    # ensure labels are up to date
    intero_data = sjlabelled::set_label(intero_demov5_data, label = matrix(unlist(intero_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(intero_data = intero_data, intero_dict = intero_labels))
    
    #write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      intero_dict <- labelled::generate_dictionary(intero_data, details = TRUE)
      intero_dict$label <- matrix(unlist(intero_labels, use.names = FALSE))
      names(intero_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      intero_dict_write <- sapply(intero_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(intero_data, path = paste0(write_path, 'intero_data.sav'))
        write.csv(intero_dict_write, file = paste0(write_path, 'dict-intero_data.csv'), row.names = FALSE)
      } else {
        haven::write_sav(intero_data, path = 'intero_data.sav')
        write.csv(intero_dict_write, file = 'dict-intero_data.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3h) Notes ####
  if (isFALSE(databases_arg) | 'notes' %in% databases){
    
    #visit 1
    v1_notes_data <- v1_data[['data']][c(1, 822:825)]
    v1_notes_labels <- v1_data[['dict']][c(1, 822:825)]
    
    #re-name variables with 'v1_' and add 'Visit 1 - ' to labels
    for (v in 2:ncol(v1_notes_data)){
      #names
      var_name <- names(v1_notes_data)[v]
      v1_name <- paste0('v1_', var_name)
      names(v1_notes_data)[v] <- v1_name
      
      #labels
      v1_notes_labels[[var_name]] <- paste0('Visit 1 - ', v1_notes_labels[[var_name]])
    }
    
    names(v1_notes_labels) <- names(v1_notes_data)
    
    #visit 2
    v2_notes_data <- v2_data[['data']][c(1:2, 459:474)]
    v2_notes_labels <- v2_data[['dict']][c(1:2, 459:474)]
    
    names(v2_notes_data)[2] <- 'v2_date'
    names(v2_notes_labels)[2] <- 'v2_date'
    
    #re-name variables with 'v2_' and add 'Visit 2 - ' to labels
    for (v in 3:ncol(v2_notes_data)){
      #names
      var_name <- names(v2_notes_data)[v]
      v2_name <- paste0('v2_', var_name)
      names(v2_notes_data)[v] <- v2_name
      
      #labels
      v2_notes_labels[[var_name]] <- paste0('Visit 2 - ', v2_notes_labels[[var_name]])
    }
    
    names(v2_notes_labels) <- names(v2_notes_data)
    
    #visit 3
    if (isTRUE(model_DD)){
      v3_notes_data <- v3_data[['data']][c(1:2, 326:341)]
      v3_notes_labels <- v3_data[['dict']][c(1:2, 326:341)]
    } else {
      v3_notes_data <- v3_data[['data']][c(1:2, 318:333)]
      v3_notes_labels <- v3_data[['dict']][c(1:2, 318:333)]
    }
    
    
    names(v3_notes_data)[2] <- 'v3_date'
    names(v3_notes_labels)[2] <- 'v3_date'
    
    #re-name variables with 'v3_' and add 'Visit 3 - ' to labels
    for (v in 3:ncol(v3_notes_data)){
      #names
      var_name <- names(v3_notes_data)[v]
      v3_name <- paste0('v3_', var_name)
      names(v3_notes_data)[v] <- v3_name
      
      #labels
      v3_notes_labels[[var_name]] <- paste0('Visit 3 - ', v3_notes_labels[[var_name]])
    }
    
    names(v3_notes_labels) <- names(v3_notes_data)
    
    #visit 4
    v4_notes_data <- v4_data[['data']][c(1:2, 271:287)]
    v4_notes_labels <- v4_data[['dict']][c(1:2, 271:287)]
    
    names(v4_notes_data)[2] <- 'v4_date'
    names(v4_notes_labels)[2] <- 'v4_date'
    
    #re-name variables with 'v4_' and add 'Visit 4 - ' to labels
    for (v in 3:ncol(v4_notes_data)){
      #names
      var_name <- names(v4_notes_data)[v]
      v4_name <- paste0('v4_', var_name)
      names(v4_notes_data)[v] <- v4_name
      
      #labels
      v4_notes_labels[[var_name]] <- paste0('Visit 4 - ', v4_notes_labels[[var_name]])
    }
    
    names(v4_notes_labels) <- names(v4_notes_data)
    
    #visit 5
    v5_notes_data <- v5_data[['data']][c(1:2, 192:209)]
    v5_notes_labels <- v5_data[['dict']][c(1:2, 192:209)]
    
    names(v5_notes_data)[2] <- 'v5_date'
    names(v5_notes_labels)[2] <- 'v5_date'
    
    #re-name variables with 'v5_' and add 'Visit 5 - ' to labels
    for (v in 3:ncol(v5_notes_data)){
      #names
      var_name <- names(v5_notes_data)[v]
      v5_name <- paste0('v5_', var_name)
      names(v5_notes_data)[v] <- v5_name
      
      #labels
      v5_notes_labels[[var_name]] <- paste0('Visit 5 - ', v5_notes_labels[[var_name]])
    }
    
    names(v5_notes_labels) <- names(v5_notes_data)
    
    #visit 6
    v6_notes_data <- v6_data[['data']][c(1:2, 337:360)]
    v6_notes_labels <- v6_data[['dict']][c(1:2, 337:360)]
    
    names(v6_notes_data)[2] <- 'v6_date'
    names(v6_notes_labels)[2] <- 'v6_date'
    
    #re-name variables with 'v6_' and add 'Visit 7 - ' to labels
    for (v in 3:ncol(v6_notes_data)){
      #names
      var_name <- names(v6_notes_data)[v]
      v6_name <- paste0('v6_', var_name)
      names(v6_notes_data)[v] <- v6_name
      
      #labels
      v6_notes_labels[[var_name]] <- paste0('Visit 6 - ', v6_notes_labels[[var_name]])
    }
    
    #make names match
    names(v6_notes_labels) <- names(v6_notes_data)
    
    #visit 7
    v7_notes_data <- v7_data[['data']][c(1:2, 999:1014)]
    v7_notes_labels <- v7_data[['dict']][c(1:2, 999:1014)]
    
    names(v7_notes_data)[2] <- 'v7_date'
    names(v7_notes_labels)[2] <- 'v7_date'
    
    #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
    for (v in 3:ncol(v7_notes_data)){
      #names
      var_name <- names(v7_notes_data)[v]
      v7_name <- paste0('v7_', var_name)
      names(v7_notes_data)[v] <- v7_name
      
      #labels
      v7_notes_labels[[var_name]] <- paste0('Visit 7 - ', v7_notes_labels[[var_name]])
    }
    
    #make names match
    names(v7_notes_labels) <- names(v7_notes_data)
    
    ## merge databases from v1
    notes_demov1_data <- merge(common_demo_data, v1_notes_data, by = 'id', all = TRUE)
    
    ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
    notes_demov1v2_data <- merge(notes_demov1_data, v2_notes_data, by = 'id', all = TRUE)
    
    ## other merges - set all = TRUE so get all participants in visits 2-7
    notes_demov1v2v3_data <- merge(notes_demov1v2_data, v3_notes_data, by = 'id', all = TRUE)
    notes_demov1v2v3v4_data <- merge(notes_demov1v2v3_data, v4_notes_data, by = 'id', all = TRUE)
    notes_demov1v2v3v4v5_data <- merge(notes_demov1v2v3v4_data, v5_notes_data, by = 'id', all = TRUE)
    notes_demov1v2v3v4v5v6_data <- merge(notes_demov1v2v3v4v5_data, v6_notes_data, by = 'id', all = TRUE)
    notes_demov1v23v4v5v6v7_data <- merge(notes_demov1v2v3v4v5v6_data, v7_notes_data, by = 'id', all = TRUE)
    
    #get labels
    notes_labels <- c(common_demo_labels, v1_notes_labels[2:length(v1_notes_labels)], v2_notes_labels[2:length(v2_notes_labels)], v3_notes_labels[2:length(v3_notes_labels)], v4_notes_labels[2:length(v4_notes_labels)], v5_notes_labels[2:length(v5_notes_labels)], v6_notes_labels[2:length(v6_notes_labels)], v7_notes_labels[2:length(v7_notes_labels)])
    
    # ensure labels are up to date
    notes_data = sjlabelled::set_label(notes_demov1v23v4v5v6v7_data, label = matrix(unlist(notes_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(notes_data = notes_data, notes_dict = notes_labels))
    
    #write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      notes_dict <- labelled::generate_dictionary(notes_data, details = TRUE)
      notes_dict$label <- matrix(unlist(notes_labels, use.names = FALSE))
      names(notes_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      notes_dict_write <- sapply(notes_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(notes_data, path = paste0(write_path, 'visit_notes.sav'))
        write.csv(notes_dict_write, file = paste0(write_path, 'dict-visit_notes.csv'), row.names = FALSE)
      } else {
        haven::write_sav(notes_data, path = 'visit_notes.sav')
        write.csv(notes_dict_write, file = 'dict-visit_notes.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3i) PNA data ####
  if (isFALSE(databases_arg) | 'pna' %in% databases){
    
    #visit 1
    v1_pna_data <- v1_data[['pna_data']]
    v1_pna_labels <- v1_data[['pna_dict']]
    
    #re-name variables with 'v1_' and add 'Visit 1 - ' to labels
    for (v in 2:ncol(v1_pna_data)){
      #names
      var_name <- names(v1_pna_data)[v]
      v1_name <- paste0('v1_', var_name)
      names(v1_pna_data)[v] <- v1_name
      
      #labels
      v1_pna_labels[[var_name]] <- paste0('Visit 1 - ', v1_pna_labels[[var_name]])
    }
    
    names(v1_pna_labels) <- names(v1_pna_data)
    
    #visit 2
    v2_pna_data <- v2_data[['pna_data']]
    v2_pna_labels <- v2_data[['pna_dict']]
    
    #re-name variables with 'v2_' and add 'Visit 2 - ' to labels
    for (v in 2:ncol(v2_pna_data)){
      #names
      var_name <- names(v2_pna_data)[v]
      v2_name <- paste0('v2_', var_name)
      names(v2_pna_data)[v] <- v2_name
      
      #labels
      v2_pna_labels[[var_name]] <- paste0('Visit 2 - ', v2_pna_labels[[var_name]])
    }
    
    names(v2_pna_labels) <- names(v2_pna_data)
    
    #visit 3
    v3_pna_data <- v3_data[['pna_data']]
    v3_pna_labels <- v3_data[['pna_dict']]
    
    #re-name variables with 'v3_' and add 'Visit 3 - ' to labels
    for (v in 2:ncol(v3_pna_data)){
      #names
      var_name <- names(v3_pna_data)[v]
      v3_name <- paste0('v3_', var_name)
      names(v3_pna_data)[v] <- v3_name
      
      #labels
      v3_pna_labels[[var_name]] <- paste0('Visit 3 - ', v3_pna_labels[[var_name]])
    }
    
    names(v3_pna_labels) <- names(v3_pna_data)
    
    #visit 4
    v4_pna_data <- v4_data[['pna_data']]
    v4_pna_labels <- v4_data[['pna_dict']]
    
    #re-name variables with 'v4_' and add 'Visit 4 - ' to labels
    for (v in 2:ncol(v4_pna_data)){
      #names
      var_name <- names(v4_pna_data)[v]
      v4_name <- paste0('v4_', var_name)
      names(v4_pna_data)[v] <- v4_name
      
      #labels
      v4_pna_labels[[var_name]] <- paste0('Visit 4 - ', v4_pna_labels[[var_name]])
    }
    
    names(v4_pna_labels) <- names(v4_pna_data)
    
    #visit 5
    v5_pna_data <- v5_data[['pna_data']]
    v5_pna_labels <- v5_data[['pna_dict']]
    
    #re-name variables with 'v5_' and add 'Visit 5 - ' to labels
    for (v in 2:ncol(v5_pna_data)){
      #names
      var_name <- names(v5_pna_data)[v]
      v5_name <- paste0('v5_', var_name)
      names(v5_pna_data)[v] <- v5_name
      
      #labels
      v5_pna_labels[[var_name]] <- paste0('Visit 5 - ', v5_pna_labels[[var_name]])
    }
    
    names(v5_pna_labels) <- names(v5_pna_data)
    
    #visit 7
    v7_pna_data <- v7_data[['pna_data']]
    v7_pna_labels <- v7_data[['pna_dict']]
    
    #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
    for (v in 2:ncol(v7_pna_data)){
      #names
      var_name <- names(v7_pna_data)[v]
      v7_name <- paste0('v7_', var_name)
      names(v7_pna_data)[v] <- v7_name
      
      #labels
      v7_pna_labels[[var_name]] <- paste0('Visit 7 - ', v7_pna_labels[[var_name]])
    }
    
    #make names match
    names(v7_pna_labels) <- names(v7_pna_data)
    
    ## merge databases - set all = TRUE so get all participants in visits 1-7
    pna_v1v2_data <- merge(v1_pna_data, v2_pna_data, by = 'id', all = TRUE)
    pna_v1v2v3_data <- merge(pna_v1v2_data, v3_pna_data, by = 'id', all = TRUE)
    pna_v1v2v3v4_data <- merge(pna_v1v2v3_data, v4_pna_data, by = 'id', all = TRUE)
    pna_v1v2v3v4v5_data <- merge(pna_v1v2v3v4_data, v5_pna_data, by = 'id', all = TRUE)
    pna_v1v2v3v4v5v7_data <- merge(pna_v1v2v3v4v5_data, v7_pna_data, by = 'id', all = TRUE)
    
    #get labels
    pna_labels <- c(v1_pna_labels, v2_pna_labels[2:length(v2_pna_labels)], v3_pna_labels[2:length(v3_pna_labels)], v4_pna_labels[2:length(v4_pna_labels)], v5_pna_labels[2:length(v5_pna_labels)], v7_pna_labels[2:length(v7_pna_labels)])
    
    # ensure labels are up to date
    pna_data = sjlabelled::set_label(pna_v1v2v3v4v5v7_data, label = matrix(unlist(pna_labels, use.names = FALSE)))
    
    # add to list
    database_return <- c(database_return, list(pna_data = pna_data, pna_dict = pna_labels))
    
    #write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      pna_dict <- labelled::generate_dictionary(pna_data, details = TRUE)
      pna_dict$label <- matrix(unlist(pna_labels, use.names = FALSE))
      names(pna_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      pna_dict_write <- sapply(pna_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(pna_data, path = paste0(write_path, 'pna_notes.sav'))
        write.csv(pna_dict_write, file = paste0(write_path, 'dict-pna_notes.csv'), row.names = FALSE)
      } else {
        haven::write_sav(pna_data, path = 'pna_notes.sav')
        write.csv(pna_dict_write, file = 'dict-pna_notes.csv', row.names = FALSE)
      }
    }
  }
  
  ## 3i) Microstructure data ####
  if (isFALSE(databases_arg) | 'micro' %in% databases){
    
    micro_data[['beh_wide']][['data']][['id']] <- as.numeric(micro_data[['beh_wide']][['data']][['id']])
    micro_data[['event']][['data']][['id']] <- as.numeric(micro_data[['event']][['data']][['id']])
    
    ## merge databases - set all = TRUE so get all participants in visits 1-7
    beh_wide_demo_data <- merge(common_demo_data, micro_data[['beh_wide']][['data']], by = 'id', all = TRUE)
    
    event_demo_data <- merge(common_demo_data, micro_data[['event']][['data']], by = 'id', all = TRUE)
    
    #get labels
    beh_wide_labels <- c(common_demo_labels, micro_data[['beh_wide']][['dict']][2:length(micro_data[['beh_wide']][['dict']])])
    event_labels <- c(common_demo_labels, micro_data[['event']][['dict']][2:length(micro_data[['event']][['dict']])])
    
    # ensure labels are up to date
    beh_wide_data = sjlabelled::set_label(beh_wide_demo_data, label = matrix(unlist(beh_wide_labels, use.names = FALSE)))
    
    event_data = sjlabelled::set_label(event_demo_data, label = matrix(unlist(event_labels, use.names = FALSE)))
    
    
    # add to list
    database_return <- c(database_return, list(micro_wide = beh_wide_data, micro_wide_dict = beh_wide_labels, micro_event = event_data, micro_event_dict = event_labels))
    
    #write out
    if (isTRUE(write_dat)){
      
      #data dictionary
      micro_wide_dict <- labelled::generate_dictionary(beh_wide_data, details = TRUE)
      micro_wide_dict$label <- matrix(unlist(beh_wide_labels, use.names = FALSE))
      names(micro_wide_dict)[1] <- 'column'
      
      micro_event_dict <- labelled::generate_dictionary(event_data, details = TRUE)
      micro_event_dict$label <- matrix(unlist(event_labels, use.names = FALSE))
      names(micro_event_dict)[1] <- 'column'
      
      #interprets the value_labels as list so need to make everything a character
      micro_wide_dict_write <- sapply(micro_wide_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      micro_event_dict_write <- sapply(micro_event_dict[c(1:3, 6:8, 12:13)], FUN = as.character)
      
      if (isTRUE(writepath_arg)){
        haven::write_sav(beh_wide_data, path = paste0(write_path, 'micro_beh_summary.sav'))
        write.csv(micro_wide_dict_write, file = paste0(write_path, 'dict-micro_beh_summary.csv'), row.names = FALSE)
        
        haven::write_sav(event_data, path = paste0(write_path, 'micro_events.sav'))
        write.csv(micro_event_dict_write, file = paste0(write_path, 'dict-micro_events.csv'), row.names = FALSE)
      } else {
        haven::write_sav(beh_wide_data, 'micro_beh_summary.sav')
        write.csv(micro_wide_dict, 'dict-micro_beh_summary.csv', row.names = FALSE)
        
        haven::write_sav(event_data, path = 'micro_events.sav')
        write.csv(micro_event_dict, file = 'dict-micro_events.csv', row.names = FALSE)
      }
    }
  }
  
  #### 10. Return List #####
  if(isTRUE(return_data)){
    return(database_return)
  }
  
}

