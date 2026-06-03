#' proc_actigraph: Process raw data actigraph data
#'
#' This function calls task scripts that:
#' \itemize{
#'  \item{1) processes raw actigraph data by session using GGIR with activity thresholds set according to: }
#'  \itemize{
#'    \item{Hildebrand M, VAN Hees VT, Hansen BH, Ekelund U. Age group comparability of raw accelerometer output from wrist- and hip-worn monitors. Med Sci Sports Exerc. 2014 Sep;46(9):1816-24. doi: 10.1249/MSS.0000000000000289. (\href{https://pubmed.ncbi.nlm.nih.gov/24887173/}{PubMed})}
#'    \item{Hildebrand M, Hansen BH, van Hees VT, Ekelund U. Evaluation of raw acceleration sedentary thresholds in children and adults. Scand J Med Sci Sports. 2017 Dec;27(12):1814-1823. doi: 10.1111/sms.12795. Epub 2016 Nov 22. PMID: 27878845.(\href{https://pubmed.ncbi.nlm.nih.gov/27878845/}{PubMed})}
#'  }
#'  \item{completes post-processing with mMARCH.AC:}
#'  \itemize{
#'    \item{Guo, W., Leroux, A., Shou, H., Cui, L., Kang, S. J., Strippoli, M. F., Preisig, M., Zipunnikov, V., & Merikangas, K. R. (2023). Processing of Accelerometry Data with GGIR in Motor Activity Research Consortium for Health. Journal for the Measurement of Physical Behaviour, 6(1), 37-44. Retrieved Feb 14, 2025, from https://doi.org/10.1123/jmpb.2022-0018}
#'  }
#'  \item{computes Sleep Regularity Index scores using sleepreg:}
#'  \itemize{
#'    \item{Windred DP, Jones SE, Russell A, Burns AC, Chan P, Weedon MN, Rutter MK, Olivier P, Vetter C, Saxena R, Lane JM, Cain SW, Phillips AJK. Objective assessment of sleep regularity in 60 000 UK Biobank participants using an open-source package. Sleep. 2021 Dec 10;44(12):zsab254. doi: 10.1093/sleep/zsab254. PMID: 34748000.(\href{https://pubmed.ncbi.nlm.nih.gov/34748000/}{PubMed})}
#'    \item{Windred DP, Burns AC, Lane JM, Saxena R, Rutter MK, Cain SW, Phillips AJK. Sleep regularity is a stronger predictor of mortality risk than sleep duration: A prospective cohort study. Sleep. 2024 Jan 11;47(1):zsad253. doi: 10.1093/sleep/zsad253. PMID: 37738616; PMCID: PMC10782501.(\href{https://pubmed.ncbi.nlm.nih.gov/37738616/}{PubMed})}
#'  }
#' }
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @inheritParams proc_tasks
#' @inheritParams util_task_org_sourcedata
#' @param proc_ggir (logical) run the GGIR and mMARCH processing step. Default = FALSE
#' @param overwrite_ggir_derivs (required if 'actigraph' is in data_list or data_list = 'all') overwrite GGIR derivatives. Defult = FLASE, this will take a LONG time. 
#' 
#'
#' @return data.frame for each task with status for each processing step
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' proc_tasks_pardat <- proc_tasks(base_wd, overwrite)
#'
#' \dontrun{
#' }
#'
#'
#' @export

proc_actigraph <- function(base_wd, overwrite = FALSE, proc_ggir = FALSE, overwrite_ggir_derivs = FASLE) {
  
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
  
  #### Define paths ####
  bids_wd <- file.path(base_wd, 'bids')
  sourcedata_wd <- file.path(base_wd, 'bids', 'sourcedata')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')

  # Processing Steps ####
  for (session in c('baseline', 'followup')){
    
    if (session == 'baseline'){
      dir_name <- 'actigraphy'
    } else {
      dir_name <- 'actigraphy_v3'
    }
    
    # get list of available subjects 
    actigraph_list <- as.data.frame(list.files(path = file.path(base_wd, 'raw_untouched', dir_name), pattern = '.gt3x'))
    names(actigraph_list) <- 'filename'
    
    #get list of subject IDs
    actigraph_list[['sub_str']] <- sapply(actigraph_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)
    actigraph_list <- actigraph_list[actigraph_list[['sub_str']] != 'pilot', ]
    
    #organize data into BIDS sourcedata
    actigraph_list[['sourcedata_done']] <- sapply(actigraph_list[['sub_str']], function(x) util_actigraph_org_sourcedata(sub_str = x, ses = session, dir_name = dir_name, base_wd = base_wd, overwrite = overwrite), simplify = TRUE)
    
    if (isTRUE(proc_ggir)){
      #process raw data - GGIR
      actigraph_path <- file.path(base_wd, dir_name)
      deriv_dir_ggir <- file.path(bids_wd, 'derivatives', 'motion', session)
      
      # get list of files
      data_list <- c(file.path(actigraph_path, actigraph_list[['filename']]))
      
      #make directory if needed
      if (!dir.exists(deriv_dir_ggir)) {
        dir.create(deriv_dir_ggir, recursive = TRUE)
      } 
      
      ggir_data <- GGIR::GGIR(datadir = data_list,
                              outputdir = deriv_dir_ggir,
                              configfile = 'config_files/util_ggir_config.csv',
                              studyname = 'brake',
                              mode = 1:6,
                              overwrite = overwrite_ggir_derivs, 
                              part5_agg2_60seconds = TRUE,
                              part6CR = TRUE)
      
      # rename ouput file
      if (!dir.exists(file.path(deriv_dir_ggir, 'ggir_output'))) {
        file.rename(file.path(deriv_dir_ggir, 'output_brake'), file.path(deriv_dir_ggir, 'ggir_output'))
      } else {
        print(paste0(deriv_dir_ggir, '/ggir_output already exisits. Could not rename output_actigraphy to ggir_output. Do manually or remove old ggir_output file and re-process data.'))
      }
      
      #post-processing - mMRACH.AC
      deriv_dir_mMARCH <- file.path(bids_wd, 'derivatives', 'motion', session, 'mMARCH')
      
      #make directory if needed
      if (!dir.exists(deriv_dir_mMARCH)) {
        dir.create(deriv_dir_mMARCH, recursive = TRUE)
      } 
      
      ggir_path <- file.path(deriv_dir_ggir, 'ggir_output')
      
      filename2id <- function(filename) {
        newID <- substr(filename, 1, unlist(gregexpr('_', filename))-1)
        return(as.character(newID))
      }
      
      
      #run set-up call
      sapply(0:4, function(x) util_actigraph_mMARCH(mode = x, deriv_dir = deriv_dir_mMARCH, study_name = 'brake', 
                                                    actigraph_path = actigraph_path, 
                                                    ggir_path = ggir_path, 
                                                    filename2id = filename2id))
      
      
      #organize data
      save_path <- file.path(bids_wd, 'derivatives', 'motion', session, 'clean')
      
      util_actigraph_clean(save_path, ggir_path, mMARCH_path = deriv_dir_mMARCH, metrics = c('SL', 'PA', 'CR'), overwrite)
      
    }
  }
}
