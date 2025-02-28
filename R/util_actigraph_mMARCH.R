#' util_actigraph_mMARCH: Set up and run mMARCH.AC.maincall() from the mMARCH.AC package
#'
#' This function fills out necessary settings to set up and run mMARCH.AC post GGIR processing.
#' 
#' Notes:
#' \itemize{
#'  \item{1) Uses activity thresholds set according to: }
#'  \itemize{
#'    \item{Hildebrand M, VAN Hees VT, Hansen BH, Ekelund U. Age group comparability of raw accelerometer output from wrist- and hip-worn monitors. Med Sci Sports Exerc. 2014 Sep;46(9):1816-24. doi: 10.1249/MSS.0000000000000289. (\href{https://pubmed.ncbi.nlm.nih.gov/24887173/}{PubMed})}
#'    \item{Hildebrand M, Hansen BH, van Hees VT, Ekelund U. Evaluation of raw acceleration sedentary thresholds in children and adults. Scand J Med Sci Sports. 2017 Dec;27(12):1814-1823. doi: 10.1111/sms.12795. Epub 2016 Nov 22. PMID: 27878845.(\href{https://pubmed.ncbi.nlm.nih.gov/27878845/}{PubMed})}
#'  }
#'  \item{2) Relies on GGIR outpus so must be completed after GGIR pre-processing}
#'  }
#'  
#'  References:
#'  \itemize{
#'  \item{Guo, Wei, Andrew Leroux, Haochang Shou, Lihong Cui, Sun Jung Kang, Marie Pierre Françoise Strippoli, Martin Preisig, Vadim Zipunnikov, and Kathleen Ries Merikangas. "Processing of Accelerometry Data with GGIR in Motor Activity Research Consortium for Health." Journal for the Measurement of Physical Behaviour 6, no. 1 (2023): 37-44.}
#'  }
#'
#' To use this function, the correct paths for all directories must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param mode the mode to run in mMARCH.AC. Notes: \itemize{
#'   \item{0 = create R/Rmd/sh files}
#'   \item{1 = read, transform, and merg data from csv folder of GGIR}
#'   \item{2 = GGIR output files checked and summarized}
#'   \item{3 = clean merged data}
#'   \item{4 = imputation of cleaned data}
#'   }
#' @param deriv_dir full path to the directory for output to be stored
#' @param study_name character string with study name
#' @param actigraph_path full path to the raw actigraph data in raw_untouched
#' @param ggir_path full path to the GGIR output files
#' @param filename2id a function defined to extract subject id from filenames
#'
#' @examples
#'
#' # setup post-processing for actigraph data
#' util_actigraph_mMARCH(deriv_dir, sub_str, study_name, actigraph_path, ggir_path)
#'
#' \dontrun{
#' }
#'
#'
#' @export
util_actigraph_mMARCH <- function(mode, deriv_dir, study_name, actigraph_path, ggir_path, filename2id){
  
  # set up call
  mMARCH.AC::mMARCH.AC.maincall(mode = mode,
                     currentdir = deriv_dir,
                     studyname = study_name,
                     bindir = actigraph_path,
                     outputdir =  ggir_path,
                     filename2id = filename2id,
                     PA.threshold=c(35.6,201.4,707),
                     useIDs.FN = NULL,
                     epochIn = 5,
                     epochOut = 60, 
                     use.cluster = FALSE,
                     log.multiplier = 9250,
                     QCdays.alpha = 5,
                     QChours.alpha = 16,
                     QCnights.feature.alpha = c(0,0,0,0),
                     DoubleHour = "average",
                     part5FN = "WW_L35.6M201.4V707_T5A5",
                     QC.sleepdur.avg = NULL,
                     QC.nblocks.sleep.avg = NULL,
                     Rversion = "R",
                     desiredtz = "US/Eastern",
                     RemoveDaySleeper = FALSE,
                     NfileEachBundle = 20,
                     holidayFN = NULL,
                     trace = FALSE) 
  
}
