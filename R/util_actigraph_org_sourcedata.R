#' util_actigraph_org_sourcedata: Organize actigraphy raw data into BIDS format sourcedata
#'
#' This function copies data from raw_untouched and saves it in sourcedata
#' 
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @param dir_name directory name for raw actigraph data (baseline: 'actigraphy'; followup = 'actigraphy_v3')
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#'
#' @examples
#'
#' # organize actigraphy data
#' org_actigraph <- util_actigraph_org_sourcedata(tsub_str = 'sub_001', ses = 'baseline', base_wd, overwrite = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_actigraph_org_sourcedata <- function(sub_str, ses, dir_name, base_wd, overwrite = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
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
  
  
  # check that session exist and is a string
  dir_arg <- methods::hasArg(dir_name)
  
  if (isTRUE(dir_arg)) {
    if (!is.character(dir_name)) {
      stop("dir_name must be entered as a string")
    } else {
      raw_untouched_path <- file.path(base_wd, 'raw_untouched', dir_name)
    }
  }
  
  if (!file.exists(raw_untouched_path)) {
    stop(paste0("file path: ", raw_untouched_path, " data does not exist"))
  }
  
  # get all files for sub in raw_untouched
  raw_files <- list.files(path = raw_untouched_path, pattern = sub_str)
  
  # new file name
  rename_files <- gsub('_actigraph', paste0('_ses-', ses, '_tracksys-actigraph_motion'), raw_files)
  rename_files <- gsub('.gt3x', '.tsv.gz', rename_files)
  rename_files <- gsub('.agd', '.tsv.gz', rename_files)
  
  #### Save in sourcedata #####
  # set paths for other directories
  source_wd <- file.path(base_wd, 'bids', 'sourcedata', sub_str, paste0('ses-', ses), 'beh')
  
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, paste0('ses-', ses), 'motion')
  
  #make directory if needed
  if (!dir.exists(source_wd)) {
    dir.create(source_wd, recursive = TRUE)
  } 
  
  #make directory if needed
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  } 
  
  sub_num <- as.numeric(substr(sub_str, unlist(gregexpr('-', sub_str))+1, nchar(sub_str)))
  # convert to open format for rawdata
  raw_file_gt3x <- raw_files[grepl('gt3x', raw_files)]
  
  print(paste0('-- reading ', raw_file_gt3x))
  raw_data_gt3x <- as.data.frame(read.gt3x::read.gt3x(file.path(raw_untouched_path, raw_file_gt3x), asDataFrame = TRUE))
  
  raw_data_gt3x['time'] <- format(raw_data_gt3x[['time']], format = "%H:%M:%S")
  raw_data_gt3x['sub'] <- sub_num
  
  raw_data_gt3x <- raw_data_gt3x[c('sub', 'time', 'X', 'Y', 'Z')]
  
  
  raw_file_agd <- raw_files[grepl('agd', raw_files)]
  
  print(paste0('-- reading ', raw_file_agd))
  
  # SQL driver connection
  con_agd <- RSQLite::dbConnect(SQLite(), dbname = file.path(raw_untouched_path, raw_file_agd))
  
  # get epoch vector counts
  agd_epoch_data <- dbGetQuery(con_agd, "SELECT * FROM data;")
  RSQLite::dbDisconnect(con_agd)
  
  # convert to unit seconds
  unix_seconds <- (agd_epoch_data[['dataTimestamp']] / 1e7) - 62135596800
  datetime_agd <- as.POSIXct(unix_seconds, origin = "1970-01-01", tz = "UTC")
  
  agd_epoch_data['time'] <- format(datetime_agd, format = "%H:%M:%S")
  agd_epoch_data['sub'] <- sub_num
  
  agd_epoch_data <- agd_epoch_data[c('sub', 'time', 'axis1', 'axis2', 'axis3', 'steps', 'lux', 'inclineOff', 'inclineSitting', 'inclineLying')]
  
  # open filenames
  open_file_gt3x <- rename_files[grepl('motion.tsv.gz', rename_files)]
  open_file_agd <- rename_files[grepl('10sec.tsv.gz', rename_files)]
  
  # copy files
  if (!file.exists(file.path(raw_wd, rename_files[1])) | isTRUE(overwrite)) {  
    file.copy(from = file.path(raw_untouched_path, raw_files), to = file.path(source_wd, rename_files), overwrite = overwrite)
    
    
    print(paste0('-- writing ', open_file_gt3x))
    readr::write_tsv(raw_data_gt3x, file = file.path(raw_wd, open_file_gt3x))
    
    print(paste0('-- writing ', open_file_agd))
    readr::write_tsv(agd_epoch_data, file = file.path(raw_wd, open_file_agd))
    
    #return message
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
    
  } else {
    return('exists')
  }
  
}
