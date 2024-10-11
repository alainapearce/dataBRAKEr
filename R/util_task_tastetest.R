#' util_task_tastetest: Process raw data from the fNIRS Taste-Test Task
#'
#' This function: 1) cleans data to save in BIDS format in rawdata and 2) generates summary data that can be used to generate a database
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @inheritParams util_task_org_sourcedata
#' @param return logical indicating if computed summary data should be returned. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' tastetest_task_pardat <- util_task_tastetest(sub_str, ses, data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_tastetest <- function(sub_str, ses, base_wd, overwrite = FALSE, return_data = FALSE) {
  
  #### 1. Set up/initial checks #####
  
  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(base_wd)
  
  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("base_wd must be entered as a string")
  }
  
  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The tastetest_task.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }
  
  # get directory paths
  
  # get version
  if (grepl('post', sub_str)){
    desc_str <- 'post'
  } else {
    desc_str <- 'pre'
  }
  
  sub_str <- gsub(paste0('-', desc_str, '-meal'), '', sub_str)
  
  raw_wd <- paste0(base_wd, slash, 'bids', slash, 'rawdata', slash, sub_str, slash, 'ses-', ses, slash, 'nirs', slash)
  data_file <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, 'ses-', ses, slash, 'nirs', slash, 'tastetest-', desc_str, 'meal', slash, sub_str, '_ses-', ses, '_task-tastetest_desc-', desc_str, 'meal_events.tsv')
  
  #### Organize Data #####
  dat <- read.csv(data_file, sep = '\t', header = TRUE, na.strings = c('n/a', 'NA'))
  
  # remove practice
  dat <- dat[!is.na(dat[['fix']]), ]
  
  # reduce columns
  dat <- dat[c('participant', 'date', 'expName', 'condFile', 'order', 'stimFile', 'fix', 'trials.thisN', 'trials.thisIndex', 'food_img.started', 'rating_slider.started', 'q_prompt.started', 'rating', 'fixationjitter.started', 'promptjitter.started', 'fixationjitter.stopped', 'promptjitter.stopped', 'psychopyVersion', 'frameRate')]
  
  # update names
  names(dat) <- c('sub', 'date', 'exp_name', 'cond', 'exp_cond_num', 'stim_file', 'fix', 'cond_trial', 'trial_index', 'food_onset', 'slider_onset', 'prompt_onset', 'rating', 'jitter_fix_onset', 'jitter_prompt_onset', 'jitter_fix_offset', 'jitter_prompt_offset', 'psychopy_ver', 'frame_rate')
  
  # add duration
  dat[['duration']] <- 2
  
  # clean up sub values
  dat[['sub']] <- sapply(dat[['sub']], function(x) substr(x, tail(unlist(gregexpr('0', x)), 1)+1, nchar(x)))
  dat[['sub']] <- as.numeric(dat[['sub']])
  
  # clean up condition values
  dat[['cond']] <- ifelse(dat[['cond']] == 'health_stim.csv', 'health', ifelse(dat[['cond']] == 'taste_stim.csv', 'taste', ifelse(dat[['cond']] == 'want_stim.csv', 'want', as.character(dat[['cond']]))))
  
  # clean up date
  dat[['date']] <- lubridate::date(dat[['date']])
  
  # organize trials by image ED
  low_ed <- c("images/apple.jpeg", "images/banana.jpeg", "images/blueberries.jpeg", "images/broccoli.jpeg", "images/cantelope.jpeg", "images/carrots.jpeg", "images/corn.jpeg", "images/cucumber.jpeg", "images/delimeat.jpeg", "images/fruitcocktail.jpeg", "images/grapes.jpeg", "images/greenbeans.jpeg", "images/jello.jpeg", "images/lettuce.jpeg", "images/orange.jpeg", "images/peas.jpeg", "images/pineapple.jpeg", "images/popcicle.jpeg", "images/potato.jpeg", "images/redpepper.jpeg", "images/strawberries.jpeg", "images/tomato.jpeg", "images/turkey.jpeg", "images/watermellon.jpeg")
  
  high_ed <- c("images/bacon.jpeg", "images/bagel.jpeg", "images/blueberrymuffin.jpeg", "images/cake.jpeg", "images/cheeseburger.jpeg", "images/chickennugs.jpeg", "images/chips.jpeg", "images/chocolate_candy.jpeg", "images/chocolate_pie.jpeg", "images/chocolatecake.jpeg", "images/cinnamonroll.jpeg", "images/cookie.jpeg", "images/donut.jpeg", "images/fries.jpeg", "images/grilledcheese.jpeg", "images/macandcheese.jpeg", "images/nachos.jpeg", "images/oreos.jpeg", "images/peanutbutter_candy.jpeg", "images/pizza.jpeg", "images/pretzle.jpeg", "images/ricecrispy.jpeg", "images/ritzcracker.jpeg", "images/starbursts.jpeg", "images/waffles.jpeg", "images/sub.jpeg")
  
  # add figure ED information
  dat[['image_ed']] <- ifelse(dat[['stim_file']] %in% high_ed, 'high_ed', 'low_ed')
  
  # add onset column - will be completed using Matlab during fNIRS processing
  dat[['onset']] <- NA
  
  # re-order columns
  dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'cond', 'exp_cond_num', 'stim_file', 'image_ed', 'fix', 'cond_trial', 'trial_index', 'food_onset', 'slider_onset', 'prompt_onset', 'rating', 'jitter_fix_onset', 'jitter_prompt_onset', 'jitter_fix_offset', 'jitter_prompt_offset', 'psychopy_ver', 'frame_rate')]
  
  
  #### Save in rawdata #####
  
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }
  
  if (!file.exists(paste0(raw_wd, sub_str, '_task-tastetest_events.tsv')) | isTRUE(overwrite)) {
    write.table(dat, paste0(raw_wd, sub_str, '_ses-', ses, '_task-tastetest_events.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'NaN')
    
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
}

