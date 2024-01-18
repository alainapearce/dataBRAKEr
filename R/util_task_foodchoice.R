#' util_task_foodchoice: Process raw data from the Food Choice Task
#'
#' This function 1) copies data from raw_untouched and saves it in sourcedata and then 2) cleans data to save in BIDS format in rawdata
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @inheritParams util_task_foodrating
#' @inheritParams util_task_foodrating
#' @inheritParams util_task_foodrating
#' @inheritParams util_task_foodrating
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' foodchoice_task_pardat <- util_task_foodchoice(data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_foodchoice <- function(sub = sub, data_path = data_path, overwrite = FALSE, return_data = FALSE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data_path)

  if (isTRUE(data_arg)) {
    if (!is.character(data_path)) {
      stop("data_path must be entered as a string")
    } else if (!file.exists(data_path)) {
      stop("data_path entered, but file does not exist. Check data_path string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("data_path must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The foodchoice_task.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }

  # find location of slashes so can decompose filepaths
  slash_loc <- unlist(gregexpr('/', data_path))

  # get sub string
  sub_str <- paste0('sub-', sprintf('%03d', sub))

  # set paths for other directories
  ## check if have slash at end of data_path and adjust
  if (nchar(data_path) == tail(slash_loc, 1)){
    base_wd <- substr(data_path, 1, tail(slash_loc, 3)[1])
  } else {
    base_wd <- substr(data_path, 1, tail(slash_loc, 2)[1])
  }

  source_wd <- paste0(base_wd, '/bids/sourcedata/', sub_str, '/nirs/')
  raw_wd <- paste0(base_wd, '/bids/rawdata/', sub_str, '/nirs/')
  derivatives_wd <- paste0(base_wd, '/bids/derivatives/', sub_str, '/nirs-toolbox/')

  # get all files for sub in raw_untouched
  raw_files <- list.files(substr(data_path, 1, tail(slash_loc, 1)), pattern = sub_str)

  # get data file
  data_file <- paste0(data_path, raw_files[grepl('.csv', raw_files)])

  # filter out csv
  raw_files <- raw_files[!grepl('.csv', raw_files)]
  rename_files <- gsub('_foodchoice', '_task-foodchoice_events', raw_files)

  #### Read in data ####
  dat <- read.csv(data_file, header = TRUE)

  #### Save in sourcedata #####
  if (!dir.exists(source_wd)) {
    dir.create(source_wd, recursive = TRUE)
    write.csv(dat, paste0(source_wd, sub_str, '_task-foodchoice_events.tsv'))
    file.copy(from = paste0(data_path, raw_files), to = paste0(source_wd, rename_files))

  } else {
    if (!file.exists(paste0(source_wd, sub_str, '_task-foodchoice_events.tsv'))) {
      write.csv(dat, paste0(source_wd, sub_str, '_task-foodchoice_events.tsv'))
      file.copy(from = paste0(data_path, raw_files), to = paste0(source_wd, rename_files))

    } else {
      if (overwrite == FALSE){
        print(paste0(sub_str, '_task-foodchoice_events.tsv exists in sourcedata'))

      } else {
        write.csv(dat, paste0(source_wd, sub_str, '_task-foodchoice_events.tsv'))
        file.copy(from = paste0(data_path, raw_files), to = paste0(source_wd, rename_files))

      }
    }
  }

  #### Organize Data #####

  # get healthy eating rt
  healthy_rt <- dat[!is.na(dat[['healthyeating_key.rt']]), 'healthyeating_key.rt']

  # remove practice
  dat <- dat[!is.na(dat[['fix']]), ]

  # reduce columns
  dat <- dat[c('participant', 'date', 'expName', 'condition', 'img1', 'img2', 'fix', 'img1_health', 'img2_health', 'img1_taste', 'img2_taste', 'img1_want', 'img2_want', 'taste_cut', 'health_cut', 'trials.thisN', 'healthyeating_key.rt', 'trial_left_img.started', 'trial_right_img.started', 'choice_prompt.started', 'key_choice.started', 'choice_fix.started', 'left_img.started', 'right_img.started', 'etRecord.started', 'key_choice.keys', 'key_choice.rt', 'left_img.numLooks', 'left_img.timesOn', 'left_img.timesOff', 'right_img.numLooks', 'right_img.timesOn', 'right_img.timesOff', 'trial_fixprompt.started', 'tfix.started', 'trial_fixprompt.stopped', 'tfix.stopped', 'psychopyVersion', 'frameRate')]

  # update names
  names(dat) <- c('sub', 'date', 'exp_name', 'cond', 'img1', 'img2', 'fix', 'img1_health', 'img2_health', 'img1_taste', 'img2_taste', 'img1_want', 'img2_want', 'taste_cut', 'health_cut', 'trial', 'healthyeating_time', 'left_img_onset', 'right_img_onset', 'choice_prompt_onset', 'key_choice_onset', 'choice_fix_onset', 'left_roi_onset', 'right_roi_onset', 'et_record_onset', 'choice', 'choice_rt', 'left_looks', 'left_look_onsets', 'left_look_offsets', 'right_looks', 'right_look_onsets', 'right_look_offsets', 'trial_fixprompt_onset', 'tfix_onset', 'trial_fixprompt_offset', 'tfix_offset', 'psychopy_ver', 'frame_rate')

  # clean up sub values
  dat[['sub']] <- sapply(dat[['sub']], function(x) substr(x, tail(unlist(gregexpr('0', x)), 1)+1, nchar(x)))
  dat[['sub']] <- as.numeric(dat[['sub']])

  # clean up date
  dat[['date']] <- lubridate::date(dat[['date']])

  # add healthy eating rt
  dat[['healthyeating_time']] <- healthy_rt

  # add in choice information
  dat[['choice_healthy']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['img1_health']] == dat[['img2_health']], 1, ifelse(dat[['img1_health']] > dat[['img2_health']], ifelse(dat[['choice']] == 1, 1, 0), ifelse(dat[['choice']] == 1, 0, 1))))

  dat[['choice_tasty']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['img1_taste']] == dat[['img2_taste']], 1, ifelse(dat[['img1_taste']] > dat[['img2_taste']], ifelse(dat[['choice']] == 1, 1, 0), ifelse(dat[['choice']] == 1, 0, 1))))

  dat[['choice_want']] <- ifelse(dat[['choice']] == 'None', NA, ifelse(dat[['img1_want']] == dat[['img2_want']], 1, ifelse(dat[['img1_want']] > dat[['img2_want']], ifelse(dat[['choice']] == 1, 1, 0), ifelse(dat[['choice']] == 1, 0, 1))))

  # re-order
  dat <- dat[c(1:27, 40:42, 28:39)]


  #### Document in json ####
  meta_list <- list(
    'Food Choice Task' = list(
      Description = 'The Food Choice Task has participants choose between foods. Food image pairings are based on participants own health and taste ratings during the Food Rating Task.',
      DatasetType = 'raw'),
    sub = list( Description = 'participant number'),
    date = list( Description = 'visit date', Units = 'YYY-MM-DD'),
    exp_name = list( Description = 'name of PsychoPy experiment file'),
    cond = list( Description = 'task condition',
                 Levels = list (healthy = 'both images rated as healthy but differ in taste',
                                tasty = 'both images rated as tasty but differ in health',
                                nottasty = 'both images rated as not tasty but differ in health',
                                unhealthy = 'both images rated as not healthy but differ in taste',
                                mix_conflict = 'one image rated as healthy/not tasty and the other as unhealthy/tasty',
                                mix_noconflict = 'one image rated as unhealthy/not tasty and the other as healthy/tasty')),
    img1 = list( Description = 'left image file'),
    img2 = list( Description = 'right image file'),
    img1_health = list( Description = 'participant health rating for left image from the Food Rating Task'),
    img2_health = list( Description = 'participant health rating for right image from the Food Rating Task'),
    img1_taste = list( Description = 'participant taste rating for left image from the Food Rating Task'),
    img2_taste = list( Description = 'participant taste rating for right image from the Food Rating Task'),
    img1_want = list( Description = 'participant wanting rating for left image from the Food Rating Task'),
    img2_want = list( Description = 'participant wanting rating for right image from the Food Rating Task'),
    taste_cut = list( Description = 'taste cutoff point to make tasty/not tasty designation - start at 1.5 and adjust if needed to get trial pairings'),
    health_cut = list( Description = 'health cutoff point to make tasty/not tasty designation - start at 1.5 and adjust if needed to get trial pairings'),
    trial = list( Description = 'trial number'),
    healthyeating_time = list( Description = 'time reading the healthy eating infographic'),
    left_img_onset = list( Description = 'onset time for left image', Units = 'sec'),
    right_img_onset = list( Description = 'onset time for right image', Units = 'sec'),
    choice_prompt_onset = list( Description = 'onset time for choice prompt', Units = 'sec'),
    key_choice_onset = list( Description = 'onset time for key choice', Units = 'sec'),
    choice_fix_onset = list( Description = 'onset time for choice fixation', Units = 'sec'),
    left_roi_onset = list( Description = 'onset time for eye-tracking left image region of interest', Units = 'sec'),
    right_roi_onset = list( Description = 'onset time for eye-tracking right image region of interest', Units = 'sec'),
    et_record_onset = list( Description = 'onset time for eye-tracking right image region of interest', Units = 'sec'),
    choice = list( Description = 'participant choice',
                   Levels = list ( '1' = 'left image',
                                   '2' = 'right image',
                                   'None' = 'missing response')),
    choice_rt = list( Description = 'participant choice reaction time', Units = 'sec'),
    choice_health = list( Description = 'indicates if participant chose the food item that they rated as healthier during the Food Rating Game',
                          Levels = list ( '0' = 'chose food rated as less healthy',
                                          '1' = 'chose food rated as healthier',
                                          'NA' = 'missing response')),
    choice_taste = list( Description = 'indicates if participant chose the food item that they rated as tastier during the Food Rating Game',
                         Levels = list ( '0' = 'chose food rated as less tasty',
                                         '1' = 'chose food rated as more tasty',
                                         'NA' = 'missing response')),
    choice_want = list( Description = 'indicates if participant chose the food item that they rated as wanting to eat more the Food Rating Game',
                        Levels = list ( '0' = 'chose food rated as wanting to eat more',
                                        '1' = 'chose food rated as wanting to eat less',
                                        'NA' = 'missing response')),
    left_looks = list( Description = 'number of looks recorded for left image region of interest'),
    left_look_onsets = list( Description = 'onset times for each recorded look for the left image region of interest', Units = 'sec'),
    left_look_offsets = list( Description = 'offset times for each recorded look for the left image region of interest', Units = 'sec'),
    right_looks = list( Description = 'number of looks recorded for right image region of interest'),
    right_look_onsets = list( Description = 'onset times for each recorded look for the right image region of interest', Units = 'sec'),
    right_look_offsets = list( Description = 'offset times for each recorded look for the right image region of interest', Units = 'sec'),
    trial_fixprompt_onset = list( Description = 'onset time for fixation', Units = 'sec'),
    tfix_onset = list( Description = 'onset time for fixation', Units = 'sec'),
    trial_fixprompt_offset = list( Description = 'offset time for fixation', Units = 'sec'),
    tfix_offset = list( Description = 'offset time for fixation', Units = 'sec'),
    psychopy_ver = list( Description = 'version of PsychoPy'),
    frame_rate = list( Description = 'frame rate', Units = 'frames per sec'))

  # convert formatting to JSON
  meta_json <- RJSONIO::toJSON(meta_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(meta_json, asText = TRUE))){
    print(paste0('Food Rating JSON file may be invalid for ', sub_str))
  }

  #### Save in rawdata #####

  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
    write.csv(dat, paste0(raw_wd, sub_str, '_task-foodchoice_events.tsv'), row.names = FALSE)
    write(meta_json, paste0(raw_wd, sub_str, '_task-foodchoice_events.json'))
    file.copy(from = paste0(data_path, raw_files), to = paste0(raw_wd, rename_files))

  } else {
    if (!file.exists(paste0(raw_wd, sub_str, '_task-foodchoice_events.tsv'))) {
      write.csv(dat, paste0(raw_wd, sub_str, '_task-foodchoice_events.tsv'), row.names = FALSE)
      write(meta_json, paste0(raw_wd, sub_str, '_task-foodchoice_events.json'))
      file.copy(from = paste0(data_path, raw_files), to = paste0(raw_wd, rename_files))

    } else {
      if (overwrite == FALSE){
        print(paste0(sub_str, '_task-foodchoice_events.tsv exists in rawdata'))

      } else {
        write.csv(dat, paste0(raw_wd, sub_str, '_task-foodchoice_events.tsv'), row.names = FALSE)
        write(meta_json, paste0(raw_wd, sub_str, '_task-foodchoice_events.json'))
        file.copy(from = paste0(data_path, raw_files), to = paste0(raw_wd, rename_files))

      }
    }
  }

  if (isTRUE(return_data)){
    return(list( foodchoice_dat = dat,
                 foodchoice_labels = meta_json))
  }
}
