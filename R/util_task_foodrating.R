#' util_task_foodrating: Process raw data from the Food Rating Task
#'
#' This function 1) copies data from raw_untouched and saves it in sourcedata and then 2) cleans data to save in BIDS format in rawdata
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#'
#' @param sub participant number
#' @param data_path data_path absolute path to untouched raw task data file
#' @param overwrite logical indicating if data should be overwritten. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' foodrating_task_pardat <- util_task_foodrating(data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_foodrating <- function(sub = sub, data_path = data_path, overwrite = FALSE, return_data = FALSE) {

  #### 1. Set up/initial checks #####
  data_path = '/Users/azp271/OneDrive - The Pennsylvania State University/StudyBRAKE/data/raw_untouched/foodrating_game/'
  sub = 2
  
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
    print('The foodrating_task.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Alaina at azp271@psu.edu if there are errors')
  }

  # find location of slashes so can decompose filepaths
  slash_loc <- unlist(gregexpr(slash, data_path))

  # get sub string
  sub_str <- paste0('sub-', sprintf('%03d', sub))

  # set paths for other directories
  ## check if have slash at end of data_path and adjust
  if (nchar(data_path) == tail(slash_loc, 1)){
    base_wd <- substr(data_path, 1, tail(slash_loc, 3)[1])
  } else {
    base_wd <- substr(data_path, 1, tail(slash_loc, 2)[1])
  }

  source_wd <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, 'nirs', slash)
  raw_wd <- paste0(base_wd, slash, 'bids', slash, 'rawdata', slash, sub_str, slash, 'nirs', slash)
  derivatives_wd <- paste0(base_wd, slash, 'bids', slash, 'derivatives', slash, sub_str, slash, 'nirs-toolbox', slash)

  # get all files for sub in raw_untouched
  raw_files <- list.files(substr(data_path, 1, tail(slash_loc, 1)), pattern = sub_str)

  # get data file
  data_file <- paste0(data_path, raw_files[grepl('.csv', raw_files)])

  # filter out csv
  raw_files <- raw_files[!grepl('.csv', raw_files)]
  rename_files <- gsub('_foodrating', '_task-foodrating_events', raw_files)

  #### Read in data ####
  dat <- read.csv(data_file, header = TRUE)

  #### Save in sourcedata #####
  if (!dir.exists(source_wd)) {
    dir.create(source_wd, recursive = TRUE)
    write.csv(dat, paste0(source_wd, sub_str, '_task-foodrating_events.tsv'))

    file.copy(from = paste0(data_path, raw_files), to = paste0(source_wd, rename_files))

  } else {
    if (!file.exists(paste0(source_wd, sub_str, '_task-foodrating_events.tsv'))) {
      write.csv(dat, paste0(source_wd, sub_str, '_task-foodrating_events.tsv'))
      file.copy(from = paste0(data_path, raw_files), to = paste0(source_wd, rename_files))

    } else {
      if (overwrite == FALSE){
        print(paste0(sub_str, '_task-foodrating_events.tsv exists in sourcedata'))

      } else {
        write.csv(dat, paste0(source_wd, sub_str, '_task-foodrating_events.tsv'))
        file.copy(from = paste0(data_path, raw_files), to = paste0(source_wd, rename_files))

      }
    }
  }
  
  #### Process fNIRS Data ####
  bash_code_str <- '"/Users/azp271/OneDrive\ -\ The\ Pennsylvania\ State\ University/StudyBRAKE/data//bids/code/nirs_foodrating_proc.m"'
  
  Sys.setenv(PATH=paste(Sys.getenv("PATH"), "/Applications/MATLAB_R2019b.app/bin", sep=":"))
  
  
  matlab_call <- paste0('matlab -nodesktop -nosplash -nojvm -r "try sub=', sub, '; overwrite="T"; run "',  bash_code_str, '"; catch; end; quit" -> /Users/azp271/OneDrive\\ -\\ The\\ Pennsylvania\\ State\\ University/StudyBRAKE/data/bids/sourcedata/', sub_str, '/nirs/nirs_foodrating_proc-output.txt')
  
  system(matlab_call)

  #### Organize Data #####

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

  
  # get onset values
  # load matlab onset values from .mat of snirf
  if (file.exists(paste0(source_wd, sub_str, '_task-foodrating_nirs.tsv'))) {
    raw_snirf <- read.csv(paste0(source_wd, sub_str, '_task-foodrating_nirs.tsv'), header = TRUE, sep = "\t")
    
    dat[['onset']] <- raw_snirf[['onset']]
    
    onset_var_desc <- 'onset values matched to nirx output'
    
  } else {
    dat[['onset']] <- dat[['food_onset']] - dat[1, 'food_onset', ]
    
    onset_var_desc <- 'onset values alligned to start of task - no nirx output to allign with'
    
  }
  
  # re-order columns
  dat <- dat[c('onset', 'duration', 'sub', 'date', 'exp_name', 'cond', 'exp_cond_num', 'stim_file', 'image_ed', 'fix', 'cond_trial', 'trial_index', 'food_onset', 'slider_onset', 'prompt_onset', 'rating', 'jitter_fix_onset', 'jitter_prompt_onset', 'jitter_fix_offset', 'jitter_prompt_offset', 'psychopy_ver', 'frame_rate')]
  
  #### Document in json ####
  meta_list <- list(
    'Food Rating Task' = list(
      Description = 'The Food Rating Task has participants view and rate food images. The same food images are rated for: health, taste, wanting. All ratings are completed for one condition before moving to the next rating condition with order counterballanced.',
      DatasetType = 'raw'),
    onset = list( Description = paste0(onset_var_desc)),
    duration = list( Description = 'trial duration'),
    sub = list( Description = 'participant number'),
    date = list( Description = 'visit date', Units = 'YYY-MM-DD'),
    exp_name = list( Description = 'name of PsychoPy experiment file'),
    cond = list( Description = 'task condition',
                 Levels = list (health = 'rate health of each food',
                                taste = 'rate taste of each food',
                                want = 'rate how much you want to eat each food')),
    exp_cond_num = list( Description = 'condition order entered into task start-up information. Corresponds to randomization table'),
    stim_file = list( Description = 'stimulus file'),
    image_ed = list( Description = 'energy density of food image'),
    fix = list( Description = 'fixation duration', Units = 'sec'),
    cond_trial = list( Description = 'trial number in condition block'),
    trial_index = list( Description = 'index number from trial setup files'),
    food_onset = list( Description = 'onset time for food stimuli', Units = 'sec'),
    slider_onset = list( Description = 'onset time for rating slider', Units = 'sec'),
    prompt_onset = list( Description = 'onset time for prompt', Units = 'sec'),
    rating = list( Description = 'participant rating',
                   Levels = list ( health = list ( '0' = 'Very Unhealthy',
                                                   '1' = 'Unhealthy',
                                                   '2' = 'Healthy',
                                                   '3' = 'Very Healthy'),
                                   taste = list ( '0' = 'Very Bad',
                                                  '1' = 'Bad',
                                                  '2' = 'Good',
                                                  '3' = 'Very Good'),
                                   want = list ( '0' = 'Not At All',
                                                 '1' = 'A Little',
                                                 '2' = 'A Lot',
                                                 '3' = 'Very Much'))),
    jitter_fix_onset = list( Description = 'onset time for jittered fixation', Units = 'sec'),
    jitter_prompt_onset = list( Description = 'onset time for jittered fixation prompt', Units = 'sec'),
    jitter_fix_offset = list( Description = 'offset time for jittered fixation', Units = 'sec'),
    jitter_prompt_offset = list( Description = 'offset time for jittered fixationprompt', Units = 'sec'),
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
    write.csv(dat, paste0(raw_wd, sub_str, '_task-foodrating_events.tsv'), row.names = FALSE)
    write(meta_json, paste0(raw_wd, sub_str, '_task-foodrating_events.json'))

    file.copy(from = paste0(data_path, raw_files), to = paste0(raw_wd, rename_files))

  } else {
    if (!file.exists(paste0(raw_wd, sub_str, '_task-foodrating_events.tsv'))) {
      write.csv(dat, paste0(raw_wd, sub_str, '_task-foodrating_events.tsv'), row.names = FALSE)
      write(meta_json, paste0(raw_wd, sub_str, '_task-foodrating_events.json'))

      file.copy(from = paste0(data_path, raw_files), to = paste0(raw_wd, rename_files))

    } else {
      if (overwrite == FALSE){
        print(paste0(sub_str, '_task-foodrating_events.tsv exists in rawdata'))

      } else {
        write.csv(dat, paste0(raw_wd, sub_str, '_task-foodrating_events.tsv'), row.names = FALSE)
        write(meta_json, paste0(raw_wd, sub_str, '_task-foodrating_events.json'))

        file.copy(from = paste0(data_path, raw_files), to = paste0(raw_wd, rename_files))
      }
    }
  }

  if (isTRUE(return_data)){
    return(list( foodrating_dat = dat,
                 foodrating_labels = meta_json))
  }
}

