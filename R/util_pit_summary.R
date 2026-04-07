#' util_pit_summary: Get summary data from individual participants for the Pavlovian Instramental Transfer task
#'
#' This function calculates summary performance data for an individual participant
#'
#'
#' @param ind_dat Processed individual dataset from rawdata for the Pavlovian Instramental Transfer task
#'
#' @return a data.frame with 3 rows including summary performance and task metrics by response type for a participant
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' pit_summary_beh <- util_pit_summary(ind_dat)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_pit_summary <- function(ind_dat) {

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(ind_dat)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(ind_dat)) {
      stop('ind_dat must be entered as a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('ind_dat must be entered as a data.frame')
  }

  #### Instrumental conditioning summary stats function ####

  inst_sum_stats <- function(inst_dat){

    ## Instrumental Conditioning ####

    inst_dat <- inst_dat[!is.na(inst_dat[['inst_trial_onset']]), ]

    # get cond_info
    inst_dat['inst_food_trial'] <- ifelse(inst_dat[['cond']] < 4, inst_dat[['inst_right_trial']], inst_dat[['inst_left_trial']])

    inst_dat['inst_toy_trial'] <- ifelse(inst_dat[['cond']] < 4, inst_dat[['inst_left_trial']], inst_dat[['inst_right_trial']])

    # get indicator for item earned
    inst_dat['inst_addfood'] <- sapply(seq(1, nrow(inst_dat)), function(x) ifelse(x == 1, inst_dat[1, 'inst_food_earned'], ifelse(inst_dat[x, 'inst_food_earned'] == inst_dat[(x-1), 'inst_food_earned'], 0, 1)))

    inst_dat['inst_addtoy'] <- sapply(seq(1, nrow(inst_dat)), function(x) ifelse(x == 1, inst_dat[1, 'inst_toy_earned'], ifelse(inst_dat[x, 'inst_toy_earned'] == inst_dat[(x-1), 'inst_toy_earned'], 0, 1)))

    # trial duration
    inst_dat['trial_dur'] <- 1000*(inst_dat[['inst_outcome_img_onset']] - inst_dat[['inst_trial_onset']])

    # number of trials to hit criteria
    inst_ntrials <- nrow(inst_dat)

    inst_ntrials_food30 <- nrow(inst_dat[inst_dat[['inst_food_earned']] < 30, ]) + 1
    inst_ntrials_toy30 <- nrow(inst_dat[inst_dat[['inst_toy_earned']] < 30, ]) + 1

    # responses per outcome earned
    foodkey_foodout_dat <- inst_dat[inst_dat['inst_food_trial'] != 0 & inst_dat['inst_addfood'] == 1, ]
    toykey_foodout_dat <- inst_dat[inst_dat['inst_toy_trial'] != 0 & inst_dat['inst_addfood'] == 1, ]
    foodkey_toyout_dat <- inst_dat[inst_dat['inst_food_trial'] != 0 & inst_dat['inst_addtoy'] == 1, ]
    toykey_toyout_dat <- inst_dat[inst_dat['inst_toy_trial'] != 0 & inst_dat['inst_addtoy'] == 1, ]

    inst_foodkey_nper_foodout <- ifelse(nrow(foodkey_foodout_dat) > 0, mean(foodkey_foodout_dat[['inst_food_trial']]), 0)

    inst_foodkey_nper_toyout <- ifelse(nrow(foodkey_toyout_dat) > 0, mean(foodkey_toyout_dat[['inst_food_trial']]), 0)

    inst_toykey_nper_foodout <- ifelse(nrow(toykey_foodout_dat) > 0, mean(toykey_foodout_dat[['inst_toy_trial']]), 0)

    inst_toykey_nper_toyout <- ifelse(nrow(toykey_toyout_dat) > 0, mean(toykey_toyout_dat[['inst_toy_trial']]), 0)

    # time to earn outcome
    inst_mean_trial_dur <- mean(inst_dat[['trial_dur']])

    # response rt
    inst_foodkey_mean_rt <- mean(inst_dat[inst_dat['inst_food_trial'] != 0, 'trial_dur'] / inst_dat[inst_dat['inst_food_trial'] != 0, 'inst_food_trial'])

    inst_toykey_mean_rt <- mean(inst_dat[inst_dat['inst_toy_trial'] != 0, 'trial_dur'] / inst_dat[inst_dat['inst_toy_trial'] != 0, 'inst_toy_trial'])

    # responses per minute
    inst_mean_food_rpm <- (1/(inst_foodkey_mean_rt/1000))*60
    inst_mean_toy_rpm <- (1/(inst_toykey_mean_rt/1000))*60

    ## return ####
    inst_sum_dat <- data.frame(participant_id = inst_dat[1, 'sub'],
                               session_id = inst_dat[1, 'ses'],
                               cond = inst_dat[1, 'cond'],
                               inst_ntrials = inst_ntrials,
                               inst_ntrials_food30 = inst_ntrials_food30,
                               inst_ntrials_toy30 = inst_ntrials_toy30,
                               inst_foodkey_nper_foodout = inst_foodkey_nper_foodout,
                               inst_toykey_nper_foodout = inst_toykey_nper_foodout,
                               inst_foodkey_nper_toyout = inst_foodkey_nper_toyout,
                               inst_toykey_nper_toyout = inst_toykey_nper_toyout,
                               inst_mean_trial_dur = inst_mean_trial_dur,
                               inst_foodkey_mean_rt = inst_foodkey_mean_rt,
                               inst_toykey_mean_rt = inst_toykey_mean_rt,
                               inst_mean_food_rpm = inst_mean_food_rpm,
                               inst_mean_toy_rpm = inst_mean_toy_rpm)
    return(inst_sum_dat)

  }

  ## PIT response type stats function ####
  pit_samedif_stats <- function(pit_sd_dat, resp_type){

    if (resp_type == 'control') {

      pit_pre_mean_rpm <- mean(pit_sd_dat[['pit_pre_total_rpm']], na.rm = TRUE)
      pit_cs_mean_rpm <- mean(pit_sd_dat[['pit_cs_total_rpm']], na.rm = TRUE)
      pit_iti_mean_rpm <- mean(pit_sd_dat[['pit_iti_total_rpm']], na.rm = TRUE)

      pit_pre_food_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'neutral', 'pit_pre_food_rpm'], na.rm = TRUE)
      pit_cs_food_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'neutral', 'pit_cs_food_rpm'], na.rm = TRUE)
      pit_iti_food_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'neutral', 'pit_iti_food_rpm'], na.rm = TRUE)

      pit_pre_toy_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'neutral', 'pit_pre_toy_rpm'], na.rm = TRUE)
      pit_cs_toy_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'neutral', 'pit_cs_toy_rpm'], na.rm = TRUE)
      pit_iti_toy_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'neutral', 'pit_iti_toy_rpm'], na.rm = TRUE)

    } else {
      if (resp_type == 'same') {

        pit_sd_dat['pit_pre_rpm'] <- ifelse(pit_sd_dat[['cs_cond']] == 'food',  pit_sd_dat[['pit_pre_food_rpm']], ifelse(pit_sd_dat[['cs_cond']] == 'toy',  pit_sd_dat[['pit_pre_toy_rpm']], NA))

        pit_sd_dat['pit_cs_rpm'] <- ifelse(pit_sd_dat[['cs_cond']] == 'food',  pit_sd_dat[['pit_cs_food_rpm']], ifelse(pit_sd_dat[['cs_cond']] == 'toy',  pit_sd_dat[['pit_cs_toy_rpm']], NA))

        pit_sd_dat['pit_iti_rpm'] <- ifelse(pit_sd_dat[['cs_cond']] == 'food',  pit_sd_dat[['pit_iti_food_rpm']], ifelse(pit_sd_dat[['cs_cond']] == 'toy',  pit_sd_dat[['pit_iti_toy_rpm']], NA))

      } else if (resp_type == 'dif') {
        pit_sd_dat['pit_pre_rpm'] <- ifelse(pit_sd_dat[['cs_cond']] == 'toy',  pit_sd_dat[['pit_pre_food_rpm']], ifelse(pit_sd_dat[['cs_cond']] == 'food',  pit_sd_dat[['pit_pre_toy_rpm']], NA))

        pit_sd_dat['pit_cs_rpm'] <- ifelse(pit_sd_dat[['cs_cond']] == 'toy',  pit_sd_dat[['pit_cs_food_rpm']], ifelse(pit_sd_dat[['cs_cond']] == 'food',  pit_sd_dat[['pit_cs_toy_rpm']], NA))

        pit_sd_dat['pit_iti_rpm'] <- ifelse(pit_sd_dat[['cs_cond']] == 'toy',  pit_sd_dat[['pit_iti_food_rpm']], ifelse(pit_sd_dat[['cs_cond']] == 'food',  pit_sd_dat[['pit_iti_toy_rpm']], NA))

      }

      pit_pre_mean_rpm <- mean(pit_sd_dat[['pit_pre_rpm']], na.rm = TRUE)
      pit_cs_mean_rpm <- mean(pit_sd_dat[['pit_cs_rpm']], na.rm = TRUE)
      pit_iti_mean_rpm <- mean(pit_sd_dat[['pit_iti_rpm']], na.rm = TRUE)

      pit_pre_food_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'food', 'pit_pre_rpm'], na.rm = TRUE)
      pit_cs_food_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'food', 'pit_cs_rpm'], na.rm = TRUE)
      pit_iti_food_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'food', 'pit_iti_rpm'], na.rm = TRUE)

      pit_pre_toy_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'toy', 'pit_pre_rpm'], na.rm = TRUE)
      pit_cs_toy_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'toy', 'pit_cs_rpm'], na.rm = TRUE)
      pit_iti_toy_mean_rpm <- mean(pit_sd_dat[pit_sd_dat['cs_cond'] == 'toy', 'pit_iti_rpm'], na.rm = TRUE)
    }

    same_dif_dat <- data.frame(resp_type, pit_pre_mean_rpm, pit_cs_mean_rpm, pit_iti_mean_rpm, pit_pre_food_mean_rpm, pit_cs_food_mean_rpm, pit_iti_food_mean_rpm, pit_pre_toy_mean_rpm, pit_cs_toy_mean_rpm, pit_iti_toy_mean_rpm)

    return(same_dif_dat)
  }


  ## PIT summary stats function ####
  pit_sum_stats <- function(pit_dat){

    ## PIT Test ####
    pit_dat <- pit_dat[!is.na(pit_dat[['pit_fix_onset']]), ]

    # get data by food/toy conditions
    pit_dat['cs_cond'] <- ifelse(pit_dat[['cond']] == 1 | pit_dat[['cond']] == 4, ifelse(grepl('1', pit_dat[['cs_img']]), 'food', ifelse(grepl('2', pit_dat[['cs_img']]), 'toy', 'neutral')), ifelse(pit_dat[['cond']] == 2 | pit_dat[['cond']] == 5, ifelse(grepl('2', pit_dat[['cs_img']]), 'food', ifelse(grepl('3', pit_dat[['cs_img']]), 'toy', 'neutral')), ifelse(grepl('3', pit_dat[['cs_img']]), 'food', ifelse(grepl('1', pit_dat[['cs_img']]), 'toy', 'neutral'))))

    pit_dat['pit_pre_food_n'] <- ifelse(pit_dat[['cond']] < 4, pit_dat[['pit_pre_right_n']], pit_dat[['pit_pre_left_n']])
    pit_dat['pit_pre_toy_n'] <- ifelse(pit_dat[['cond']] < 4, pit_dat[['pit_pre_left_n']], pit_dat[['pit_pre_right_n']])
    pit_dat['pit_pre_total_n'] <- pit_dat[['pit_pre_left_n']] + pit_dat[['pit_pre_right_n']]

    pit_dat['pit_cs_food_n'] <- ifelse(pit_dat[['cond']] < 4, pit_dat[['pit_cs_right_n']], pit_dat[['pit_cs_left_n']])
    pit_dat['pit_cs_toy_n'] <- ifelse(pit_dat[['cond']] < 4, pit_dat[['pit_cs_left_n']], pit_dat[['pit_cs_right_n']])
    pit_dat['pit_cs_total_n'] <- pit_dat[['pit_cs_left_n']] + pit_dat[['pit_cs_right_n']]


    pit_dat['pit_iti_food_n'] <- ifelse(pit_dat[['cond']] < 4, pit_dat[['pit_iti_right_n']], pit_dat[['pit_iti_left_n']])
    pit_dat['pit_iti_toy_n'] <- ifelse(pit_dat[['cond']] < 4, pit_dat[['pit_iti_left_n']], pit_dat[['pit_iti_right_n']])
    pit_dat['pit_iti_total_n'] <- pit_dat[['pit_iti_left_n']] + pit_dat[['pit_iti_right_n']]


    # get reaction time
    pit_dat['pit_pre_food_mean_rt'] <- ifelse(pit_dat[['pit_pre_food_n']] == 0, NA, (2000 / pit_dat[['pit_pre_food_n']]))
    pit_dat['pit_pre_toy_mean_rt'] <- ifelse(pit_dat[['pit_pre_toy_n']] == 0, NA, (2000 / pit_dat[['pit_pre_toy_n']]))
    pit_dat['pit_pre_total_mean_rt'] <- ifelse(pit_dat[['pit_pre_total_n']] == 0, NA, (2000 / pit_dat[['pit_pre_total_n']]))

    pit_dat['pit_cs_food_mean_rt'] <- ifelse(pit_dat[['pit_cs_food_n']] == 0, NA, (4000 / pit_dat[['pit_cs_food_n']]))
    pit_dat['pit_cs_toy_mean_rt'] <- ifelse(pit_dat[['pit_cs_toy_n']] == 0, NA, (4000 / pit_dat[['pit_cs_toy_n']]))
    pit_dat['pit_cs_total_mean_rt'] <- ifelse(pit_dat[['pit_cs_total_n']] == 0, NA, (2000 / pit_dat[['pit_cs_total_n']]))

    pit_dat['pit_iti_food_mean_rt'] <- ifelse(pit_dat[['pit_iti_food_n']] == 0, NA, (1000 / pit_dat[['pit_iti_food_n']]))
    pit_dat['pit_iti_toy_mean_rt'] <- ifelse(pit_dat[['pit_iti_toy_n']] == 0, NA, (1000 / pit_dat[['pit_iti_toy_n']]))
    pit_dat['pit_iti_total_mean_rt'] <- ifelse(pit_dat[['pit_iti_total_n']] == 0, NA, (2000 / pit_dat[['pit_iti_total_n']]))

    # get responses per minute
    pit_dat['pit_pre_food_rpm'] <- ifelse(is.na(pit_dat[['pit_pre_food_mean_rt']]), 0, (1/(pit_dat[['pit_pre_food_mean_rt']]/1000))*60)
    pit_dat['pit_pre_toy_rpm'] <- ifelse(is.na(pit_dat[['pit_pre_toy_mean_rt']]), 0, (1/(pit_dat[['pit_pre_toy_mean_rt']]/1000))*60)
    pit_dat['pit_pre_total_rpm'] <- ifelse(is.na(pit_dat[['pit_pre_total_mean_rt']]), 0, (1/(pit_dat[['pit_pre_total_mean_rt']]/1000))*60)

    pit_dat['pit_cs_food_rpm'] <- ifelse(is.na(pit_dat[['pit_cs_food_mean_rt']]), 0, (1/(pit_dat[['pit_cs_food_mean_rt']]/1000))*60)
    pit_dat['pit_cs_toy_rpm'] <- ifelse(is.na(pit_dat[['pit_cs_toy_mean_rt']]), 0, (1/(pit_dat[['pit_cs_toy_mean_rt']]/1000))*60)
    pit_dat['pit_cs_total_rpm'] <- ifelse(is.na(pit_dat[['pit_cs_total_mean_rt']]), 0, (1/(pit_dat[['pit_cs_total_mean_rt']]/1000))*60)

    pit_dat['pit_iti_food_rpm'] <- ifelse(is.na(pit_dat[['pit_iti_food_mean_rt']]), 0, (1/(pit_dat[['pit_iti_food_mean_rt']]/1000))*60)
    pit_dat['pit_iti_toy_rpm'] <- ifelse(is.na(pit_dat[['pit_iti_toy_mean_rt']]), 0, (1/(pit_dat[['pit_iti_toy_mean_rt']]/1000))*60)
    pit_dat['pit_iti_total_rpm'] <- ifelse(is.na(pit_dat[['pit_iti_total_mean_rt']]), 0, (1/(pit_dat[['pit_iti_total_mean_rt']]/1000))*60)

    # get summary data frame by cond
    pit_sum_dat <- as.data.frame(t(sapply(c('same', 'dif', 'control'), function(x) pit_samedif_stats(pit_dat, x), USE.NAMES = FALSE)))
    rownames(pit_sum_dat) <- NULL

    pit_sum_dat[!grepl('resp_type', names(pit_sum_dat))] <- sapply(names(pit_sum_dat)[!grepl('resp_type', names(pit_sum_dat))], function(x) as.numeric(pit_sum_dat[[x]]))

    # PIT score
    pit_sum_dat['pit_score'] <- pit_sum_dat['pit_cs_mean_rpm'] - pit_sum_dat['pit_pre_mean_rpm']
    pit_sum_dat['pit_food_score'] <- pit_sum_dat['pit_cs_food_mean_rpm'] - pit_sum_dat['pit_pre_food_mean_rpm']
    pit_sum_dat['pit_toy_score'] <- pit_sum_dat['pit_cs_toy_mean_rpm'] - pit_sum_dat['pit_pre_toy_mean_rpm']

    # general information
    pit_sum_dat['participant_id'] <- pit_dat[1, 'sub']
    pit_sum_dat['session_id'] <- pit_dat[1, 'ses']
    pit_sum_dat['cond'] <- pit_dat[1, 'cond']

    # reorder
    pit_sum_dat <- pit_sum_dat[c('participant_id', 'session_id', 'cond', names(pit_sum_dat)[!grepl('id|cond', names(pit_sum_dat))])]

    ## return ####

    return(pit_sum_dat)
  }



  # Summarize data ----
  inst_sum_dat <- inst_sum_stats(inst_dat <- ind_dat)
  pit_sum_dat <- pit_sum_stats(ind_dat)

  sum_dat <- merge(inst_sum_dat, pit_sum_dat[!grepl('cond|session', names(pit_sum_dat))], by = 'participant_id', all = TRUE)


  return(sum_dat)
}

