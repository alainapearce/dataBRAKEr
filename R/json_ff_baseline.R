#' json_ff_baseline: Generates a json file for baseline Freddy Fullness ratings from double entry REDCap
#'
#' This function generates a json file for baseline Freddy Fullness ratings
#'
#' @return A string with data stored in JSON format containing meta-data for baseline Freddy Fullness ratings
#'
#'
#' @export

json_ff_baseline <- function() {

  ff_baseline_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Freddy Fullness Scale',
      Reference = 'Keller KL, Assur SA, Torres M, Lofink HE, Thornton JC, Faith MS, Kissileff HR. Potential of an analog scaling device for measuring fullness in children: development and preliminary testing. Appetite. 2006 Sep;47(2):233-43. doi: 10.1016/j.appet.2006.04.004. Epub 2006 Jul 7. PMID: 16828929.',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/16828929/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    v1_date = list( Description = 'Date of visit 1',
                       Unit = 'YYYY-MM-DD'),
    v2_date = list( Description = 'Date of visit 2',
                       Unit = 'YYYY-MM-DD'),
    fullness_preshape = list( Description = 'Pre-Shape Game fullness rating on a 150 mm visual analgue scale',
                              Units = 'mm'),
    preshape_hungry = list( Description = 'Was the child hungry after the first snack prior to the start of the Shape game? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                                      Levels = list ('0' = 'No',
                                                     '1' = 'Yes')),
    fullness_preshape_postsnack = list( Description = 'Pre-Shape Game fullness rating on a 150 mm visual analgue scale after the first snack',
                              Units = 'mm'),
    preshape_hungry_postsnack = list( Description = 'Was the child hungry after the second snack prior to the start of the Shape game? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                                      Levels = list ('0' = 'No',
                                                     '1' = 'Yes')),
    fullness_preshape_postsnack2 = list( Description = 'Pre-Shape Game fullness rating on a 150 mm visual analgue scale after the second snack',
                                        Units = 'mm'),
    fullness_prefnirs = list( Description = 'Pre-fNIRS fullness rating on a 150 mm visual analgue scale',
                              Units = 'mm'),
    prefnirs_hungry = list( Description = 'Was the child hungry after the first snack prior to the start of fNIRS? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                            Levels = list ('0' = 'No',
                                           '1' = 'Yes')),
    fullness_prefnirs_postsnack = list( Description = 'Pre-Shape Game fullness rating on a 150 mm visual analgue scale after the first snack',
                                        Units = 'mm'),
    prefnirs_hungry_postsnack = list( Description = 'Was the child hungry after the second snack prior to the start of fNIRS? Indicated by the child marking their fullness on Freddy Fullnuss as < 25% of scale (<37.5 mm) based on visual inspection',
                                      Levels = list ('0' = 'No',
                                                     '1' = 'Yes')),
    fullness_prefnirs_postsnack2 = list( Description = 'Pre-fNIRS fullness rating on a 150 mm visual analgue scale after the second snack',
                                         Units = 'mm'),
    fullness_preliking_meal = list( Description = 'Pre-liking ratings prior to the meal fullness rating on a 150 mm visual analgue scale after the second snack',
                                         Units = 'mm'),
    fullness_premeal = list( Description = 'Pre-meal fullness rating on a 150 mm visual analgue scale after the second snack',
                                    Units = 'mm'),
    fullness_postmeal = list( Description = 'Post-meal fullness rating on a 150 mm visual analgue scale after the second snack',
                             Units = 'mm'),
    fullness_preliking_eah = list( Description = 'Pre-liking ratings prior to the EAH protocol fullness rating on a 150 mm visual analgue scale after the second snack',
                              Units = 'mm'),
    fullness_pre_eah = list( Description = 'Pre-EAH fullness rating on a 150 mm visual analgue scale after the second snack',
                             Units = 'mm'),
    fullness_post_eah = list( Description = 'Post-EAH fullness rating on a 150 mm visual analgue scale after the second snack',
                             Units = 'mm'),
    fullness_premeal_v2 = list( Description = 'Pre-meal fullness rating on a 150 mm visual analgue scale after the second snack',
                             Units = 'mm'),
    fullness_postmeal_v2 = list( Description = 'Post-meal fullness rating on a 150 mm visual analgue scale after the second snack',
                              Units = 'mm')
  )

  # convert formatting to JSON
  ff_baseline_json <- RJSONIO::toJSON(ff_baseline_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(ff_baseline_json, asText = TRUE))){
    print('Double-entered Taste-Test samples JSON file may be invalid')
  }

  return(ff_baseline_json)

}
