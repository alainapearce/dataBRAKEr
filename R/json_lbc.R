#' json_lbc: Generates a json file for the Lifestyle Behavior Checklist
#'
#' This function generates a json file for the scored Lifestyle Behavior Checklist and raw participant responses. This function provides accurate json files ONLY if data is processed using score_lbc function in dataprepr and is only accurate for data collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the Lifestyle Behavior Checklist
#'
#'
#' @export

json_lbc <- function() {
  
  lbc_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Lifestyle Behavior Checklist.',
      Reference = 'West F, Morawska A, Joughin K. The Lifestyle Behaviour Checklist: evaluation of the factor structure. Child: Care, Health and Development. 2010;36(4):508-515. doi:10.1111/j.1365-2214.2010.01074.x', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/20337641/'),
    participant_id = list( Description = 'participant id number'),
    lbc1 = list( Description = 'My child eats too quickly',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc2 = list( Description = 'My child eats too much',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc3 = list( Description = 'My child eats unhealthy snacks',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc4 = list( Description = 'My child whinges or whines about food',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc5 = list( Description = 'My child yells about food',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc6 = list( Description = 'My child throws a tantrum about food',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc7 = list( Description = 'My child refuses to eat certain foods (i.e. fussy eating)',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc8 = list( Description = 'My child argues about food (e.g. when you say "No More")',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc9 = list( Description = 'My child demands extra helpings at meals',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc10 = list( Description = 'My child requests food continuously between meals',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc11 = list( Description = 'My child demands food when shopping or on outings',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc12 = list( Description = 'My child sneaks food when they know they are not supposed to',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc13 = list( Description = 'My child hides food',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc14 = list( Description = 'My child steals food (e.g. from other children\'s lunchboxes)',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc15 = list( Description = 'My child eats food to comfort themselves when feeling let down or depressed',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc16 = list( Description = 'My child watches too much television',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc17 = list( Description = 'My child spends too much time playing video or computer games',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc18 = list( Description = 'My child complains about doing physical activity (e.g. "This is boring, I\'m too tired, My leg hurts")',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc19 = list( Description = 'My child refuses to do physical activity',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc20 = list( Description = 'My child complains about being unfit or feeling low in energy',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc21 = list( Description = 'My child complains about being overweight',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc22 = list( Description = 'My child complains about being teased',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc23 = list( Description = 'My child complains about not having enough friends',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc24 = list( Description = 'My child complains about being unattractive',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc25 = list( Description = 'My child complains about not fitting into clothes',
                 Levels = list ('1' = 'Not at all',
                                '4' = 'Somewhat',
                                '7' = 'Very much')),
    lbc1_conf = list( Description = 'Confidence deal with: My child eats too quickly',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc2_conf = list( Description = 'Confidence deal with: My child eats too much',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc3_conf = list( Description = 'Confidence deal with: My child eats unhealthy snacks',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc4_conf = list( Description = 'Confidence deal with: My child whinges or whines about food',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc5_conf = list( Description = 'Confidence deal with: My child yells about food',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc6_conf = list( Description = 'Confidence deal with: My child throws a tantrum about food',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc7_conf = list( Description = 'Confidence deal with: My child refuses to eat certain foods (i.e. fussy eating)',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc8_conf = list( Description = 'Confidence deal with: My child argues about food (e.g. when you say "No More")',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc9_conf = list( Description = 'Confidence deal with: My child demands extra helpings at meals',
                 Levels = list ('1' = 'Not at all',
                                '5' = 'Somewhat',
                                '10' = 'Very much')),
    lbc10_conf = list( Description = 'Confidence deal with: My child requests food continuously between meals',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc11_conf = list( Description = 'Confidence deal with: My child demands food when shopping or on outings',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc12_conf = list( Description = 'Confidence deal with: My child sneaks food when they know they are not supposed to',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc13_conf = list( Description = 'Confidence deal with: My child hides food',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc14_conf = list( Description = 'Confidence deal with: My child steals food (e.g. from other children\'s lunchboxes)',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc15_conf = list( Description = 'Confidence deal with: My child eats food to comfort themselves when feeling let down or depressed',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc16_conf = list( Description = 'Confidence deal with: My child watches too much television',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc17_conf = list( Description = 'Confidence deal with: My child spends too much time playing video or computer games',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc18_conf = list( Description = 'Confidence deal with: My child complains about doing physical activity (e.g. "This is boring, I\'m too tired, My leg hurts")',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc19_conf = list( Description = 'Confidence deal with: My child refuses to do physical activity',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc20_conf = list( Description = 'Confidence deal with: My child complains about being unfit or feeling low in energy',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc21_conf = list( Description = 'Confidence deal with: My child complains about being overweight',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc22_conf = list( Description = 'Confidence deal with: My child complains about being teased',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc23_conf = list( Description = 'Confidence deal with: My child complains about not having enough friends',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc24_conf = list( Description = 'Confidence deal with: My child complains about being unattractive',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc25_conf = list( Description = 'Confidence deal with: My child complains about not fitting into clothes',
                  Levels = list ('1' = 'Not at all',
                                 '5' = 'Somewhat',
                                 '10' = 'Very much')),
    lbc_misbeh = list( Description = 'Food-related misbehavior.', 
                     Derivative = TRUE),
    lbc_overeat = list( Description = 'Overeating.', 
                       Derivative = TRUE),
    lbc_em_overweight = list( Description = 'Emotion related to being overweight.', 
                       Derivative = TRUE),
    lbc_pa = list( Description = 'Physical activity.', 
                       Derivative = TRUE),
    lbc_total = list( Description = 'Total score.', 
                       Derivative = TRUE))
  

  # convert formatting to JSON
  lbc_json <- RJSONIO::toJSON(lbc_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(lbc_json, asText = TRUE))){
    print('EFCR JSON file may be invalid')
  }
  
  return(lbc_json)
  
}