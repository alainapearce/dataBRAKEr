#' json_infancy: Generates a json file for infancy data
#'
#' This function generates a json file for infant-related data collected in the visit 1 demographics form
#'
#' @return A string with data stored in JSON format containing meta-data for infant-related data
#'
#'
#' @export

json_infancy <- function() {

  infancy_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Questions related to the child\'s birth and infancy that parents answered as part of the REDCap demographics form'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    demo_birth_length = list( Description = 'What was your child\'s birth length, in inches?',
                              Unit = "inches"),
    demo_birthweight_pounds = list( Description = 'What was your child\'s birthweight? -- Pounds box',
                                    Unit = "pounds"),
    demo_birthweight_ounces = list( Description = 'What was your child\'s birthweight? -- Ounces box',
                                    Unit = "ounces"),
    demo_premature = list( Description = 'Was your child born premature (born before 37 weeks)?'),
    demo_premature_weeks = list( Description = 'By how many weeks? (The normal gestation period is 40 weeks)'),
    demo_feeding = list( Description = 'Was your child primarily breast-fed or primarily formula fed?'),
    demo_exclusive_feeding = list( Description = 'What was the total duration, in months, that your child was exclusively (only) breastfed? Please enter the duration, in months, that your child was only fed breastmilk and no formula or solid food was consumed. '),
    demo_tot_breastfeeding = list( Description = 'What was the total duration, in months, that your child was breastfed? Please enter the duration, in months, that your child was fed both breast milk and formula and/or solid food. '),
    demo_solid_food = list( Description = 'At what age, in months, was your child first introduced to solid food? '),
    birthweight_ounces_total = list( Description = 'Child birthweight computed from demo_birthweight_pounds_component and demo_birthweight_ounces_component (16*demo_birthweight_pounds_component + demo_birthweight_ounces_component)',
                                     Derivative = TRUE,
                                     Unit = "ounces")
    )

  # convert formatting to JSON
  infancy_json <- RJSONIO::toJSON(infancy_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(infancy_json, asText = TRUE))){
    print('Infancy JSON file may be invalid')
  }

  return(infancy_json)

}
