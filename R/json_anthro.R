#' json_anthro: Generates a json file for anthropometrics data
#'
#' This function generates a json file for anthropometrics data
#'
#' @return A json file documenting the Study BRAKE anthropometrics data
#'
#'
#' @export

json_anthro <- function() {
  
  anthro_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1 (baseline)',
                                          '3'	= 'Child visit protocol 3 (follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    child_relationship = list( Description = 'What is your relationship to the child in the study?',
                              Levels = list ('0' = 'Biological mother',
                                             '1' = 'Biological father',
                                             '2' = 'Non-biological mother',
                                             '3' = 'Non-biological father',
                                             '4' = 'Other')),
    age = list( Description = 'Child age',
                Units = 'years'),
    sex = list( Description = 'Child sex',
                Levels = list ('male' = 'male',
                               'female' = 'female')),
    height1_cm = list( Description = 'child height measurement 1',
                       Unit = "cm"),
    height2_cm = list( Description = 'child height measurement 2',
                       Unit = "cm"),
    weight1_kg = list( Description = 'child weight measurement 1',
                       Unit = "kg"),
    weight2_kg = list( Description = 'child weight measurement 2',
                       Unit = "kg"),
    height_mean_cm = list( Description = 'average of height1_cm and height2_cm',
                           Unit = "cm",
                           Derivative = TRUE),
    weight_mean_kg = list( Description = 'average of weight1_kg and weight2_kg',
                           Unit = "kg",
                           Derivative = TRUE),
    heightweight_notes = list( Description = 'notes about measured height and weight'),
    child_bmi = list( Description = 'child body mass index computed from height_mean_cm and weight_mean_kg',
                      Units = 'kg/m2',
                      Derivative = TRUE),
    child_bmi_z = list( Description = 'child body mass index z-score computed using the childsds library with cdc.ref using child_bmi, sex, and age',
                      Derivative = TRUE),
    child_bmi_p = list( Description = 'child body mass index percentile computed using the childsds library with cdc.ref using child_bmi, sex, and age',
                      Derivative = TRUE),
    parent1_relationship = list( Description = 'Relationship of parent being measured to the child',
                                 Levels = list ('0' = 'Mom',
                                                '1' = 'Dad')),
    parent1_height1_cm = list( Description = 'parent height measurement 1',
                               Unit = "cm"),
    parent1_height2_cm = list( Description = 'parent height measurement 2',
                               Unit = "cm"),
    parent1_weight1_kg = list( Description = 'parent weight measurement 1',
                               Unit = "kg"),
    parent1_weight2_kg = list( Description = 'parent weight measurement 2',
                               Unit = "kg"),
    parent1_height_mean_cm = list( Description = 'average of parent1_height1_cm and parent1_height2_cm',
                                   Unit = "cm",
                                   Derivative = TRUE),
    parent1_weight_mean_kg = list( Description = 'average of parent1_weight1_kg and parent1_weight2_kg',
                                   Unit = "kg",
                                   Derivative = TRUE),
    demo_parent2_rep_height_ft = list( Description = 'What is your child\'s biological parent\'s height who was not measured or present at today\'s visit (Feet):', 
                                       Unit = 'feet'),
    demo_parent2_rep_height_in = list( Description = 'What is your child\'s biological parent\'s height who was not measured or present at today\'s visit (Inches):', 
                                       Unit = 'inches'),
    demo_parent2_rep_weight_lbs = list( Description = 'What is the weight, in pounds, of your child\'s biological parent who is not present at todays visit?', 
                                        Unit = 'pounds'),
    demo_parent2_rep_height_m = list( Description = 'Parent2 (i.e., biological parent not at visit) height calculated from demo_parent2_rep_height_ft and demo_parent2_rep_height_in',
                                      Unit = "m",
                                      Derivative = TRUE),
    parent2_rep_weight_kg = list( Description = 'Parent2 (i.e., biological parent not at visit) weight calculated from demo_parent2_rep_height_lbs',
                                  Unit = "kg",
                                  Derivative = TRUE),
    parent2_rep_bmi = list( Description = 'BMI of other parent computed by reported hgith and weight',
                            Unit = "kg/m2",
                            Derivative = TRUE),
    parent1_bmi = list( Description = 'BMI of measured parent computed from parent1_height_mean_cm and parent1_weight_mean_kg',
                            Unit = "kg/m2",
                            Derivative = TRUE),
    mom_anthro_method = list( Description = 'method used to get mom\'s anthropometric information',
                                   Levels = list ('measured' = 'measured at visit 1 or visit 3',
                                                  'reported' = 'reported by partner at visit')),
    mom_bmi = list( Description = 'BMI of biological mother, obtained via method specified in mom_anthro_method',
                         Unit = "kg/m2",
                         Derivative = TRUE),
    dad_anthro_method = list( Description = 'method used to get dad\'s anthropometric information',
                                   Levels = list ('measured' = 'measured at visit 1 or visit 3',
                                                  'reported' = 'reported by partner at visit')),
    dad_bmi = list( Description = 'BMI of biological mother, obtained via method specified in mom_anthro_method',
                         Unit = "kg/m2",
                         Derivative = TRUE),
    heightweight_notes = list( Description = 'notes about measured height and weight'))
  
  # convert formatting to JSON
  anthro_json <- RJSONIO::toJSON(anthro_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(anthro_json, asText = TRUE))){
    print('anthro info JSON file may be invalid')
  }
  
  return(anthro_json)
  
}