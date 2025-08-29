#' json_demo: Generates a json file for demographic data
#'
#' This function generates a json file for demographic data
#'
#' @return A json file documenting the Study BRAKE demographic data
#'
#'
#' @export

json_demo <- function() {
  
  demo_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    sex = list( Description = 'Child sex',
                Levels = list ('male' = 'male',
                               'female' = 'female')),
    age = list( Description = 'Child age',
                Units = 'years'),
    ethnicity = list( Description = 'Parent-reported child ethnicity. Collected at ses-baseline only but values applied to ses-baseline and ses-followup',
                      Levels = list ('0' = 'Not Hispanic or Latino',
                                     '1' = 'Hispanic or Latino')),
    race = list( Description = 'Parent-reported child race. ollected at ses-baseline only but values applied to ses-baseline and ses-followup',
                 Levels = list ('0' = 'American Indian/Alaskan Native',
                                '1' = 'Asian',
                                '2' = 'Black or African American',
                                '3' = 'White',
                                '4' = 'Hawaiian/Pacific Islander',
                                '5' = 'Other')),
    child_other_race = list( Description = 'If other race, please specify'),
    demo_income = list( Description = 'Including ALL sources (such as social security income, child support payments, government assistance, dividends from investments, etc.) what was your household\'s combined yearly income last year BEFORE taxes?',
                        Levels = list ('0' = 'Less than $20,000',
                                       '1' = '$21,000 - $35,000',
                                       '2' = '$36,000 - $50,000',
                                       '3' = '$51,000 - $75,000',
                                       '4' = '$76,000 - $100,000',
                                       '5' = '$100,000 +')),
    demo_mom_ed = list( Description = 'What is the child\'s biological mother\'s highest level of formal education?'),
    demo_ded_ed = list( Description = 'What is the child\'s biological father\'s highest level of formal education?'),
    child_bmi = list( Description = 'child body mass index computed from height_mean_cm and weight_mean_kg',
                      Units = 'kg/m2',
                      Derivative = TRUE),
    child_bmi_z = list( Description = 'child body mass index z-score computed using the childsds library with cdc.ref using child_bmi, sex, and age',
                      Derivative = TRUE),
    child_bmi_p = list( Description = 'child body mass index percentile computed using the childsds library with cdc.ref using child_bmi, sex, and age',
                      Derivative = TRUE),
    child_weight_status = list( Description = 'Child weight status based on CDC cutoffs by child BMI percential',
                                 Levels = list ('obese' = 'has obesity; child_bmi_p >= 95 ',
                                                'overweight' = 'has overwegiht; child_bmi_p >= 85 and < 95',
                                                'healthy weight' = 'does not have overweight/obesity; child_bmi_p < 85')),
    mom_bmi = list( Description = 'BMI of biological mother, obtained via method specified in mom_anthro_method',
                    Unit = "kg/m2",
                    Derivative = TRUE),
    dad_bmi = list( Description = 'BMI of biological mother, obtained via method specified in mom_anthro_method',
                    Unit = "kg/m2",
                    Derivative = TRUE),
    risk_status = list( Description = 'child risk for overweight or obesity based on mom weight status',
                                Levels = list ('low-risk' = 'low risk; mom_bmi <= 26',
                                               'hgih-risk' = 'high risk; mom_bmi >=29')),
    mom_anthro_method = list( Description = 'method used to get mom\'s anthropometric information',
                                   Levels = list ('measured' = 'measured at visit 1 or visit 3',
                                                  'reported' = 'reported by partner at visit')),
    dad_anthro_method = list( Description = 'method used to get dad\'s anthropometric information',
                                   Levels = list ('measured' = 'measured at visit 1 or visit 3',
                                                  'reported' = 'reported by partner at visit'))
    )
  
  # convert formatting to JSON
  demo_json <- RJSONIO::toJSON(demo_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(demo_json, asText = TRUE))){
    print('demographic info JSON file may be invalid')
  }
  
  return(demo_json)
  
}