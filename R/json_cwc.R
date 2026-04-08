#' json_cwc: Generates a json file for the Child Weight Concerns scale
#'
#' This function generates a json file for the scored Child Weight Concerns scale and raw participant responses.
#'
#' @return A string with data stored in JSON format containing meta-data for the Child Weight Concerns scale
#'
#'
#' @export

json_cwc <- function() {
  
  cwc_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Child Weight Concerns Scale. Trained research assistants adminsiter the scale to children using the following instructions: "Okay, now I\'m going to ask you some questions about how you feel about your body. There are no right or wrong answers, I just want to know how you feel. I also won\'t talk to your parents about anything unless you want me to. Are you ready to start?"',
      Reference = 'Killen JD, Taylor CB, Hayward C, et al. Pursuit of thinness and onset of eating disorder symptoms in a community sample of adolescent girls: A three-year prospective analysis. Int J Eat Disord. 1994;16(3):227-238. doi:10.1002/1098-108X(199411)16:3<227::AID-EAT2260160303>3.0.CO;2-L',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/7833956/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1 (baseline)',
                                          '2' =	'Child visit protocol 2 (baseline)',
                                          '3'	= 'Child visit protocol 3 (follow-up')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    cwc1 = list( Description = 'How much more or less do you feel you worry about your weight and body shape than other [girls/boys] your age?',
                 Levels = list ('1' = 'I worry a lot less than other [girls/boys]',
                                '2' = 'I worry a little less than other [girls/boys]',
                                '3' = 'I worry about the same as other [girls/boys]',
                                '4' = 'I worry a little more than other [girls/boys]',
                                '5' = 'I worry a little more than other [girls/boys]',
                                '99' = 'Prefer not to answer')),
    cwc2 = list( Description = 'How afraid are you of gaining 3 pounds?',
                 Levels = list ('1' = 'How afraid are you of gaining 3 pounds?
',
                                '2' = 'How afraid are you of gaining 3 pounds?
',
                                '3' = 'Moderately afraid of gaining',
                                '4' = 'Very afraid of gaining',
                                '5' = 'Very afraid of gaining',
                                '99' = 'Prefer not to answer')),
    cwc3 = list( Description = 'When was the last time you went on a diet?',
                 Levels = list ('1' = 'When was the last time you went on a diet?',
                                '2' = 'I was on a diet about 1 year ago',
                                '3' = 'I was on a diet about 1 year ago',
                                '4' = 'I was on a diet about 3 months ago',
                                '5' = 'I was on a diet about 3 months ago',
                                '6' = 'I was on a diet less than 1 month ago',
                                '7' = 'I was on a diet less than 1 month ago',
                                '99' = 'Prefer not to answer')),
    cwc4 = list( Description = 'Compared to other things in your life, how important is your weight to you?',
                 Levels = list ('1' = 'Compared to other things in your life, how important is your weight to you?',
                                '2' = 'My weight is a little more important than some other things',
                                '3' = 'My weight is a little more important than some other things',
                                '4' = 'My weight is the most important thing in my life',
                                '99' = 'Prefer not to answer')),
    cwc5 = list( Description = 'Do you ever feel fat?',
                 Levels = list ('1' = 'Never',
                                '2' = 'Rarely',
                                '3' = 'Sometimes',
                                '4' = 'Often',
                                '5' = 'Always',
                                '99' = 'Prefer not to answer')),
    cwc_total = list( Description = 'Total score',
                      Derivative = TRUE))
  
  # convert formatting to JSON
  cwc_json <- RJSONIO::toJSON(cwc_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(cwc_json, asText = TRUE))){
    print('CWC JSON file may be invalid')
  }
  
  return(cwc_json)
  
}
