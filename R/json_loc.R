#' json_loc: Generates a json file for the Loss of Control-Eating Questionnaire
#'
#' This function generates a json file for the Loss of Control-Eating Questionnaire with raw participant responses. This function provides accurate json files ONLY if data was collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the Loss of Control-Eating Questionnaire.
#'
#' @export
#' 
json_loc <- function() {
  
  loc_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Loss of Control-Eating Questionniare',
      Reference = 'Tanofsky-Kraff, M., Marcus, M. D., Yanovski, S. Z., and Yanovski, J. A. (2008). Loss of control eating disorder in children age 12 years and younger: Proposed research criteria. Eating Behaviors 9, 360–365. doi: 10.1016/j.eatbeh.2008.03.002.', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/18549996/'),
    participant_id = list( Description = 'participant id number'),
    loc_1 = list( Description = 'During the past 3 months have you ever felt that you were not able to stop eating, or not able to control the type of food or amount of food that you ate?  For example, can you remember a time over the last 3 months that you were eating something so yummy that you couldn’t stop eating, even if you really wanted to?', 
                Levels = list ('0' = 'No',
                               '1' = 'Yes',
                               '2' = 'Skip')),
    loc_2a = list( Description = 'How many times in the last month?'),
    loc_2b = list( Description = 'How many times 2 months ago?'),
    loc_2c = list( Description = 'How many times 3 months ago?'),
    loc_3 = list( Description = 'Were you hungry?',
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes',
                                 '2' = 'Skip')),
    loc_4 = list( Description = 'Were you trying to cut back or eat less food than
usual?',
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes',
                                 '2' = 'Skip')),
    loc_5 = list( Description = 'Did you have a bad feeling, like angry, sad, or lonely before you ate?',
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes',
                                 '2' = 'Skip')),
    loc_6 = list( Description = 'Were you feeling bored or tired before you ate?',
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes',
                                 '2' = 'Skip')),
    loc_7 = list( Description = 'Did something bad happen to make you want to eat? (For example: Had a fight with a friend, got in trouble with a parent?)', 
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes',
                                 '2' = 'Skip')),
    loc_8 = list( Description = 'Did something good happen to make you want to eat? (For example: Did well on a test, went to a party or celebration)', 
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes',
                                 '2' = 'Skip')),
    loc_9 = list( Description = 'Did you keep eating even though you were full or had already eaten enough?', 
                  Levels = list ('0' = 'No',
                                 '1' = 'Yes',
                                 '2' = 'Skip')),
    loc_10 = list( Description = 'Did the amount of food feel like too much for you at the time?',
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_11 = list( Description = 'Do you think other people would think you ate too much food?', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_12 = list( Description = 'Were you eating in secret or trying to hide the food you were eating?', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_13 = list( Description = 'Did it feel like you were eating more than others?', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_14 = list( Description = 'During any time when you were eating, did you feel numb or like you spaced or zoned out?', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_15 = list( Description = 'Did you feel badly about yourself for eating or about what you ate? (For example, did you feel guilt, shame, unhappiness, or another kind of bad feeling?)', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_16a = list( Description = 'Did you throw up?', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_16b = list( Description = 'If yes, did you make yourself throw up?', 
                    Levels = list ('0' = 'No',
                                   '1' = 'Yes',
                                   '2' = 'Skip')),
    loc_17 = list( Description = 'Did you use laxatives or any kind of pills to make the food go out of your body?', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_18 = list( Description = 'Did you exercise for an hour or more, in order to make up for the food that you ate?', 
                   Levels = list ('0' = 'No',
                                  '1' = 'Yes',
                                  '2' = 'Skip')),
    loc_19 = list( Description = 'Did you not eat anything at all for a whole day or more because you ate too much?', 
                   DLevels = list ('0' = 'No',
                                   '1' = 'Yes',
                                   '2' = 'Skip')))
  
  # convert formatting to JSON
  loc_json <- RJSONIO::toJSON(loc_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(loc_json, asText = TRUE))){
    print('LOC-eating JSON file may be invalid')
  }
  
  return(loc_json)
}