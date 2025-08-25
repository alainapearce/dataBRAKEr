#' json_sic: Generates a json file for the Stress in Children Questionnaire
#'
#' This function generates a json file for the scored Stress in Children Questionnaire and raw participant responses.
#' This function provides accurate json files ONLY if data is processed using score_sic function in dataprepr and is only accurate for data collected in Study REACH
#'
#' @return A string with data stored in JSON format containing meta-data for the Stress in Children Questionnaire
#'
#'
#' @export

json_sic <- function() {

  sic_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Stress in Children Questionnaire. Participants were provided the following instructions: "The following questions ask you about your feelings and thoughts. For each question, you will be asked to provide a response that best fits."',
      Reference = 'Osika W, Friberg P, Wahrborg P. A new short self-rating questionnaire to assess stress in children. Int J Behav Med. 2007;14(2):108-117. doi:10.1007/BF03004176',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/17926439/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-baseline' = 'baseline',
                                      'ses-followup' = '1-year follow-up')),
    sic1 = list( Description = 'I get angry',
                  Levels = list ('0' = '1 = Never',
                                 '1' = '2 = Sometimes',
                                 '2' = '3 = Often',
                                 '3' = '4 = Very Often')),
    sic2 = list( Description = 'I get headaches',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic3 = list( Description = 'I like going to school',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic4 = list( Description = 'I feel calm and happy',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic5 = list( Description = 'I get stomach pains',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic6 = list( Description = 'I feel lonely',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic7 = list( Description = 'I get sad',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic8 = list( Description = 'I like to be at school',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic9 = list( Description = 'The other kids tease me',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic10 = list( Description = 'I fall asleep easily at night',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic11 = list( Description = 'I feel calm',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic12 = list( Description = 'Things work out as I have planned',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic13 = list( Description = 'I feel happy',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic14 = list( Description = 'When I am happy I show it',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic15 = list( Description = 'Sometimes I do not reach the goal I have planned for',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic16 = list( Description = 'When I have a hard time it helps being with my friends',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic17 = list( Description = 'When I am sad I show it',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic18 = list( Description = 'Sometimes I can\'t manage with the things I have to do',
                 Levels = list ('0' = '1 = Never',
                                '1' = '2 = Sometimes',
                                '2' = '3 = Often',
                                '3' = '4 = Very Often')),
    sic19 = list( Description = 'When I have a hard time there is an adult to talk to',
                  Levels = list ('0' = '1 = Never',
                                 '1' = '2 = Sometimes',
                                 '2' = '3 = Often',
                                 '3' = '4 = Very Often')),
    sic20 = list( Description = 'If anyone teases me I will protest',
                  Levels = list ('0' = '1 = Never',
                                 '1' = '2 = Sometimes',
                                 '2' = '3 = Often',
                                 '3' = '4 = Very Often')),
    sic21 = list( Description = 'It is easy to concentrate during lessons at school',
                  Levels = list ('0' = '1 = Never',
                                 '1' = '2 = Sometimes',
                                 '2' = '3 = Often',
                                 '3' = '4 = Very Often')),
    sic_lackwellbeing = list( Description = 'Lack of Well being',
                       Derivative = TRUE),
    sic_distress = list( Description = 'Distress',
                      Derivative = TRUE),
    sic_lacksocialsupport = list( Description = 'Lack of Social support',
                      Derivative = TRUE),
    sic_grand_mean = list( Description = 'Stress in Children Grand Mean',
                              Derivative = TRUE)
    )


  # convert formatting to JSON
  sic_json <- RJSONIO::toJSON(sic_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(sic_json, asText = TRUE))){
    print('sic JSON file may be invalid')
  }

  return(sic_json)

}
