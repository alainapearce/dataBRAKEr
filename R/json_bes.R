#' json_bes: Generates a json file for the Binge Eating Scale
#'
#' This function generates a json file for the scored Binge Eating Scale and raw participant responses. This function provides accurate json files ONLY if data is processed using score_bes function in dataprepr and is only accurate for data collected in Study BRAKE.
#'
#' @return A json file documenting the raw inputs and scored values for the Binge Eating Scale
#'
#'
#' @export

json_bes <- function() {
  
  bes_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Binge Eating Scale.',
      Reference = 'Gormally, J., Black, S., Daston, S., & Rardin, D. (1982). The assessment of binge eating severity among obese persons. Addictive Behaviors, 7(1), 47–55. https://doi.org/10.1016/0306-4603(82)90024-7', 
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/7080884/'),
    participant_id = list( Description = 'participant id number'),
    bes1 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child doesn\'t feel self-conscious about his/her weight or body size when he/she is with others',
                                '0' = 'My child feels concerned about how he/she looks to others, but it normally does not make him/her feel disappointed with him/herself',
                                '1' = 'My child does get self-conscious about his/her appearance and weight which makes my child feel disappointed in him/herself',
                                '3' = 'My child feels very self-conscious about his/her weight and frequently feels intense shame and disgust for him/herself. My child tries to avoid social contacts because of his/her self-consciousness.', 
                                'NA' = 'Don\'t want to answer')),
    bes2 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child doesn\'t have any difficulty eating slowly in the proper manner',
                                '1' = 'Although my child seems to "gobble down" foods, my child doesn\'t end up feeling stuffed because of eating too much.',
                                '2' = 'At times, my child tends to eat quickly and then, feels uncomfortably full afterwards',
                                '3' = 'My child has the habit of bolting down my food, without really chewing it. When this happens my child usually feels uncomfortably stuffed because he/she has eaten too much', 
                                'NA' = 'Don\'t want to answer')),
    bes3 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child feels capable to control his/her eating urges when he/she wants to.',
                                '1' = 'My child feels like he/she has failed to control his/her eating more than the average person',
                                '3' = 'My child feels utterly helpless when it comes to feeling in control of his/her eating urges',
                                '3' = 'Because my child feels so helpless about controlling his/her eating, he/she has become very desperate about trying to get in control.', 
                                'NA' = 'Don\'t want to answer')),
    bes4 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child doesn\'t have the habit of eating when he/she is bored.',
                                '0' = 'My child sometimes eats when he/she is bored, but often he/she is able to "get busy" and get their mind off food.',
                                '0' = 'My child has a regular habit of eating when he/she is bored, but occasionally, he/she can use some other activity to get their mind off eating.',
                                '2' = 'My child has a strong habit of eating when he/she is bored. Nothing seems to help my child break the habit.', 
                                'NA' = 'Don\'t want to answer')),
    bes5 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child is usually physically hungry when he/she eats something',
                                '1' = 'Occasionally, my child eats something on impulse even though he/she really is not hungry.',
                                '2' = ' My child has a regular habit of eating foods that he/she might not really enjoy to satisfy a hungry feeling, even though physically, my child doesn\'t need the food.',
                                '3' = 'Even though my child is not physically hungry, my child gets a hungry feeling in his/her mouth that only seems to be satisfied when he/she eats a food, like a sandwich, that fills his/her mouth. Sometimes, when he/she eats the food to satisfy his/her hunger, he/she then spits out the food so he/she won\'t gain weight.', 
                                'NA' = 'Don\'t want to answer')),
    bes6 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child doesn\'t feel any guilt or self-hate after he/she overeats',
                                '1' = 'After my child overeats, occasionally he/she feels guilt or self-hate',
                                '3' = 'Almost all the time my child experiences strong guilt or self-hate after he/she overeats',
                                '3' = 'Don\'t want to answer')),
    bes7 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child doesn\'t lose total control of his/her eating when dieting even after periods when he/she overeats.',
                                '2' = ' Sometimes when my child eats a "forbidden food" on a diet, he/she feels like they "blew it" and eat even more',
                                '3' = 'Frequently, my child has the habit of saying to him/herself, "I\'ve blown it now, why not go all the way" when he/she overeats on a diet. When that happens he/she eats even more.',
                                '3' = ' My child has a regular habit of starting strict diets for him/herself, but he/she breaks the diets by going on an eating binge. My child\'s life seems to be either a "feast" or "famine"', 
                                'NA' = 'Don\'t want to answer')),
    bes8 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child rarely eats so much food that he/she feels uncomfortably stuffed afterwards.',
                                '1' = 'Usually about once a month, my child eats such a quantity of food, that he/she ends up feeling very stuffed.',
                                '2' = ' My child has regular periods during the month when he/she eat large amounts of food, either at mealtime or at snacks',
                                '3' = 'My child eats so much food that he/she regularly feels quite uncomfortable after eating and sometimes a bit nauseous.', 
                                'NA' = 'Don\'t want to answer')),
    bes9 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                 Levels = list ('0' = 'My child\'s level of calorie intake does not go up very high or go down very low on a regular basis.',
                                '1' = 'Sometimes after my child overeats, my child will try to reduce his/her caloric intake to almost nothing to compensate for the excess calories he/she has eaten.',
                                '2' = 'My child has a regular habit of overeating during the night. It seems that my child\'s routine is not to be hungry in the morning but overeat in the evening.',
                                '3' = 'My child has week-long periods where he/she practically starves him/herself. This follows periods when he/she overeats. It seems my child lives a life of either "feast" or "famine"', 
                                'NA' = 'Don\'t want to answer')),
    bes10 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                  Levels = list ('0' = 'My child is usually able to stop eating when he/she wants to. My child knows when "enough is enough"',
                                 '1' = 'Every so often, my child experiences a compulsion to eat which my child seems to control.',
                                 '2' = 'Frequently, my child experiences strong urges to eat which he/she seems unable to control, but at other times he/she can control their eating urges.',
                                 '3' = ' My child feels incapable of controlling urges to eat. My child has a fear of not being able to stop eating voluntarily.', 
                                 'NA' = 'Don\'t want to answer')),
    bes11 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                  Levels = list ('0' = 'My child doesn\'t have any problem stopping eating when he/she feels full.',
                                 '1' = 'My child usually can stop eating when he/she feels full but occasionally overeats leaving my child feeling uncomfortably stuffed',
                                 '2' = 'My child has a problem stopping eating once he/she starts and usually feels uncomfortably stuffed after he/she eats a meal.',
                                 '3' = 'Because my child has a problem not being able to stop eating when he/she wants, my child sometimes has to induce vomiting to relieve the stuffed feeling.', 
                                 'NA' = 'Don\'t want to answer')),
    bes12 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                  Levels = list ('0' = 'My child seems to eat just as much when with other (family, social gatherings) as when by him/herself.',
                                 '1' = 'Sometimes, when my child is with other persons, my child doesn\'t eat much as he/she wants to eat because he/she is self-conscious about his/her eating.',
                                 '2' = 'Frequently, my child eats only a small amount of food when others are present, because he/she is very embarrassed about his/her eating.',
                                 '3' = 'My child feels so ashamed about overeating that he/she picks times to overeat when my child knows no one will see them. My child is a "closet eater."', 
                                 'NA' = 'Don\'t want to answer')),
    bes13 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                  Levels = list ('0' = 'My child eats three meals a day with only an occasional between meal snack.',
                                 '0' = 'My child eats three meals a day, but also normally snack between meals.',
                                 '2' = 'When my child is snacking heavily, he/she gets in the habit of skipping regular meals.',
                                 'NA' = 'Don\'t want to answer')),
    bes14 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                  Levels = list ('0' = 'My child doesn\'t think much about trying to control unwanted eating urges.',
                                 '1' = 'At least some of the time, my child thoughts are pre-occupied with trying to control eating urges.',
                                 '2' = 'My child frequently spends much time thinking about how much he/she ate or about trying not to eat anymore.',
                                 'NA' = 'Don\'t want to answer')),
    bes15 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                  Levels = list ('0' = 'My child doesn\'t think about food a great deal',
                                 '1' = 'My child has strong cravings for food but they last only for brief periods of time.',
                                 '2' = 'My child has days when he/she can\'t seem to think about anything else but food.',
                                 'NA' = 'Don\'t want to answer')),
    bes16 = list( Description = 'Read all of the statements and mark the one that best describes the way you feel about problems your child has controlling his/her eating behavior',
                  Levels = list ('0' = 'My child usually knows whether or not he/she is physically hungry. My child takes the right portion of food to satisfy him/herself.',
                                 '1' = 'Occasionally, my child is uncertain about knowing whether he/she is physically hungry. At these times, it\'s hard to know how much food my child should take to satisfy him/herself.',
                                 '2' = 'Even though my child might know how many calories he/she should eat, my child doesn\'t have any idea what is a "normal" amount of food for him/herself.',
                                 'NA' = 'Don\'t want to answer')),
    bes_total = list( Description = 'Total Binge Eating Score', 
                     Derivative = TRUE))
  
  # convert formatting to JSON
  bes_json <- RJSONIO::toJSON(bes_list, pretty = TRUE)
  
  # double check
  if (isFALSE(RJSONIO::isValidJSON(bes_json, asText = TRUE))){
    print('bes JSON file may be invalid')
  }
  
  return(bes_json)
  
}