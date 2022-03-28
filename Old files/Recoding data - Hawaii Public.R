library(readxl)
library(dplyr)
library(tidyr)
library(rsconnect)
library(scales)
library(plotly)
Final_SM_Data_Combined <- read_excel("~/Final SM Data Combined.xlsx")
Final_SM_Data_Combined2 <- read_excel("~/General/All KS Data Public.xlsx")

var <- names(Final_SM_Data_Combined2)[(stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "In the last 12 months") |
                                         stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "If leadership means serving")) |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q20 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q26 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q27 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q30 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q39 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q43 ")]

v2 <- names(Final_SM_Data_Combined2)[stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q34")]

test_df <- Final_SM_Data_Combined2 %>%
  mutate_at(vars(v2), funs(recode(.,
                                  "Never" = "0",
                                  "Rarely" = '1'))) %>%
  mutate_if(is.character, funs(recode(.,
                                      "Don't know" = 'NA',
                                      "Don't know	" = 'NA',
                                      "Don‚Äôt know" = 'NA',
                                      "Don't Know" = 'NA',
                                      "Prefer not to answer" = 'NA',
                                      "Refused – Please explain" = '-1',
                                      "No" = '0',
                                      "Not making ends meet (have to rely on loans or credit cards to pay bills)" = '0',
                                      "None of the above" = '1',
                                      "Very unsatisfied" = '1',
                                      "Not at all" = '1',
                                      "Not at all hopeful" = '1',
                                      "Living paycheck-to-paycheck (not saving any money)" = '1',
                                      "Other - Please specify" = '1',
                                      "Never" = "1",
                                      "Unsatisfied" = '2',
                                      "A little bit" = "2",
                                      "To some extent" = '2',
                                      "A little hopeful" ='2',
                                      "A couple of times a year" ='2',
                                      "Once or twice a year" ='2',
                                      "Enough savings for 1 to 2 months of expenses" = '2',
                                      "Enough savings to pay for 1 to 2 months of expenses" = '2',
                                      "Rarely" = '2',
                                      "Yes" = '1',
                                      "Neutral" = '3',
                                      "To a moderate extent" = "3",
                                      "Pretty hopeful" = "3",
                                      "Moderately" = "3",
                                      "Monthly" = '3',
                                      "Sometimes" = '3',
                                      "Enough savings for 3 months to 1 year of expenses" = '3',
                                      "Enough savings to pay for 3 months to 1 year of expenses" = '3',
                                      "Satisfied" = '4',
                                      "To a great extent" = "4",
                                      "Quite a bit" = '4',
                                      "Very hopeful" = '4',
                                      "Weekly" = '4',
                                      "Most of the time" = '4',
                                      "Enough savings to pay for more than 1 year of expenses" = '4',
                                      "Enough savings to pay for more than 1 year of expenses" = '4',
                                      "Very satisfied" = '5',
                                      "Extremely" = '5',
                                      "Extremely hopeful" = '5',
                                      "Daily" = '5',
                                      "Always" = '5',
                                      "N/A" = "NA",
                                     .default = NULL))) %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"â€™","'"))) %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"â€“","-"))) %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"â€˜Ä","A"))) %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"Ê»ÅŒ","O"))) %>%
  mutate_at(vars(var), funs(ifelse(is.na(.),"0","1")))

write.csv(test_df,file = "~/General/KSREcode.csv")
#For the dashboard
var <- names(Final_SM_Data_Combined2)[(stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Select all that apply") |
                                stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Choose your top three")) &
                                !stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Please specify") &
                                  !stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "please specify") &
                                !stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "None of the above")]

var <- names(Final_SM_Data_Combined2)[(stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "In the last 12 months") |
                                         stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "If leadership means serving")) |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q20 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q26 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q27 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q30 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q39 ") |
                                        stringr::str_detect(names(Final_SM_Data_Combined2),pattern = "Q43 ")]

app_df <- Final_SM_Data_Combined2 %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"â€™","'"))) %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"‚Äô","'"))) %>%
  mutate_if(is.character, funs(stringr::str_replace(.,"â€“","-"))) %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"â€˜Ä","A"))) %>% 
  mutate_if(is.character, funs(stringr::str_replace(.,"Ê»ÅŒ","O"))) %>%
  mutate_at(vars(var), funs(ifelse(is.na(.),"No","Yes"))) %>% 
  mutate_if(is.character, funs(recode(.,"N/A" = "NA",
                                      .default = NULL)))


x <- names(app_df)
x <- stringr::str_replace(x,"â€™","'")
x <- stringr::str_replace(x,"â€“","-")
x <- stringr::str_replace(x,"â€˜Ä","A")
x <- stringr::str_replace(x,"Ê»Ä\u0081","A")
x <- stringr::str_replace(x,"Ê»ÅŒ","O")
x <- stringr::str_replace(x,"Ê»ÅŒ","O")
x <- stringr::str_replace(x,"â€˜","")

names(app_df) <- x
stopwords <- c("a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thick", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the")

response_order <- list(c("Yes","No","NA",NA),
                       c("Very satisfied","Satisfied","Neutral","Unsatisfied","Very unsatisfied","NA", NA,"Don't know"),
                       c("To a great extent", "To a moderate extent", "To some extent", "Not at all", "NA", "Don't know", NA),
                       c("Extremely", "Quite a bit", "Moderately", "A little bit", "Not at all", "Don't know","NA", NA),
                       c("Extremely hopeful",  "Very hopeful", "Pretty hopeful", "A little hopeful", "Not at all hopeful", "Don't know","NA", NA),
                       c("Daily", "Weekly", "Monthly", "Once or twice a year", "Never", "Don't know","NA", NA),
                       c("A couple of times a year", "Monthly", "Weekly","Daily", "Rarely","Never",  "Don't know","NA", NA),
                       c("Always", "Most of the time", "Sometimes", "Rarely", "Never", "Don't know","NA", NA),
                       c("Yes", "No", "Prefer not to answer", "Don't know","NA", NA),
                       c("Yes", "No", "Prefer not to answer","NA", NA),
                       c("Yes", "No", "Don’t know", "Refused – Please explain", "Prefer not to answer","NA", NA),
                       c("Very satisfied", "Satisfied", "Neutral", "Unsatisfied","Very unsatisfied", "Prefer not to answer","NA", NA),
                       c("Less than high school", "High school or equivalent", "Some college or Associate's degree", "Bachelor's degree", "Masters degree or higher", "Other (for example, vocational training) - Please specify", "Prefer not to answer",NA,"NA"),
                       c("Strengthening Native Hawaiian communities is both a <strong>personal and professional</strong> goal.", "Strengthening Native Hawaiian communities is a <strong>personal</strong> goal.", "Strengthening Native Hawaiian communities is a <strong>professional </strong>goal.", "Strengthening Native Hawaiian communities is <strong>not a goal for me but it might be someday</strong>.", "Strengthening Native Hawaiian communities is <strong>not a goal</strong> for me.", "Don't know","NA",NA),
                       c("Enough savings to pay for more than 1 year of expenses", "Enough savings to pay for 3 months to 1 year of expenses", "Enough savings to pay for 1 to 2 months of expenses", "Living paycheck-to-paycheck (not saving any money)", "Not making ends meet (have to rely on loans or credit cards to pay bills)",NA,"NA"))


i=1
while(i <= NCOL(app_df)){
  listreturn <- sapply(1:length(response_order),function(x){all(tolower(unique(app_df[[i]])) %in% tolower(response_order[[x]]))})
  if(any(listreturn)){
    app_df[[i]] <- factor(app_df[[i]],ordered = TRUE,levels = response_order[listreturn][[1]])
  }
  i = i+1
}

#Merge Q45
app_df$`Q45 In what field do you spend most of your time working?...118` <- ifelse(app_df$`Q45 In what field do you spend most of your time working?...118` == "Other - Please specify",
       paste(app_df$`Q45 In what field do you spend most of your time working?...118`,
             app_df$`Q45 In what field do you spend most of your time working?...119`,
             sep = ": "),
       app_df$`Q45 In what field do you spend most of your time working?...118`)

app_df <- select(app_df, select = -c("Q45 In what field do you spend most of your time working?...119",
                                     "Q47 What is your highest level of education? For Other, please sepcify (for example, vocational training)",
                                     "Q48 If you refused to vote in the last election held on November 3, 2020, please explain."))

names(app_df)[names(app_df) == "Q45 In what field do you spend most of your time working?...118"] <- "Q45 In what field do you spend most of your time working?"

save(app_df,stopwords,file = "Hawaii Dashboard V2/Output.RData")
