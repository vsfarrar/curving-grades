#NUTRITION 10 Data 

#packages
library(tidyverse)
library(plotrix) #for std.error
library(broom)
library(forcats)

#data
setwd("~/Documents/projects/curving-grades/")
NUT10_likert <- read.csv("NUT10_Likert_only.csv")
NUT10supp <- read.csv("NUT10_remaining_surveys.csv") #4 extra questions added after survey launched
prompt_key <-read.delim("curving_questions_prompt_key_tabdelim.txt",sep = "\t")

#DATA FILTERING, CLEANING

#filter likert dataset

likert <-
  NUT10_likert %>% 
  filter(Progress == 100 & Finished == "TRUE") %>% #remove students who did not finish 
  filter(Duration..in.seconds. > 300) %>% #remove stuents who took less than 3 minutes
  filter(consent_full_name != "") %>% #remove students who did not consent by entering their full name at start
  filter(Q57_11 == "Strongly Disagree" & Q59_1 == "Strongly Disagree") #remove students who missed validation Qs

#remaining sample size =  611

#supplemental surveys

supp <- NUT10supp %>%
  filter(Progress == 100 & Finished == "TRUE") #filter by those who finished the survey (n = 273)

#join datasets together
  #join all likert data (n = 611) with all those who finished the supplemental survey (n = 273)

full_data <- 
  left_join(likert, supp, by = "sid")

#CONVERT LIKERT Qs to NUMERIC

#center neutrals at 0 
#-2 = Strongly Disagree, -1 = Disagree, 0 = Neutral, +1 = Agree, +2 = Strongly Disagree 

lik_long <-
  full_data %>%
  #select likert Qs only
  select(-Q45_1, -Q47, -Q79, -Q75_15, -Q9, -Q11_4_TEXT, #other mult.choice, Q79 free response, Q75_15 blank
         curve_def = Q11) %>% #rename Q11 curve definition so that it does not get pivoted
  #pivot to long format: one question per row
  pivot_longer(cols = starts_with("Q"), names_to = "question",   
               values_to = "response",values_drop_na = TRUE)  %>%
  #relevel the answer codes to numeric likert scale
  mutate(response = tolower(as.character(response))) %>% #convert to character for ease of recode, all lower to avoid case issues
  mutate(score = recode(response,
                        "strongly disagree" = "-2",  #recodes Q35_
                        "disagree" = "-1",
                        "neither agree/ nor disagree" = "0",
                        "neither agree/nor disagree" = "0",
                        "neither agree / nor disagree" = "0",
                        "neither agree nor disagree" = "0",
                        "neutral" = "0",
                        "agree" = "1",
                        "strongly agree" = "2")) %>%
  mutate(score = as.numeric(score)) %>%
  mutate(curve_def = ifelse(curve_def == "", NA, curve_def)) %>%
  #join questions with prompts from survey (for graph titles / legends)
  left_join(., prompt_key) #joins by "question"


#.tsv used to edit long responses in Excel. 