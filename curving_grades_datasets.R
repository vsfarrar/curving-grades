#NUTRITION 10 Data 

#packages
library(tidyverse)
library(plotrix) #for std.error
library(broom)
library(forcats)
library(RColorBrewer)
library(cowplot)
library(ggsignif)

#data
setwd("~/Documents/projects/curving-grades/")
NUT10_likert <- read.csv("NUT10_Likert_only.csv")
NUT10supp <- read.csv("NUT10_remaining_surveys.csv") #4 extra questions added after survey launched
prompt_key <-read.delim("curving_questions_prompt_key_tabdelim.txt",sep = "\t")

#demographic data
nut10demo <- read.csv(file = "~/Documents/projects/curving-grades/nut10_wrangled_2020-12-10.csv")

#majors list by STEM field
majors_list <-read.csv(file = "~/Documents/projects/curving-grades/majors_list.csv")

#DATA FILTERING, CLEANING ####

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

#remove duplicates from the data 

sum(duplicated(likert$sid)) #21 have duplicates in likert dataset
sum(duplicated(supp$sid)) #only 2 sid with duplicates in supplemental

supp_unique <- distinct(supp, sid, .keep_all = TRUE) #distinct keeps the first row - so first survey attempt. 
likert_unique <-distinct(likert, sid, .keep_all = TRUE)

#join datasets together
  #join all likert data (n = 590) with all those who finished the supplemental survey (n = 271)

full_data <- 
  left_join(likert_unique, supp_unique, by = "sid")

#GET CONSENSUS ON CURVING DEFINITION 
#definitions are either in Q79 or Q11 
  #students who answered in original survey (Q79) or appended survey (Q11)

#set white space to NA (lots of whitespace in Q79 and Q11)
full_data[full_data == ""] <- NA

#get consensus of Q11 and Q79 
  #if one NA, takes non-NA value between two 
  #if neither NA and there is a mismatch, Q79 from original survey will be chosen (first in order)
full_data$curve_def <-
  coalesce(full_data$Q79, full_data$Q11)

#convert curve_definitions to more interpretable levels 
  #grab levels 
  full_curve_defs <- levels(as.factor(full_data$curve_def))
  #recode curve_def
    full_data <- 
    full_data %>%
    mutate(curve_def = case_when(
      curve_def == full_curve_defs[1] ~ "ranked", 
      curve_def == full_curve_defs[2] ~ "scaled_pass", 
      curve_def == full_curve_defs[3] ~ "scaled_avg",
      TRUE ~  curve_def
    ))
    
#Add Demographic Data to full_data ####
nut10demo$SID <- as.character(nut10demo$SID) #prepare for join

#join together demographics data and likert responses
full_data_demo <- left_join(full_data, nut10demo, by = c("sid" = "SID"))

#CONVERT LIKERT Qs to NUMERIC ####

#center neutrals at 0 
#-2 = Strongly Disagree, -1 = Disagree, 0 = Neutral, +1 = Agree, +2 = Strongly Disagree 

lik_long <-
  full_data_demo %>%
  #select likert Qs only
  select(-Q45_1, -Q47, -Q79, -Q75_15, -Q9, -Q11_4_TEXT) %>% #other mult.choice, Q79 free response, Q75_15 blank #rename Q11 curve definition so that it does not get pivoted
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
  mutate(curve_def = factor(curve_def, levels = c("ranked", "scaled_avg",
                                                  "scaled_pass", "Other"))) %>%
  #join questions with prompts from survey (for graph titles / legends)
  left_join(., prompt_key) #joins by "question"

 

#create likert wide to send to Natalia with numeric-converted Likerts (with cleaned data only)
likert_wide <-
lik_long %>%
  distinct(sid,question, .keep_all = TRUE) %>%
  select(sid, question, score) %>%
  group_by(sid) %>%
  tidyr::pivot_wider(names_from = question, values_from = score) 

#export file
#write.csv(likert_wide, "~/Downloads/2020-11-17_NUT10_cleaned_likert_questions.csv")


