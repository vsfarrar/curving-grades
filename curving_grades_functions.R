#functions
setwd("~/Documents/projects/curving-grades/")

#Significance Signs: p-value character function
#creates character signs for p-value levels 

sig_signs <- function(p){
  case_when(between(p, 0.05, 0.07) ~  "#",
            between(p, 0.01, 0.05) ~ "*",
            p < 0.01 ~ "**",
            TRUE ~ "")
}

