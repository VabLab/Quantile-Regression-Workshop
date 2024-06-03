library(tidyverse)
library(labelled)

#------------------------------------------------------------------------------
# Load data
#------------------------------------------------------------------------------
trk <- read_rds("trk2018tr_r.rds") #HRS Tracker file
trk_sub <- trk %>%
  dplyr::select(hhid, pn, firstiw, birthyr, gender, hispanic, race, schlyrs,
                usborn)
trk_sub$hhidpn <- paste0(trk_sub$hhid, trk_sub$pn)


rand <- read_rds("randhrs1992_2018v1.rds") #HRS RAND file
rand_sub <- rand %>%
  dplyr::select(hhid, pn, rabplace, rameduc, rafeduc)
rand_sub$hhidpn <- paste0(rand_sub$hhid, rand_sub$pn)


out <- read_rds("Data/SBP_Measures.rds") #Resulting dataset from "Pull SBP Measures.R" script

#Merge data
data <- merge(trk_sub, rand_sub, by = c("hhid", "pn", "hhidpn"))
data <- merge(data, out, by = c("hhid", "pn", "hhidpn"))

var_label(data) <- NULL
val_labels(data) <- NULL
data <- remove_attributes(data, "format.stata")


#------------------------------------------------------------------------------
# Covariate cleaning
#------------------------------------------------------------------------------
#Birthplace
table(data$rabplace, useNA = "always")
data <- data %>%
  dplyr::mutate(birthplc = case_when(rabplace == 1 ~ "Not Southern",
                                     rabplace == 2 ~ "Not Southern",
                                     rabplace == 3 ~ "Not Southern",
                                     rabplace == 4 ~ "Not Southern",
                                     rabplace == 5 ~ "Southern",
                                     rabplace == 6 ~ "Southern",
                                     rabplace == 7 ~ "Southern",
                                     rabplace == 8 ~ "Not Southern",
                                     rabplace == 9 ~ "Not Southern",
                                     rabplace == 10 ~ "NA",
                                     rabplace == 11 ~ "Not in US",
                                     is.na(rabplace) ~ "NA"))
data$birthplc <- ifelse(data$birthplc == "NA", NA, data$birthplc)
data$birthplc <- factor(data$birthplc)

#Education
table(data$schlyrs, useNA = "always")
data$schlyrs <- ifelse(data$schlyrs == 99, NA, data$schlyrs)

#Truncate education (5-17)
data$schlyrs <- ifelse(data$schlyrs < 5, 5, data$schlyrs) #Individual
data$rameduc <- ifelse(data$rameduc < 5, 5, data$rameduc) #Mom
data$rafeduc <- ifelse(data$rafeduc < 5, 5, data$rafeduc) #Dad


#Gender
data$gender <- ifelse(data$gender == 1, "Male", "Female")
data$gender <- factor(data$gender)
table(data$gender, useNA = "always") #1 missing

#Race
data <- data %>%
  dplyr::mutate(race_adj = case_when(hispanic == 1 ~ "Latinx", #Latinx
                                     hispanic == 2 ~ "Latinx", 
                                     hispanic == 3 ~ "Latinx",
                                     race == 1 ~ "White", #White
                                     race == 2 ~ "Black", #Black
                                     race == 0 ~ "NA", #Missing/Other
                                     race == 7 ~ "NA"))
data$race_adj <- ifelse(data$race_adj == "NA", NA, data$race_adj)
table(data$race_adj, useNA = "always")
data$race_adj <- factor(data$race_adj)

#Age
data$age <- data$year - data$birthyr
data$age2 <- data$age^2
range(data$age) #19-105


#Remove old variables
final_data <- data %>%
  dplyr::select(-c(race, hispanic, usborn, rabplace)) %>%
  rename("race" = "race_adj")


#-------------------------------------------------------------------------------
# Inclusion criteria and formatting
#-------------------------------------------------------------------------------
final_data <- final_data %>%
  dplyr::filter(firstiw >= 1998) %>%
  dplyr::filter(birthplc != "Not in US") %>%
  dplyr::filter(age >= 51) %>%
  dplyr::select(hhidpn, age, age2, gender, race, birthplc, schlyrs,
                rameduc, rafeduc, year, sbp) %>%
  rename("id" = "hhidpn",
         "southern" = "birthplc",
         "mom_ed" = "rameduc",
         "dad_ed" = "rafeduc")

#Complete cases
fin <- final_data[complete.cases(final_data), ]

summary(fin)

#write_rds(fin, "Data/QRWorkshop.rds")
#write_dta(fin, "Data/QRWorkshop.dta")

