library(tidyverse)

rand <- readRDS("randhrs1992_2018v1.rds") #HRS RAND dataset

#------------------------------------------------------------------------------
# SBP
#------------------------------------------------------------------------------
sbp <- rand %>%
  dplyr::select(hhid, pn, r8bpsys, r9bpsys, r10bpsys, r11bpsys, r12bpsys,
                r13bpsys, r14bpsys)
sbp$hhidpn <- paste0(sbp$hhid, sbp$pn)
sbp <- sbp %>%
  dplyr::select(hhid, pn, hhidpn, r8bpsys, r9bpsys, r10bpsys, r11bpsys,
                r12bpsys, r13bpsys, r14bpsys)


#Find number of recorded values for each person
for(i in 1:nrow(sbp)){
  
  sbp[i, 11] <- length(which(sbp[i, 4:10] > 0) == TRUE)
  
}
colnames(sbp)[11] <- "count"

#Distribution of number of values recorded
table(sbp$count) #A lot have none


#Remove people with no recorded values
sbp_clean <- sbp %>%
  dplyr::filter(count > 0)

#Find first value
for(i in 1:nrow(sbp_clean)){
  
  place <- which(sbp_clean[i, 4:10] > 0)[1]
  sbp_clean[i, 12] <- sbp_clean[i, place + 3]
  sbp_clean[i, 13] <- place
  
}

colnames(sbp_clean)[12] <- "sbp"
colnames(sbp_clean)[13] <- "wave"

sbp_clean$wave <- case_when(sbp_clean$wave == 1 ~ 8,
                            sbp_clean$wave == 2 ~ 9,
                            sbp_clean$wave == 3 ~ 10,
                            sbp_clean$wave == 4 ~ 11,
                            sbp_clean$wave == 5 ~ 12,
                            sbp_clean$wave == 6 ~ 13,
                            sbp_clean$wave == 7 ~ 14)

sbp_clean$year <- case_when(sbp_clean$wave == 8 ~ 2006,
                            sbp_clean$wave == 9 ~ 2008,
                            sbp_clean$wave == 10 ~ 2010,
                            sbp_clean$wave == 11 ~ 2012,
                            sbp_clean$wave == 12 ~ 2014,
                            sbp_clean$wave == 13 ~ 2016,
                            sbp_clean$wave == 14 ~ 2018)

sbp_fin <- sbp_clean %>%
  dplyr::select(hhid, pn, hhidpn, sbp, wave, year)
#write_rds(sbp_fin, "SBP_Measures.rds")

