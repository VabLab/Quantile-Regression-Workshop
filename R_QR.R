library(tidyverse)
library(boot)
library(quantreg)
library(dineq)
library(simpleboot)

#------------------------------------------------------------------------------
# OLS
#------------------------------------------------------------------------------
ols_mod <- lm(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
                mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18, 
              data = data)
summary(ols_mod)

#Bootstrap standard error
ols_boot <- lm.boot(ols_mod, R = 500)
boot_sum <- summary(ols_boot)

ols_schlyrs_est <- as.numeric(boot_sum$orig.lm$coefficients["schlyrs"])
ols_schlyrs_sd <- as.numeric(boot_sum$stdev.params["schlyrs"])

ols_row <- data.frame("Quantile" = -0.1,
                      "Estimate" = ols_schlyrs_est,
                      "Lower" = ols_schlyrs_est - (1.96*ols_schlyrs_sd),
                      "Upper" = ols_schlyrs_est + (1.96*ols_schlyrs_sd),
                      "regtype" = "OLS")


#------------------------------------------------------------------------------
# CQR
#------------------------------------------------------------------------------
#Function
cqr_func <- function(data){
  
  conditional_results <- data.frame()
  
  for(i in seq(0.1, 0.9, by = 0.01)){
    
    i <- round(i, 2)
    
    con <- rq(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
                mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18,
              data = data, tau = i) 
    coef <- summary(con, se = "boot", bsmethod = "mcmb", R = 500) #Bootstrapped SE
    
    #Bootstrap ci
    boot <- boot.rq(cbind(1, data$schlyrs, data$age, data$age2, data$female,
                          data$black, data$latinx, data$southern, data$mom_ed,
                          data$dad_ed, data$y08, data$y10, data$y12, data$y14,
                          data$y16, data$y18),
                    data$sbp, tau = i, R = 500) #Takes a little while to run
    ci <- t(apply(boot$B, 2, quantile, c(0.025, 0.975)))
    
    cqr_est <- cbind(i, ci, coef$coefficient)
    rownames(cqr_est) <- rownames(coef$coefficient)
    
    schlyr_est <- data.frame(t(cqr_est["schlyrs", 1:4]))
    
    conditional_results <- rbind(conditional_results, schlyr_est)
    
  }
  
  names(conditional_results) <- c("Quantile", "Lower", "Upper",
                                  "Estimate")
  conditional_results <- conditional_results %>%
    dplyr::select(Quantile, Estimate, Lower, Upper)
  conditional_results$regtype <- "CQR"
  
  return(conditional_results)
  
}

cqr_results <- cqr_func(data)
#Nonunique solutions warning prompted by categorical variables in model

#------------------------------------------------------------------------------
# UQR - RIF
#------------------------------------------------------------------------------
#Function
uqr_func <- function(data){
  
  unconditional_results <- data.frame()
  
  for(i in seq(0.1, 0.9, by = 0.01)){
    
    i <- round(i, 2)
    
    data$rif_sbp <- rif(data$sbp, weights = NULL, method = "quantile",
                        quantile = i)
    uqr <- lm(rif_sbp ~ schlyrs + age + age2 + female + black + latinx +
                southern + mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18, 
              data = data)
    
    #Bootstrap standard error
    uqr_boot <- lm.boot(uqr, R = 500)
    boot_sum <- summary(uqr_boot)
    
    uqr_schlyrs_est <- as.numeric(boot_sum$orig.lm$coefficients["schlyrs"])
    uqr_schlyrs_sd <- as.numeric(boot_sum$stdev.params["schlyrs"])
    
    uqr_row <- data.frame("Quantile" = i,
                          "Estimate" = uqr_schlyrs_est,
                          "Lower" = uqr_schlyrs_est - (1.96*uqr_schlyrs_sd),
                          "Upper" = uqr_schlyrs_est + (1.96*uqr_schlyrs_sd),
                          "regtype" = "UQR")
    
    unconditional_results <- rbind(unconditional_results, uqr_row)
    
  }
  
  return(unconditional_results)
  
}

uqr_results <- uqr_func(data)


