#Libraries######################################################################
library(tidyverse) #Data manipulation
library(boot) #Bootstrapping confidence intervals
library(quantreg) #CQR
library(dineq) #UQR


#OLS Loop#######################################################################
ols_func <- function(data){
  
  set.seed(123)
  ols_mod <- lm(sbp ~ schlyrs_c + age + age2 + gender + race + southern +
                  mom_ed + dad_ed + year, data = data)
  summary(ols_mod)
  
  #Bootstrap standard error
  ols_boot <- lm.boot(ols_mod, R = 500)
  boot_sum <- summary(ols_boot)
  
  ols_schlyrs_est <- as.numeric(boot_sum$orig.lm$coefficients["schlyrs_c"])
  ols_schlyrs_sd <- as.numeric(boot_sum$stdev.params["schlyrs_c"])
  
  ols_row <- data.frame("Quantile" = -0.1,
                        "Estimate" = ols_schlyrs_est,
                        "Lower" = ols_schlyrs_est - (1.96*ols_schlyrs_sd),
                        "Upper" = ols_schlyrs_est + (1.96*ols_schlyrs_sd),
                        "regtype" = "OLS")
  
  return(ols_row)
  
}

#ols_results <- ols_func(data)


#CQR Loop#######################################################################
cqr_func <- function(data){
  
  result <- data.frame()
  
  for(i in seq(0.01, 0.99, by = 0.01)){
    
    i <- round(i, 2)
    
    set.seed(123)
    cqr_mod <- rq(sbp ~ schlyrs_c + age + age2 + gender + race + southern +
                    mom_ed + dad_ed + year, data = data, tau = i)
    
    
    #Bootstrap standard error
    cqr_boot <- summary.rq(cqr_mod, se = "boot", R = 500)
    cqr_schlyrs_est <- as.numeric(cqr_boot$coefficients["schlyrs_c", "Value"])
    cqr_schlyrs_sd <- as.numeric(cqr_boot$coefficients["schlyrs_c", 
                                                       "Std. Error"])
    
    cqr_row <- data.frame("Quantile" = i,
                          "Estimate" = cqr_schlyrs_est,
                          "Lower" = cqr_schlyrs_est - (1.96*cqr_schlyrs_sd),
                          "Upper" = cqr_schlyrs_est + (1.96*cqr_schlyrs_sd),
                          "regtype" = "CQR")
    
    result <- rbind(result, cqr_row)
    
  }
  
  return(result)
  
}

#cqr_results <- cqr_func(data)


#UQR Loop#######################################################################
uqr_func <- function(data){
  
  result <- data.frame()
  
  for(i in seq(0.01, 0.99, by = 0.01)){
    
    i <- round(i, 2)
    data$rif <- rif(data$sbp, weights = NULL, method = "quantile",
                    quantile = i)
    
    set.seed(123)
    uqr_mod <- lm(rif ~ schlyrs_c + age + age2 + gender + race + southern +
                    mom_ed + dad_ed + year, data = data)
    
    
    #Bootstrap standard error
    uqr_boot <- lm.boot(uqr_mod, R = 500)
    boot_sum <- summary(uqr_boot)
    
    uqr_schlyrs_est <- as.numeric(boot_sum$orig.lm$coefficients["schlyrs_c"])
    uqr_schlyrs_sd <- as.numeric(boot_sum$stdev.params["schlyrs_c"])
    
    uqr_row <- data.frame("Quantile" = i,
                          "Estimate" = uqr_schlyrs_est,
                          "Lower" = uqr_schlyrs_est - (1.96*uqr_schlyrs_sd),
                          "Upper" = uqr_schlyrs_est + (1.96*uqr_schlyrs_sd),
                          "regtype" = "UQR")
    
    result <- rbind(result, uqr_row)
    
  }
  
  return(result)
  
}

#uqr_results <- uqr_func(data)

