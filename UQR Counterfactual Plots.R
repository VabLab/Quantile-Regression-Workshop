#------------------------------------------------------------------------------
# Create counter factual plot using coefficients from the unconditional
# quantile regression models

# By: Jilly Hebert and Aayush Khadka
# 
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Load libraries and datasets
#------------------------------------------------------------------------------
library(radiant.data)
library(ggthemes)

data <- read_rds("QRWorkshop.rds")
results <- read_rds("uqr_results.rds") #UQR Results
names(results) <- c("Quantile", "Estimate", "Lower", "Upper")
results$Quantile <- round((results$Quantile/100), 2) #Bounded (0,1)

#------------------------------------------------------------------------------
# Create counterfactual plots
#------------------------------------------------------------------------------
#Create dataset using coefficients from the UQR results
coef <- results %>%
  #dplyr::filter(regtype == "UQR") %>%
  dplyr::select(Quantile, Estimate) %>%
  rename("quant" = "Quantile",
         "coef" = "Estimate")
coef$quant <- round(coef$quant * 100, 1) #Need to round for merging

red <- data %>%
  dplyr::select(sbp) %>%
  mutate(quant = xtile(sbp, n = 99))

merg <- right_join(coef, red, by = "quant")
sum(is.na(merg)) #0

cf <- merg %>%
  mutate(sbp = sbp + coef,
         group = "Counterfactual") %>%
  select(group, sbp)

f <- merg %>%
  mutate(group = "Factual") %>%
  select(group, sbp)

cf_data <- rbind(f, cf)
cf_data$group <- relevel(factor(cf_data$group), ref = "Factual")


#Plot
uqr_cf_plot <- ggplot(data = cf_data, aes(x = sbp, color = group,
                                          linetype = group)) +
  geom_density(aes(y = after_stat(density)), linewidth = 1) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20)) + 
  labs(color = "Distribution",
       linetype = "Distribution",
       x = "SBP (mmHg)",
       y = "Density") +
  scale_color_colorblind()

uqr_cf_plot


#------------------------------------------------------------------------------
# Function
#------------------------------------------------------------------------------
#Counterfactual dataset function
counter_data <- function(result_data, dataset){
  
  coef <- result_data %>%
    #dplyr::filter(regtype == "UQR") %>%
    dplyr::select(Quantile, Estimate) %>%
    rename("quant" = "Quantile",
           "coef" = "Estimate")
  coef$quant <- round(coef$quant * 100, 1) #Need to round for merging
  
  red <- dataset %>%
    dplyr::select(sbp) %>%
    mutate(quant = xtile(sbp, n = 99))
  
  merg <- right_join(coef, red, by = "quant")
  
  cf <- merg %>%
    mutate(sbp = sbp + coef,
           group = "Counterfactual") %>%
    select(group, sbp)
  
  f <- merg %>%
    mutate(group = "Factual") %>%
    select(group, sbp)
  
  cf_data <- rbind(f, cf)
  cf_data$group <- relevel(factor(cf_data$group), ref = "Factual")
  
  return(cf_data)
  
}

cf_data_fun <- counter_data(results, data)




