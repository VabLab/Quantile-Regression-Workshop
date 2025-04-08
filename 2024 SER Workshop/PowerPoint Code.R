#Load libraries and plot colors#################################################
library(tidyverse) #Data manipulation
library(patchwork) #Printing ggplots together
library(lmtest) #coeftest for robust standard errors
library(sandwich) #vcovHC for robust standard errors
library(boot) #Bootstrapping
library(simpleboot) #Bootstrapping
library(quantreg) #Conditional quantile regression
library(WRTDStidal) #Goodness of fit for CQR models
library(statar) #Bin data into quantiles
library(jtools) #summ function for bootstrapped CIs for CQR
library(dineq) #RIF package
library(readxl) #Read in excel results


#Graphics
library(gganimate)
library(gifski)
library(ggthemes)
library(transformr)
library(msm)
library(grid)
library(ggpubr)
library(magick)
library(ggplot2)


#Colors names
dark_teal <- "#468c8a"
tangerine <- "#FF9300"
dark_rose <- "#f2494c"
blue <- "#0070C0"
black <- "#000000"
gray <- "#999999"


#Load data and format###########################################################
data <- read_rds("QRWorkshop.rds")
summary(data)

#Format categorical variables
data$gender <- factor(data$gender)
data$race <- factor(data$race)
data$year <- factor(data$year)

#Center school years at 12 years of schooling for ease of interpretation
data$schlyrs_c <- data$schlyrs - 12


#Education and SBP scatter plot#################################################
ggplot(data = data, aes(x = schlyrs, y = sbp)) +
  geom_point() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_x_continuous(limits = c(5, 17),
                     breaks = seq(5, 17, by = 1)) +
  scale_y_continuous(limits = c(60, 240),
                     breaks = seq(60, 240, by = 40)) +
  xlab("Total years of schooling") +
  ylab("Systolic Blood Pressure (mmHg)")


#Education and SBP mean plots###################################################
#Scatter plot
ggplot(data = data, aes(x = schlyrs, y = sbp)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = tangerine, se = FALSE) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_x_continuous(limits = c(5, 17),
                     breaks = seq(5, 17, by = 1)) +
  scale_y_continuous(limits = c(60, 240),
                     breaks = seq(60, 240, by = 40)) +
  xlab("Total years of schooling") +
  ylab("Systolic Blood Pressure (mmHg)")


#Creating a dataset of conditional means
df_sbp_means <- data %>%
  group_by(schlyrs) %>%
  summarise(mean_sbp = mean(sbp, na.rm = TRUE))

#Linear regression line approximates the conditional expectation function
ggplot(data = df_sbp_means, aes(x = schlyrs, y = mean_sbp)) +
  geom_point(color = black, size = 2) +
  geom_line(color = black, linewidth = 1) +
  geom_smooth(method = "lm", formula = y ~ x, color = tangerine, se = FALSE) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_x_continuous(limits = c(5, 17),
                     breaks = seq(5, 17, by = 1)) +
  scale_y_continuous(limits = c(60, 240),
                     breaks = seq(60, 240, by = 40)) +
  xlab("Total years of schooling") +
  ylab("Systolic Blood Pressure (mmHg)") 


#OLS models at the mean#########################################################
#Unadjusted
un_ols <- lm(sbp ~ schlyrs_c, data = data)

#School year estimate
summary(un_ols)$coefficients["schlyrs_c", ]


#Adjusted
ols <- lm(sbp ~ schlyrs_c + age + age2 + gender + race + southern + mom_ed +
            dad_ed + year, data = data)

#School year estimate
summary(ols)$coefficients["schlyrs_c", ]


#Education and SBP scatter plot at the median###################################
ggplot(data = data, aes(x = schlyrs, y = sbp)) +
  geom_point() +
  geom_quantile(quantiles = 0.5, formula = y ~ x, 
                color = dark_rose, linewidth = 1) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_x_continuous(limits = c(5, 17),
                     breaks = seq(5, 17, by = 1)) +
  scale_y_continuous(limits = c(60, 240),
                     breaks = seq(60, 240, by = 40)) +
  xlab("Total years of schooling") +
  ylab("Systolic Blood Pressure (mmHg)")


#CQR models at the median#######################################################
#Unadjusted
un_cqr_50 <- rq(sbp ~ schlyrs_c, data = data, tau = 0.5) 
summary(un_cqr_50)$coefficients["schlyrs_c", ]


#Adjusted
cqr_50 <- rq(sbp ~ schlyrs_c + age + age2 + gender + race + southern + mom_ed +
               dad_ed + year, data = data, tau = 0.50) 
##Warning produced because of categorical variables in the model
summary(cqr_50)$coefficients["schlyrs_c", ]


#Education and SBP 25th quantile plots##########################################
#Scatter plot
ggplot(data = data, aes(x = schlyrs, y = sbp)) +
  geom_point() +
  geom_quantile(quantiles = 0.25, formula = y ~ x, color = blue, size = 1) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_x_continuous(limits = c(5, 17),
                     breaks = seq(5, 17, by = 1)) +
  scale_y_continuous(limits = c(60, 240),
                     breaks = seq(60, 240, by = 40)) +
  xlab("Total years of schooling") +
  ylab("Systolic Blood Pressure (mmHg)")


#Creating a dataset of conditional means and quantiles only
df_sbp_quantiles <- data %>%
  group_by(schlyrs) %>%
  summarise(mean_sbp = mean(sbp, na.rm = TRUE),
            sbp_q25 = quantile(sbp, 0.25),
            sbp_q50 = quantile(sbp, 0.5),
            sbp_q75 = quantile(sbp, 0.75))

#Conditional regression line approximates the conditional quantile function
ggplot(data = df_sbp_quantiles, aes(x = schlyrs, y = sbp_q25)) +
  geom_point(color = blue, size = 2) +
  geom_line(color = blue, size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, color = tangerine, se = FALSE) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_x_continuous(limits = c(5, 17),
                     breaks = seq(5, 17, by = 1)) +
  scale_y_continuous(limits = c(60, 240),
                     breaks = seq(60, 240, by = 40)) +
  xlab("Total years of schooling") +
  ylab("Systolic Blood Pressure (mmHg)") 


#CQR models at the 25th quantile################################################
#Unadjusted
un_cqr_25 <- rq(sbp ~ schlyrs_c, data = data, tau = 0.25) 
summary(un_cqr_25)$coefficients["schlyrs_c", ]


#Adjusted
cqr_25 <- rq(sbp ~ schlyrs_c + age + age2 + gender + race + southern + mom_ed +
               dad_ed + year, data = data, tau = 0.25) 
summary(cqr_25)$coefficients["schlyrs_c", ]


#CQR goodness of fit at the 25th quantile#######################################
cqr_int <- rq(sbp ~ 1, data = data, tau = 0.25)
res_cqr_int <- resid(cqr_int)

#Compared to fully adjusted
res_cqr_25 <- resid(cqr_25)
goodfit(res_cqr_25, res_cqr_int, tau = 0.25)


#Compared to exposure only
# res_un_cqr_25 <- resid(un_cqr_25)
# goodfit(res_un_cqr_25, res_cqr_int, tau = 0.25) #Value between 0-1


#How an outlier changes the distribution########################################
data_out <- data
outlier <- data[1, ]
outlier$sbp <- 1000000
data_out <- rbind(data_out, outlier)

mean(data$sbp) #127.63
mean(data_out$sbp) #240.28

quantile(data$sbp, probs = 0.75) #138.5
quantile(data_out$sbp, probs = 0.75) #138.5


#Density distribution###########################################################
mn_sbp <- mean(data$sbp, na.rm = TRUE)
q75_sbp <- quantile(data$sbp, 0.75)

ann1 <- data.frame(x = 149.5, y = 0.027, label = "Q75 = 138.5mmHg")
ann2 <- data.frame(x = 115.5, y = 0.027, label = "Mean = 127.6mmHg")
ggplot(data = data, aes(x = sbp)) +
  geom_density(color = dark_teal, size = 2) +
  geom_vline(xintercept = mn_sbp, color = tangerine, size = 2) +
  geom_vline(xintercept = q75_sbp, color = dark_rose, size = 2) +
  geom_text(data = ann1, aes(x = x, y = y, label = label), 
            color = dark_rose, size = 4, fontface = "bold") +
  geom_text(data = ann2, aes(x = x, y = y, label = label), 
            color = tangerine, size = 4, fontface = "bold") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom") +
  xlab("Systolic blood pressure (mmHg)") +
  ylab("Density")


#Top coding#####################################################################
data <- data %>% 
  dplyr::mutate(sbp_cens = ifelse(sbp > 150, 150, sbp))

#Compare values
mean(data$sbp) #127.63
mean(data$sbp_cens) #125.8

quantile(data$sbp, prob = 0.75) #138.5
quantile(data$sbp_cens, prob = 0.75) #138.5


#Add cutoffs to plot
mn_cens_sbp <- mean(data$sbp_cens, na.rm = TRUE)
q75_cens_sbp <- quantile(data$sbp_cens, 0.75)

ann1 <- data.frame(x = 145.5, y = 0.027, label = "Q75 = 138.5mmHg")
ann2 <- data.frame(x = 118.5, y = 0.027, label = "Mean = 125.8mmHg")

ggplot(data = data, aes(x = sbp_cens)) +
  geom_density(color = dark_teal, size = 2) +
  geom_vline(xintercept = mn_cens_sbp, color = tangerine, size = 2) +
  geom_vline(xintercept = q75_cens_sbp, color = dark_rose, size = 2) +
  geom_text(data = ann1, aes(x = x, y = y, label = label), 
            color = dark_rose, size = 4, fontface = "bold") +
  geom_text(data = ann2, aes(x = x, y = y, label = label), 
            color = tangerine, size = 4, fontface = "bold") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom") +
  xlab("Systolic blood pressure (mmHg)") +
  ylab("Density") +
  labs(title = "All SBP values > 150 coded as 150 (top coding)")


#Monotonic transformations######################################################
data <- data %>% 
  dplyr::mutate(ln_sbp = log(sbp))

#Compare values
mean(data$sbp) #127.63
log(127.63) #4.85
mean(data$ln_sbp) #4.84 

quantile(data$sbp, prob = 0.75) #138.5
log(138.5) #4.93
quantile(data$ln_sbp, prob = 0.75) #4.93


#Add cutoffs to plot
mn_sbp <- mean(data$sbp, na.rm = TRUE)
q75_sbp <- quantile(data$sbp, 0.75)

ann1 <- data.frame(x = 153, y = 0.0245, label = "Q75 = 138.5mmHg")
ann2 <- data.frame(x = 112, y = 0.0245, label = "Mean = 127.6mmHg")

ggplot(data = data, aes(x = sbp)) +
  geom_density(color = dark_teal, size = 2) +
  geom_vline(xintercept = mn_sbp, color = tangerine, size = 2) +
  geom_vline(xintercept = q75_sbp, color = dark_rose, size = 2) +
  geom_text(data = ann1, aes(x = x, y = y, label = label), 
            color = dark_rose, size = 4, fontface = "bold") +
  geom_text(data = ann2, aes(x = x, y = y, label = label), 
            color = tangerine, size = 4, fontface = "bold") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom") +
  xlab("Systolic blood pressure (mmHg)") +
  ylab("Density") +
  labs(title = "a) Actual SBP values")


#Add cutoffs to plot
mn_ln_sbp <- mean(data$ln_sbp, na.rm = TRUE)
q75_ln_sbp <- quantile(data$ln_sbp, 0.75)

ann1_ln <- data.frame(x = 5.05, y = 2.88, label = "Q75 = 4.931 = ln(138.5)")
ann2_ln <- data.frame(x = 4.70, y = 2.88, label = "Mean = 4.838 != ln(127.6)")

ggplot(data = data, aes(x = ln_sbp)) +
  geom_density(color = dark_teal, size = 2) +
  geom_vline(xintercept = mn_ln_sbp, color = tangerine, size = 2) +
  geom_vline(xintercept = q75_ln_sbp, color = dark_rose, size = 2) +
  geom_text(data = ann1_ln, aes(x = x, y = y, label = label), 
            color = dark_rose, size = 4, fontface = "bold") +
  geom_text(data = ann2_ln, aes(x = x, y = y, label = label), 
            color = tangerine, size = 4, fontface = "bold") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom") +
  xlab("Systolic blood pressure (mmHg)") +
  ylab("Density") +
  labs(title = "b) Log transformed SBP values")


#OLS counterfactual plot @ 12 years#############################################
#Subsetting to those with 12 years of education
ols_counter <- data %>%
  dplyr::filter(schlyrs_c == 0) %>%
  dplyr::select(sbp) 

#Determining the 1st-99th quantile of SBP for those with 12 years of schooling
ols_counter$quantile <- xtile(ols_counter$sbp, n = 99)

#Add uniform change to SBP from coefficient estimate
ols_counter$counterfactual <- ols_counter$sbp + 
  summary(ols)$coefficients["schlyrs_c", "Estimate"]

#Long version of data
ols_counter_long <- ols_counter %>%
  pivot_longer(!quantile, names_to = "model", values_to = "sbp") %>%
  dplyr::mutate(model = case_when(model == "sbp" ~ "Factual",
                                  model == "counterfactual" ~ "Counterfactual"))

ols_counter_long$model <- factor(ols_counter_long$model, 
                                 levels = c("Factual", "Counterfactual"))

ggplot(data = ols_counter_long, aes(x = sbp, color = model, 
                                    linetype = model)) +
  geom_density(aes(y = after_stat(density)), linewidth = 1.5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20)) + 
  labs(color = "Distribution",
       linetype = "Distribution",
       x = "Systolic blood pressure (mmHg)",
       y = "Density") +
  scale_color_manual(values = c("Factual" = dark_teal, 
                                "Counterfactual" = tangerine)) +
  scale_linetype_manual(values = c("Factual" = "solid", 
                                   "Counterfactual" = "dashed")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom")


#CQR counterfactual plot @ 12 years#############################################
#Subsetting those with 12 years of schooling
data_12years <- data %>% 
  dplyr::filter(schlyrs_c == 0) %>%
  dplyr::select(sbp)

#Determining the 1st-99th quantile of SBP for those with 12 years of schooling
data_12years$quant <- xtile(data_12years$sbp, n = 99)

#Creating a dataset of only our coefficient estimates (reading in results to save time)
cqr_results <- read_rds("Results/cqr_results.rds")
coef_estimates <- cqr_results %>% 
  dplyr::select(quantile, est) %>%
  rename("quant" = "quantile",
         "coef" = "est")

#Merging coefficient estimates with the SBP data for those with 12 years of schooling
data_12years <- merge(data_12years, coef_estimates, by = "quant")

#Creating a counterfactual SBP value
data_12years$counterfactual_sbp <- data_12years$sbp + data_12years$coef


#Long version of data
data_12years_long <- data_12years %>%
  dplyr::select(-coef) %>%
  pivot_longer(!quant, names_to = "model", values_to = "sbp") %>%
  dplyr::mutate(model = case_when(model == "sbp" ~ "Factual",
                                  model == "counterfactual_sbp" ~ "Counterfactual"))

data_12years_long$model <- factor(data_12years_long$model, 
                                  levels = c("Factual", "Counterfactual"))

ggplot(data = data_12years_long, aes(x = sbp, color = model, 
                                     linetype = model)) +
  geom_density(aes(y = after_stat(density)), linewidth = 1.5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20)) + 
  labs(color = "Distribution",
       linetype = "Distribution",
       x = "Systolic blood pressure (mmHg)",
       y = "Density") +
  scale_color_manual(values = c("Factual" = dark_teal, 
                                "Counterfactual" = tangerine)) +
  scale_linetype_manual(values = c("Factual" = "solid", 
                                   "Counterfactual" = "dashed")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13.5, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom")


#RIF Plot#######################################################################
#q25
data$rif_q25 <- rif(data$sbp, weights = NULL, method = "quantile", 
                    quantile = 0.25)
data$rif_q25_r <- round(data$rif_q25, 2)

ggplot(aes(x = factor(rif_q25_r)), data = data) +
  geom_bar(width = 0.2, fill = dark_rose) + 
  labs(x = "RIF Value",
       y = "Count",
       title = "25th Quantile") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20))


#q50
data$rif_q50 <- rif(data$sbp, weights = NULL, method = "quantile", 
                    quantile = 0.50)
data$rif_q50_r <- round(data$rif_q50, 2)

ggplot(aes(x = factor(rif_q50_r)), data = data) +
  geom_bar(width = 0.2, fill = dark_teal) + 
  labs(x = "RIF Value",
       y = "Count",
       title = "50th Quantile") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20))


#q90
data$rif_q75 <- rif(data$sbp, weights = NULL, method = "quantile", 
                    quantile = 0.75)
data$rif_q75_r <- round(data$rif_q75, 2)

ggplot(aes(x = factor(rif_q75_r)), data = data) +
  geom_bar(width = 0.2, fill = tangerine) + 
  labs(x = "RIF Value",
       y = "Count",
       title = "75th Quantile") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20))


#Combine
rif_vals <- data %>%
  ungroup() %>%
  dplyr::select(id, ends_with("_r")) %>%
  rename("q25" = "rif_q25_r",
         "q50" = "rif_q50_r",
         "q75" = "rif_q75_r") %>%
  pivot_longer(!id, names_to = "Quantile", values_to = "RIF_Values")

ggplot(aes(x = RIF_Values, fill = factor(Quantile)),
       data = rif_vals) +
  geom_bar(width = 5) +
  labs(x = "RIF Value",
       y = "Count",
       fill = "Quantile",
       title = "25th, 50th, and 75th Quantiles") +
  scale_fill_manual(values = c(dark_rose, dark_teal, tangerine)) + 
  scale_x_continuous(breaks = seq(25, 300, by = 25)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20))


#UQR models at the 25th quantile - RIF-OLS######################################
#Find RIF at 25th quantile for every person
data$rif25 <- rif(data$sbp, weights = NULL, method = "quantile",
                  quantile = 0.25)

#Unadjusted
un_uqr_25 <- lm(rif25 ~ schlyrs_c, data = data)
summary(un_uqr_25)$coefficients["schlyrs_c",]

#Adjusted
uqr_25 <- lm(rif25 ~ schlyrs_c + age + age2 + gender + race + southern +
               mom_ed + dad_ed + year, data = data)
summary(uqr_25)$coefficients["schlyrs_c",]


#Robust standard errors (OLS)###################################################
# ols <- lm(sbp ~ schlyrs_c + age + age2 + gender + race + southern + mom_ed +
#             dad_ed + year, data = data)

#Robust t-test
ols_robust <- coeftest(ols, vcov = vcovHC(ols, type = 'HC0'))
ols_robust["schlyrs_c", ]

#Compare to unadjusted standard errors
summary(ols)$coefficients["schlyrs_c", ]


#Bootstrap SE (OLS)#############################################################
# ols <- lm(sbp ~ schlyrs_c + age + age2 + gender + race + southern + mom_ed +
#             dad_ed + year, data = data)

ols_boot <- lm.boot(ols, R = 500)
ols_boot_sum <- summary(ols_boot)

ols_schlyrs_est <- as.numeric(ols_boot_sum$orig.lm$coefficients["schlyrs_c"])
ols_schlyrs_sd <- as.numeric(ols_boot_sum$stdev.params["schlyrs_c"])

cbind(ols_schlyrs_est, ols_schlyrs_sd)

#Compare to robust standard errors
ols_robust["schlyrs_c", ]

#Compare to unadjusted standard errors
summary(ols)$coefficients["schlyrs_c", ]


#Bootstrap SE (CQR)#############################################################
# cqr_25 <- rq(sbp ~ schlyrs_c + age + age2 + gender + race + southern + 
#                mom_ed + dad_ed + year, data = data, tau = 0.25) 

cqr_25_boot <- summary.rq(cqr_25, se = "boot", R = 500)
cqr_25_boot$coefficients["schlyrs_c", ]

#Compare to unadjusted standard errors
summary(cqr_25)$coefficients["schlyrs_c", ]


#Bootstrap SE (UQR)#############################################################
# uqr_25 <- lm(rif25 ~ schlyrs_c + age + age2 + gender + race + southern +
#                mom_ed + dad_ed + year, data = data)

uqr_25_boot <- lm.boot(uqr_25, R = 500)
uqr_25_boot_sum <- summary(uqr_25_boot)

uqr_25_schlyrs_est <- as.numeric(uqr_25_boot_sum$orig.lm$coefficients["schlyrs_c"])
uqr_25_schlyrs_sd <- as.numeric(uqr_25_boot_sum$stdev.params["schlyrs_c"])

cbind(uqr_25_schlyrs_est, uqr_25_schlyrs_sd)

#Compare to unadjusted standard errors
summary(uqr_25)$coefficients["schlyrs_c",]


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


#Read in results################################################################
#Read in paper results to save time and separate by model
###Replace this with function code results from above if running yourself
all_results <- readRDS("Results/AllResults.rds") 
names(all_results) <- c("Quantile", "Estimate", "Lower", "Upper", "regtype")
ols_results <- all_results[all_results$regtype == "OLS", ]

cqr_results <- all_results[all_results$regtype == "CQR", ]
cqr_results$regtype = "CQR"

uqr_results <- all_results[all_results$regtype == "UQR", ]
uqr_results$regtype = "UQR"

#Define axis labels
yaxis <- expression(Delta~SBP~(mmHg))
xaxis <- "Quantile"


#CQR figure#####################################################################
ggplot(data = cqr_results, aes(x = Quantile, y = Estimate)) +
  geom_line(color = dark_teal, linewidth = 1) +
  geom_ribbon(aes(x = Quantile, ymin = Lower, ymax = Upper), 
              alpha = 0.45, fill = dark_teal) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = xaxis,
       y = yaxis,
       title = "Association of educational attainment with systolic blood pressure") +
  scale_x_continuous(breaks = c(1, seq(10, 90, 10), 99),
                     labels = c("q1", "q10", "q20", "q30", "q40", "q50", 
                                "q60", "q70", "q80", "q90", "q99"))


#UQR figure#####################################################################
ggplot(data = uqr_results, aes(x = Quantile, y = Estimate)) +
  geom_line(color = tangerine, linewidth = 1) +
  geom_ribbon(aes(x = Quantile, ymin = Lower, ymax = Upper), 
              alpha = 0.45, fill = tangerine) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = xaxis,
       y = yaxis,
       title = "Association of educational attainment with systolic blood pressure") +
  scale_x_continuous(breaks = c(1, seq(10, 90, 10), 99),
                     labels = c("q1", "q10", "q20", "q30", "q40", "q50", 
                                "q60", "q70", "q80", "q90", "q99"))


#All results figure#############################################################
all_results$regtype <- factor(all_results$regtype, 
                              levels = c("OLS", "CQR", "UQR"))

ggplot(data = all_results %>% filter(regtype != "OLS"),
       aes(x = Quantile, y = Estimate, group = regtype,
           color = regtype, fill = regtype)) +
  geom_line(alpha = 50, linewidth = 0.75) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.27, color = NA) +
  geom_point(data = all_results %>% filter(regtype == "OLS")) +
  geom_errorbar(data = all_results %>% filter(regtype == "OLS"),
                aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = -2, color = gray, linetype = "dashed") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 10))  +
  scale_color_manual(breaks = c("OLS", "CQR", "UQR"),
                     labels = c("OLS", "CQR", "UQR"),
                     values = c(dark_rose, dark_teal, tangerine)) +
  scale_fill_manual(breaks = c("OLS", "CQR", "UQR"),
                    labels = c("OLS", "CQR", "UQR"),
                    values = c(dark_rose, dark_teal, tangerine))  +
  scale_x_continuous(limits = c(-10, 99),
                     breaks = c(-6, 1, 10, 20, 30, 40, 50,
                                60, 70, 80, 90, 99),
                     labels = c("OLS", "q1", "q10", "q20", "q30", "q40",
                                "q50", "q60", "q70", "q80", "q90", "q99")) +
  labs(x = "",
       y = yaxis,
       color = "Model", group = "Model", fill = "Model")


#UQR counterfactual plot########################################################
#Counterfactual function (creates dataset for plotting)
counter_data <- function(results, data){
  
  coef <- results %>%
    dplyr::filter(regtype == "UQR") %>% #Change model here if you want to create a CQR plot
    dplyr::select(Quantile, Estimate) %>%
    rename("quant" = "Quantile",
           "coef" = "Estimate")
  coef$quant <- round(coef$quant, 1) #Need to round for merging
  
  red <- data %>%
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
  
  cf_plot <- rbind(f, cf)
  cf_plot$group <- relevel(factor(cf_plot$group), ref = "Factual")
  
  return(cf_plot)
  
}

#Create counterfactual data
uqr_counter <- counter_data(all_results, data)
summary(uqr_counter)


#Look at distribution characteristics
factual <- uqr_counter %>% 
  dplyr::filter(group == "Factual")

counterfactual <- uqr_counter %>%
  dplyr::filter(group == "Counterfactual")

quantile(factual$sbp, probs = 0.50)
quantile(counterfactual$sbp, probs = 0.50)

mean(factual$sbp)
mean(counterfactual$sbp)

var(factual$sbp)
var(counterfactual$sbp)


#Plot distributions
ggplot(data = uqr_counter, aes(x = sbp, color = group, linetype = group)) +
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
  scale_color_manual(values = c("Factual" = dark_teal, 
                                "Counterfactual" = tangerine)) +
  scale_linetype_manual(values = c("Factual" = "solid", 
                                   "Counterfactual" = "dashed"))


