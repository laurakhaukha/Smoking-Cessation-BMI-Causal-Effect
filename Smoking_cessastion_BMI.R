# -----------------------------------------------------------------------------#
# Created by Laura Lisa Khaukha
# Please Note: Load all the packages beforehand

# -----------------------------------------------------------------------------#
#  #Packages installed, corresponding libraries used
# -----------------------------------------------------------------------------#
#Installing the relevant packages
install.packages("tidyverse")
install.packages("tableone")
install.packages("haven")
install.packages("boot")
install.packages("car")
install.packages("corrplot")
install.packages('nortest')
install.packages('splines')
install.packages("pROC")
install.packages("skimr")
install.packages("rstatix")
install.packages("flextable")
install.packages("caret")
install.packages("ggplot2")
install.packages("survey")
install.packages("ipw")
install.packages("ggdag")
install.packages("lmtest")
install.packages("zoo")
install.packages("boot")

# Loading the corresponding libraries
library(tidyverse)
library(tableone)     
library(haven)
library(boot)
library(car)
library(corrplot)
library(nortest)
library(splines)
library(pROC)
library(rstatix)
library(flextable)
library(caret)
library(ipw)
library(survey)
library(ggplot2)
library(ggdag)
library(lmtest)
library(zoo)
library(boot)


# -----------------------------------------------------------------------------#
#          1 # Data Exploration & Summary Statistics
# -----------------------------------------------------------------------------#
# Loading the data-set into R, using the same working directory
df <- read_dta("...")

######## Initial Data Exploration of data-set
view(df)
names(df)
str(df)       
summary(df)    

########### Creating a TableOne to establish descriptive statistics of the dataset
cv_b <- c("age", "bmi", "bmi_ch_percent", "n_cigarettes")
catv_b <- c("sex", "education_d", "CVD", "dementia", "diuretics")

#Generating TableONE 
dt <- CreateTableOne(vars = c(cv_b, catv_b), strata = "smoking_cessation",data = df)
# Printing the resulting table
print(dt, showAllLevels = TRUE)

# Converting dt to a data frame, in preperation for export
dt_export <- as.data.frame(print(dt, quote = FALSE, noSpaces = TRUE))
write.csv(dt_export, file = "T2.csv", row.names = TRUE) # Exporting Table for display in report 

#Distribution Analysis via histograms of continuous variables (Detecting potential skewness & visually evident outliers)
#Assigning variable of list of relevant variables
nvb <- c("age", "n_cigarettes", "education_d", "bmi", "bmi_ch_percent", "diabetes_genetic_score")

#Plotting histograms for continuous variables, via a for loop  
for (var in nvb) {
  p_hist <- ggplot(df, aes_string(x = var)) +
    geom_histogram(fill = "lightpink", color = "black", bins = 30) +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  
  print(p_hist)
}
#Observation:Each numeric variable is normally distributed, no outliers or skewness found
# -----------------------------------------------------------------------------#
#               2 # Outcome Regression & Assumption Analysis
# -----------------------------------------------------------------------------#
#Generating a simple, unadjusted linear regression without any adjustments
lm_s <- lm(bmi_ch_percent ~ smoking_cessation, data = df)
summary(lm_s)
confint(lm_s)

# Linear regression adjusting for potential cofounders/covarients #factorised education
lm_a <- lm(bmi_ch_percent ~ smoking_cessation + age + sex + 
             as.factor(education) + CVD + dementia + diuretics + 
             bmi + n_cigarettes, data = df)
summary(lm_a)

#########Checking Linear regression assumptions 
#Checking Multi-colinearity Assumption
vif(lm_a) 
#Observation:No Multi-colinearity detected, assumption satisfied

#Check Linearity Assumption via a Residuals vs Fitted plot
ggplot(data = data.frame(Fitted = fitted(lm_a), Residuals = residuals(lm_a)), aes(x = Fitted, y = Residuals)) +
  geom_point(color = 'blue', alpha = 0.4) +
  geom_hline(yintercept = 0, color = "darkred") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
#Observation: linearity assumptions of the residuals is satisfied

#Creating a residual column, relevant for subsequent code
df$resid <- rstudent(lm_a)

#Plotting the Histogram of residuals
hist(df$resid)
#Observation Residuals follow a normal distribution, assumption satisfied

#Generating a Q-Q Plot of the residuals 
qqnorm(df$resid)
qqline(df$resid, col = "blue")
#Observation:Q-Q Plot verify that residuals are normally distributed

#Checking for heteroscedasticity via the Breusch-Pagan test 
bptest(lm_a) 
#Observation: No heteroscedasticity detected,homoscedasticity assumption is satisfied

#Looking at the independence of errors assumption by plotting a Residuals vs Observation Index
ggplot(data = data.frame(Index = 1:length(residuals(lm_a)), Residuals = residuals(lm_a))) +
  geom_point(aes(x = Index, y = Residuals), color = 'blue', alpha = 0.4) +
  geom_hline(yintercept = 0, color = "darkred") +
  labs(title = "Residuals vs Observation Index", x = "Observation Index", y = "Residuals") +
  theme_minimal()
#Observation: Assumption of independence of residuals seems to be satisfied.

#Calculating the Predicted values (yhat) 
df$yhat <- predict(lm_a)
summary(df$yhat)

#Plotting and Visualizing the predicted values (yhat) against the observed values
ggplot(df, aes(x = yhat, y = bmi_ch_percent)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(
    title = "Predicted vs Observed BMI Change",
    x = "Predicted BMI Change (%)",
    y = "Observed BMI Change (%)"
  ) +
  theme_minimal()

####### Checking for outliers, briefly
#Studentised residuals
df$resid <- rstudent(lm_a)  
summary(df$resid)

# -----------------------------------------------------------------------------#
#          3  #Inverse Probability Weighting (IPW) & DAGS
# -----------------------------------------------------------------------------#
names(df) #Re-checking variable names
#Plotting a Logistic regression of the variables against  the binary outcome smoking_cessation
Lgs <- glm(smoking_cessation ~ sex + age + as.factor(education)  #Education level as factor, as it is categorical
           + n_cigarettes + CVD +  dementia + diuretics + bmi, family = binomial(), data = df)
#Viewing the model summary
summary(Lgs)
#Viewing the model summary in a tidy format
tidy_Lgs  <- tidy(Lgs , exponentiate = TRUE, conf.int = TRUE)
print(tidy_Lgs)

#########Checking Logistic Regression assumptions 
#Checking Multi-colineariy assumption
vif(Lgs)
#Observation:None of the variables violated Multi-colineariy assumption

#Checking Linearity assumption for each continuous variable via LOESS
lbwt <- 0.75  # Setting the bandwidth for subsequent LOESS smoothing
n_v_b <- c("age", "bmi", "bmi_ch_percent", "n_cigarettes")

#Predicting the Log odds in preparation for the plotting of LOESS 
df$flodds <- predict(Lgs, type = "link")

#Plotting a LOESS graph of each continuous predictor via for loop,to check linearity assumption of predictors for logistic regression 
for (var in n_v_b) {
  p <- ggplot(df, aes_string(x = var, y = "flodds")) + geom_point(alpha = 0.5, color = "blue") +  
    geom_smooth(method = "loess", color = "darkred", se = FALSE, span = lbwt) + labs(itle = paste("LOESS Smoothing: Fitted Log-Odds vs", var), x = var, y = "Fitted Log-Odds" ) + 
    theme_minimal() 
  print(p)
}

#Observation: All variables are Linear, except bmi, indicating a violation of linearity assumption
#Categorizing BMI into clinically significant groups(Likewise seen in PART A, upstream the script)
df$bmi_category <- cut(df$bmi, breaks = c(-Inf, 18.5, 24.9, 29.9, Inf), labels = c("Underweight", "Normal", "Overweight", "Obesity"))

#########
#Re-plotting the Logistic regression
Lmg_c <- glm(smoking_cessation ~ sex + age + as.factor(education)  #Education level as factor, as it is categorical
             + n_cigarettes + CVD +  dementia + diuretics + bmi_category, family = binomial(), data = df)
summary(Lmg_c)

tidy_Lmg_c <- tidy(Lmg_c, exponentiate = TRUE, conf.int = TRUE)
print(tidy_Lmg_c)

# Estimating the propensity scores (PS) 
ps_lm <- glm(smoking_cessation ~ age + sex + education + CVD + dementia + diuretics + bmi_category, family = binomial(), data = df)
df$ps <- predict(ps_lm, type = "response")

#Checking for Overlap via the distribution of propensity scores by Smoking Cessation  
ggplot(df, aes(x = ps, fill = as.factor(smoking_cessation))) + geom_density(alpha = 0.5) +labs(title = "Propensity Score Distribution by Smoking Cessation", x = "Propensity Score",
                                                                                                        fill = "Smoking Cessation" ) + scale_fill_manual(values = c("skyblue", "pink"), labels = c("Did Not Quit (0)", "Quit (1)") ) + theme_minimal()

#Applying, summarizing and visualizing IPW weights
df$ipw <- ifelse( df$smoking_cessation == 1, 1 / df$ps, 1 / (1 -  df$ps))
summary(df$ipw)
hist(df$ipw, main = "Histogram of IPW Weights", xlab = "Weights")

#Creating weighted regression and viewing the its corresponding summary
ipw_model <- svydesign(ids = ~1, weights = ~ipw, data = df)
lm_ipw <- svyglm(bmi_ch_percent ~ smoking_cessation, design = ipw_model)
summary(lm_ipw)
confint(lm_ipw)

# -----------------------------------------------------------------------------#
#           #G-Formula & Bootstrap: Calculating average causal effect (ACE)
# -----------------------------------------------------------------------------#
mean_observed_test <- mean(df$bmi_ch_percent, na.rm = TRUE)
print(mean_observed_test)

#creating a function that calculates the average causal effect (ACE) of 
standardizationn <- function(data, indices) {
  #Re-sampling the data
  d <- data[indices, ]
  
  #Calculating the observed means
  mean_observed <- mean(d$bmi_ch_percent, na.rm = TRUE)
  
  #Creating counterfactual data-sets scenarios
  d0 <- d
  d0$smoking_cessation <- 0
  d0$bmi_ch_percent <- NA
  
  d1 <- d
  d1$smoking_cessation <- 1
  d1$bmi_ch_percent <- NA
  
  #Combining both observed  and the counterfactual data-sets 
  d_combined <- rbind(d, d0, d1)
  
  #Re-plotting a logistic regression model on the original data only (interv = -1)
  f <- glm(bmi_ch_percent ~ smoking_cessation + age + sex + education + CVD + dementia + diuretics + bmi,
           data = d_combined[d_combined$smoking_cessation %in% c(0, 1), ])
  
  #Predicting the mean outcomes for each scenario
  d_combined$predicted_meanY <- predict(f, newdata = d_combined)
  
  mean_no_treatment <- mean(d_combined$predicted_meanY[d_combined$smoking_cessation == 0], na.rm = TRUE)
  mean_treatment <- mean(d_combined$predicted_meanY[d_combined$smoking_cessation == 1], na.rm = TRUE)
  
  # Printing the means and the average causal effect (treatment - no treatment)
  return(c(mean_observed, mean_no_treatment, mean_treatment, mean_treatment - mean_no_treatment))
}

############# Bootstrap to estimate confidence intervals
#Perform bootstrapping with the standardization function
f_r <- boot(data = df, statistic = standardizationn, R = 500)

#Calculating the standard errors and confidence intervals from the bootstrapped results
se_ <- apply(f_r$t, 2, sd)
mean_ <- f_r$t0
ll_ <- mean_ - qnorm(0.975) * se_
ul_ <- mean_ + qnorm(0.975) * se_

#Creating and combining resulting into data-frame
bs_df <- data.frame(Scenario = c("Observed", "No Treatment", "Treatment", "Treatment - No Treatment"), 
                    Mean = mean_,
                    SE = se_,
                    Lower_CI = ll_, Upper_CI = ul_)

#Printing the bootstrapping results
print(bs_df) 
