# Smoking-Cessation-BMI-Causal-Effect
## Estimating the Causal Effect of Smoking Cessation on 5-Year BMI Change (IPW + G-Formula)

# Overview👩🏽‍💻
This analysis estimates the causal effect of smoking cessation on 5-year BMI percentage change using observational longitudinal data. Confounding is addressed through logistic regression, propensity score estimation, inverse probability weighting (IPW), and the G-formula with bootstrapping.

# Dataset Variables 📊
- Smoking Cessation Status (Exposure)
- BMI % Change over 5 Years (Outcome)
- Age, Sex, Education Level
- Number of Cigarettes per Day
- Cardiovascular Disease
- Dementia
- Diuretic Use
- Baseline BMI (categorised)

# Methodological Steps 🔨
- Descriptive Statistics & Group Comparison
- Unadjusted and Adjusted Linear Regression
- Logistic Regression for Propensity Score Estimation
- Positivity and Covariate Balance Assessment
- IPW-Weighted Outcome Regression
- G-Formula Estimation to simulate outcomes:
  1. If everyone continued smoking
  2. If everyone quit
- Bootstrapping (500 iterations) to compute confidence intervals
