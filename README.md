# Gaming Addiction and Academic Performance in University Students

R analysis for a cross-sectional study examining the prevalence of Internet Gaming
Disorder (IGD) and its association with academic performance (GPA) among 485 university
students across six health sciences colleges in Egypt, using ordinal logistic regression.

## Contents

- **Data Preprocessing:** Cleaning and standardization of Arabic variable names to English,
  recoding of numerically-encoded categorical variables into labeled factors.

- **Baseline Characteristics:** Cross-tabulation of all sociodemographic and behavioral
  variables stratified by GPA category, reported as n (%) with an overall column.

- **IGD-20 Psychometric Analysis:** Computation of mean ± SD and Cronbach's α for each of
  the six IGD-20 subscales (Salience, Mood, Tolerance, Withdrawal, Conflict, Relapse).

- **Addiction Prevalence:** Frequency and 95% confidence interval of Internet Gaming
  Disorder classification based on the IGD-20 total score threshold.

- **Unadjusted Ordinal Logistic Regression:** Univariable cumulative link models (CLM) for
  each predictor against GPA, reported as unadjusted odds ratios (OR) with 95% confidence
  intervals and bold-highlighted p-values.

- **Multivariable Ordinal Logistic Regression:** Two nested adjusted models fitted — Model 1
  (age + living arrangement + college + mental illness history + smoking) and Model 2
  (age + college + smoking, parsimonious) — with nested model comparison via likelihood ratio
  test and model selection using AIC and BIC.

- **Model Diagnostics:** Proportional odds assumption assessed; scale/dispersion effects tested;
  multicollinearity evaluated using Generalized VIF (GVIF) via a surrogate linear model;
  overall model performance summarized using Nagelkerke's pseudo-R², AICc, RMSE, and Sigma.

- **Publication-Ready Outputs:** Merged regression table (Unadjusted + Model 1 + Model 2);
  forest plot of adjusted ORs from Model 2 with 95% CIs, color-coded by statistical significance and shaped by predictor group, on a
  log-scaled x-axis.
