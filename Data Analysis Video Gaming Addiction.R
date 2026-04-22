

source(file = "Data Cleaning Video Gaming Addiction.R")

# ── Libraries for Analysis ────────────────────────────────────────────────────
library(gtsummary)   # publication-ready regression and summary tables
library(psych)       # Cronbach's alpha and psychometric analysis
library(flextable)   # table formatting for Word/HTML export
library(ordinal)     # clm() for cumulative link (ordinal logistic) models
library(gofcat)      # Brant test for proportional odds assumption
library(janitor)     # clean_names() for snake_case variable names
library(MASS)        # loaded as dependency; masks dplyr::select — resolved below
select <- dplyr::select  # explicitly reassign select to dplyr to prevent MASS conflict
recode <- dplyr::recode # explicitly reassign select to dplyr to prevent car conflict
library(car)         # vif() for multicollinearity via surrogate linear model
library(performance) # check_collinearity() and model performance indices
library(broom) # Better manipulation of regression outputs 


# ── Table 1: Descriptive Statistics by GPA ───────────────────────────────────
# Cross-tabulation of all demographics by GPA category
# Note: GPA <2.5 group has N=6; percentages in this cell should be interpreted cautiously
gaming_data_without_Q %>% select(-Score) %>% 
  tbl_summary(by = GPA ,missing = "no") %>%  
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Grade Point Average (GPA)**") %>%  
  bold_labels() %>% add_overall() %>% as_flex_table() 


# ── IGD-20 Subscale Analysis ──────────────────────────────────────────────────
# Define the 6 IGD-20 subscales as per the original instrument scoring sheet
subscales <- list(
  Salience   = c("item1",  "item7",  "item13"),
  Mood       = c("item2r", "item8",  "item14"),
  Tolerance  = c("item3",  "item9",  "item15"),
  Withdrawal = c("item4",  "item10", "item16"),
  Conflict   = c("item5",  "item11", "item17", "item19r", "item20"),
  Relapse    = c("item6",  "item12", "item18")
)


# For each subscale: compute mean ± SD and Cronbach's alpha (internal consistency)
results <- lapply(names(subscales), function(s) {
  
  items       <- subscales[[s]]
  sub_scores  <- rowMeans(gaming_data[, items], na.rm = TRUE)  # subscale mean per person
  
  alpha_out   <- alpha(gaming_data[, items], check.keys = T)
  alpha_val   <- round(alpha_out$total$raw_alpha, 2)
  
  data.frame(
    Dimension = s,
    Mean_SD   = paste0(round(mean(sub_scores, na.rm = TRUE), 2),
                       " ± ",
                       round(sd(sub_scores,   na.rm = TRUE), 2)),
    Alpha     = alpha_val
  )
})

bind_rows(results) %>% flextable() %>% bold(part = "header") %>% set_header_labels(
  Mean_SD = "Mean ± SD", Alpha = "Chronbach's Alpha"
) %>% bold(j = "Dimension") %>% autofit()


# ── Addiction Prevalence ──────────────────────────────────────────────────────
# Frequency and 95% CI for IGD classification (Addicted vs. Not Addicted)
gaming_data %>% select(Addiction) %>% tbl_summary(
  type = Addiction ~ "categorical") %>% 
  add_ci() %>% bold_labels() %>% as_flex_table()


# ── Regression: Preparation ───────────────────────────────────────────────────
# clean_names() converts variable names to snake_case, required for clm() compatibility
# with backtick-free formula syntax
names_cleaned_gaming_data_without_Q <- gaming_data_without_Q %>% clean_names()


# ── Unadjusted Ordinal Logistic Regression (Univariable) ─────────────────────
# One clm() model per predictor; ORs are exponentiated cumulative log-odds ratios
unadjusted_tbl <- names_cleaned_gaming_data_without_Q %>% 
  tbl_uvregression(y = gpa, method = clm, exponentiate = T , hide_n = T,
                   label = list(age ~ "Age",
                                social_status ~ "Social Status", 
                                living_with ~ "Living With",
                                college ~ "College",
                                ever_been_diagnosed_with_a_mental_illness ~ 
                                  "Ever Been Diagnosed with a Mental Illness",
                                smoking ~ "Smoking",
                                score ~ "Addiction Score",
                                addiction ~ "Addiction")) %>% bold_p()


# ── Adjusted Model 1: Full Model ──────────────────────────────────────────────
# Includes all variables significant or borderline in univariable analysis
# living_with and ever_been_diagnosed included for theoretical confounding consideration
multi_model1_cleaned_names <- clm(gpa ~ age + living_with + college +
                                    ever_been_diagnosed_with_a_mental_illness + smoking,
                                  data = names_cleaned_gaming_data_without_Q)
tbl_model1 <- tbl_regression(multi_model1_cleaned_names, exponentiate = T, 
                             label = list(age ~ "Age",
                                          living_with ~ "Living With",
                                          college ~ "College",
                                          ever_been_diagnosed_with_a_mental_illness ~ 
                                            "Ever Been Diagnosed with a Mental Illness",
                                          smoking ~ "Smoking")) %>% bold_p()


# ── Adjusted Model 2: Parsimonious Model ─────────────────────────────────────
# Reduced model after removing non-significant predictors (living_with,
# ever_been_diagnosed); justified by LRT (p=0.374), lower AIC and BIC vs. Model 1
multi_model2_cleaned_names <- clm(gpa ~ age + college + smoking,
                                  data = names_cleaned_gaming_data_without_Q)
tbl_model2 <- tbl_regression(multi_model2_cleaned_names, exponentiate = T,
                             label = list(age ~ "Age",
                                          college ~ "College",
                                          smoking ~ "Smoking")) %>% bold_p()

# ── Merged Regression Table ───────────────────────────────────────────────────
# Side-by-side display of unadjusted, Model 1, and Model 2 for manuscript Table
tbl_merge(tbls = list(unadjusted_tbl, tbl_model1, tbl_model2), 
          tab_spanner = c("**Unadjusted**", "**Model 1**", "**Model 2**")) %>% 
  bold_labels() %>% as_flex_table()


# ── Assumption Checks ─────────────────────────────────────────────────────────

# 1. Proportional Odds Assumption — nominal_test()
#    Tests whether each predictor violates the parallel regression assumption
#    A significant result would require a partial proportional odds model
nominal_test(multi_model1_cleaned_names)
nominal_test(multi_model2_cleaned_names)


# 2. Scale/Dispersion Effects — scale_test()
#    Tests whether any predictor affects the variance of the latent GPA distribution
#    A significant result would require adding that variable to clm(scale = ~var)
scale_test(multi_model1_cleaned_names)
scale_test(multi_model2_cleaned_names)


# 3. Nested Model Comparison — Likelihood Ratio Test
#    Formally tests whether dropping living_with and ever_been_diagnosed worsens fit
#    Non-significant result (p=0.374) supports the parsimonious Model 2
anova(multi_model1_cleaned_names, multi_model2_cleaned_names)

# 4. Model Selection Indices
#    Lower AIC/BIC favors Model 2; confirms parsimony without fit loss
AIC(multi_model1_cleaned_names, multi_model2_cleaned_names)
BIC(multi_model1_cleaned_names, multi_model2_cleaned_names)

# 5. Multicollinearity — Surrogate Linear Model VIF
#    clm() lacks native VIF support; a surrogate LM approximates predictor correlations
#    GVIF^(1/(2*Df)) is the comparable metric for multi-level factors (threshold: <2.24)
surrogate_lm <- lm(as.numeric(GPA) ~ Age + `Living with` + College +
                     `Ever been Diagnosed with a mental illness` + Smoking,
                   data = gaming_data_without_Q)
vif(surrogate_lm)


# 6. Proportional Odds — Brant Test (alternative method via gofcat)
#    Non-significant omnibus result confirms proportional odds holds
#    Warning about zero cells is expected given small N in GPA <2.5 group
brant.test(multi_model1_cleaned_names)
brant.test(multi_model2_cleaned_names)


# 7. Multicollinearity — check_collinearity() via performance package
#    Uses clm-specific internal method; college shows high VIF due to age-college
#    correlation (students in certain colleges cluster in specific age groups)
check_collinearity(multi_model1_cleaned_names)
check_collinearity(multi_model2_cleaned_names)


# 8. Overall Model Performance Indices (AIC, BIC, Nagelkerke R², RMSE)
#    Note: log-loss and proper scoring rules are not supported for ordinal/CLM models
performance(multi_model1_cleaned_names)
performance(multi_model2_cleaned_names)







# ── Forest Plot: Adjusted ORs from Model 2 ────────────────────────────────────
# Extracts model coefficients, cleans labels, and produces a publication-ready
# forest plot of adjusted odds ratios with 95% CIs from the parsimonious CLM.


# ── Step 1: Extract and tidy model coefficients ───────────────────────────────
# tidy() converts clm output to a data frame of estimates, SEs, CIs, and p-values
# exponentiate = TRUE converts log-odds to odds ratios
# filter() removes threshold/intercept terms (contain "|") — these are CLM
# cut-points, not predictor effects, and should not appear in the forest plot
tidy_multi_model2_cleaned_names <- tidy(
  multi_model2_cleaned_names,
  exponentiate = TRUE, conf.int = TRUE
) %>%
  filter(!grepl("\\|", term)) %>%
  
  # ── Step 2: Rename terms to clean display labels ───────────────────────────
  # recode() maps internal model term names (e.g., "collegeMedicine") to
  # human-readable labels for the y-axis of the forest plot
  mutate(
    term = dplyr::recode(term,
                         `age>20-23`                                  = "Age: >20–23 years",
                         `age>23`                                     = "Age: >23 years",
                         `collegeMedicine`                            = "College: Medicine",
                         `collegeDentistry`                           = "College: Dentistry",
                         `collegePharmacy`                            = "College: Pharmacy",
                         `collegeApplied Medical Sciences`            = "College: Applied Medical Sciences",
                         `collegePublic Health and Tropical Meidicne` = "College: Public Health & Tropical Medicine",
                         `smokingPrevious Smoker`                     = "Smoking: Previous Smoker",
                         `smokingCurrent Smoker`                      = "Smoking: Current Smoker"
    ),
    
    # Flag statistical significance at alpha = 0.05 for color-coding in plot
    significant = p.value < 0.05,
    
    # Assign predictor group for shape aesthetic — allows visual grouping
    # without adding a separate legend column or facet
    group = case_when(
      startsWith(term, "Age")     ~ "Age",
      startsWith(term, "College") ~ "College",
      startsWith(term, "Smoking") ~ "Smoking"
    )
  )


# ── Step 3: Forest Plot ───────────────────────────────────────────────────────
# Predictor terms are sorted by OR magnitude (reorder) so the plot reads
# from smallest to largest effect, making patterns immediately visible.
# Color encodes significance; shape encodes predictor group.
ggplot(tidy_multi_model2_cleaned_names,
       aes(x = estimate, y = reorder(term, estimate),
           color = significant, shape = group)) +
  
  # Shaded band around OR = 1 — visually marks the null effect zone
  # (approximately ±10% around 1 on the log scale)
  annotate("rect",
           xmin = 0.9, xmax = 1.1,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  
  # Dashed vertical reference line at OR = 1 (no effect)
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  
  # Horizontal error bars representing 95% confidence intervals
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.7) +
  
  # Point estimates — size and stroke chosen for print legibility
  geom_point(size = 4, stroke = 0.8) +
  
  # Log-scale x-axis with human-readable breaks
  # Log scale is standard for OR plots (symmetric around 1)
  scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 4),
    labels = c("0.25", "0.50", "1.00", "2.00", "4.00")
  ) +
  
  # Blue = significant (p < 0.05), Red = non-significant
  scale_color_manual(
    values = c("TRUE" = "#2166ac", "FALSE" = "#b2182b"),
    labels = c("TRUE" = "p < 0.05",  "FALSE" = "p ≥ 0.05"),
    name   = "Significance"
  ) +
  
  # Different point shapes per predictor group for additional visual separation
  scale_shape_manual(
    values = c("Age" = 16, "College" = 17, "Smoking" = 15),
    name   = "Predictor Group"
  ) +
  
  # Axis labels, title, subtitle, and caption with reference group information
  labs(
    x        = "Odds Ratio (log scale)",
    y        = NULL,
    title    = "Adjusted Odds Ratios for GPA",
    subtitle = "Parsimonious Model (Model 2): Age, College, Smoking",
    caption  = "Reference: Age 18–20 | College: Nursing | Smoking: Never Smoker\nError bars = 95% Confidence Intervals"
  ) +
  
  # ── Theme ─────────────────────────────────────────────────────────────────
  # Clean minimal theme; horizontal gridlines removed to reduce clutter
  # (y-axis ordering already conveys structure); only vertical guides retained
  theme_minimal(base_size = 13, base_family = "sans") +
  theme(
    plot.title         = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle      = element_text(color = "grey40", size = 11, hjust = 0,
                                      margin = margin(b = 8)),
    plot.caption       = element_text(color = "grey50", size = 9,  hjust = 0,
                                      margin = margin(t = 10)),
    axis.text.y        = element_text(color = "black", size = 11),
    axis.text.x        = element_text(color = "black", size = 10),
    axis.title.x       = element_text(margin = margin(t = 8), size = 11),
    panel.grid.major.y = element_blank(),          # remove horizontal guides
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.4),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 10),
    legend.text        = element_text(size = 10),
    plot.margin        = margin(15, 20, 10, 10)
  )
