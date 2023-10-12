rm(list=ls())

# Loading packages -------------------------------------------------------------

#devtools::install_github("antrologos/lme4.hlm")

pacman::p_load(lme4, merTools, lme4.hlm, tibble, dplyr, ggplot2)

# Loading data -----------------------------------------------------------------

data("hsb")

# Recoding ---------------------------------------------------------------------

hsb = as_tibble(hsb)

hsb <- hsb %>%
        group_by(schid) %>%
        mutate(ses_school   = mean(ses),
               ses_diff_ind = ses - ses_school)

# Models -----------------------------------------------------------------------

formula = "
# Level 1
mathach ~ b0 + b1*female + b2*minority + b3*ses_diff_ind

# Level 2
b0 ~ g00 + g01*log(size)  + g02*ses_school + r(schid)
b2 ~ g20 + g21*log(size)                   + r(schid)
b3 ~ g30 + g31*ses_school                  + r(schid)
"

# NEW FUNCTION: Using the function that converts a HLM character formula to
# a mixed effects formula
f = formula_hlm_to_lmer(formula_char = formula,
                        correlated_ranef = T)

# Model
lmer_estimate = lmer(formula = f,
                     data = hsb,
                     REML = T)

# Pre-existing function from another package that also summarizes lmer objects,
# but does not differentiates between equation levels
sjPlot::tab_model(lmer_estimate)

# NEW FUNCTION: Tidy summary objects more suited to HLM models ****
tidy_hlm(lmer_estimate)

# NEW FUNCTION: Tidy summary ****
summary_hlm(lmer_estimate)

# Plots ------------------------------------------------------------------------

hsb$random.coefficients.preds <-  predict(lmer_estimate)

hsb %>%
        ggplot(aes(x = ses_diff_ind, y = random.coefficients.preds, color = schid)) +
        geom_smooth(method = "lm", se = F, alpha = .2) +
        theme_bw() +
        theme(legend.position = "none")



