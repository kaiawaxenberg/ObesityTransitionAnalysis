library(dplyr)
library(ggplot2)
library(tidyr)
library(mclogit)
library(RColorBrewer)
library(performance)
library(modelsummary)
library(MASS)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

# Load pooled survey data ------------------------------------------------------

#Load file prepared by data_preparation.R
load("Rdata/final_data.RData")

sample_data = sample_data |>
  mutate(wealth_quintile = factor(wealth_quintile, ordered = FALSE))

pooled_data = sample_data |> filter(pooledData)

sample_rural = pooled_data |> filter(insample_rural)

# Parameterize multinomial models ----------------------------------------------

m1 <- mblogit(
  BMI_Category3 ~ age_z + I(age_z^2) + sex + logGdpPc_z * wealth_quintile,
  random = ~ 1 | Country,
  weights = w_eq_country,
  data = pooled_data
)

m2 <- mblogit(
  BMI_Category3 ~ sex * rural * (age_z + I(age_z^2) + logGdpPc_z * wealth_quintile),
  random = ~ 1 | Country,
  weights = w_eq_country_rural,
  data = sample_rural
)

#Save parameterised model objects
save.image(file="Rdata/multinomials.RData")
load("Rdata/multinomials.Rdata")

# Coefficient tables -----------------------------------------------------------

modelsummary(m1,
             shape = term ~ response,
             exponentiate = TRUE,
             statistic = "conf.int",
             stars = TRUE,
             output = "m1_summary.docx")

modelsummary(m2,
             shape = term ~ response,
             exponentiate = TRUE,
             statistic = "conf.int",
             stars = TRUE, 
             "m2_summary.docx")

# Parametric bootstrap function ------------------------------------------------

#Set up function for parametric bootstrap of predicted values
predict_mblogit <- function(mod, newdata, reps = 1000, level = 0.95) {
  
  #Save model coefficient and covariance tables
  beta <- coef(mod)
  V    <- vcov(mod)
  
  #Model terms and BMI categories
  cn    <- names(beta)
  trm   <- sub("^.*?~", "", cn)
  cat_k <- sub("~.*$", "", cn)
  terms_used <- unique(trm)
  
  #Design matrix for predicted values
  tt <- delete.response(terms(mod))
  X  <- model.matrix(tt, newdata, contrasts.arg = mod$contrasts)
  X  <- X[, terms_used, drop = FALSE]
  
  #all bmi categories
  cats <- c("Obesity", "Overweight")
  baseline <- "Healthy/low weight"
  all_cats <- c(baseline, cats)
  
  #draw coefficient vectors
  draws <- MASS::mvrnorm(reps, mu = beta, Sigma = V)
  
  n <- nrow(newdata)
  P <- array(0, dim = c(reps, n, length(all_cats)),
             dimnames = list(NULL, rownames(newdata),all_cats))
  
  for (s in 1:reps) {
    B <- matrix(0, length(terms_used), length(cats),
                dimnames = list(terms_used, cats))
    for (k in cats) 
      B[trm[cat_k == k], k] <- draws[s, cat_k == k]
    
    eta <- X %*% B                       # n x (K-1)
    eta <- cbind(eta, 0)                 # baseline linear predictor = 0
    colnames(eta)[ncol(eta)] <- baseline
    eta <- eta[, all_cats, drop = FALSE] # reorder
    ex  <- exp(eta)
    P[s, , ] <- ex / rowSums(ex)
    print(paste("rep", s, "complete"))
  }
  
  sim <- as.data.frame.table(P, responseName = "prob",
                             stringsAsFactors = FALSE)
  names(sim)[1:3] <- c("rep", "row", "category")
  
  sim = sim |> filter(category %in% c("Obesity","Overweight"))
  
  return(sim)
}

# Observed vs predicted plots --------------------------------------------------

pred <- predict(m1, type = "response")

results <- pooled_data |>
  dplyr::select(Country, BMI_Category3, wealth_quintile, logGdpPc_z) |>
  bind_cols(pred) |>
  pivot_longer(
    cols      = c(`Healthy/low weight`, Overweight, Obesity),
    names_to  = "BMI_Category_pred",
    values_to = "pred_prev"
  ) |>
  mutate(
    y = as.integer(BMI_Category3 == BMI_Category_pred)
  ) |>
  group_by(wealth_quintile, Country, BMI_Category_pred) |>
  summarise(
    obs_prev = mean(y),
    pred_prev = mean(pred_prev),
    n = n(),
    .groups = "drop"
  )

calib = ggplot(results, aes(x = pred_prev, y = obs_prev)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed", colour = "#C02942") +
  facet_wrap(~BMI_Category_pred, scales = "free") +
  labs(
    x     = "Predicted prevalence",
    y     = "Observed prevalence"
  ) +
  theme_minimal(base_size =16)

ggsave("plots/pooledCalibrationPlot.pdf", calib,
       width = 9, height = 4.5)

results <- sample_rural |>
  dplyr::select(Country, BMI_Category3, logGdpPc_z, wealth_quintile, sex, rural) |>
  mutate(stratum = case_when(sex == 1 & rural == "Rural" ~ "Rural, F",
                             sex == 1 & rural == "Urban" ~ "Urban, F",
                             sex == 0 & rural == "Rural" ~ "Rural, M",
                             sex == 0 & rural == "Urban" ~ "Urban, M",
  ))|>
  bind_cols(predict(m2, type = "response"))|>
  pivot_longer(
    cols      = c(`Healthy/low weight`, Overweight, Obesity),
    names_to  = "BMI_Category_pred",
    values_to = "pred_prev"
  ) |>
  mutate(
    y = as.integer(BMI_Category3 == BMI_Category_pred)
  ) |>
  group_by(wealth_quintile, Country, BMI_Category_pred, stratum) |>
  summarise(
    obs_prev = mean(y),
    pred_prev = mean(pred_prev),
    n = n(),
    .groups = "drop"
  )

sex_geography_calib = ggplot(results, aes(x = pred_prev, y = obs_prev)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed", colour = "#e8351e") +
  facet_grid(stratum~BMI_Category_pred, scales = "free") +
  labs(
    x     = "Predicted prevalence",
    y     = "Observed prevalence"
  ) +
  theme_minimal(base_size=14)

ggsave("plots/stratifiedCalibration.pdf", sex_geography_calib,
       width = 7.75, height = 8, dpi = 150)

rm(results, pred, calib, sex_geography_calib)


# PLOT PREDICTIONS WITH UNCERTAINTY -------------------------------------------------

gdp_seq <- seq(min(pooled_data$logGdpPc),
               max(pooled_data$logGdpPc),
               length.out = 15)
age_seq <- 20:90
sex_levels <- levels(model.frame(m1)$sex)
r_levels = levels(model.frame(m2)$rural)
quintile_levels <- levels(model.frame(m1)$wealth_quintile)
age_mean <- scaling_params |>
  filter(variable == "age", stat == "mean") |>
  pull(value)
age_sd <- scaling_params |>
  filter(variable == "age", stat == "sd") |>
  pull(value)
who_standard_pop = read.csv("data/singleages.pops.tables.csv") |>
  filter(Age>=20)|>
  select(Age, fracAdults)

pred_grid_rural <- expand.grid(
  wealth_quintile = factor(c("1", "2", "3", "4", "5"), levels = quintile_levels),
  logGdpPc = gdp_seq,
  rural = factor(c("Rural", "Urban"), levels = r_levels),
  age = age_seq,
  sex = factor(c("0", "1"), levels = sex_levels),
  stringsAsFactors = FALSE
) |>
  mutate(
    age_z = (age - age_mean)/age_sd,
    logGdpPc_z = (logGdpPc - loggdp_mean)/loggdp_sd,
    Country = "")
pred_grid_no_rural = pred_grid_rural |> 
  select(wealth_quintile, logGdpPc, age, sex, age_z, logGdpPc_z, Country)|> 
  distinct()

sim_population <- predict_mblogit(m1, newdata = pred_grid_no_rural) |>
  filter(category %in% c("Obesity","Overweight"))|>
  left_join(pred_grid_no_rural |> mutate(row = as.character(row_number())), join_by(row))|>
  left_join(who_standard_pop, join_by(age==Age))|> 
  group_by(logGdpPc, wealth_quintile, category, rep) |>
  summarise(
    prob   = weighted.mean(prob, fracAdults)
  )|>
  group_by(logGdpPc, wealth_quintile, category)|>
  summarise(
    prevalence = mean(prob),
    se    = sd(prob),
    lower = quantile(prob, probs = 0.95),
    upper = quantile(prob, probs = 0.05),
    .groups = "drop"
  )

M1_confint = ggplot(
  sim_population,
  aes(x = exp(logGdpPc), y = prevalence, color = wealth_quintile, group = wealth_quintile)) +
  geom_line(alpha = 0.9)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=wealth_quintile), alpha = 0.2, color = NA)+
  facet_wrap(~ category) +
  scale_color_manual(values = c("#D53E4F","#FDAE61","#FEE08B","#0571B0", "#5E4FA2")) +
  scale_fill_manual(values = c("#D53E4F","#FDAE61","#FEE08B","#0571B0", "#5E4FA2"))+
  scale_x_log10(
    breaks = c(1500, 3000, 10000, 30000, 100000),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  )+
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x       = "GDP per capita (PPP 2021 $)",
    y       = "Predicted prevalence",
    fill = "Wealth quintile", 
    colour = "Wealth quintile"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1, "lines")
  )

ggsave("plots/transition.pdf", M1_confint,
       width = 9, height = 5)

sim_subpopulation <- predict_mblogit (m2, newdata = pred_grid_rural) |>
  left_join(pred_grid_rural |> mutate(row = as.character(row_number())), join_by(row))|>
  left_join(who_standard_pop, join_by(age==Age))|> 
  mutate(sex = ifelse(sex == "0", "Male", "Female"),
         stratum = paste0(sex, ", ", rural),
         stratum = factor(stratum, 
                          levels = c(
                            "Female, Urban", "Female, Rural",
                            "Male, Urban", "Male, Rural")))|>
  group_by(logGdpPc, stratum, sex, rural, category, rep) |>
  summarise(
    prob   = weighted.mean(prob, fracAdults)
  )|>
  group_by(logGdpPc, stratum,  sex, rural, category)|>
  summarise(
    prevalence = mean(prob),
    se    = sd(prob),
    lower = quantile(prob, probs = 0.95),
    upper = quantile(prob, probs = 0.05),
    .groups = "drop"
  )

gender_place_cols <- c(
  "Female, Urban" = "#12674d",
  "Female, Rural" = "#8fd4bd",
  "Male, Urban"    = "#d95f02",
  "Male, Rural"    = "#F0A878"
)

simfig2 = ggplot(sim_subpopulation, aes(x = exp(logGdpPc), y = prevalence, group = stratum)) +
  geom_line(aes(color = stratum), linewidth = 1)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=stratum), alpha = 0.2)+
  facet_wrap(~ category) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_color_manual(values = gender_place_cols)+
  scale_fill_manual(values = gender_place_cols, guide  = "none")+
  labs(
    x       = "GDP per capita (PPP 2021 $)",
    y       = "Predicted prevalence",
    color = ""
  ) +
  scale_x_log10(
    breaks = c(1500, 3000, 10000, 30000, 100000),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  )+
  theme_minimal(base_size = 16) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1, "lines"))

ggsave("plots/stratifiedPredictions2.pdf", simfig2,
       width = 9, height = 5)

#Calculate transition point values ---------------------------------------------

#Load scaling parameters to covert GDP z-scores back to unscaled values
loggdp_mean <- scaling_params |> 
  filter(variable == "logGdpPc", stat == "mean") |> 
  pull(value)
loggdp_sd <- scaling_params |> 
  filter(variable == "logGdpPc", stat == "sd") |> 
  pull(value)

#Create prediction grid for identifying transition points
set.seed(123)
gdp_seq <- seq(-0.5, #All transition point are above this based on graphs
               1.3 * max(pooled_data$logGdpPc_z), 
               length.out = 100)
pred_grid_rural <- expand.grid(
  wealth_quintile = factor(c("1","2","3","4","5"), levels = quintile_levels),
  logGdpPc_z = gdp_seq,
  age =age_seq,
  sex = factor(c("0", "1"), levels = sex_levels),
  rural = factor(c("Rural", "Urban"), levels = r_levels),
  stringsAsFactors = FALSE
) |>
  mutate(
    age_z = (age - age_mean)/age_sd,
    Country = "")
pred_grid_no_rural = pred_grid_rural |> 
  select(wealth_quintile, logGdpPc_z, age, sex, age_z, Country)|>
  distinct()

m1_transitions <- predict(m1, newdata = pred_grid_no_rural, conditional = FALSE, type = "response")|>
  bind_cols(pred_grid_no_rural) |>
  pivot_longer(
    cols      = c("Obesity","Overweight"),
    names_to  = "category",
    values_to = "prob"
  )|>
  left_join(who_standard_pop, join_by(age == Age)) |>
  group_by(category, logGdpPc_z, wealth_quintile) |>
  summarise(prob = weighted.mean(prob, fracAdults), .groups = "drop") |>
  group_by(category, logGdpPc_z) |>
  summarise(
    diff = prob[wealth_quintile == "5"] - prob[wealth_quintile == "1"],
    .groups = "drop"
  )|>
  group_by(category) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(
    prev_diff  = lag(diff),
    prev_gdp   = lag(logGdpPc_z),
    cross      = prev_diff > 0 & diff <= 0,
    crossing_z = if_else(cross,
                         prev_gdp + (0 - prev_diff) *
                           (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                         NA_real_),
    crossing   = exp(crossing_z * loggdp_sd + loggdp_mean) #Transform back
  ) |>
  filter(cross) |>
  select(category, crossing)

m2_transitions <- predict(m2, newdata = pred_grid_rural, conditional = FALSE, type = "response")|>
  bind_cols(pred_grid_rural) |>
  pivot_longer(
    cols      = c("Obesity","Overweight"),
    names_to  = "category",
    values_to = "prob"
  )|>
  left_join(who_standard_pop, join_by(age == Age)) |>
  group_by(category, logGdpPc_z, wealth_quintile, sex, rural) |>
  summarise(prob = weighted.mean(prob, fracAdults), .groups = "drop") |>
  group_by(category, logGdpPc_z, sex, rural) |>
  summarise(
    diff = prob[wealth_quintile == "5"] - prob[wealth_quintile == "1"],
    .groups = "drop"
  )|>
  group_by(category, sex, rural) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(
    prev_diff  = lag(diff),
    prev_gdp   = lag(logGdpPc_z),
    cross      = prev_diff > 0 & diff <= 0,
    crossing_z = if_else(cross,
                         prev_gdp + (0 - prev_diff) *
                           (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                         NA_real_),
    crossing   = exp(crossing_z * loggdp_sd + loggdp_mean) #Transform back
  ) |>
  filter(cross) |>
  select(category, sex, rural, crossing)

print(m1_transitions)
print(m2_transitions)

sex_transitions <- predict(m2, newdata = pred_grid_rural, conditional = FALSE, type = "response")|>
  bind_cols(pred_grid_rural) |>
  pivot_longer(
    cols      = c("Obesity","Overweight"),
    names_to  = "category",
    values_to = "prob"
  )|>
  left_join(who_standard_pop, join_by(age == Age)) |>
  group_by(category, logGdpPc_z, sex) |>
  summarise(prob = weighted.mean(prob, fracAdults), .groups = "drop") |>
  group_by(category, logGdpPc_z) |>
  summarise(
    diff = prob[sex == "1"] - prob[sex == "0"],
    .groups = "drop"
  )|>
  group_by(category) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(
    prev_diff  = lag(diff),
    prev_gdp   = lag(logGdpPc_z),
    cross      = prev_diff > 0 & diff <= 0,
    crossing_z = if_else(cross,
                         prev_gdp + (0 - prev_diff) *
                           (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                         NA_real_),
    crossing   = exp(crossing_z * loggdp_sd + loggdp_mean) #Transform back
  ) |>
  filter(cross) |>
  select(category, crossing)
print(sex_transitions)

rural_transitions <- predict(m2, newdata = pred_grid_rural, conditional = FALSE, type = "response")|>
  bind_cols(pred_grid_rural) |>
  pivot_longer(
    cols      = c("Obesity","Overweight"),
    names_to  = "category",
    values_to = "prob"
  )|>
  left_join(who_standard_pop, join_by(age == Age)) |>
  group_by(category, logGdpPc_z, rural) |>
  summarise(prob = weighted.mean(prob, fracAdults), .groups = "drop") |>
  group_by(category, logGdpPc_z) |>
  summarise(
    diff = prob[rural == "Urban"] - prob[rural == "Rural"],
    .groups = "drop"
  )|>
  group_by(category) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(
    prev_diff  = lag(diff),
    prev_gdp   = lag(logGdpPc_z),
    cross      = prev_diff > 0 & diff <= 0,
    crossing_z = if_else(cross,
                         prev_gdp + (0 - prev_diff) *
                           (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                         NA_real_),
    crossing   = exp(crossing_z * loggdp_sd + loggdp_mean) #Transform back
  ) |>
  filter(cross) |>
  select(category, crossing)
print(rural_transitions)

#Calculate transition point CIs ------------------------------------------------

#M1 quintile transition points
pred = predict_mblogit(m1, pred_grid_no_rural |> filter(wealth_quintile %in% c("1", "5")))
prev <- pred |>
  left_join(pred_grid_no_rural |> 
            filter(wealth_quintile %in% c("1", "5")) |>
            mutate(row= as.character(row_number())), 
            join_by(row))|>
  left_join(who_standard_pop, join_by(age==Age))|>
  group_by(rep, category, logGdpPc_z, wealth_quintile) |>
  summarise(prob = weighted.mean(prob, fracAdults))
diff = prev |>
  arrange(category, rep, logGdpPc_z, wealth_quintile) |>
  group_by(category, logGdpPc_z, rep) |>
  summarise(diff = prob[wealth_quintile == "5"] - prob[wealth_quintile == "1"], .groups = "drop")
transitions = diff |>
  group_by(category, rep) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(prev_diff = lag(diff),
         prev_gdp  = lag(logGdpPc_z),
         cross = prev_diff > 0 & diff <= 0,
         crossing = ifelse(cross, 
                           prev_gdp + (0 - prev_diff) * (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                           NA),
        crossing =exp((crossing * loggdp_sd)+loggdp_mean)) |>
  group_by(category)|>
  summarise(transition_point = mean(crossing, na.rm = TRUE),
            lower = quantile(crossing, .025, na.rm = TRUE),
            upper = quantile(crossing, .975, na.rm = TRUE))
print(transitions)
rm(pred, prev, diff, transitions)

#M2 quintile transition points
pred = predict_mblogit(m2, pred_grid_rural |> filter(wealth_quintile %in% c("1", "5")))
prev <- pred |>
  left_join(pred_grid_rural |> 
              filter(wealth_quintile %in% c("1", "5"))|>
              mutate(row= as.character(row_number())), join_by(row))|>
  left_join(who_standard_pop, join_by(age==Age))|>
  group_by(rep, category, logGdpPc_z, sex, rural, wealth_quintile) |>
  summarise(prob = weighted.mean(prob, fracAdults))
diff = prev |>
  arrange(category, sex, rural, rep, logGdpPc_z, wealth_quintile) |>
  group_by(category, sex, rural, logGdpPc_z, rep) |>
  summarise(diff = prob[wealth_quintile == "5"] - prob[wealth_quintile == "1"], .groups = "drop")
transitions = diff |>
  group_by(category, rep, sex, rural) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(prev_diff = lag(diff),
         prev_gdp  = lag(logGdpPc_z),
         cross = prev_diff > 0 & diff <= 0,
         crossing = ifelse(cross, 
                           prev_gdp + (0 - prev_diff) * (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                           NA),
         crossing =exp((crossing * loggdp_sd)+loggdp_mean)) |>
  group_by(category, sex, rural)|>
  summarise(transition_point = mean(crossing, na.rm = TRUE),
            lower = quantile(crossing, .025, na.rm = TRUE),
            upper = quantile(crossing, .975, na.rm = TRUE))
  
print(transitions)
rm(pred, prev, diff, transitions)

#Sex and urbanicity transition points
pred = predict_mblogit(m2, pred_grid_rural)|>
  left_join(pred_grid_rural |> 
              mutate(row= as.character(row_number())), join_by(row))|>
  left_join(who_standard_pop, join_by(age==Age))

sex_points <- pred |>
  group_by(rep, category, logGdpPc_z, sex) |>
  summarise(prob = weighted.mean(prob, fracAdults))|>
  arrange(category, rep, logGdpPc_z, sex) |>
  group_by(category, logGdpPc_z, rep) |>
  summarise(diff = prob[sex == "1"] - prob[sex == "0"], .groups = "drop")|>
  group_by(category, rep) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(prev_diff = lag(diff),
         prev_gdp  = lag(logGdpPc_z),
         cross = prev_diff > 0 & diff <= 0,
         crossing = ifelse(cross, 
                           prev_gdp + (0 - prev_diff) * (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                           NA),
         crossing =exp((crossing * loggdp_sd)+loggdp_mean))|>
  group_by(category)|>
  summarise(transition_point = mean(crossing, na.rm = TRUE),
            lower = quantile(crossing, .025, na.rm = TRUE),
            upper = quantile(crossing, .975, na.rm = TRUE))
print(sex_points)
save(sex_points, file = "Rdata/sex_points.Rdata")

rural_points <- pred |>
  group_by(rep, category, logGdpPc_z, rural) |>
  summarise(prob = weighted.mean(prob, fracAdults))|>
  arrange(category, rep, logGdpPc_z, rural) |>
  group_by(category, logGdpPc_z, rep) |>
  summarise(diff = prob[rural == "Urban"] - prob[rural == "Rural"], .groups = "drop")|>
  group_by(category, rep) |>
  arrange(logGdpPc_z, .by_group = TRUE) |>
  mutate(prev_diff = lag(diff),
         prev_gdp  = lag(logGdpPc_z),
         cross = prev_diff > 0 & diff <= 0,
         crossing = ifelse(cross, 
                           prev_gdp + (0 - prev_diff) * (logGdpPc_z - prev_gdp) / (diff - prev_diff),
                           NA),
         crossing =exp((crossing * loggdp_sd)+loggdp_mean))|>
  group_by(category)|>
  summarise(transition_point = mean(crossing, na.rm = TRUE),
            lower = quantile(crossing, .025, na.rm = TRUE),
            upper = quantile(crossing, .975, na.rm = TRUE))
print(rural_points)
save(rural_points, file = "Rdata/rural_points.RData")

rm(pred)

#### When will overall prevalence decrease? ------------------------------------

prevalence_pop = predict(m2, newdata = pred_grid_rural, conditional = FALSE, type = "response") |>
  as.data.frame() |>
  bind_cols(pred_grid_rural) |>
  left_join(who_standard_pop, join_by(age==Age))|> 
  pivot_longer(
    cols      =  c("Obesity", "Overweight"),
    names_to  = "bmi_cat",
    values_to = "prob"
  )|>
  group_by(logGdpPc_z, bmi_cat) |>
  summarise(
    prob   = weighted.mean(prob, fracAdults)
  )

ggplot(prevalence_pop, aes(x = exp((logGdpPc_z * loggdp_sd)+loggdp_mean), y = prob)) +
  geom_line(linewidth = 1)+
  facet_wrap( ~ bmi_cat) +
  labs(
    x       = "GDP per capita (PPP 2021 $)",
    y       = "Predicted prevalence",
    color = ""
  ) +
  scale_x_log10()+
  theme_minimal(base_size = 16) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1, "lines"))

# Retired plots of point predictions -------------------------------------------
# 
# bmi_colors <- c(
#   "Healthy/low weight" = "#96D271",
#   "Overweight" = "#1F9D9A",
#   "Obesity" = "#7A3DB8"
# )
# 
# # Percentiles for the food availability "low / mid / high" slices
# slices <- c(0.10, 0.50, 0.90)
# labels <- c(
#   "Low National GDP \n(10th percentile)",
#   "Medium National GDP\n(50th percentile)",
#   "High National GDP\n(90th percentile)"
# )
# 
# #BUILD PREDICTION GRID ---------------------------------------------------------
# 
# gdp_vals <- quantile(pooled_data$logGdpPc_z, slices, na.rm = TRUE)
# pred_grid_rural <- expand.grid(
#   wealth_quintile = factor(c("1", "2", "3", "4", "5"), levels = quintile_levels),
#   logGdpPc_z = gdp_vals,
#   age=age_seq,
#   sex = factor(c("0", "1"), levels = sex_levels),
#   rural = factor(c("Rural", "Urban"), levels = r_levels),
#   stringsAsFactors = FALSE
# ) |>
#   mutate(
#     age_z = (age - age_mean)/age_sd,
#     Country = "", # Average random effect (or 0 random effect)
#     label = factor(
#       logGdpPc_z,
#       levels = gdp_vals,
#       labels = labels
#     ))
# pred_grid_no_rural = pred_grid_rural |> 
#   select(wealth_quintile, logGdpPc_z, age, sex, age_z, Country, label)|>
#   distinct()
# 
# #PREDICTED PROBALITIES ---------------------------------------------------------
# 
# probs_m1 <- predict(m1, newdata = pred_grid_no_rural, conditional = FALSE, type = "response") |>
#   as.data.frame() |>
#   bind_cols(pred_grid_no_rural) |>
#   pivot_longer(
#     cols      = all_of(labels(bmi_colors)),
#     names_to  = "bmi_cat",
#     values_to = "prob"
#   ) |>
#   mutate(bmi_cat = factor(bmi_cat,
#                           levels = c(
#                             "Obesity",
#                             "Overweight",
#                             "Healthy/low weight"
#                           ),
#                           ordered = TRUE
#   )) |>
#   left_join(who_standard_pop, join_by(age==Age))|>
#   group_by(wealth_quintile, label, bmi_cat)|>
#   summarise(prob = weighted.mean(prob, fracAdults))
# 
# probs_effect_mod <- predict(m1, newdata = pred_grid_rural, conditional = FALSE, type = "response") |>
#   as.data.frame() |>
#   bind_cols(pred_grid_rural) |>
#   pivot_longer(
#     cols      = all_of(labels(bmi_colors)),
#     names_to  = "bmi_cat",
#     values_to = "prob"
#   ) |>
#   mutate(bmi_cat = factor(bmi_cat,
#                           levels = c(
#                             "Obesity",
#                             "Overweight",
#                             "Healthy/low weight"
#                           ),
#                           ordered = TRUE
#   ), 
#   sex = ifelse(sex == "0", "M", "F"),
#   stratum = paste0(rural, ", ", sex),
#   stratum = factor(stratum, 
#                    levels = c(
#                      "Urban, F", "Rural, F",
#                      "Urban, M", "Rural, M"
#                    )))|>
#   left_join(who_standard_pop, join_by(age==Age))|>
#   group_by(stratum, sex, rural, wealth_quintile, label, bmi_cat)|>
#   summarise(prob = weighted.mean(prob, fracAdults))
# 
# #PLOT PREDICTED VALUES ---------------------------------------------------------
# M1_figure = ggplot(
#   probs_m1,
#   aes(x = wealth_quintile, y = prob, fill = bmi_cat, group = bmi_cat)
# ) +
#   geom_area(alpha = 0.9)+
#   facet_wrap(~ label) +
#   scale_fill_manual(values = bmi_colors, name = "",
#                     guide = guide_legend(nrow = 3),
#                     labels = c("Obesity" = "Obesity (BMI >= 30)",
#                                "Healthy/low weight" = "Healthy/low weight (BMI < 25)",
#                                "Overweight" = "Overweight (BMI >= 25)")) +
#   scale_y_continuous(
#     limits = c(0, NA),
#     expand = expansion(mult = c(0, 0.05))
#   ) +
#   labs(
#     x       = "Household wealth quintile",
#     y       = "Predicted prevalence",
#   ) +
#   theme_minimal(base_size = 16) +
#   theme(
#     plot.title       = element_text(face = "bold", size = 13),
#     strip.text       = element_text(face = "bold"),
#     legend.position  = "bottom",
#     panel.grid.minor = element_blank(),
#     panel.spacing    = unit(1, "lines")
#   )
# 
# ggsave("plots/quintileModel.pdf", M1_figure,
#        width = 9, height = 5)
# 
# M2_figure = ggplot(
#   probs_effect_mod,
#   aes(x = wealth_quintile, y = prob, fill = bmi_cat, group = bmi_cat)
# ) +
#   geom_area(alpha = 0.9)+
#   facet_grid(stratum ~ label) +
#   scale_fill_manual(values = bmi_colors, name = "",
#                     guide = guide_legend(nrow = 3),
#                     labels = c("Obesity" = "Obesity (BMI >= 30)",
#                                "Healthy/low weight" = "Healthy/low weight (BMI < 25)",
#                                "Overweight" = "Overweight (BMI >= 25)")) +
#   scale_y_continuous(
#     limits = c(0, NA),
#     expand = expansion(mult = c(0, 0.05))
#   ) +
#   labs(
#     x       = "Household wealth quintile",
#     y       = "Predicted prevalence",
#   ) +
#   theme_minimal(base_size = 16) +
#   theme(
#     plot.title       = element_text(face = "bold", size = 13),
#     strip.text       = element_text(face = "bold"),
#     legend.position  = "bottom",
#     panel.grid.minor = element_blank(),
#     panel.spacing    = unit(1, "lines")
#   )
# 
# ggsave("plots/stratifiedModel.pdf", M2_figure,
#        width = 7.5, height = 8, dpi = 150)

