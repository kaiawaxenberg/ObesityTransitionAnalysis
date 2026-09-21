library(dplyr)
library(tidyr)
library(mclogit)
library(performance)
library(ggplot2)
library(readxl)

################################################################################
## Load survey data and baseline models
################################################################################

# Load data and models prepared by multinomials.R
load("Rdata/multinomials.Rdata")

# Save coefficient tables for M1 and M2 as defined in the manuscript
coefs_m1 <- data.frame(
  parameter = names(coef(m)),
  value = coef(m)
) |> mutate(model = "M1")

coefs_m2 <- data.frame(
  parameter = names(coef(m_all)),
  value = coef(m_all)
) |> mutate(model = "M2")

# Load country income groups as defined by the World Bank
incomeGroups <- read_excel("data/wb-incomeclassification.xlsx") |>
  select(Iso3 = "...1", group = "2017")

################################################################################
## Build prediction grid
################################################################################

## Load ranges of key values
gdp_seq <- seq(min(pooled_data$logGdpPc),
  max(pooled_data$logGdpPc),
  length.out = 20
)
age_seq <- 20:90
sex_levels <- levels(model.frame(m)$sex)
r_levels <- levels(model.frame(m_all)$rural)
quintile_levels <- levels(model.frame(m)$wealth_quintile)

## Save scaling parameters for deriving z-scores
loggdp_mean <- scaling_params |>
  filter(variable == "logGdpPc", stat == "mean") |>
  pull(value)
loggdp_sd <- scaling_params |>
  filter(variable == "logGdpPc", stat == "sd") |>
  pull(value)
age_mean <- scaling_params |>
  filter(variable == "age", stat == "mean") |>
  pull(value)
age_sd <- scaling_params |>
  filter(variable == "age", stat == "sd") |>
  pull(value)

## Load population age structure
who_standard_pop <- read.csv("data/singleages.pops.tables.csv") |>
  filter(Age >= 20) |>
  select(Age, fracAdults)

## Create prediction grids with and without urbanicity
pred_grid_rural <- expand.grid(
  wealth_quintile = factor(c("1", "2", "3", "4", "5"), levels = quintile_levels),
  logGdpPc = gdp_seq,
  age = age_seq,
  sex = factor(c("0", "1"), levels = sex_levels),
  rural = factor(c("Rural", "Urban"), levels = r_levels),
  stringsAsFactors = FALSE
) |>
  mutate(
    age_z = (age - age_mean) / age_sd,
    logGdpPc_z = (logGdpPc - loggdp_mean) / loggdp_sd,
    Country = ""
  )
pred_grid_no_rural <- pred_grid_rural |>
  select(wealth_quintile, logGdpPc_z, logGdpPc, age, sex, age_z, Country) |>
  distinct()

################################################################################
## Sensitivity Analysis: Pregnancy
################################################################################

## Calculate the fraction of all women in HPACC known to be pregnant
sample_rural |>
  filter(sex == "1", dataset == "HPACC") |>
  group_by(pregnant) |>
  summarise(n = n()) |>
  mutate(perc = 100 * n / sum(n))
# 3.11% of women in the HPACC dataset are pregnant

## Parameterise M2 on data excluding pregnant women
m2_nopreg <- mblogit(
  BMI_Category3 ~ sex * rural * (age_z + I(age_z^2) + logGdpPc_z * wealth_quintile),
  random = ~ 1 | Country,
  weights = w_eq_country_rural,
  data = sample_rural |> filter(is.na(pregnant) | pregnant == "No")
)

## Calculate predicted probabilities from the baseline model
probs_m2 <- predict(m_all, newdata = pred_grid_rural, conditional = FALSE, type = "response") |>
  as.data.frame() |>
  mutate(Model = "Full sample") |>
  bind_cols(pred_grid_rural)

## Calculate predicted probabilities from the alternative model
probs <- predict(m2_nopreg, newdata = pred_grid_rural, conditional = FALSE, type = "response") |>
  as.data.frame() |>
  mutate(Model = "No pregnancy") |>
  bind_cols(pred_grid_rural) |>
  rbind(probs_m2) |>
  pivot_longer(
    cols      = c("Obesity", "Overweight"),
    names_to  = "bmi_cat",
    values_to = "prob"
  ) |>
  left_join(who_standard_pop, join_by(age == Age)) |>
  group_by(Model, wealth_quintile, logGdpPc, bmi_cat) |>
  summarise(prob = weighted.mean(prob, fracAdults))

## Plot predictions from models including and excluding pregnant women from
## the parameterisation data
## (Supplementary Figure 10)
M2_pregnancy <- ggplot(
  probs,
  aes(x = exp(logGdpPc), y = prob, color = wealth_quintile, linetype = Model)
) +
  geom_line(alpha = 0.9, linewidth = 1) +
  facet_wrap(~bmi_cat) +
  scale_color_manual(values = c("#D53E4F", "#FDAE61", "#FEE08B", "#0571B0", "#5E4FA2")) +
  scale_x_log10(
    breaks = c(1500, 3000, 10000, 30000, 100000),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "GDP per capita (PPP 2021 $)",
    y = "Predicted prevalence",
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

ggsave("plots/pregnancyM2.pdf", M2_pregnancy,
  width = 9, height = 5
)

rm(m2_nopreg, M2_pregnancy, probs_m2, probs)

################################################################################
## Sensitivity Analysis: Non-response
################################################################################

## Generate parameterisation data excluding surveys with <70% complete cases
remove_countries <- missingness_summary |>
  filter(pct_incomplete_case >= 0.3) |>
  mutate(code = paste0(dataset, Country))
test_data <- pooled_data |>
  filter(!(paste0(dataset, Country) %in% remove_countries$code))

## Look at which surveys were removed in this process
removed <- pooled_data |>
  filter(paste0(dataset, Country) %in% remove_countries$code) |>
  select(dataset, Country, Iso3) |>
  distinct() |>
  left_join(incomeGroups, join_by(Iso3))

## Parameterise M1 on data excluding surveys with high non-response rates
m_nonresponse <- mblogit(
  BMI_Category3 ~ age_z + I(age_z^2) + sex + logGdpPc_z * wealth_quintile,
  random = ~ 1 | Country,
  weights = w_eq_country,
  data = test_data
)

## Calculate predicted probabilities from the baseline model
probs_m1 <- predict(m, newdata = pred_grid_no_rural, conditional = FALSE, type = "response") |>
  as.data.frame() |>
  mutate(Model = "Full sample") |>
  bind_cols(pred_grid_no_rural)

## Calculate predicted probabilities from the alternative model
probs <- predict(m_nonresponse, newdata = pred_grid_no_rural, conditional = FALSE, type = "response") |>
  as.data.frame() |>
  mutate(Model = "Non-response < 30%") |>
  bind_cols(pred_grid_no_rural) |>
  rbind(probs_m1) |>
  pivot_longer(
    cols      = c("Obesity", "Overweight"),
    names_to  = "bmi_cat",
    values_to = "prob"
  ) |>
  left_join(who_standard_pop, join_by(age == Age)) |>
  group_by(Model, wealth_quintile, logGdpPc, bmi_cat) |>
  summarise(prob = weighted.mean(prob, fracAdults))

## Plot predictions from models including and excluding surveys with high
## non-response rates in the parameterisation data
## (Supplementary Figure 9)
M1_nonresponse <- ggplot(
  probs,
  aes(x = exp(logGdpPc), y = prob, color = wealth_quintile, linetype = Model)
) +
  geom_line(alpha = 0.9, linewidth = 1) +
  facet_wrap(~bmi_cat) +
  scale_color_manual(values = c("#D53E4F", "#FDAE61", "#FEE08B", "#0571B0", "#5E4FA2")) +
  scale_x_log10(
    breaks = c(1500, 3000, 10000, 30000, 100000),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "GDP per capita (PPP 2021 $)",
    y = "Predicted prevalence",
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

ggsave("plots/nonresponseM1.pdf", M1_nonresponse,
  width = 9, height = 5
)

rm(
  remove_countries, test_data, remove,
  m_nonresponse, probs, probs_m1, M1_nonresponse
)

################################################################################
## Sensitivity Analysis: Survey years
################################################################################

## Parameterise M1 with the addition of a fixed effect for normalised year
## of data collection
m_year <- mblogit(
  BMI_Category3 ~ dataYear_z + age_z + I(age_z^2) + sex + logGdpPc_z * wealth_quintile,
  random = ~ 1 | Country,
  weights = w_eq_country,
  data = pooled_data
)
summary(m_year)
summary(m)

## Add normalized data collection year to the prediction grid, set to the mean
pred_grid_year <- pred_grid_no_rural |> mutate(dataYear_z = 0)

## Calculate predicted probabilities from the baseline model
probs_m1 <- predict(m, newdata = pred_grid_year, conditional = FALSE, type = "response") |>
  as.data.frame() |>
  mutate(Model = "Baseline model") |>
  bind_cols(pred_grid_year)

## Calculate predicted probabilities from the alternative model
probs <- predict(m_year, newdata = pred_grid_year, conditional = FALSE, type = "response") |>
  as.data.frame() |>
  mutate(Model = "Year fixed effect") |>
  bind_cols(pred_grid_year) |>
  rbind(probs_m1) |>
  pivot_longer(
    cols      = c("Obesity", "Overweight"),
    names_to  = "bmi_cat",
    values_to = "prob"
  ) |>
  left_join(who_standard_pop, join_by(age == Age)) |>
  group_by(Model, wealth_quintile, logGdpPc, bmi_cat) |>
  summarise(prob = weighted.mean(prob, fracAdults))

## Plot predictions from models including and excluding a fixed effect for year
## (Supplementary Figure 11)
M1_year <- ggplot(
  probs,
  aes(x = exp(logGdpPc), y = prob, color = wealth_quintile, linetype = Model)
) +
  geom_line(alpha = 0.9, linewidth = 1) +
  facet_wrap(~bmi_cat) +
  scale_color_manual(values = c("#D53E4F", "#FDAE61", "#FEE08B", "#0571B0", "#5E4FA2")) +
  scale_x_log10(
    breaks = c(1500, 3000, 10000, 30000, 100000),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "GDP per capita (PPP 2021 $)",
    y = "Predicted prevalence",
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

ggsave("plots/yearM1.pdf", M1_year,
  width = 9, height = 5
)

rm(m_decade, probs, probs_m1, M1_decade)

################################################################################
## Compare weighting approaches
################################################################################

## The tables below demonstrate how different weighting approaches shift
## weighting values towards certain countries. For example, the population-based
## approach heavily weights the analysis towards India and China.

weightsSurvey <- pooled_data |>
  group_by(Country, dataset) |>
  summarise(
    EqualCountry = sum(w_eq_country),
    CountryPopulation = sum(w_country_pop),
    EqualPerson = sum(w_eq_indiv)
  )

weightsCountry <- pooled_data |>
  group_by(Iso3, dataset) |>
  summarise(
    EqualCountry = sum(w_eq_country),
    CountryPopulation = sum(w_country_pop),
    EqualPerson = sum(w_eq_indiv)
  )
