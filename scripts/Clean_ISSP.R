library(haven)
library(dplyr)

################################################################################
## Load and clean ISSP survey data
################################################################################

## Load the raw downloaded 2021 dataset
ISSP_2021 <- read_dta("data/ISSP_2021/ZA8000_v2-0-0.dta")

## Save lists of country-specifici income and region columns
region_cols <- names(select(ISSP_2021, ends_with("_REG")))
income_cols <- names(select(ISSP_2021, ends_with("_INC")))

## Define NA codes present in both 2021 and 2011 for income
na_codes <- c(
  -2, 999990, 999999, 999997, 999998, 9999990, 99999990, 99999999, 9999999,
  9999998, 9999997
)

## Clean up the raw data
ISSP <- ISSP_2021 |>
  ## Replace NA codes with NA
  mutate(
    across(all_of(income_cols), ~ ifelse(.x %in% na_codes, NA_character_, as.character(as_factor(.x))),
      .names = "{.col}_label"
    ),
    across(all_of(region_cols), ~ ifelse(.x %in% c(0, 9999), NA_character_, as.character(as_factor(.x))),
      .names = "{.col}_label"
    ),
    across(all_of(income_cols), ~ ifelse(.x %in% na_codes, NA_real_, .x))
  ) |>
  ## Combine separate income and region columns for each country
  mutate(
    income = do.call(coalesce, pick(income_cols)),
    income_lab = do.call(coalesce, pick(paste0(income_cols, "_label"))),
    region_lab = do.call(coalesce, pick(paste0(region_cols, "_label")))
  ) |>
  ## Clean up
  mutate(
    income = ifelse(income < 0, NA_real_, income),
    WORK = ifelse(WORK < 0 | WORK == 9, NA_real_, WORK),
    EDULEVEL = ifelse(EDULEVEL < 0 | EDULEVEL == 9, NA_real_, EDULEVEL),
    region_lab = ifelse(c_alphan == "BE-WAL", "Wallonia", region_lab),
    c_alphan = substring(c_alphan, 1, 2)
  ) |>
  select(
    -region_cols, -income_cols, -paste0(income_cols, "_label"), -paste0(region_cols, "_label"),
    -contains("ETHN"), -ends_with("PRTY"), -ends_with("RELIG"), -ends_with("ISCD"), -ends_with("RINC")
  ) |>
  ## Calculate BMI from self reported height and weight
  mutate(bmi = v54 / ((v53 / 100)^2))

rm(na_codes, region_cols, income_cols)

################################################################################
## Determine income data types by country
################################################################################

## Countries with only categorical income
discrete_countries <- ISSP |>
  select(c_alphan, income_lab) |>
  distinct() |>
  group_by(c_alphan) |>
  summarise(n = n()) |>
  filter(n<30) |>
  select(c_alphan) |>
  pull()

## Countries with continuous and categorical income, and the relevant categories
mix_cats <- ISSP |>
  select(c_alphan, income_lab) |>
  distinct() |>
  filter(
    !(c_alphan %in% discrete_countries),
    nchar(income_lab) > 7,
    substring(income_lab, 1, 1) != "-"
  ) |>
  filter(grepl("-", income_lab) | grepl("more", income_lab, ignore.case = T) | grepl("less", income_lab, ignore.case = T)) |>
  group_by(c_alphan) |>
  filter(n() > 3)
mix_countries <- mix_cats |>
  select(c_alphan) |>
  distinct() |>
  pull()

## Countries with only continuous income
continuous_countries <- ISSP |>
  select(c_alphan) |>
  distinct() |>
  filter(
    !(c_alphan %in% mix_countries),
    !(c_alphan %in% discrete_countries)
  ) |>
  pull()

################################################################################
## Generate income quintiles from continuous income
################################################################################

## Specify function for continuous quintiles
## Stops single income levels splitting across quintiles
xtile5 <- function(x) {
  out <- rep(NA_integer_, length(x))
  ok <- is.finite(x)

  qs <- quantile(x[ok], probs = seq(0, 1, by = 0.2), type = 7, na.rm = TRUE)
  qs <- cummax(qs) # protect against non-increasing breaks from ties

  out[ok] <- as.integer(cut(x[ok], breaks = qs, include.lowest = TRUE, right = TRUE, labels = FALSE))
  out
}

## Run function only on ISSP data with continuous income
continuous_quintiles <- ISSP |>
  filter(c_alphan %in% continuous_countries) |>
  group_by(c_alphan) |>
  mutate(wealth_quintile_new = xtile5(income))

################################################################################
## Generate income quintiles from categorical income
################################################################################

## To generate quintiles from categorical income, we use income predicted from 
## sub-national region, education, and work status to rank individuals within 
## income categories. This method is adapted from the HPACC approach

#Set up empty data frame for storing country outputs from the loop
categorical_quintiles <- data.frame()

## Loop through countries with categorical income data, generating quintiles
for (c in discrete_countries) {
  
  ## Select data from country of interest
  d <- ISSP |>
    filter(c_alphan == c) |>
    mutate(level = as.integer(factor(income)))
  idx <- !is.na(d$level)

  ## Fit regression models to predict income category mean from
  ## subnational region, work status, and eductaion level
  ## Multiple regressions parameterised such that predictions can still be run
  ## where either work or education information are missing
  m_full <- lm(level ~ factor(EDULEVEL) + factor(WORK) + region_lab, data = d, subset = idx, na.action = na.exclude)
  m_work <- lm(level ~ factor(WORK) + region_lab, data = d, subset = idx, na.action = na.exclude)
  m_edu <- lm(level ~ factor(EDULEVEL) + region_lab, data = d, subset = idx, na.action = na.exclude)

  ## Set up empty vector for income predictions
  pr <- rep(NA_real_, nrow(d))

  ## Predict income level for all individuals with income recorded
  ## Using fallback models if predictors are missing
  p_full <- as.numeric(predict(m_full, newdata = d[idx, , drop = FALSE]))
  p_work <- as.numeric(predict(m_work, newdata = d[idx, , drop = FALSE]))
  p_edu <- as.numeric(predict(m_edu, newdata = d[idx, , drop = FALSE]))
  p <- p_full
  w <- is.na(p)
  p[w] <- p_work[w]
  w <- is.na(p)
  p[w] <- p_edu[w]

  ## Fill in individuals with missing income randomly
  mu <- mean(p, na.rm = TRUE)
  s <- sd(p, na.rm = TRUE)
  p[is.na(p)] <- rnorm(sum(is.na(p)), mean = mu, sd = s)

  ## Ad final income predictions to the input data
  pr[idx] <- p
  d$princome <- pr

  ## Sort input data by income category then predicted income, then row number
  d1 <- d |>
    arrange(level, princome) |>
    ## Compute income quintiles based on overall rank
    mutate(
      control = ifelse(!is.na(level), cumsum(!is.na(level)), NA_integer_),
      wealth_quintile_new = ntile(control, 5)
    ) |>
    select(-control, -princome)

  categorical_quintiles <- rbind(categorical_quintiles, d1)
}

rm(m_edu, m_full, m_work, d, c, p, pr, p_edu, p_full, p_work, w, idx, mu, s)

################################################################################
## Generate income quintiles from mixed continuous and categorical income
################################################################################

## Parse categorical income ranges and calculate their lower and upper bounds
cat_bounds <- mix_cats |>
  ungroup() |>
  mutate(
    category = sub(".*\\.", "", income_lab),
    category = sub(".*\\/", "", category),
    category = sub("\\;.*", "", category),
    # Correct typo in data
    category = ifelse(category == " 70 001 - 100 00 RUB", "70 001 - 100 000 RUB", category)
  ) |>
  mutate(
    lb_cat = ifelse(grepl("less", category, ignore.case = TRUE), "0", sub("\\-.*", "", category)),
    ub_cat = ifelse(grepl("more", category, ignore.case = TRUE), "1000000000", sub(".*\\-", "", category)),
    across(c(lb_cat, ub_cat), ~ as.numeric(gsub("[^0-9]", "", .x)))
  ) |>
  group_by(c_alphan) |>
  arrange(lb_cat, ub_cat, .by_group = TRUE) |>
  mutate(level = row_number())


## Set up data table with continuous quintiles
mix_quintiles_continuous <- ISSP |>
  ## Filter ISSP data to select only countries with mixed income data
  filter(c_alphan %in% mix_countries) |>
  left_join(cat_bounds, join_by(income_lab, c_alphan)) |>
  mutate(
    ## Retain income only for observations with a continuous income response
    continuous_income = ifelse(is.na(category), income, NA_real_)
  ) |>
  group_by(c_alphan) |>
  ## Compute within-country quintiles using continuous income observations only
  mutate(wealth_quintile_continuous = xtile5(continuous_income))

## Estimate lognormal distribution parameters from continuous income
params <- mix_quintiles_continuous |>
  group_by(c_alphan) |>
  summarise(
    mean_income = mean(continuous_income, na.rm = T),
    var_income = var(continuous_income, na.rm = T),
    sd = sqrt(log(1 + var_income / (mean_income^2))),
    u = log(mean_income) - (sd^2) / 2,
    .groups = "drop"
  )

## Compute the upper and lower bounds of income quintiles by country
quintile_bounds <- mix_quintiles_continuous |>
  select(c_alphan, wealth_quintile_continuous, continuous_income) |>
  filter(!is.na(wealth_quintile_continuous)) |>
  group_by(c_alphan, wealth_quintile_continuous) |>
  summarise(
    lb = min(continuous_income, na.rm = T),
    ub = max(continuous_income, na.rm = T)
  ) |>
  arrange(c_alphan, wealth_quintile_continuous) |>
  group_by(c_alphan) |>
  ## Estimate boundaries between adjacent quintiles by allocating the gap
  ## according to the relative widths of the adjacent income ranges
  mutate(
    len = ub - lb,
    len_next = lead(len),
    lb_next = lead(lb),
    w = len / (len + len_next),
    b = ub + w * (lb_next - ub),
    ub = ifelse(wealth_quintile_continuous <= 4, b, ub),
    lb = ifelse(wealth_quintile_continuous >= 2, lag(b), lb)
  ) |>
  ## Treat the bottom and top quintiles as effectively unbounded
  mutate(
    ub = ifelse(wealth_quintile_continuous == 5, 1000000000, ub),
    lb = ifelse(wealth_quintile_continuous == 1, 0.001, lb)
  ) |>
  select(-len, -len_next, -lb_next, -w, -b) |>
  left_join(params, join_by(c_alphan)) |>
  ## Transform upper and lower bounds to standardized log income
  ## assuming a log-normal distribution
  mutate(
    x_lb = (log(lb) - u) / sd,
    x_ub = (log(ub) - u) / sd
  )

## Transform categorical income bounds to the same standard-normal scale
## as the continuous bounds
cat_bounds <- cat_bounds |>
  left_join(params, join_by(c_alphan)) |>
  mutate(
    lb_cat = ifelse(lb_cat == 0, 0.0001, lb_cat),
    x_lb_cat = (log(lb_cat) - u) / sd,
    x_ub_cat = (log(ub_cat) - u) / sd
  )

## Calculate the expected share of each categorical income range
## falling within each continuous-income quintile under the fitted lognormal distribution
shares <- crossing(
  cat_bounds |> select(c.x = c_alphan, level, x_lb_cat, x_ub_cat),
  quintile_bounds |> select(c.y = c_alphan, wealth_quintile_continuous, x_lb, x_ub)
) |>
  filter(c.x == c.y) |>
  mutate(
    a = pmax(x_lb_cat, x_lb),
    b = pmin(x_ub_cat, x_ub),
    mass = pmax(0, pnorm(b) - pnorm(a))
  ) |>
  select(c_alphan = c.x, level, wealth_quintile_continuous, a, b, mass) |>
  group_by(c_alphan, level) |>
  mutate(mass = mass / sum(mass)) |>
  arrange(wealth_quintile_continuous, level, by.group = T) |>
  ## Calculate cumulative probability mass across quintiles
  mutate(cum = cumsum(mass)) |>
  ungroup() |>
  mutate(q = wealth_quintile_continuous) |>
  select(c_alphan, level, q, cum) |>
  tidyr::pivot_wider(names_from = q, values_from = cum, names_prefix = "c")

## Rank income levels within categories using the same approach as category-only
mix_quintiles_cat <- mix_quintiles_continuous |> mutate(princome = NA_real_)
for (c in mix_countries) {
  d <- mix_quintiles_cat |> filter(c_alphan == c)

  train <- d |>
    filter(is.finite(continuous_income)) |>
    mutate(continuous_income = ifelse(continuous_income == 0, 0.001, continuous_income))

  ## Predict log income using education, work status, and region,
  ## with simpler models used when predictors are unavailable
  m_edu <- lm(log(continuous_income) ~ factor(EDULEVEL) + region_lab, data = train)
  pred <- predict(m_edu, newdata = d)

  m_full <- lm(log(continuous_income) ~ factor(EDULEVEL) + factor(WORK) + region_lab, data = train)
  p_full <- predict(m_full, newdata = d)

  m_work <- lm(log(continuous_income) ~ factor(WORK) + region_lab, data = train)
  p_work <- predict(m_work, newdata = d)

  pred <- p_full
  w <- is.na(pred)
  pred[w] <- p_work[w]
  w <- is.na(pred)
  pred[w] <- predict(m_edu, newdata = d)[w]

  ## Random fill for any missing predictions
  mu <- mean(pred, na.rm = TRUE)
  s <- sd(pred, na.rm = TRUE)
  pred[is.na(pred)] <- rnorm(sum(is.na(pred)), mean = mu, sd = s)

  mix_quintiles_cat$princome[mix_quintiles_cat$c_alphan == c] <- pred
}

## Allocate categorical income observations to quintiles using
## their predicted within-category income ranks
mix_quintiles <- mix_quintiles_cat |>
  left_join(shares, join_by(c_alphan, level)) |>
  group_by(c_alphan, level) |>
  mutate(
    control = ifelse(!is.na(category), rank(princome, ties.method = "first"), NA_real_),
    N = max(control, na.rm = TRUE),
    p = control / N,
    wealth_quintile_new = case_when(
      is.na(category) ~ wealth_quintile_continuous,
      p <= c1 ~ 1L,
      p <= c2 ~ 2L,
      p <= c3 ~ 3L,
      p <= c4 ~ 4L,
      p <= c5 ~ 5L,
      TRUE ~ NA_integer_
    )
  ) |>
  ungroup()

## Check how categorical income observations are allocated across quintiles
## This is done to remove countries with disagreement between continuous and
## categorical quintile bounds (eg. all categorical data falls into one 
## quintile as defined by continuous range)
check <- mix_quintiles |>
  filter(!is.na(category)) |>
  group_by(c_alphan, wealth_quintile_new) |>
  summarise(n = n())
ggplot(check, aes(x = wealth_quintile_new, y = n)) +
  geom_col() +
  facet_wrap(~c_alphan)

mix_quintiles <- mix_quintiles |>
  filter(!(c_alphan %in% c("CZ", "HR"))) |>
  select(names(continuous_quintiles))

################################################################################
## Generate final clean ISSP dataset
################################################################################

## Combine data from separated ISSP tables
final_data <- rbind(continuous_quintiles, categorical_quintiles, mix_quintiles)

## Load country codes from FAOSTAT and population size from the World Bank
countries <- read.csv("data/FAOSTAT_countries.csv")
population <- read.csv("data/wb-population-feb2025.csv") |>
  dplyr::select(Country.Code, population2015 = X2015)

## Generate output data table
final_data <- final_data |>
  ungroup() |>
  ## Combine ISSP data with population and country codes
  left_join(countries |> select(ISO2.Code, ISO3.Code, Country), join_by(c_alphan == ISO2.Code)) |>
  left_join(population, join_by(ISO3.Code == Country.Code)) |>
  mutate(
    bmicat = case_when(
      bmi < 10 ~ NA_real_,
      bmi > 80 ~ NA_real_,
      bmi < 18.5 ~ 1,
      bmi < 25 ~ 2,
      bmi < 30 ~ 3,
      bmi >= 30 ~ 4
    ),
    sex = ifelse(SEX == 1, 0, ifelse(SEX == 2, 1, NA_real_))
  ) |>
  ## select relevant fields and set column names to match other datasets
  select(
    Country = Country,
    sex,
    population2015,
    bmicat,
    rural = URBRURAL,
    age = AGE,
    bmi,
    year = DATEYR,
    w2 = WEIGHT_COM,
    Iso3 = ISO3.Code,
    wealth_quintile = wealth_quintile_new
  )

## Save final output file
write.csv(final_data, "data/ISSP_2021/ISSP_2021_clean.csv", row.names = FALSE)
