library(haven)
library(dplyr)

plum_prep_dir = "C:/Users/kwaxenberg/git/plumv2-data-prep"

#Load raw ISSP survey data ----------------------------------------

#ISSP_2021 = read_dta("data/ISSP_2021/ZA8000_v2-0-0.dta")
ISSP_2011 = read_dta("data/ISSP_2011/ZA5800_v3-0-0.dta") |>
  rename(c_alphan=C_ALPHAN, EDULEVEL = DEGREE)

#Combine individual country variables ------------------------------------------

region_cols = names(select(ISSP_2011, ends_with("_REG")))
income_cols = names(select(ISSP_2011, ends_with("_INC")))

#Define NA codes present in both 2021 and 2011 for income
na_codes = c(-2, 999990, 999999, 999997, 999998, 9999990, 99999990, 99999999, 9999999,
             9999998,9999997)

ISSP = ISSP_2011 |>
  mutate(across(all_of(income_cols), ~ ifelse(.x %in% na_codes, NA_character_, as.character(as_factor(.x))),
                .names = "{.col}_label"), 
         across(all_of(region_cols), ~ ifelse(.x %in% c(0, 9999), NA_character_, as.character(as_factor(.x))),
                .names = "{.col}_label"), 
         across(all_of(income_cols), ~ ifelse(.x %in% na_codes, NA_real_, .x))) |>
  mutate(income = do.call(coalesce, pick(income_cols)),
         income_lab = do.call(coalesce, pick(paste0(income_cols, "_label"))),
         region_lab = do.call(coalesce, pick(paste0(region_cols, "_label")))) |>
  mutate(income = ifelse(income < 0, NA_real_, income),
         WORK = ifelse(WORK <0 | WORK == 9, NA_real_, WORK), 
         EDULEVEL = ifelse(EDULEVEL<0 | EDULEVEL ==9, NA_real_, EDULEVEL),
         region_lab = ifelse(c_alphan == "BE-WAL", "Wallonia", region_lab),
         c_alphan = substring(c_alphan, 1, 2)) |>
  select(-region_cols, -income_cols, -paste0(income_cols, "_label"), -paste0(region_cols, "_label"), 
         -contains("ETHN"), -ends_with("PRTY"), -ends_with("RELIG"), -ends_with("ISCD"), -ends_with("RINC")) |>
  #mutate(bmi = v54/((v53/100)^2)) #2021 variables
  mutate(bmi = V62/((V61/100)^2)) #2011 variables

rm(na_codes, region_cols, income_cols)

#Test for MNAR -----------------------------------------------------------------

missingness = ISSP |>
  group_by(c_alphan) |>
  summarise(n = n(),
            missingRural = sum(is.na(URBRURAL) | URBRURAL == 9)/n,
            missingBMI = sum(is.na(bmi)| bmi <10 | bmi > 80)/n,
            missingIncome = sum(is.na(income) | income <0)/n,
            missingRegion = sum(is.na(region_lab))/n)

omit = missingness |>
  filter(missingBMI > 0.4 |
           missingIncome > 0.2)
summary(glm(is.na(income) ~ AGE + SEX + EDULEVEL,
    data = ISSP %>% filter(c_alphan == "NO"),
    family = binomial()))

#Categorise countries by income data type --------------------------------------

#Create list of countries with only categorical income
discrete_countries = ISSP |>
  select(c_alphan, income_lab)|>
  distinct()|>
  group_by(c_alphan)|>
  summarise(n= n())|>
  #filter(n<30) |> #set threshold for ISSP 2021
  filter(n<50) |> #set threshold for ISSP 2011
  select(c_alphan) |>
  pull()

#Create list of countries with continuous and categorical income, and the relevant categories
mix_cats = ISSP |>
  select(c_alphan, income_lab)|>
  distinct()|>
  filter(!(c_alphan %in% discrete_countries), 
         nchar(income_lab)>7, 
         substring(income_lab, 1, 1) != "-") |>
  filter(grepl("-",income_lab) | grepl("more", income_lab, ignore.case = T) | grepl("less",income_lab, ignore.case=T)) |>
  group_by(c_alphan)|>
  filter(n()>3)
mix_countries = mix_cats |>  
  select(c_alphan) |>
  distinct()|>
  pull()

#Create list of countries with only continuous income
continuous_countries = ISSP |>
  select(c_alphan)|>
  distinct()|>
  filter(!(c_alphan %in% mix_countries),
         !(c_alphan %in% discrete_countries)) |>
  pull()

#Quintiles for continuous income -----------------------------------------------

#Specify function which stops single income levels splitting across quintiles
xtile5 = function(x) {
  out = rep(NA_integer_, length(x))
  ok = is.finite(x)

  qs = quantile(x[ok], probs = seq(0, 1, by = 0.2), type = 7, na.rm = TRUE)
  qs = cummax(qs)  # protect against non-increasing breaks from ties
  
  out[ok] = as.integer(cut(x[ok], breaks = qs, include.lowest = TRUE, right = TRUE, labels = FALSE))
  out
}

continuous_quintiles = ISSP |> 
  filter(c_alphan %in% continuous_countries) |> 
  group_by(c_alphan) |>
  mutate(wealth_quintile_new = xtile5(income))

#Quintiles for countries with income categories only ---------------------------

categorical_quintiles = data.frame()
for(c in discrete_countries) {
  
  d = ISSP |> 
    filter(c_alphan == c) |> 
    mutate(level = as.integer(factor(income)))
  idx = !is.na(d$level)
  
  # Fit models on non-missing income only
  m_full = lm(level ~ factor(EDULEVEL) + factor(WORK) + region_lab, data = d, subset = idx, na.action = na.exclude)
  m_work = lm(level ~ factor(WORK) + region_lab, data = d, subset = idx, na.action = na.exclude)
  m_edu  = lm(level ~ factor(EDULEVEL) + region_lab, data = d, subset = idx, na.action = na.exclude)
  
  # Predict only for idx rows, with fallback
  pr = rep(NA_real_, nrow(d))
  
  p_full = as.numeric(predict(m_full, newdata = d[idx, , drop = FALSE]))
  p_work = as.numeric(predict(m_work, newdata = d[idx, , drop = FALSE]))
  p_edu = as.numeric(predict(m_edu, newdata = d[idx, , drop = FALSE]))
  
  p = p_full
  w = is.na(p); p[w] = p_work[w]
  w = is.na(p); p[w] = p_edu[w]
  
  mu = mean(p, na.rm = TRUE)
  s  = sd(p, na.rm = TRUE)
  p[is.na(p)] = rnorm(sum(is.na(p)), mean = mu, sd = s)
  
  pr[idx] = p
  d$princome = pr
  
  # Sort by level then princome, then row number
  d1 = d |>
    arrange(level, princome) |>
    mutate(control = ifelse(!is.na(level), cumsum(!is.na(level)), NA_integer_),
           wealth_quintile_new = ntile(control, 5)) |>
    select(-control, -princome)
  
  categorical_quintiles = rbind(categorical_quintiles, d1)
}

rm(m_edu, m_full, m_work, d, c, p, pr, p_edu, p_full, p_work, w, idx, mu, s)

#Quintiles for mixed categorical and continuous income -------------------------

#Calculate upper and lower bounds for categories
cat_bounds =  mix_cats |>
  ungroup()|>
  mutate(category = sub(".*\\.", "", income_lab),
         category = sub(".*\\/", "", category),
         category = sub("\\;.*", "", category),
         #Correct typo in data
         category = ifelse(category == " 70 001 - 100 00 RUB","70 001 - 100 000 RUB", category)) |>
  mutate(lb_cat = ifelse(grepl("less", category, ignore.case = TRUE),"0", sub("\\-.*", "", category)),
         ub_cat = ifelse(grepl("more", category, ignore.case = TRUE), "1000000000", sub(".*\\-", "", category)),
         across(c(lb_cat, ub_cat), ~as.numeric(gsub("[^0-9]", "", .x)))) |>
  group_by(c_alphan) |>
  arrange(lb_cat, ub_cat, .by_group = TRUE) |>
  mutate(level = row_number())

mix_quintiles_continuous = ISSP |>
  filter(c_alphan %in% mix_countries) |>
  left_join(cat_bounds, join_by(income_lab, c_alphan)) |>
  mutate(
    #Create variable which does not include categorical income responses
    continuous_income = ifelse(is.na(category), income, NA_real_))|>
  group_by(c_alphan)|>
  mutate(wealth_quintile_continuous = xtile5(continuous_income))

params = mix_quintiles_continuous |>
  group_by(c_alphan) |>
  summarise(
    mean_income = mean(continuous_income, na.rm = T),
    var_income  = var(continuous_income, na.rm = T),
    sd = sqrt(log(1 + var_income / (mean_income^2))),
    u  = log(mean_income) - (sd^2)/2,
    .groups = "drop"
  )

quintile_bounds = mix_quintiles_continuous |>
  select(c_alphan, wealth_quintile_continuous, continuous_income) |>
  filter(!is.na(wealth_quintile_continuous))|>
  group_by(c_alphan, wealth_quintile_continuous)|>
  summarise(lb = min(continuous_income, na.rm = T),
         ub = max(continuous_income, na.rm = T)) |>
  arrange(c_alphan, wealth_quintile_continuous) |>
  group_by(c_alphan) |>
  mutate(
    len = ub - lb,
    len_next = lead(len),
    lb_next  = lead(lb),
    w = len / (len + len_next),
    b = ub + w * (lb_next - ub),
    ub = ifelse(wealth_quintile_continuous <=4, b, ub),
    lb = ifelse(wealth_quintile_continuous >=2, lag(b), lb)) |>
  mutate(ub = ifelse(wealth_quintile_continuous == 5, 1000000000, ub),
         lb = ifelse(wealth_quintile_continuous == 1, 0.001, lb)) |>
  select(-len, -len_next, -lb_next, -w, -b) |>
  left_join(params, join_by(c_alphan)) |>
  mutate(
    x_lb = (log(lb) - u) / sd,
    x_ub = (log(ub) - u) / sd
  )

cat_bounds = cat_bounds |>
  left_join(params, join_by(c_alphan) ) |>
  mutate(
    lb_cat = ifelse(lb_cat == 0, 0.0001, lb_cat),
    x_lb_cat = (log(lb_cat) - u) / sd,
    x_ub_cat = (log(ub_cat) - u) / sd
  )

shares = crossing(
  cat_bounds |> select(c.x = c_alphan, level, x_lb_cat, x_ub_cat),
  quintile_bounds |> select(c.y = c_alphan, wealth_quintile_continuous, x_lb, x_ub)
) |>
  filter(c.x == c.y) |>
  mutate(
    a = pmax(x_lb_cat, x_lb),
    b = pmin(x_ub_cat, x_ub),
    #calculate interval overlap
    mass = pmax(0, pnorm(b) - pnorm(a))
  ) |>
  select(c_alphan = c.x, level, wealth_quintile_continuous, a, b, mass) |>
  group_by(c_alphan, level) |>
  mutate(mass = mass / sum(mass)) |>
  arrange(wealth_quintile_continuous, level, by.group = T) |>
  mutate(cum = cumsum(mass)) |>     # cumulative shares for quintiles 1..5
  ungroup() |>
  mutate(q = wealth_quintile_continuous) |>
  select(c_alphan, level, q, cum) |>
  tidyr::pivot_wider(names_from = q, values_from = cum, names_prefix = "c")

mix_quintiles_cat = mix_quintiles_continuous |> mutate(princome = NA_real_)
for (c in mix_countries) {
  
  d = mix_quintiles_cat |> filter(c_alphan == c)

  train = d |> filter(is.finite(continuous_income)) |> 
    mutate(continuous_income = ifelse(continuous_income == 0, 0.001, continuous_income))
  
  # main model + fallbacks (like Stata)
  m_edu = lm(log(continuous_income) ~ factor(EDULEVEL) + region_lab, data = train)
  pred = predict(m_edu, newdata = d)
  
  m_full = lm(log(continuous_income) ~ factor(EDULEVEL) + factor(WORK) + region_lab, data = train)
  p_full = predict(m_full, newdata = d)
  
  m_work = lm(log(continuous_income) ~ factor(WORK) + region_lab, data = train)
  p_work = predict(m_work, newdata = d)
  
  pred = p_full
  w = is.na(pred); pred[w] = p_work[w]
  w = is.na(pred); pred[w] = predict(m_edu, newdata = d)[w]
  
  # Stata-style random fill for any remaining missing predictions
  mu = mean(pred, na.rm = TRUE)
  s  = sd(pred, na.rm = TRUE)
  pred[is.na(pred)] = rnorm(sum(is.na(pred)), mean = mu, sd = s)
  
  mix_quintiles_cat$princome[mix_quintiles_cat$c_alphan == c] = pred
}

mix_quintiles = mix_quintiles_cat |>
  left_join(shares, join_by(c_alphan, level)) |>
  group_by(c_alphan, level) |>
  mutate(
    control = ifelse(!is.na(category), rank(princome, ties.method="first"), NA_real_),
    N = max(control, na.rm=TRUE),
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
  ) |> ungroup()

#Check which countries continuous income doesn't match categories
check = mix_quintiles |> filter(!is.na(category)) |> group_by(c_alphan, wealth_quintile_new) |> summarise(n = n())
ggplot(check, aes(x=wealth_quintile_new, y = n))+
  geom_col() + 
  facet_wrap(~c_alphan)

mix_quintiles = mix_quintiles |>
  filter(!(c_alphan %in% c("CZ", "HR")))|>
  select(names(continuous_quintiles))

#Combine data and write to file ------------------------------------------------
final_data = rbind(continuous_quintiles, categorical_quintiles)
final_data = rbind(continuous_quintiles, categorical_quintiles, mix_quintiles)
countries = read.csv("data/FAOSTAT_countries.csv")
population = read.csv("data/wb-population-feb2025.csv") |>
  dplyr::select(Country.Code, population2015 = X2015)

final_data = final_data |>
  ungroup()|>
  mutate(bmicat = case_when(
           bmi<10 ~ NA_real_,
           bmi>80 ~ NA_real_,
           bmi<18.5 ~ 1,
           bmi<25 ~ 2,
           bmi<30 ~ 3,
           bmi>=30 ~ 4
         ),
         sex = ifelse(SEX==1, 0, ifelse(SEX==2, 1, NA_real_)))|>
  left_join(countries |> select(ISO2.Code, ISO3.Code, Country), join_by(c_alphan == ISO2.Code))|>
  left_join(population, join_by(ISO3.Code==Country.Code))|>
  select(Country = Country, 
         sex,
         population2015,
         bmicat,
         rural = URBRURAL,
         age = AGE,
         bmi,
         year = DATEYR, 
         #w2 = WEIGHT_COM, #2021
         w2= WEIGHT, #2011
         Iso3 = ISO3.Code,
         wealth_quintile = wealth_quintile_new)

missingness_final = final_data |>
  group_by(Iso3) |>
  summarise(n = n(),
            missingBMI = sum(is.na(bmicat))/n,
            missingIncome = sum(is.na(wealth_quintile))/n)

final_data$Country |> unique()

write.csv(final_data, "data/ISSP_2011/ISSP_2011_clean.csv", row.names=FALSE)

ggplot(final_data |> filter(!is.na(bmicat), !is.na(wealth_quintile), Iso3 == "ITA"), aes(x=factor(wealth_quintile), y=bmi))+
  geom_boxplot()+
  facet_wrap(~Country)
