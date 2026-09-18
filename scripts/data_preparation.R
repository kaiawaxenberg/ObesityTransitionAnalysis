library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(data.table)
library(gt)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# Load raw HPACC survey data and combine ----------------------------------------

# HPACC_LMIC1 <- read.csv("data/HPACC/HPACC_LMIC_Pt1_2025-07-15.csv")
# HPACC1 <- HPACC_LMIC1 |>
#   dplyr::select(Country, sex, marital, edyears, rural, age, bmi, bmicat, wealth_quintile,
#                 year, X_ISO3C_, w2, population2015, psu_num, stratum_num, pregnant)
# 
# HPACC_LMIC2 <- read.csv("data/HPACC/HPACC_LMICs_Pt2_2025-07-15.csv")
# HPACC2 <- HPACC_LMIC2 |>
#   dplyr::select(Country, sex, marital, edyears, rural, age, bmi, bmicat, wealth_quintile,
#                 year, X_ISO3C_, psu_num, w2, population2015, stratum_num, pregnant)
# 
# HPACC_HIC <- read.csv("data/HPACC/HPACC_main_HIC_2025-07-15.csv")
# HPACC3 <- HPACC_HIC |>
#   dplyr::select(Country, sex, marital, edyears, rural, age, bmi, bmicat, wealth_quintile,
#                 year, X_ISO3C_, psu_num, w2, stratum_num, pregnant)
# 
# #variables ignored: svy, income_cat, educat, wt, ht, asset_index, 
# #income_mth, income_wk, income_yr, race
# 
# population = read.csv("data/wb-population-feb2025.csv") |>
#   dplyr::select(Country.Code, population2015 = X2015)
# HPACC3 = left_join(HPACC3, population, join_by(X_ISO3C_==Country.Code))
# 
# HPACC = rbind(HPACC1, HPACC2, HPACC3) |> rename(Iso3 = X_ISO3C_)
# rm(HPACC1, HPACC2, HPACC3, HPACC_LMIC1, HPACC_LMIC2, HPACC_HIC, population)
# 
# HPACC = HPACC |>
#   filter(Country != "BOND" & Country != "3")|>
#   mutate(Iso3 = case_when(Country == "Liberia 2022" ~ "LBR",
#                           Country == "Guyana" ~ "GUY",
#                           TRUE ~ Iso3))|>
#   mutate(pregnant = case_when(pregnant == "0" ~ "No",
#                               pregnant == "1" ~ "Yes",
#                               TRUE ~ pregnant))
# 
# save.image("HPACC.RData")

# Load cleaned ISSP and EHIS survey data and combine ---------------------------

#Load cleaned survey datasets

load("Rdata/HPACC.RData") # HPACC.RData from lines 6-42

ISSP_data <- read.csv("data/ISSP_2021/ISSP_2021_clean.csv") |> 
  mutate(dataset = "ISSP",
         pregnant = NA)

EHIS_data <- read.csv("data/EHIS/EHIS_Wave3_full.csv") |> 
  mutate(dataset = "EHIS",
         pregnant = NA)

# Combine datasets
all_data <- HPACC |>
  mutate(dataset = "HPACC") |>
  select(names(ISSP_data)) |>
  rbind(ISSP_data, EHIS_data)

#Clean up intermediate data tables
rm(HPACC, ISSP_data, EHIS_data)

# Read in  national-level data --------------------------------------------------

gdpPcData <- read.csv("data/wb_gdp_2021ppp.csv") |>
  dplyr::select(-c(Indicator.Name, Indicator.Code)) |>
  pivot_longer(cols = -c(Country.Name, Country.Code), names_to = "year", values_to = "gdpPc") |>
  mutate(year = substring(year, 2, 5))

food_access <- read.csv("data/daily-per-capita-caloric-supply/daily-per-capita-caloric-supply.csv") |>
  filter(Code != "")

giniData <- read.csv("data/Gini/API_SI.POV.GINI_DS2_en_csv_v2_115456.csv") |>
  dplyr::select(-c(Indicator.Name, Indicator.Code)) |>
  pivot_longer(cols = -c(Country.Name, Country.Code), names_to = "year", values_to = "gini") |>
  mutate(year = substring(year, 2, 5)) |>
  drop_na()

incomeShareData <- read.csv("data/WDI Income Shares/a094a923-2b13-4fef-b4fd-218221b23dc3_Data.csv") |>
  pivot_longer(cols = -c(Country.Name, Country.Code, Series.Name, Series.Code), names_to = "year", values_to = "incomeShare") |>
  mutate(year = substring(year, 2, 5)) |>
  mutate(incomeShare = ifelse(incomeShare == ".." | incomeShare == "", NA, incomeShare)) |>
  mutate(quintile = case_match(
    Series.Code,
    "SI.DST.FRST.20" ~ 1,
    "SI.DST.02ND.20" ~ 2,
    "SI.DST.03RD.20" ~ 3,
    "SI.DST.04TH.20" ~ 4,
    "SI.DST.05TH.20" ~ 5
  )) |>
  dplyr::select(-c(Series.Name, Series.Code)) |>
  drop_na()

region <- read.csv("data/countryMapping.csv") |>
  dplyr::select(CountryCode, Region) |>
  add_row(CountryCode = "TLS", Region = "") |>
  add_row(CountryCode = "NRU", Region = "") |>
  add_row(CountryCode = "SYC", Region = "") |>
  mutate(SID = ifelse(
    CountryCode %in% c(
      "CPV", "COM", "FJI", "GRD", "GUY", "HTI", "KIR", "MHL",
      "NRU", "LCA", "WSM", "STP", "SYC", "SLB", "TLS",
      "TTO", "TUV", "VUT"
    ),
    TRUE, FALSE
  )) |>
  mutate(Region = ifelse(SID, "SID", Region))

KOFdata <- read.csv("data/KOFGI_2025_public.csv")|>
  select(code, year, KOFGI)

UPFdata <- read.csv("data/UPF_sales_FSCI.csv")|>
  rename(Country = X)|>
  pivot_longer(cols = -c(Country), names_to = "year", values_to = "UPFSales") |>
  mutate(year = substring(year, 2, 5)) |>
  left_join(read.csv("data/countryMapping.csv"), join_by(Country))|>
  mutate(CountryCode = case_when(Country == "Netherlands (Kingdom of the)" ~ "NLD",
                             Country == "Eswatini" ~ "SWZ",
                             Country == "Cayman Islands" ~ "CYM",
                             Country == "Seychelles" ~ "SYC",
                             Country == "Nauru" ~ "NRU",
                             TRUE ~ CountryCode))
  

serviceShareData <- read.csv("data/WorldBankServiceShare/API_NV.SRV.TOTL.ZS_DS2_en_csv_v2_353855.csv") |>
  dplyr::select(-c(Indicator.Name, Indicator.Code)) |>
  pivot_longer(cols = -c(Country.Name, Country.Code), names_to = "year", values_to = "serviceShareGDP") |>
  mutate(year = substring(year, 2, 5)) |>
  drop_na()

# Combine Survey data with national-level data ----------------------------------

nearest_year_join <- function(survey_data, macro_data,
                              country_col_survey, year_col_survey,
                              country_col_macro, year_col_macro,
                              quintiles = F,
                              suffix) {
  dt <- as.data.table(survey_data)
  macro_dt <- as.data.table(macro_data)
  
  # Make local copies of join cols with standard names to simplify the join
  dt[, `ctry` := get(country_col_survey)]
  dt[, `yr` := get(year_col_survey)]
  macro_dt[, `ctry` := get(country_col_macro)]
  macro_dt[, `yr` := get(year_col_macro)]
  
  # Key macro for fast rolling join
  setkey(macro_dt, ctry, yr)
  
  # Do the join: keeps all rows from survey data
  if (quintiles == T) {
    out <- macro_dt[dt, on = .(ctry, wealth_quintile, yr), roll = "nearest"]
  } else {
    out <- macro_dt[dt, on = .(ctry, yr), roll = "nearest"]
  }
  out[, year_macro := get(year_col_macro)]
  gap_name <- paste0("year_gap_", suffix)
  out[, (gap_name) := abs(yr - year_macro)]
  
  # drop the helper cols and/or macro keys
  out[, c("ctry", "yr") := NULL]
  
  out[]
}

model_data_0 <- all_data |>
  mutate(
    dataYear = as.integer(substring(year, 1, 4)),
    Iso3 = ifelse(Iso3 == "F41", "CHN", Iso3)
  ) |>
  nearest_year_join(
    macro_data = gdpPcData |> mutate(gdpYear = as.integer(year)) |> select(-year),
    country_col_survey = "Iso3",
    year_col_survey = "dataYear",
    country_col_macro = "Country.Code",
    year_col_macro = "gdpYear",
    suffix = "gdpPc"
  ) |>
  nearest_year_join(
    macro_data = food_access |> select(-Entity) |> rename(kcalYear = Year),
    country_col_survey = "Iso3",
    year_col_survey = "dataYear",
    country_col_macro = "Code",
    year_col_macro = "kcalYear",
    suffix = "kcal"
  ) |>
  nearest_year_join(
    macro_data = giniData |> mutate(giniYear = as.integer(year)) |> select(-Country.Name, -year),
    country_col_survey = "Iso3",
    year_col_survey = "dataYear",
    country_col_macro = "Country.Code",
    year_col_macro = "giniYear",
    suffix = "gini"
  ) |>
  nearest_year_join(
    macro_data = serviceShareData |> mutate(serviceYear = as.integer(year)) |> select(-Country.Name, -year),
    country_col_survey = "Iso3",
    year_col_survey = "dataYear",
    country_col_macro = "Country.Code",
    year_col_macro = "serviceYear",
    suffix = "serviceShare"
  ) |>
  nearest_year_join(
    macro_data = UPFdata |> mutate(upfYear = as.integer(year)) |> select(upfYear, CountryCode, UPFSales),
    country_col_survey = "Iso3",
    year_col_survey = "dataYear",
    country_col_macro = "CountryCode",
    year_col_macro = "upfYear",
    suffix = "upf"
  ) |>
  nearest_year_join(
    macro_data = KOFdata |> rename(kofYear = year),
    country_col_survey = "Iso3",
    year_col_survey = "dataYear",
    country_col_macro = "code",
    year_col_macro = "kofYear",
    suffix = "kof"
  ) |>
  nearest_year_join(
    macro_data = incomeShareData |>
      mutate(shareYear = as.integer(year)) |>
      rename(wealth_quintile = quintile) |>
      select(-Country.Name, year),
    country_col_survey = "Iso3",
    year_col_survey = "dataYear",
    country_col_macro = "Country.Code",
    year_col_macro = "shareYear",
    suffix = "incomeShare",
    quintiles = T
  ) |>
  left_join(region, join_by(Iso3 == CountryCode))

#clean up intermediate tables
rm(food_access, gdpPcData, incomeShareData, region, giniData, nearest_year_join,
   KOFdata, UPFdata, serviceShareData)

#Clean data and fix survey inconsistencies -------------------------------------

model_data_1 <- model_data_0 |>
  select(w2, Country, Iso3, bmi, bmicat, dataYear,
         UPFSales, KOFGI, serviceShareGDP, pregnant,
          wealth_quintile, age, sex, gdpPc, rural,
          kcalSupply = Daily.calorie.supply.per.person,
          incomeShare, gini, population2015, dataset, Region
  ) |>
  mutate(
    #Harmonise rural metric across surveys
    rural = case_when(dataset == "EHIS"~ case_when(rural %in% c("3", "2") ~ "Rural",
                                                   rural %in% c("1") ~ "Urban"),
                      dataset == "ISSP" ~ case_when(rural %in% c("4","5","3") ~ "Rural",
                                                    rural %in% c("1","2") ~ "Urban"),
                      TRUE ~ rural),
    #Fix ISSP China mainland data
    Iso3 = ifelse(Country == "China, mainland", "CHN", Iso3),
    population2015 = ifelse(Country == "China, mainland", 
                            all_data$population2015[which(all_data$Iso3=="CHN")][1],
                            population2015),
    #Correct EHIS Italy which is a bit inconsistent, set age to midpoint 
    bmicat = ifelse(Country=="Italy" & dataset == "EHIS", bmi, bmicat),
    age=ifelse(Country=="Italy" & dataset == "EHIS", 
               as.numeric(substring(age, 1, 2)) +2, age),
    # Clean multiple codes for missing data, replace missing values with NA
    bmicat = ifelse(bmicat %in% 1:4, bmicat, NA_real_),
    bmi = ifelse(bmi > 80 | bmi < 10, NA_real_, bmi),
    wealth_quintile = ifelse(wealth_quintile %in% 1:5, wealth_quintile, NA_real_),
    age = ifelse(as.numeric(age) < 0 | as.numeric(age) > 150, NA_real_, as.numeric(age)),
    sex = ifelse(sex %in% 0:1, sex, NA_real_),
    pregnant = ifelse(pregnant %in% c("Yes", "No"), pregnant, NA),
    rural = ifelse(rural %in% c("Urban", "Rural"), rural, NA),
    #Fix variable types
    across(all_of(c("sex")), as.factor),
    across(all_of(c("wealth_quintile")), ~ factor(.x, ordered = T)),
    #Add BMI category
    BMI_Category = case_when(
      bmicat == 1 ~ "Underweight",
      bmicat == 2 ~ "Healthy weight",
      bmicat == 3 ~ "Overweight",
      bmicat == 4 ~ "Obesity",
      bmi < 10 ~ NA,
      bmi > 80 ~ NA,
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Healthy weight",
      bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obesity"
    ),
    BMI_Category = factor(BMI_Category, ordered = F),
    #Calculate income distance from mean and group income
    income_ratio = as.double(incomeShare) / 20,
    group_gdpPc = as.double(incomeShare) * gdpPc * 5,
    #Determine which rows are complete cases
    insample = ifelse(!is.na(BMI_Category) & age >= 20 &
                        !is.na(wealth_quintile) &
                        !is.na(sex), 
                      TRUE, FALSE
    ),
    insample_rural = ifelse(insample & !is.na(rural),
                      TRUE, FALSE
    ),
    obesity = factor(BMI_Category == "Obesity"),
    overweight = factor(BMI_Category %in% c("Overweight", "Obesity")),
    BMI_Category3 = factor(ifelse(BMI_Category %in% c("Healthy weight", "Underweight"),
                                           "Healthy/low weight",
                                           as.character(BMI_Category)
             )),
    rural = factor(rural)
  )

#Clean up intermediate data
rm(model_data_0)

# Remove surveys with missing data ----------------------------------------------

missingness_summary <- model_data_1 |>
  group_by(Country, dataset, Iso3, Region) |>
  summarise(
    n = n(),
    pct_missing_bmi = mean(is.na(bmicat)),
    pct_missing_income = mean(is.na(wealth_quintile)),
    pct_missing_age = mean(is.na(age)),
    pct_missing_sex = mean(is.na(sex)),
    pct_missing_rural = mean(is.na(rural)),
    pct_incomplete_case = mean(is.na(sex) |
                                 is.na(age) |
                                 is.na(wealth_quintile) |
                                 is.na(bmicat)),
    missing_country_data = mean(is.na(gdpPc)),
    .groups = "drop"
  )

# Determine surveys with omitted variables to exclude
structural_missing <- missingness_summary |>
  group_by(Country, dataset, Iso3, Region) |>
  filter(missing_country_data==1 | pct_incomplete_case==1) |>
  mutate(code = paste0(Iso3, dataset))

missingness_summary_rural <- model_data_1 |>
  group_by(Country, dataset, Iso3, Region) |>
  summarise(
    n = n(),
    pct_incomplete_case = mean(is.na(sex) |
                                 is.na(age) |
                                 is.na(wealth_quintile) |
                                 is.na(rural) |
                                 is.na(bmicat)),
    missing_country_data = mean(is.na(gdpPc)),
    .groups = "drop"
  )
structural_missing_rural <- missingness_summary_rural |>
  group_by(Country, dataset, Iso3, Region) |>
  filter(missing_country_data==1 | pct_incomplete_case==1)

# Remove surveys with structural missingness
sample_data_1 <- model_data_1 |>
  filter(!paste0(Iso3, dataset) %in% structural_missing$code) |>
  droplevels()

#Generate final sample ---------------------------------------------------------

# Prescribe decades for age groups
age_cuts <- c(-Inf, 28, 38, 48, 58, 68, 78, Inf)

#Weight surveys with multiple weighting options, and rescale for mblogit
sample_data = sample_data_1 |>
  filter(insample == TRUE)|>
  group_by(Country, dataset) |>
  mutate(
    rural_w = ifelse(insample_rural == TRUE, w2, 0),
    mean_w = mean(w2[w2!=0], na.rm = TRUE),
    mean_w_rural = mean(rural_w[rural_w!=0], na.rm  = TRUE),
    rural_w = ifelse(is.na(rural_w) & insample_rural == TRUE, 
                     mean_w_rural/sum(rural_w, na.rm=T), 
                     rural_w/sum(rural_w, na.rm=T)),
    w = ifelse(is.na(w2), 
                mean_w/sum(w2, na.rm=T), 
                w2/sum(w2, na.rm=T))
  )|>
  filter(w>0)|>
  group_by(Iso3, dataset)|>
  mutate(
    all_w = sum(w, na.rm = T),
    all_w_rural = sum(rural_w, na.rm = T),
    w_eq_country = w / all_w,
    w_eq_country_rural = rural_w/all_w_rural,
    w_eq_indiv = w * sum(w>0) / all_w,
    w_eq_indiv_rural = rural_w * sum(rural_w > 0) / all_w_rural
  ) |>
  ungroup() |>
  mutate(
    #Prepare variables for mblogit
    age_group = factor(cut(age, breaks = age_cuts)),
    logGdpPc = log(gdpPc),
    logGroupGdpPc = log(group_gdpPc),
    #Normalize weights to avoid very small weights
    popshare = population2015/sum(population2015, na.rm=T),
    w_eq_country = w_eq_country * sum(w_eq_country>0) / sum(w_eq_country, na.rm = TRUE),
    w_eq_indiv =  w_eq_indiv * sum(w_eq_indiv>0) / sum(w_eq_indiv, na.rm = TRUE),
    w_country_pop = w_eq_country * popshare * sum(w_eq_country>0) / sum(w_eq_country * popshare, na.rm = TRUE),
    w_eq_country_rural = w_eq_country_rural * sum(w_eq_country_rural>0, na.rm=TRUE) / sum(w_eq_country_rural, na.rm = TRUE),
    w_eq_indiv_rural =  w_eq_indiv_rural * sum(w_eq_indiv_rural>0, na.rm=TRUE) / sum(w_eq_indiv_rural, na.rm = TRUE),
    w_country_pop_rural = w_eq_country_rural * popshare * sum(w_eq_country_rural>0, na.rm=TRUE) / sum(w_eq_country_rural * popshare, na.rm = TRUE)
  ) |>
  #Scale variables for stable parameterisation
  mutate(across(c(age, kcalSupply, gini, KOFGI, UPFSales, serviceShareGDP,
                  incomeShare, income_ratio, logGdpPc, logGroupGdpPc),
                ~ as.numeric(.x)),
         across(c(age, income_ratio, logGroupGdpPc),
                ~ as.numeric(scale(.x)),
                   .names = "{.col}_z"),
         across(c(kcalSupply, gini, KOFGI, UPFSales, serviceShareGDP,
                  incomeShare, logGdpPc, dataYear),
                ~ (.x-mean(unique(.x)))/sd(unique(.x)),
                .names = "{.col}_z")) |>
  droplevels()

scaling_params_country <- sample_data |>
  rename(incomeRatio = income_ratio)|>
  summarise(across(c(kcalSupply, gini, KOFGI, UPFSales, serviceShareGDP,
                     incomeShare, logGdpPc, dataYear),
                   list(mean = ~mean(unique(as.numeric(.x)), na.rm=T),
                        sd = ~sd(unique(as.numeric(.x)), na.rm=T))))
scaling_params_indiv <- sample_data |>
  rename(incomeRatio = income_ratio)|>
  summarise(across(c(age, incomeRatio, logGroupGdpPc),
                   list(mean = ~mean(as.numeric(.x), na.rm=T),
                        sd = ~sd(as.numeric(.x), na.rm=T))))

scaling_params = cbind(scaling_params_country, scaling_params_indiv)|>
  tidyr::pivot_longer(everything(),
                      names_to = c("variable", "stat"),
                      names_sep = "_",
                      values_to = "value")

rm(scaling_params_country, scaling_params_indiv)

# remove duplicated country data for pooled dataset
countries_to_keep <- sample_data |>
  group_by(Iso3, dataset) |>
  summarise(n = n()) |>
  group_by(Iso3) |>
  mutate(n = n()) |>
  summarise(dataset = case_when(
    "EHIS" %in% dataset ~ "EHIS",
    "HPACC" %in% dataset ~ "HPACC",
    TRUE ~ dataset[1]
  )) |>
  mutate(code = paste0(dataset, Iso3))

incomeGroups = read_excel("data/wb-incomeclassification.xlsx") |>
  select(Iso3="...1", group ="2017")
regions = read.csv("data/countryMapping.csv") |>
  select(Iso3=CountryCode, Region)

left_join(incomeGroups, countries_to_keep, join_by(Iso3)) |>
  group_by(group)|>
  filter(!is.na(dataset))|>
  summarise(n=n())

sample_data <- sample_data |>
  mutate(pooledData = paste0(dataset, Iso3) %in% countries_to_keep$code)

#Combine and render summary table ----------------------------------------------

#weighted SD and mean functions
wsd <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]
  w <- w[keep]
  xbar <- weighted.mean(x, w)
  V1 <- sum(w)
  V2 <- sum(w^2)
  sqrt(sum(w * (x - xbar)^2) / (V1 - V2 / V1))
}
wmean <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  weighted.mean(x[keep], w[keep])
}

summary <- sample_data_1 |>
  filter(paste0(dataset, Iso3) %in% countries_to_keep$code) |>
  mutate(female = (sex == "1"), urban = (rural == "Urban")) |>
  group_by(Iso3, dataset) |>
  summarise(
    n = n(),
    `Survey year, Mean` = round(mean(dataYear)),
    `BMI, Mean (SD)`     = sprintf("%.1f (%.1f)", wmean(bmi, w2), wsd(bmi, w2)),
    `BMI, Missing`       = sprintf("%d (%.1f%%)", sum(is.na(bmi)), 100 * mean(is.na(bmi))),
    `Age, Mean (SD)`     = sprintf("%.1f (%.1f)", wmean(age, w2), wsd(age, w2)),
    `Age, Missing`       = sprintf("%d (%.1f%%)", sum(is.na(age)), 100 * mean(is.na(age))),
    `Female, n (%)`      = sprintf("%d (%.1f%%)", sum(female, na.rm=T), 100 * sum(female, na.rm=T) / n()),
    `Female, Missing`    = sprintf("%d (%.1f%%)", sum(is.na(female)), 100 * mean(is.na(female))),
    `Urban, n (%)`       = sprintf("%d (%.1f%%)", sum(urban, na.rm=T), 100 * sum(urban, na.rm=T) / n()),
    `Urban, Missing`     = sprintf("%d (%.1f%%)", sum(is.na(urban)), 100 * mean(is.na(urban))),
    .groups = "drop"
  )

countryMapping <- read.csv("data/countryMapping.csv")

summary_table = summary |>
  left_join(countryMapping |> select(CountryCode, Country), join_by(Iso3==CountryCode))|>
  mutate(Country = case_when(Iso3=="NRU"~ "Nauru",
                   Iso3=="PSE"~ "Palestine",
                   Iso3=="SYC"~ "Seychelles",
                   Iso3=="TLS"~ "Timor-Leste",
                   TRUE ~ Country))|>
  arrange(Country, dataset) |>
  select(-Iso3)|>
  gt(rowname_col = "Country", groupname_col = "dataset") |>
  opt_row_striping() |>
  tab_style(cell_text(weight = "bold"), cells_column_labels())

gtsave(summary_table, "country_summary_table.docx")


summary_dataset = sample_data_1 |>
  filter(paste0(dataset, Iso3) %in% countries_to_keep$code) |>
  left_join(incomeGroups, join_by(Iso3)) |>
  select(group, rural, sex, age, bmi)|>
  group_by(group)|>
  tbl_summary(
    by = group,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "always",           # shows a "Missing" row for every variable
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)",   # this is the fix
    digits = list(all_continuous() ~ 1)
  ) |>
  modify_header(label = "")|>
  bold_labels()

summary_group <- sample_data_1 |>
  filter(paste0(dataset, Iso3) %in% countries_to_keep$code) |>
  left_join(incomeGroups, join_by(Iso3))|>
  mutate(female = (sex == "1"), urban = (rural == "Urban")) |>
  group_by(group) |>
  summarise(
    n = n(),
    `BMI, Mean (SD, n)`     = sprintf("%.1f (%.1f) n = %.1f", wmean(bmi, w2), wsd(bmi, w2), sum(!(is.na(bmi)))),
    `Age, Mean (SD, n)`     = sprintf("%.1f (%.1f) n = %.1f", wmean(age, w2), wsd(age, w2), sum(!(is.na(age)))),
    `Female, n (%)`      = sprintf("%.1f%% n = %.1f", 100 * sum(female * w2, na.rm=T) / sum(w2, na.rm=T), sum(!is.na(female))),
    `Urban, n (%)`       = sprintf("%.1f%% n = %.1f", 100 * sum(urban * w2, na.rm=T) / sum(w2, na.rm=T), sum(!is.na(urban))),
    .groups = "drop"
  )|>
  t()

weightedPrev = sample_data |> 
  filter(pooledData)|>
  group_by(Iso3) |>
  mutate(obesity = BMI_Category == "Obesity",
         overweight = BMI_Category == "Overweight")|>
  summarise(obesityPrev = sum(obesity * w_eq_country)/sum(w_eq_country),
            overweightPrev = sum(overweight * w_eq_country)/sum(w_eq_country),
            BMI = wmean(bmi,w_eq_country),
            BMIsd = wsd(bmi,w_eq_country))

gdp = sample_data |> filter(pooledData) |> group_by(Country) |> summarise(gdpPc = mean(gdpPc))
median(gdp$gdpPc)
year = sample_data |> filter(pooledData) |> group_by(Country) |> summarise(year = mean(dataYear))
median(year$year)

#Output final workspace image --------------------------------------------------

rm(structural_missing, model_data_1, age_cuts,
   regions, incomeGroups,summary,weightedPrev,
   summary_cont,summary_table, countryMapping, gdp, year,
   countries_to_keep, all_data, sample_data_1,
   structural_missing_rural, missingness_summary_rural,
   summary_dataset, wmean, wsd,
   countries_rural)

save.image("Rdata/final_data.RData")
