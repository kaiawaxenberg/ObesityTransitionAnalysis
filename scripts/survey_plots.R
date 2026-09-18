library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(mapdata)
library(RColorBrewer)
library(radiant.data)
library(purrr)
library(forcats)
library(gt)
library(cowplot)

# Load pooled survey data ----------------------------------------

# Load data file prepared by data_preparation.R
load("Rdata/final_data.RData")

#Match country names to mapping data
map_data =  sample_data|> 
  filter(pooledData)|>
  mutate(Country = case_when(Country == "Russian Federation" ~ "Russia",
                             Country == "Trinidad and Tobago" ~ "Trinidad",
                             Country == "United States of America" ~ "USA",
                             Country == "Eswatini" ~ "Swaziland",
                             Country == "South Africa DHS" ~ "South Africa",
                             Country == "Czechia" ~ "Czech Republic",
                             Country == "Bangladesh 2022" ~ "Bangladesh",
                             Country == "Chile ENS" ~ "Chile",
                             Country == "Moldova 2021" ~ "Moldova",
                             Country == "Liberia 2022" ~ "Liberia",
                             Country == "IndiaLASI" ~ "India",
                             Country == "Cabo Verde" ~ "Cape Verde",
                             Country == "Timor Leste" ~ "Timor-Leste",
                             Country %in% c("China CHARLS", "China, mainland") ~ "China",
                             TRUE ~ Country))

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

# BMI summary statistics ----------------------------------------------------
summary = map_data |> 
  filter(!is.na(BMI_Category))|>
  group_by(dataset)|>
  mutate(obesity = BMI_Category == "Obesity",
         overweight = BMI_Category == "Overweight")|>
  summarise(obesityprev = sum(obesity * w_eq_country)/sum(w_eq_country),
            overweightprev = sum(overweight * w_eq_country)/sum(w_eq_country))

summary_country = map_data |> 
  filter(!is.na(BMI_Category))|>
  mutate(obesity = BMI_Category == "Obesity",
         overweight = BMI_Category == "Overweight")|>
  group_by(Country, dataset)|>
  summarise(bmi = wmean(bmi, w_eq_country),
            obesityprev = sum(obesity * w_eq_country)/sum(w_eq_country),
            overweightprev = sum(overweight * w_eq_country)/sum(w_eq_country),
            gdpPc = mean(gdpPc))

#Plot GDP by dataset
boxplot_data = summary_country |> 
  group_by(dataset) |>
  mutate(label = paste0(dataset,"\n(n=", n(), ")"))

gdpBoxplot = ggplot(data = boxplot_data, aes(fill = dataset, x = label, y = gdpPc)) +
  geom_boxplot(alpha = 0.6)+
  scale_fill_brewer(palette = "Set1") +  # Adjust color scale
  labs(fill = "Dataset",
       y= "GDP per capita (2021 $, PPP)",
       x= "Data source")+
  theme_minimal(base_size =14)+
  theme(legend.position = "none")

ggsave("plots/dataDescription/datasetGDPPc.pdf", gdpBoxplot,
       width = 7, height = 5, dpi = 150)

# Map final sample ----------------------------------------------------------

#load data for mapping
world_map = map_data("world") |> filter(region != "Antarctica")
map_countries = as.data.frame(unique(world_map$region))

unique(map_data$Country)[which(!(unique(map_data$Country) %in% map_countries[,1]))]

#Sub-sample map
subsample = map_data |> filter(insample_rural)|> select(Country, dataset) |> distinct()
merged_data <- merge(world_map, subsample, by.x = "region", by.y = "Country", all.x = TRUE)

subsampleMap = ggplot() +
  geom_map( 
    data = merged_data |> filter(!is.na(dataset)), map = world_map, 
    aes(long, lat, map_id = region, fill= dataset)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill =NA, color = alpha("#404040", 0.6), linewidth=0.2)+
  scale_fill_manual(values = c(
    "#06B6D4", 
    "#A855F7", 
    "#F97316" 
  )) +
  theme_void(base_size = 12) +  
  labs(fill = "Data source")

ggsave("plots/dataDescription/subsampleMap.pdf", subsampleMap,
       width = 11, height = 6, dpi = 150)

# Plot sample coverage by data source
merged_data <- merge(world_map, summary_country, by.x = "region", by.y = "Country", all.x = TRUE)

datasetMap = ggplot() +
  geom_map( 
    data = merged_data |> filter(!is.na(dataset)), map = world_map, 
    aes(long, lat, map_id = region, fill= dataset)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill =NA, color = alpha("#404040", 0.6), linewidth=0.2)+
  scale_fill_manual(values = c(
    "#06B6D4", 
    "#A855F7", 
    "#F97316" 
  )) +
  theme_void(base_size = 12) +  
  labs(fill = "Data source")

ggsave("plots/dataDescription/datasetMap.pdf", datasetMap,
       width = 11, height = 6, dpi = 150)

# Obesity prevalence
obesity = ggplot() +
  geom_map( 
    data = merged_data, map = world_map, 
    aes(long, lat, map_id = region, fill= obesityprev)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill =NA, color = alpha("#404040", 0.6), linewidth=0.001)+
  scale_fill_gradient(low = "#E9E9E9",
                      high = "#7A3DB8") + 
  theme_void() +
  labs(fill = "Prevalence (%)", 
       title = "B) Obesity")+
  theme(plot.title = element_text(face = "bold"))

# Overweight prevalence
overweight = ggplot() +
  geom_map( 
    data = merged_data, map = world_map, 
    aes(long, lat, map_id = region, fill= overweightprev)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill =NA, color = alpha("#404040", 0.6), linewidth=0.001)+
  scale_fill_gradient(low = "#E9E9E9",
                      high = "#1F9D9A",) + 
  theme_void() +
  labs(fill = "Prevalence (%)", 
       title = "A) Overweight (Excluding Obesity)")+
  theme(plot.title = element_text(face = "bold"))

ggsave("plots/dataDescription/prevalenceMaps.pdf", 
       plot_grid(overweight, obesity, 
                 ncol = 1),
       width = 7, height = 7, dpi = 150)

# BMI barbell plot ----------------------------------------------------

plot_df <- map_data |>
  filter(population2015 > quantile(population2015, 0.25))|>
  group_by(Country) |>
  summarise(
    lo  = quantile(bmi, 0.25, na.rm = TRUE),
    mid = median(bmi,na.rm = TRUE),
    hi  = quantile(bmi, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = reorder(Country, mid)) # sort countries by median

barbell = ggplot(plot_df |> filter(!is.na(mid)), aes(y = Country)) +
  geom_segment(aes(x = lo, xend = hi, yend = Country),
               linewidth = 1.2, colour = "grey80") +
  geom_point(aes(x = lo),  size = 3, colour = "#1b9e77") +
  geom_point(aes(x = hi),  size = 3, colour = "#d95f02") +
  geom_point(aes(x = mid), size = 2.2, colour = "grey80") +
  labs(x = "BMI (kg/m²)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave("plots/dataDescription/sampleBMI.pdf", barbell,
       width = 6, height = 6.5, dpi = 150)

# final sample inequality ----------------------------------------------------
plot_data = map_data |> 
  group_by(Country, wealth_quintile) |>
  mutate(obesity = BMI_Category == "Obesity",
         overweight = BMI_Category == "Overweight")|>
  summarise(gdpPc = mean(gdpPc),
            obesity_prevalence = sum(obesity * w_eq_country)/sum(w_eq_country),
            overweight_prevalence = sum(overweight * w_eq_country)/sum(w_eq_country)
  ) |>
  ungroup()|>
  mutate(gdpLevel = xtile(gdpPc, 3),
         gdpLevel = case_when(
           gdpLevel == 1 ~ "Low GDP per capita",
           gdpLevel == 2 ~ "Medium GDP per capita",
           gdpLevel == 3 ~ "High GDP per capita",
         ),
         gdpLevel = factor(gdpLevel,
                           levels = c(
                             "Low GDP per capita",
                             "Medium GDP per capita",
                             "High GDP per capita"
                           ),
                           ordered = TRUE))

quintileBoxplotObesity = ggplot(plot_data, aes(x = wealth_quintile, y=obesity_prevalence, fill = gdpLevel))+
  geom_boxplot(alpha = 0.5)+
  scale_fill_manual(values = c(
    "#06B6D4", 
    "#A855F7", 
    "#F97316" 
  ))+
  theme_minimal(base_size=12)+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))+
  labs(y= "Prevalence", 
       x = "Household wealth quintile", 
       title = "B) Obesity")+
  facet_wrap(~gdpLevel)

quintileBoxplotOverweight = ggplot(plot_data, aes(x = wealth_quintile, y=overweight_prevalence, fill = gdpLevel))+
  geom_boxplot(alpha = 0.5)+
  scale_fill_manual(values = c(
    "#06B6D4", 
    "#A855F7", 
    "#F97316" 
  ))+
  theme_minimal(base_size=12)+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))+
  labs(y= "Prevalence", 
       x = "Household wealth quintile",
       title = "A) Overweight (Excluding Obesity)")+
  facet_wrap(~gdpLevel)

ggsave("plots/dataDescription/quintileBoxplot.pdf", 
       plot_grid(quintileBoxplotOverweight, quintileBoxplotObesity, ncol = 1),
       width = 8, height = 6, dpi = 150)

plot_data = sample_data |> 
  filter(pooledData)|>
  mutate(obesity = BMI_Category == "Obesity",
         overweight = BMI_Category == "Overweight")|>
  group_by(Country, wealth_quintile, sex) |>
  summarise(gdpPc = mean(gdpPc),
            group_gdpPc = mean(group_gdpPc),
            obesity_prevalence = sum(obesity * w_eq_country)/sum(w_eq_country),
            overweight_prevalence = sum(overweight * w_eq_country)/sum(w_eq_country)
  ) |>
  ungroup()|>
  mutate(gdpLevel = xtile(gdpPc, 3),
         gdpLevel = case_when(
           gdpLevel == 1 ~ "Low GDP per capita",
           gdpLevel == 2 ~ "Medium GDP per capita",
           gdpLevel == 3 ~ "High GDP per capita",
         ),
         gdpLevel = factor(gdpLevel,
                                 levels = c(
                                   "Low GDP per capita",
                                   "Medium GDP per capita",
                                   "High GDP per capita"
                                 ),
                                 ordered = TRUE),
         sex = case_when(sex == 0 ~ "Male", sex =="1" ~ "Female"))

boxplotSexObesity = ggplot(plot_data, aes(x = wealth_quintile, y=obesity_prevalence, fill = gdpLevel))+
  geom_boxplot(alpha = 0.5)+
  scale_fill_manual(values = c(
    "#06B6D4", 
    "#A855F7", 
    "#F97316" 
  ))+
  theme_minimal(base_size =14)+
  theme(legend.position = "none")+
  labs(y= "Obesity prevalence", x = "Household wealth quintile")+
  facet_grid(sex~gdpLevel)

ggsave("plots/dataDescription/obesityQuintileBoxplot.pdf", boxplotSexObesity,
       width = 8, height = 5, dpi = 150)

boxplotSexOverweight = ggplot(plot_data, aes(x = wealth_quintile, y=overweight_prevalence, fill = gdpLevel))+
  geom_boxplot(alpha = 0.5)+
  scale_fill_manual(values = c(
    "#06B6D4", 
    "#A855F7", 
    "#F97316" 
  ))+
  theme_minimal(base_size =14)+
  theme(legend.position = "none")+
  labs(y= "Overweight prevalence", x = "Household wealth quintile")+
  facet_grid(sex~gdpLevel)

ggsave("plots/dataDescription/overweightQuintileBoxplot.pdf", boxplotSexOverweight,
       width = 8, height = 5, dpi = 150)

# Map obesity and underweight prevalence by quintile------------------------

#Create data for prevalence maps by quintile
diffData <- map_data |>
  filter(wealth_quintile %in% c(1,5))|>
  mutate(wealth_quintile = case_when(wealth_quintile ==1 ~ "Bottom 20%",
                                     wealth_quintile ==5 ~ "Top 20%"))|>
  group_by(Country, wealth_quintile) |>
  summarise(obesityPrev = 100 * wmean(bmicat==4, w_eq_country),
            overweightPrev = 100 * wmean(bmicat ==3, w_eq_country),
            underweightPrev = 100 * wmean(bmicat==1, w_eq_country))|>
  group_by(Country) |>
  arrange(wealth_quintile, by.group=T)|>
  summarise(diffObesity = obesityPrev[2]-obesityPrev[1],
            diffOverweight = overweightPrev[2]-overweightPrev[1],
            diffUnderweight = underweightPrev[2]-underweightPrev[1])
merged_data <- merge(world_map, diffData, by.x = "region", by.y = "Country", all.x = TRUE)

obesityIneqMap = ggplot() +
  geom_map( 
    data = merged_data, map = world_map, 
    aes(long, lat, map_id = region, fill= diffObesity)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill =NA, color = alpha("#404040", 0.6), linewidth=0.001)+
  scale_fill_gradient2(  low = "#7A3DB8",
                         mid = "white",
                         high = "#1F9D9A") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))+
  labs(fill = "Difference in prevalence \n top vs bottom 20%", 
       title = "B) Obesity")

overweightIneqMap = ggplot() +
  geom_map( 
    data = merged_data, map = world_map, 
    aes(long, lat, map_id = region, fill= diffOverweight)) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill =NA, color = alpha("#404040", 0.6), linewidth=0.001)+
  scale_fill_gradient2(  low = "#7A3DB8",
                         mid = "white",
                         high = "#1F9D9A") +  # Adjust color scale
  theme_void() +  # Removes axis and gridlines
  theme(plot.title = element_text(face = "bold"))+
  labs(fill = "Difference in prevalence \n top vs bottom 20%", 
       title = "A) Overweight (Excluding Obesity)")

ggsave("plots/dataDescription/obesityOverweightIneqMaps.pdf", 
       plot_grid(overweightIneqMap, obesityIneqMap, ncol = 1),
       width = 8, height = 8, dpi = 150)

diffData |> group_by(diffOverweight>0) |> summarise(n=n())
diffData |> group_by(diffObesity>0) |> summarise(n=n())
