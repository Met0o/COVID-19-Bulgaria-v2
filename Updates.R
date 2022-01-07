library(remotes)

devtools::install_github("RamiKrispin/coronavirus",
                         upgrade = c("never"),
                         quiet   = FALSE,
                         force   = TRUE
)

################################################################################

library(tidyverse)
library(coronavirus)
library(plotly)
library(ggplot2)
library(grid)
library(gridExtra)
library(viridis)

df <- coronavirus %>%
     filter(country == "Bulgaria") %>%
     group_by(country, type) %>%
     summarise(total = sum(cases)) %>%
     pivot_wider(
         names_from = type,
         values_from = total
     ) %>%
     arrange(-confirmed) %>%
     ungroup() %>%
     mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
     mutate(country = trimws(country)) %>%
     mutate(country = factor(country, levels = country))

df_daily <- coronavirus %>%
     filter(country == "Bulgaria") %>%
     group_by(date, type) %>%
     summarise(total = sum(cases, na.rm = TRUE)) %>%
     pivot_wider(
         names_from = type,
         values_from = total
     ) %>%
     arrange(date) %>%
     ungroup() %>%
     mutate(active = confirmed - death) %>%
     mutate(
         confirmed_cum = cumsum(confirmed),
         death_cum = cumsum(death),
         active_cum = cumsum(active)
     )

daily_confirmed_deaths <- coronavirus %>%
     filter(type == "death") %>%
     filter(date >= "2020-03-07") %>%
     mutate(country = country) %>%
     group_by(date, country) %>%
     summarise(total = sum(cases)) %>%
     ungroup() %>%
     pivot_wider(names_from = country, values_from = total)

daily_confirmed <- coronavirus %>%
     filter(type == "confirmed") %>%
     filter(date >= "2020-03-07") %>%
     mutate(country = country) %>%
     group_by(date, country) %>%
     summarise(total = sum(cases)) %>%
     ungroup() %>%
     pivot_wider(names_from = country, values_from = total)

cv_data_for_plot <- coronavirus %>%
     filter(cases > 0) %>%
     group_by(country, province, lat, long, type) %>%
     summarise(cases = sum(cases)) %>%
     mutate(log_cases = 2 * log(cases)) %>%
     ungroup()

cv_data_for_plot.split <- cv_data_for_plot %>% split(cv_data_for_plot$type)

mortality_rate <- coronavirus %>% 
  
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(- confirmed) %>%
  filter(confirmed >= 100000) %>%
  mutate(death_rate = death / confirmed) %>% 
  mutate(death_rate = if_else(is.na(death_rate), 0, death_rate)) %>%
  ungroup() %>%
  mutate(confirmed_normal = as.numeric(confirmed) / max(as.numeric(confirmed)))
 
 ############### Vaccination Data ############### 
 
 vac_df <- covid19_vaccine %>% 
         filter(date == max(date),
                !is.na(population)) %>% 
         filter(continent_name == "Europe") %>% 
         mutate(fully_vaccinated_ratio = people_fully_vaccinated / population) %>%
         arrange(- fully_vaccinated_ratio) %>%
         slice_head(n = 44) %>%
         arrange(fully_vaccinated_ratio) %>%
         mutate(country = factor(country_region, levels = country_region))
 
 cov_df <- coronavirus %>% 
         filter(country %in% c("Russia","Ukraine","France","Spain","Sweden","Norway","Germany","Finland"
                               ,"Poland","Italy","United Kingdom","Romania","Belarus","Kazakhstan","Greece"
                               ,"Bulgaria","Iceland","Hungary","Portugal","Austria","Czech Republic","Serbia"
                               ,"Ireland","Lithuania","Latvia","Croatia","Bosnia and Herzegovina","Slovakia"
                               ,"Estonia","Denmark","Switzerland","Netherlands","Moldova","Belgium","Armenia"
                               ,"Albania","North Macedonia","Turkey","Slovenia","Montenegro","Kosovo","Azerbaijan"
                               ,"Cyprus","Luxembourg","Georgia","Andorra","Malta","Liechtenstein","San Marino"
                               ,"Monaco","Vatican City", "Czechia")) %>%
         group_by(country, type) %>%
         summarise(total_cases = sum(cases)) %>%
         pivot_wider(names_from = type, values_from = total_cases) %>%
         arrange(- confirmed) %>%
         mutate(death_rate = death / confirmed) %>% 
         arrange(- death_rate) %>%
         ungroup() 
 
# Join Vaccination and Covid data
joined_df <- left_join(vac_df, cov_df, by = "country") %>% 
         select(country, population, fully_vaccinated_ratio, death_rate)

# Convert & order country as factor by number of vaccinations to plot in high to low order
joined_df$country <- factor(joined_df$country, levels = joined_df[order(joined_df$fully_vaccinated_ratio),"country"])

# Export Data

write_rds(df, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/df.rds")
write_rds(df_daily, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/df_daily.rds")
write_rds(daily_confirmed_deaths, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/daily_confirmed_deaths.rds")
write_rds(daily_confirmed, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/daily_confirmed.rds")
write_rds(cv_data_for_plot, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/cv_data_for_plot.rds")
write_rds(cv_data_for_plot.split, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/cv_data_for_plot.split.rds")
write_rds(joined_df, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/joined_df.rds")
write_rds(mortality_rate, "E://Dev/R/GitHub/COVID-19-Bulgaria-v2/mortality_rate.rds")

files <- c("COVID-19-Bulgaria-v2.Rproj",                      
           "COVID-19-Bulgaria-v2.Rmd",                                          
           "df.rds",                                              
           "df_daily.rds",                    
           "daily_confirmed_deaths.rds",
           "daily_confirmed.rds",
           "cv_data_for_plot.rds",
           "cv_data_for_plot.split.rds",
           "joined_df.rds",
           "mortality_rate.rds")

################################################################################

library(rsconnect)

deployApp(appDir   = "E://Dev/R/GitHub/COVID-19-Bulgaria-v2",
          account  = "metodisimeonov",
          upload   = TRUE,
          appFiles = files,
          appName  = "COVID-19-Bulgaria-v2",
          forceUpdate = TRUE,
          launch.browser = FALSE)
