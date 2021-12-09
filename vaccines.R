library(coronavirus)
library(tidyverse)
library(plotly)
library(ggplot2)

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

joined_df <- left_join(vac_df, cov_df, by = "country") %>% 
    select(country, population, fully_vaccinated_ratio, death_rate) #%>%
    mutate(country = as.factor(country)) %>% 
    arrange(desc(fully_vaccinated_ratio))
   # mutate_if(is.numeric, round, digits = 3)

joined_df$country <- factor(joined_df$country, levels = joined_df$country[order(joined_df$death_rate)])
joined_df$country  # notice the changed order of factor levels

################################################################################

p_vaccination <- joined_df %>% 
    arrange(- fully_vaccinated_ratio) %>%
    #slice_head(n = 44) %>%
    arrange(fully_vaccinated_ratio) %>%
    mutate(country = factor(country, levels = country)) %>% 
    plot_ly(y = ~ country,
            x = ~ round(100 * fully_vaccinated_ratio, 2),
            text = ~ paste(round(100 * fully_vaccinated_ratio, 1), "%"),
            textposition = 'auto',
            orientation = "h",
            type = "bar") %>%
        
        layout(title = "Percentage of Fully Vaccineted Population",
               yaxis = list(title = ""),
               xaxis = list(title = "Source: Johns Hopkins Centers for Civic Impact",
                            ticksuffix = "%"))

p_mortality <- joined_df %>% 
    arrange(- death_rate) %>%
    #slice_head(n = 44) %>%
    arrange(death_rate) %>%
    mutate(country = factor(country, levels = country)) %>% 
    plot_ly(y = ~ country,
            x = ~ round(100 * death_rate, 2),
            text = ~ paste(round(100 * death_rate, 1), "%"),
            textposition = 'auto',
            orientation = "h",
            type = "bar") %>%
    
    layout(title = "Mortality Rate - Top 20 Countries",
           yaxis = list(title = ""),
           xaxis = list(title = "Source: Johns Hopkins Centers for Civic Impact",
                        ticksuffix = "%"))

subplot(p_vaccination, p_mortality, margin = 0.10, shareY = FALSE)

################################################################################

library(grid)
library(gridExtra)
library(ggplot2)
library(viridis)

# Convert & order country as factor by number of vaccinations to plot in high to low order
joined_df$country <- factor(joined_df$country, levels = joined_df[order(joined_df$fully_vaccinated_ratio),"country"])

g_mid <- ggplot(joined_df, aes(x = 1, y = country)) + 
    geom_text(aes(label = country))+
    geom_segment(aes(x = 0.94, xend = 0.96, yend  = country )) +
    geom_segment(aes(x = 1.04, xend = 1.065, yend = country)) +
    ggtitle("") +
    ylab(NULL) +
    scale_x_continuous(expand = c(0, 0), limits = c(0.94, 1.065)) +
    
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x  = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid   = element_blank(),
          panel.background = element_blank(),
          plot.margin  = unit(c(1, -1, 1, -1), "mm"))
    

# Color Palette
low  <- colorRampPalette(c('red', 'yellow' ))
mid  <- colorRampPalette(c("blue", "lightblue"))
high <- colorRampPalette(c('lightgreen', 'darkgreen'))

# Center ALL plot titles
theme_update(plot.title = element_text(hjust = 0.5))


g1 <- ggplot(data = joined_df, 
             aes(x = country, y = as.numeric(fully_vaccinated_ratio), 
                 fill = fully_vaccinated_ratio, 
                 label = scales::percent(fully_vaccinated_ratio, accuracy = 0.01))) +
    xlab(NULL) +
    geom_bar(stat = "identity", alpha = 0.7, width = 0.6) + 
    
    geom_text(size = 3, position = position_stack(vjust = -0.3)) +
    
    #scale_fill_gradient2(mid = 'red', high = 'blue', space='Lab') +
    
    scale_colour_viridis(direction = -1) +
    scale_fill_viridis(direction = -1) +
    
    ggtitle("Vaccination % of Adult Population") +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x  = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          
          legend.position  = "none",
          
          plot.margin  = unit(c(1, -1, 1, 0), "mm")) +
    scale_y_reverse() +
    coord_flip()


g2 <- ggplot(data = joined_df, 
             aes(x = country, y = as.numeric(death_rate), 
                 fill = death_rate, 
                 label = scales::percent(death_rate, accuracy = 0.01))) + 
    xlab(NULL) +
    geom_bar(stat = "identity", alpha = 0.7, width = 0.6) + 
    
    geom_text(size = 3, position = position_stack(vjust = 1.2)) +
    
    #scale_fill_gradient2(mid = 'yellow', high ='darkred', space ='Lab') +
    
    scale_colour_viridis_c(direction = -1) +
    scale_fill_viridis_c(direction = -1) +
    
    ggtitle("Mortality Rate Since March, 2020") +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x  = element_blank(),
          axis.text.y  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          
          legend.position  = "none",
          
          plot.margin  = unit(c(1, 0, 1, -1), "mm")) +
    coord_flip()


gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg_mid <- ggplot_gtable(ggplot_build(g_mid))

grid.arrange(gg1, gg_mid, gg2, ncol = 3, widths = c(4/9, 2.8/9, 4/9))







