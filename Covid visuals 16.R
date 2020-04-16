# Package to download & clean JHU data: 
install.packages("remotes")
library(remotes)
install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

#Download data to merged dataframe: 
covidfull <- download_merged_data(cached = TRUE)

# Convert tibble to dataframe as countrycode package does not work on tibbles: 
covdf <- as.data.frame(covidfull)
str(covdf)

# Add continents/regions: 
library(countrycode)
continent <- countrycode(sourcevar = covdf[,2], origin = "iso3c", destination = "continent", warn=TRUE)
covid19 <- cbind(covdf, continent)

# Download oxford stringency index data: 
oxfordsi <- download_oxford_npi_data(cached = TRUE)
library(readxl)
path <- file.path("~" , "Desktop", "Covid datapop", "Data","OxCGRT_timeseries_all.xlsx")
oxfordsiraw <- read_excel(path)

# Reshape from wide to long: 
oxdf <- as.data.frame(oxfordsiraw)
library(tidyr)
oxford_long <- oxdf %>% gather(Date, SI, col3:col107)

# Selected countries to exclude from continents to form global south: 
selected_countries = c('CHN', 'JPN', 'SGP', 'KOR', 'ISR', 'TUR', 'AUS', 'NZL', 'CAN', 'USA')

# Filter global south countries: 
library(dplyr)
southcovid <- covid19 %>% 
  filter(iso3c != 'CHN', iso3c !='JPN', iso3c !='SGP',
         iso3c !='KOR', iso3c !='ISR', iso3c !='TUR', 
         iso3c !='AUS', iso3c !='NZL', iso3c !='CAN', iso3c !='USA', continent!='Europe') 

##### 1  Animated bar chart 
library(ggplot2)
library(gganimate)
library(viridisLite)

total_text_y = 0.87*(max(southcovid$confirmed))
vline_original_y = seq(floor(max(southcovid$confirmed)/8), 
                       max(southcovid$confirmed), by = floor(max(southcovid$confirmed)/8))

covid_formatted2 <- southcovid %>%
  group_by(date) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-confirmed),
         Value_rel = confirmed/confirmed[rank==1],
         total = sum(confirmed),
         Value_lbl = paste0(" ", format(confirmed, scientific=FALSE))) %>%
  group_by(country) %>% 
  filter(rank <=10) %>%
  ungroup()

staticplottest = ggplot(covid_formatted2, aes(rank, group = country, 
                                              fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = confirmed/2,
                height = confirmed,
                width = 0.9), alpha = 0.9, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1, size=8, fontface="bold") +
  geom_text(aes(y=confirmed,label = Value_lbl, hjust=0), fontface= 'bold', size = 10) +
  geom_text(aes(x = 8, y = total_text_y,
                label = sprintf('%s\n Global South Total =%s', date, format(total, big.mark=",", scientific=FALSE))),
            size = 13, color = 'grey') +
  coord_flip(clip = "off", expand = FALSE) +
  geom_hline(yintercept = vline_original_y, size = .08, color = "grey", linetype = 'dotted') +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=40, hjust=0.5, face="bold", colour="grey", vjust=1),
        plot.subtitle=element_text(size=25, hjust=0.5, face="italic", color="grey"),
        plot.caption = element_text(size=15, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,5, 2, 8, "cm"))

anim = staticplottest + transition_states(date, transition_length = 3, state_length = 0) +
  ease_aes('cubic-in-out') +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Confirmed cases in countries of the global south : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = sprintf("Data Source: John Hopkins Unversity CSSE Data Repository                     Datapop-Alliance & ADE"))

animate(anim, 500, fps = 10,  width = 1500, height = 1000, end_pause = 50, start_pause= 10,
        renderer = gifski_renderer("covidsouthtop10.gif"))