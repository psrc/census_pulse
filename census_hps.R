##########################################
# description:
# This script allows you to download all available 
# waves of the Census Household Pulse survey
# and bind them together into "long" format.
# Sophie Hill, 9/12/21
##########################################

##########################################
# load packages
##########################################
library(rvest) # for reading HTML
library(httr) # for GET request
library(purrr) # for binding lots of datasets together
library(fst) # efficient way to read/write large datasets
library(stringr)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)

##########################################
# web scraping
##########################################

# get all the URLs on this page
census_urls <- GET("https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html") %>%
  read_html() %>% 
  html_elements("a.uscb-layout-align-start-start") %>% 
  html_attr("href") %>% 
  as.character()

# subset to just the CSV files
census_urls <- census_urls[str_detect(census_urls, "CSV.zip")]
census_urls <- paste0("https:", census_urls)

# file names
census_filenames <- unlist(lapply(str_split(census_urls, "HPS_"), function(x)(x[2])))

setwd("C:/Users/cpeak/Documents/Repos/twitter_random")
# download each file
for (i in 1:length(census_urls)){
  download.file(census_urls[i], 
                destfile = paste0("Data/Census_HPS/Raw/",census_filenames[i]))
}

# unzip
for (i in 1:length(census_urls)){
  unzip(paste0("Data/Census_HPS/Raw/",census_filenames[i]), exdir="./Data/Census_HPS/Raw")
}


# select the raw data
# (not the weights files or the data dictionaries)
file_list <- list.files("Data/Census_HPS/Raw/")
file_list <- file_list[str_detect(file_list, ".csv") & !str_detect(file_list, "repwgt")]
file_list <- file_list[str_detect(file_list, "pulse2021_*")]

# bind together
hps <- paste0("Data/Census_HPS/Raw/", file_list) %>% 
  lapply(read_csv) %>% 
  bind_rows()

head(hps)
table(hps$WEEK)
names(hps)
table(hps$ANXIOUS)

hps$RECVDVACC

#filter for WA, TX & CO
hps %>% 
  filter(EST_ST == c('53', '48', '08'))

g <- ggplot()

#intention of getting vaccine
##  Need to check weighting scheme
hps_intentions <-  hps %>% 
  filter(RECVDVACC == 2) %>%
  #filter(EST_ST == c('53','08','48')) %>%
  select(WEEK, EST_ST, ANXIOUS, GETVACRV, PWEIGHT) %>% 
  mutate(intention = case_when(GETVACRV %in% c(1,2) ~ 'Probably get',
                            GETVACRV==3 ~ 'Unsure',
                            GETVACRV %in% c(4,5) ~ 'Probably not'
                            )
         ) %>%
  group_by(WEEK, intention ) %>% 
  summarize(tot_weight = sum(PWEIGHT, na.rm=TRUE))
  


ggplotly(
  g +
    geom_point(
      #data=hps_intentions[hps_intentions$EST_ST == st_code & hps_intentions$intention=='Probably get',],
      data=hps_intentions[hps_intentions$intention=='Probably get',],
      aes(x=WEEK, y=tot_weight),
      color='black') +
    geom_point(
      data=hps_intentions[hps_intentions$intention=='Probably not',],
      aes(x=WEEK, y=tot_weight), 
      color='red') +
    theme_minimal() +
    xlab("HPS wave") +
    ylab("") +
    labs(title = "Intention of getting vaccine",
         caption = "Source: Census Household Pulse survey")
)

#reasons for not getting vaccine
hps_antigov <-  hps %>% 
  filter(RECVDVACC == 2) %>%
  mutate(antigov = case_when(WHYNORV8 == 1 ~ 1 )) %>%
  group_by(EST_ST, antigov ) %>% 
  summarize(tot_weight = sum(PWEIGHT, na.rm=TRUE))

ggplotly(
  g +
    geom_point(
      #data=hps_antigov[hps_antigov$EST_ST == st_code & hps_antigov$antigov==1,],
      data=hps_antigov[hps_antigov$antigov==1,],
      aes(x=EST_ST, y=tot_weight),
      color='black') +
    theme_minimal() +
    xlab("State code") +
    ylab("") +
    labs(title = "Intention of getting vaccine",
         caption = "Source: Census Household Pulse survey")
)

# original 
hps %>% 
  filter(RECVDVACC == 2) %>%
  select(WEEK, EST_ST, ANXIOUS) %>% 
  mutate(anxious_mostdays = case_when(ANXIOUS<0 ~ NA_real_,
                                          ANXIOUS %in% c(1,2) ~ 0,
                                          ANXIOUS %in% c(3,4) ~ 1)) %>%
  group_by(WEEK) %>%
  summarize(mean_anxious_mostdays = mean(anxious_mostdays, na.rm=TRUE)) %>%
  ggplot(aes(x=WEEK, y=mean_anxious_mostdays*100)) + 
  geom_line() +
  theme_minimal() +
  xlab("HPS wave") +
  ylab("") +
  ylim(0, 40) +
  labs(title = "% feeling anxious most days last week", 
       subtitle = "Unweighted (due to laziness)",
       caption = "Source: Census Household Pulse survey")


































































