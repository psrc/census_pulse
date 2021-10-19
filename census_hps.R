# Adapted from twitter_random by Sophie Hill, 9/12/21

library(rvest) # for reading HTML
library(httr) # for GET request
library(tidyverse)
library(here)
library(odbc)
library(DBI)


# inputs ------------------------------------------------------------------


# phases (week number)
phases <- list(phase1 = seq(1, 12),
               phase2 = seq(13, 17),
               phase3 = seq(18, 27),
               phase3_1 = seq(28, 33),
               phase3_2 = seq(34, 37)
               )

# download each file into data folder
dir <- "Data/Census_HPS/Raw/"


# web scrape --------------------------------------------------------------


# get all the URLs on this page
census_urls <- GET("https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html") %>%
  read_html() %>% 
  html_elements(".uscb-layout-align-start-start a") %>% 
  html_attr("href") %>% 
  as.character()

# subset to just the CSV files
census_urls <- census_urls[str_detect(census_urls, "CSV.zip")]
census_urls <- paste0("https:", census_urls)


# functions ---------------------------------------------------------------


download_compile_pulse <- function(phase) {
  # function to download, extract, and bind a specific phase
  
  weeks <- paste0('wk', phases[[phase]])
  cu <- map(weeks, ~census_urls[str_detect(census_urls, paste0(.x, '\\/'))]) %>% unlist()
  cu_filenames <- unlist(lapply(str_split(cu, "HPS_"), function(x)(x[2])))
  
  # check if files already exist in directory
  nums <- map(cu_filenames, ~str_extract(.x, "\\d+")) %>% unlist()
  filenames <- map(nums, ~list.files(dir, pattern = paste0("pulse\\d{4}_puf_", .x))) %>% 
    unlist()
  
  if(length(nums) == length(filenames)){
    
    cat('Files for', phase, 'have already been downloaded:\n')
    walk(filenames, ~cat(.x, '\n'))
    print('Reading files and returning tibble')
    file_list <- filenames
  } else {
    
    # download and extract files
    for (i in 1:length(cu)){
      download.file(cu[i], destfile = file.path(dir, cu_filenames[i]))
      unzip(file.path(dir, cu_filenames[i]), exdir = here(dir))
    }
    
    # select the raw data (not the weights files or the data dictionaries)
    file_list <- list.files(dir, pattern = ".csv") %>% 
      .[!str_detect(., "repwgt")]
  }
  
  # compile phase into one table
  df <- map(file_list, ~read_csv(here(dir, .x))) %>% 
    reduce(bind_rows)
}

export_to_elmer <- function(table, outtbl_name) {
  # SQL Database Connection settings
  elmer_connection <- dbConnect(odbc::odbc(),
                                driver = "ODBC Driver 17 for SQL Server",
                                server = "AWS-PROD-SQL\\Sockeye",
                                database = 'Sandbox',
                                trusted_connection = "yes"
  )
  
  # dbWriteTable(elmer_connection, Id(schema = schema, table = outtbl_name), as.data.frame(table), overwrite = TRUE)
  dbWriteTable(elmer_connection, outtbl_name, as.data.frame(table), overwrite = TRUE)
  dbDisconnect(elmer_connection)
}

# dfs <- download_compile_pulse('phase1')
# export_to_elmer(dfs, 'census_pulse_phase1')

elmer_names <- map(names(phases), ~paste0('census_pulse_', .x)) %>% unlist()

