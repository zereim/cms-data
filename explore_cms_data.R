library(tidyverse)
library(janitor)
library(readr)
library(logger)

df <- as_tibble(read_csv("/Users/william/Library/CloudStorage/Dropbox/Rscripts/cms-data/PATIENT_REPORTED_OUTCOMES_FACILITY(1).csv"))


# cleaning names of the columns in the dataframe 
df |> 
  clean_names()

View(df)

hospital_cols <- select(df, contains("hospital"))


#df %>% 
#  select(contains("hospital")) %>% 
#  View()