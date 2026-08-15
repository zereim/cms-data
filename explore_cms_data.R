library(tidyverse)
library(janitor)
library(readr)
library(logger)


df <- as_tibble(read_csv("/Users/william/Library/CloudStorage/Dropbox/Rscripts/cms-data/PATIENT_REPORTED_OUTCOMES_FACILITY(1).csv"))

# d?f <- clean_names(df, "lower_camel")

# Cleaning 
df <- df |> 
  clean_names("snake") |> 
  mutate(voluntary_reporting = recode
        (voluntary_reporting, "Yes" = TRUE,
         "No" = FALSE)) |> 
  mutate(
    score = score |> 
      replace_when(score == "Not Available" ~ NA)
  )
  

# turn all of the Not Available into NA 



# cleaning names of the columns in the dataframe 
# df |> 
#   clean_names("lower_camel") |> 
#   select(Facility Name, Address, City/Town,
#          State, County/Parish, Measure ID, 
#          Measure Name) |> 
#   mutate(tolower())

  

View(df)

hospital_cols <- select(df, contains("HOSPITAL"))

View(hospital_cols)

#df %>% 
#  select(contains("hospital")) %>% 
#  View()