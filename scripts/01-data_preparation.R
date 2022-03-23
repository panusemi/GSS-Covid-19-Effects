#### Preamble ####
# Purpose: Prepare the 2021 GSS data
# Author: Emily Panus
# Data: 14 March 2022
# Contact: emily.panus@mail.utoronto.ca
# Pre-requisites: 
# - Need to have downloaded the GSS data and saved it to inputs/data


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- haven::read_dta("inputs/data/2021_stata/gss2021.dta")


#### Prepare data ####
# Just keep some variables that may be of interest
names(raw_data)

reduced_data <- raw_data %>%
  select(sexnow1, age, degree, quallife, health, life, happy, income, confedvac,
         confedvacy, finalter, hlthphys, hlthmntl, satsoc, raceacs1, raceacs2, 
         raceacs3, raceacs4, raceacs5, raceacs6, raceacs7, raceacs8, raceacs9, 
         raceacs10, raceacs11, raceacs12, raceacs13, raceacs14, raceacs15,
         raceacs16) %>% 
  rename(gender = sexnow1, white = raceacs1, black = raceacs2, 
         amer_indian_alaska = raceacs3, asian_indian = raceacs4, 
         chinese = raceacs5, filipino = raceacs6, japenese = raceacs7, 
         korean = raceacs8, vietnamese = raceacs9, oth_asian = raceacs10, 
         native_hawaiian = raceacs11,  guamanian_chamorro = raceacs12, 
         samoan = raceacs13, oth_pacific_islander = raceacs14, 
         oth_race = raceacs15, hispanic = raceacs16)

rm(raw_data)

#### Recode gender ####
# The question for SEXNOW1 is:
# "Do you describe yourself as male, female, or transgender?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data <- 
  reduced_data %>% 
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Transgender",
    gender == 4 ~ "None of these"),
    degree = case_when(
    degree == 0 ~ "Less than High School",
    degree == 1 ~ "High School",
    degree == 2 ~ "Associate/Junior College",
    degree == 3 ~ "Bachelors",
    degree == 4 ~ "Graduates"),
    quallife = case_when(
      quallife == 1  ~ "Excellent",
      quallife == 2 ~ "Very Good",
      quallife == 3 ~ "Good",
      quallife == 4 ~ "Fair",
      quallife == 5 ~ "Poor"), 
    health = case_when(
      health == 1  ~ "Excellent",
      health == 2 ~ "Good",
      health == 3 ~ "Fair",
      health == 4 ~ "Poor"),
    life = case_when(
      life == 1  ~ "Exciting",
      life == 2 ~ "Routine",
      life == 3 ~ "Dull"),
    happy = case_when(
      happy == 1  ~ "Very Happy",
      happy == 2 ~ "Pretty Happy",
      happy == 3 ~ "Not too Happy"),
    income = case_when(
      income == 1 ~ "Under $1000",
      income == 2 ~ "$1000 to $2999",
      income == 3 ~ "$3000 to $3999",
      income == 4 ~ "$4000 to $4999",
      income == 5 ~ "$5000 to $5999",
      income == 6 ~ "$6000 to $6999",
      income == 7 ~ "$7000 to $7999", 
      income == 8 ~ "$8000 to $9999",
      income == 9 ~ "$10000 to $14999",
      income == 10 ~ "$15000 to $19999",
      income == 11 ~ "$20000 to $24999",
      income == 12 ~ "$25000 or more",
      income == 13 ~ "Refused"),
    confedvac = case_when(
      confedvac == 1 ~ "A great deal",
      confedvac == 2 ~ "Only some",
      confedvac == 3 ~ "Hardly any"),
    confedvacy = case_when(
      confedvacy == 1 ~ "A great deal",
      confedvacy == 2 ~ "Only some",
      confedvacy == 3 ~ "Hardly any"),
    finalter = case_when(
      finalter == 1  ~ "Getting better",
      finalter == 2 ~ "Getting worse",
      finalter == 3 ~ "Stayed the same"),
    hlthphys = case_when(
      hlthphys == 1  ~ "Excellent",
      hlthphys == 2 ~ "Very Good",
      hlthphys == 3 ~ "Good",
      hlthphys == 4 ~ "Fair",
      hlthphys == 5 ~ "Poor"),
    hlthmntl = case_when(
      hlthmntl == 1  ~ "Excellent",
      hlthmntl == 2 ~ "Very Good",
      hlthmntl == 3 ~ "Good",
      hlthmntl == 4 ~ "Fair",
      hlthmntl == 5 ~ "Poor"),
    satsoc = case_when(
      satsoc == 1  ~ "Excellent",
      satsoc == 2 ~ "Very Good",
      satsoc == 3 ~ "Good",
      satsoc == 4 ~ "Fair",
      satsoc == 5 ~ "Poor"))

reduced_data %>% 
  ggplot(aes(x = age)) +
  geom_bar()

#### Save ####
write_csv(reduced_data, "outputs/data/prepared_gss.csv")

library(haven)
library(tidyverse)
raw_data2018 <- haven::read_dta("inputs/data/2018_stata/GSS2018.dta")
names(raw_data2018)

reduced_data2018 <- raw_data2018 %>%
  select(sexnow, age, degree, quallife, health, life, happy, 
         income, finalter, hlthphys, hlthmntl, satsoc) %>% 
  rename(gender = sexnow)

rm(raw_data2018)


#### Recode gender ####
# The question for SEXNOW1 is:
# "Do you describe yourself as male, female, or transgender?"
# Options are from the Codebook 'GSS 2021 Codebook R1b.pdf'
reduced_data2018 <- 
  reduced_data2018 %>% 
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Transgender",
    gender == 4 ~ "None of these"),
    degree = case_when(
      degree == 0 ~ "Less than High School",
      degree == 1 ~ "High School",
      degree == 2 ~ "Associate/Junior College",
      degree == 3 ~ "Bachelors",
      degree == 4 ~ "Graduates"),
    quallife = case_when(
      quallife == 1  ~ "Excellent",
      quallife == 2 ~ "Very Good",
      quallife == 3 ~ "Good",
      quallife == 4 ~ "Fair",
      quallife == 5 ~ "Poor"), 
    health = case_when(
      health == 1  ~ "Excellent",
      health == 2 ~ "Good",
      health == 3 ~ "Fair",
      health == 4 ~ "Poor"),
    life = case_when(
      life == 1  ~ "Exciting",
      life == 2 ~ "Routine",
      life == 3 ~ "Dull"),
    happy = case_when(
      happy == 1  ~ "Very Happy",
      happy == 2 ~ "Pretty Happy",
      happy == 3 ~ "Not too Happy"),
    income = case_when(
      income == 1 ~ "Under $1000",
      income == 2 ~ "$1000 to $2999",
      income == 3 ~ "$3000 to $3999",
      income == 4 ~ "$4000 to $4999",
      income == 5 ~ "$5000 to $5999",
      income == 6 ~ "$6000 to $6999",
      income == 7 ~ "$7000 to $7999", 
      income == 8 ~ "$8000 to $9999",
      income == 9 ~ "$10000 to $14999",
      income == 10 ~ "$15000 to $19999",
      income == 11 ~ "$20000 to $24999",
      income == 12 ~ "$25000 or more"),
    finalter = case_when(
      finalter == 1  ~ "Getting better",
      finalter == 2 ~ "Getting worse",
      finalter == 3 ~ "Stayed the same"),
    hlthphys = case_when(
      hlthphys == 1  ~ "Excellent",
      hlthphys == 2 ~ "Very Good",
      hlthphys == 3 ~ "Good",
      hlthphys == 4 ~ "Fair",
      hlthphys == 5 ~ "Poor"),
    hlthmntl = case_when(
      hlthmntl == 1  ~ "Excellent",
      hlthmntl == 2 ~ "Very Good",
      hlthmntl == 3 ~ "Good",
      hlthmntl == 4 ~ "Fair",
      hlthmntl == 5 ~ "Poor"),
    satsoc = case_when(
      satsoc == 1  ~ "Excellent",
      satsoc == 2 ~ "Very Good",
      satsoc == 3 ~ "Good",
      satsoc == 4 ~ "Fair",
      satsoc == 5 ~ "Poor"))

#### Save ####
write_csv(reduced_data2018, "outputs/data/prepared_gss2018.csv")     