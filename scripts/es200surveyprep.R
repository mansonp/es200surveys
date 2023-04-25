##
#
# Full survey data input
#
##

library(pacman) # This package lets you load and install packages quickly.

p_load(haven, readr, dplyr, sjmisc, labelled, srvyr, forcats, tidycensus, stringr)

# load raw data from Qualtrics
es200survey <- read_sav("data/fullsurvey.sav") 

# load demographic data from Prolific
prolificdemos <- read_csv("data/prolific_export_64446a181d9a838603b34bd5.csv") %>% 
   select('Participant id', Age, Sex, 'Ethnicity simplified')

prolificdemo_pilot <- read_csv("data/prolific_export_64444758972e2749d0368a54(1).csv")%>% 
   select('Participant id', Age, Sex, 'Ethnicity simplified')

prolificdemos <- rbind(prolificdemos, prolificdemo_pilot)

# Join Qualtrics and Prolific data

es200survey <- es200survey %>%
   left_join(prolificdemos, by=c("prolificID"="Participant id")) %>%
   select(-StartDate, -EndDate, -Status, -IPAddress, 
          -Progress, -Duration__in_seconds_, -Finished, 
          -RecordedDate, -RecipientLastName, -RecipientFirstName,
          -RecipientEmail, -ExternalReference, -DistributionChannel,
          -UserLanguage) %>%
   mutate(Age = as.numeric(Age)) %>%
   filter(ResponseId!="R_1NrZPaIK9rlkTjN") # This one dupe was found using code below

# Check for dupes
# prolificdemos %>% group_by(`Participant id`) %>% count() %>% filter(n>1)
# 
# es200survey %>% group_by(PROLIFIC_PID) %>% count() %>% filter(n>1)

# Prep Weighting Categories

# The target variables are in ALL CAPS and must be numerics that match the census

es200survey <- es200survey %>%
   mutate(
      multirace = rowSums(select(., starts_with("race_")), na.rm=TRUE), # identifies one race or more in use
      AGECAT = cut(Age, 
                   breaks = c(17, 30, 40, 50, 60, 70, Inf),
                   labels = c("18-30", "31-40", "41-50", "51-60", "61-70", "71 and older")),
      RACE = case_when(
         multirace == 1 & race_1 == 1 ~ 1,
         multirace == 1 & race_2 == 1 ~ 2,
         multirace == 1 & race_3 == 1 ~ 3,
         multirace == 1 & race_4 == 1 ~ 6,
         multirace == 1 & race_5 == 1 ~ 7,
         multirace == 1 & race_6 == 1 ~ NA,
         multirace == 1 & race_7 == 1 ~ NA,
         TRUE ~ 9),
      GENDER = case_when(
         gender == 1 ~ 1,
         gender == 2 ~ 2,
         TRUE ~ NA),
      EDUCATT = case_when(
         education == 7 ~ NA,
         TRUE ~ education)
      )


# Generate Survey Weights
#
# Prolific sample is young and liberal
# Weight on age, sex, education and maybe race

# Census PUMA Setup
# Note - make sure your Census API code is loaded in the environment

acs21sub <- load_variables(2021, "acs1/subject", cache = TRUE)
acs21pro  <- load_variables(2021, "acs1/profile", cache = TRUE)


# Get variables for weighting the down load takes a LONG time.

# us_pums <- get_pums(
#    variables = c("SEX", "AGEP", "RAC1P", "SCHL"),
#    state = "all",
#    survey = "acs1",
#    year = 2021)

# Create New Variables for Raking and Target ####


# Age ####
us_pums_props <-
   us_pums %>%
   filter(AGEP >= 18) %>% # Filter out minors
   mutate(AGECAT = cut(AGEP, 
                       breaks = c(17, 30, 40, 50, 60, 70, Inf),
                       labels = c("18-30", "31-40", "41-50", "51-60", "61-70", "71 and older")),
          RACE = case_when(
             RAC1P == 1 ~ "White",
             RAC1P == 2 ~ "Black",
             RAC1P == 3 ~ "NatAm",
             RAC1P == 4 ~ "NatAm",
             RAC1P == 5 ~ "NatAm",
             RAC1P == 6 ~ "Asian",
             RAC1P == 7 ~ "NatHawPacIsl",
             RAC1P == 8 ~ "Other",
             RAC1P == 9 ~ "Multiple") %>% factor(levels = c("White", "Black", "NatAm", "Asian", "NatHawPacIsl", "Other", "Multiple")),
          GENDER = case_when(
             SEX == 1 ~ "Male",
             SEX == 2 ~ "Female") %>% factor(levels = c("Male", "Female")),
          EDUCATT = case_when(
             SCHL %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15") ~ "Some high school or less",
             SCHL %in% c("16", "17") ~ "High school diploma or GED",
             SCHL %in% c("18", "19") ~ "Some college, but no degree",
             SCHL %in% c("20") ~ "Associates or technical degree",
             SCHL %in% c("21") ~ "Bachelor’s degree",
             SCHL %in% c("22", "23", "24") ~ "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)") %>% factor(levels = c("Some high school or less",
                                                                                                                                          "High school diploma or GED",
                                                                                                                                          "Some college, but no degree",
                                                                                                                                          "Associates or technical degree",
                                                                                                                                          "Bachelor’s degree",
                                                                                                                                          "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)"))
          
          )





# Check Proportions ####

# Age ###
us_pums_props %>%
   count(AGECAT, wt=PWGTP) %>%
   mutate(agefreq = n / sum(n))

es200survey %>%
   count(AGECAT) %>%
   mutate(agefreq = n / sum(n))

# Race ####

us_pums_props %>%
   count(RACE, wt=PWGTP) %>%
   mutate(racefreq = n / sum(n))

es200survey %>%
   count(RACE) %>%
   mutate(racefreq = n / sum(n))

# Gender

us_pums_props %>%
   count(GENDER, wt=PWGTP) %>%
   mutate(genderfreq = n / sum(n))

es200survey %>%
   count(GENDER) %>%
   mutate(racefreq = n / sum(n))

# Education

us_pums_props %>%
   count(EDUCATT, wt=PWGTP) %>%
   mutate(educatt = n / sum(n))

es200survey %>%
   count(education) %>%
   mutate(educfreq = n / sum(n))

# Survey Option for Creating Targets

library(survey)
svy.acs <- svydesign(ids=~1, data=us_pums_props, weights=us_pums_props$PWGTP) 

gender <- svytable(~SEX, design=svy.acs) %>%
   prop.table() %>%
   round(digits=3) %>%
   as.numeric()

age <- svytable(~AGECAT, design=svy.acs) %>%
   prop.table() %>%
   round(digits=3) %>%
   as.numeric()

educatt <- svytable(~EDUCATT, design=svy.acs) %>%
   prop.table() %>%
   round(digits=3) %>%
   as.numeric()

race <- svytable(~RACE, design=svy.acs) %>%
   prop.table() %>%
   round(digits=3) %>%
   as.numeric()

targets <- list(age, educatt, gender, race)
names(targets) <- c("AGECAT", "EDUCATT", "GENDER", "RACE")

targets <- list(AGECAT)
names(targets) <- c("AGECAT")

# Clean ES200 Data

es200survey.w <- es200survey %>%
   filter(!is.na(RACE) & !is.na(GENDER) & !is.na(AGECAT) & !RACE == "Refuse" & !EDUCATT == "Prefer not to say") %>%
   as.data.frame()

library(anesrake)

esweights <- anesrake(targets, es200survey.w,
                      caseid = es200survey.w$ResponseId, cap = 8, type = "nolim")


# Work around AGE 

qualtricsjoin <- es200survey %>% select(ResponseId, AGECAT)
write.csv(qualtricsjoin, "agejoin.csv")
