
library(pacman)
p_load(srvyr, gtsummary, sjmisc, haven, tidyverse)


nsee2017 <- read_sav("data/nsee2017.sav")

nsee.svy <- nsee2017 %>% as_survey_design(weight=Weight)

nsee.svy %>% mutate(across(where(is.labelled), ~as_factor(.x) %>% fct_drop)) %>%
   tbl_svysummary(by=gw_belief)

nsee2017 %>%
   select(gw_belief,weather_pastsummer_v1, Weight) %>% # Select the Variables and include Weight!
   mutate(across(where(is.labelled), ~as_factor(.x) %>% fct_drop)) %>%
   as_survey_design(weight=Weight) %>%
   tbl_svysummary(include=-Weight, missing = "no")

nsee2017 %>%
   select(gw_belief, Weight) %>%
   mutate(across(where(is.labelled), ~as_factor(.x) %>% fct_drop)) %>%
   tbl_summary()
