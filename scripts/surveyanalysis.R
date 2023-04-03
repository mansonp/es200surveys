library(pacman)

p_load(gtsummary, tidyverse, srvyr, haven, sjlabelled)

es.class.survey <- read_sav("data/class_survey.sav")

# Fix Labels

surveylabels <- get_label(es.class.survey)

surveylabels <- stringr::str_remove(surveylabels,
                                    fixed("Please read the statements below and on the left. Please respond with your level of agreement with each statement. - "))

surveylabels <- stringr::str_remove(surveylabels,
                                    fixed("Please rate your agreement or disagreement with the following statements - "))

set_label(es.class.survey) <- surveylabels

# Explore Data


es.class.survey %>% 
   mutate(across(where(is.labelled), ~as_factor(.x))) %>%
   tbl_summary(include= starts_with("nep_"))

es.class.survey %>% 
   mutate(across(where(is.labelled), ~as_factor(.x))) %>%
   tbl_summary(include= starts_with("reed_attitude"))


# es.class.survey %>% 
#    mutate(across(where(is.labelled), ~as_factor(.x) %>% fct_drop)) %>%
#    tbl_summary(include= starts_with("reed_attitude"))


nsee.svy %>% 
   mutate(across(where(is.labelled), ~as_factor(.x) %>% fct_drop)) %>%
   tbl_svysummary(include= starts_with("gw_"))

nsee2017 %>% 
   mutate(across(where(is.labelled), ~as_factor(.x) %>% fct_drop)) %>%
   as_survey(weights=Weight) %>%
   tbl_svysummary(include= starts_with("state_"), by = demog_gender)

neg_text <- stringr::str_split(classsurvey$neg_association, " ")

library(vader)

get_vader(classsurvey$neg_association)
get_vader(classsurvey$pos_association)




es.class.survey %>%
   select(starts_with("nep_")) %>%
   zap_label() %>%
   summarise(across(ends_with("nep"), ~mean(.x, na.rm=TRUE))) %>%
   t() %>%

es.class.survey %>%
   select(starts_with("nep_")) %>%
   zap_label() %>%
   summarise(across(ends_with("dsp"), ~mean(.x, na.rm=TRUE))) %>%
   t()

es.class.survey %>%
   select(starts_with("nep_")) %>%
   zap_label() %>%
   summarise(across(ends_with("nep"), ~mean(.x, na.rm=TRUE))) %>%
   rowMeans()

es.class.survey %>%
   select(starts_with("nep_") & ends_with("nep")) %>%
   zap_label() %>%
   pivot_longer(cols = ends_with("nep"), names_to = "Value", values_to = "Rating") %>%
   summarise_at(vars(Rating), list(mean=mean, sd=sd, max = max, min=min))


%>%
   summarise(across(ends_with("nep"), list(mean=mean, max = max, min=min)))

es.class.survey %>%
   select(starts_with("nep_")) %>%
   zap_label() %>%
   summarise(across(ends_with("dsp"), ~mean(.x, na.rm=TRUE))) %>%
   rowMeans()


test <- nsee2017 %>% rec(AgeRecode, rec="1,2=1 [Young]; 3,4=2 [Older]; else=99")

```{r twoway_recode}


nsee2017 %>%
   select(worldviews_trump, AgeRecode, Weight) %>% # Select the Variables and include Weight!
   rec(AgeRecode, rec="1,2=1 [Young]; 3,4=2 [Older]") %>%
   mutate(across(where(is.labelled), ~as_factor(.x) %>% fct_drop)) %>%
   mutate(noweight = 1) %>%
   as_survey_design(weight=noweight) %>%
   tbl_svysummary(include=-c(AgeRecode, Weight, noweight), by=AgeRecode_r, missing = "no") %>%
   add_p() %>%
   as_kable_extra(
      booktabs = TRUE,
      longtable = TRUE,
      linesep = "") %>%
   column_spec(1, width = "18em") %>% # Adjusted to fit the page
   column_spec(2:5, width = "5em") # Adjusted to fit the page

```

