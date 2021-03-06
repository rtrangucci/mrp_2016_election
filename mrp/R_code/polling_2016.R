library(dplyr)

load("poll.Rdata")
poll <- df
rm(df)

age_cuts <- c(18,30,45,65,Inf)
poll <- poll %>% 
  filter(
    endtime >= as.Date("2016-08-16"),
    !is.na(presvote16),
    presvote16 != 'Will not vote',
    !is.na(regstate), 
    !is.na(race4), 
    !is.na(gender), 
    !is.na(educ), 
    !is.na(marstat), 
    !is.na(stcd),  
    age >= 18 & age <= 99,
    as.integer(max(as.Date(endtime)) - as.Date(endtime) + 1) <= 14
  ) %>%
  rename(
    educ_orig = educ,
    age_orig = age,
    marstat_orig = marstat
  ) %>% 
  mutate(
    presvote16 = as.character(presvote16),
    income = dplyr::recode(as.character(faminc4),
                           "Under $30k"="Under $30k","$30-50k"="$30-50k",
                           "$50-100k"="$50-100k","Over $100k"="Over $100k",
                           "Prefer not to say"="Refused",.missing="Refused"),
    edu = as.character(dplyr::recode(educ_orig,
                                     "No HS"="No High School",
                                     "High school graduate"="High School",
                                     "Some college"="Some College",
                                     "2-year"="Some College",
                                     "4-year"="College",
                                     "Post-grad"="Post Graduate")),
    state = as.character(regstate),
    age = as.character(cut(age_orig,
                           age_cuts,
                           right=F)),
    race = as.character(dplyr::recode(race4,
                         "White"="White",
                         "Black"="Black",
                         "Hispanic"="Hispanic",
                         "Other"="Other")),
    marstat = as.character(dplyr::recode(marstat_orig,
                            'Divorced'='Not married','Married'='Married',
                            'Separated'='Not married','Domestic partnership'='Married',
                            'Single'='Never married','Widowed'='Not married')),
    female = ifelse(gender == 'Female',0.5,-0.5),
    presvote16 = as.character(dplyr::recode(presvote16,
                                            "Donald Trump"="Trump",
                                            "Gary Johnson"="Johnson",
                                            "Hillary Clinton"="Clinton",
                                            "Jill Stein"="Stein",
                                            "Not sure"="Undecided",
                                            "Other"="Other"))
    
  )

poll_gpd <- poll %>% group_by(age, gender, state, race, educ, marstat) %>%
  summarise(female = first(female),
            clinton = sum(presvote16 == 'Hillary Clinton'),
            N = n(),
            trump = N - clinton) %>% ungroup()

poll_prior <- readRDS("data/poll_prior_2016.RDS")
poll_prior <- data.frame(state = names(poll_prior),
                         state_pres_vote = poll_prior)
poll_gpd <- poll_gpd %>% left_join(poll_prior,by = 'state') %>% filter(state != 'DC')

saveRDS(poll_gpd, file = 'data/poll_2016_modeling_frame.RDS')