# pums_cleaning

# citizen: cit: remove code 5
# age: agep, remove under 18
# gender: sex, 1=Male, 2=Female
# state: st
# race: rac1p
# raceh: hisp
# educ: schl
# weight: pwgtp

library(data.table)
library(plyr)
library(dplyr)
library(rstanarm)
library(maps)

fips_mod <- maps::state.fips
ak_hi <- data.frame(fips = c(2, 15), ssa = NA,
                    region = NA, division = NA,
                    abb = c('AK','HI'),
                    polyname = NA)
fips_mod <- rbind(fips_mod, ak_hi)

a <- fread(input = 'data/csv_pus/ss12pusa.csv',
           select = c('CIT','AGEP','SEX','ST','RAC1P','HISP','SCHL','PWGTP','SERIALNO','RT','SPORDER','MAR'))
b <- fread(input = 'data/csv_pus/ss12pusb.csv',
           select = c('CIT','AGEP','SEX','ST','RAC1P','HISP','SCHL','PWGTP','SERIALNO','RT','SPORDER','MAR'))
c <- fread(input = 'data/csv_pus/ss12pusc.csv',
           select = c('CIT','AGEP','SEX','ST','RAC1P','HISP','SCHL','PWGTP','SERIALNO','RT','SPORDER','MAR'))
d <- fread(input = 'data/csv_pus/ss12pusd.csv',
           select = c('CIT','AGEP','SEX','ST','RAC1P','HISP','SCHL','PWGTP','SERIALNO','RT','SPORDER','MAR'))

mgd <- bind_rows(a,b,c,d)
mgd <- mgd %>% filter(CIT != 5,AGEP >= 18, AGEP <= 99)
mgd <- mgd %>% mutate(sex = dplyr::recode(SEX, `1`='Male', `2`='Female'),
                      state = dplyr::recode(ST, 
                                            `1`='AL',
                                            `2`='AK',
                                            `4`='AZ',
                                            `5`='AR',
                                            `6`='CA',
                                            `8`='CO',
                                            `9`='CT',
                                            `10`='DE',
                                            `11`='DC',
                                            `12`='FL',
                                            `13`='GA',
                                            `15`='HI',
                                            `16`='ID',
                                            `17`='IL',
                                            `18`='IN',
                                            `19`='IA',
                                            `20`='KS',
                                            `21`='KY',
                                            `22`='LA',
                                            `23`='ME',
                                            `24`='MD',
                                            `25`='MA',
                                            `26`='MI',
                                            `27`='MN',
                                            `28`='MS',
                                            `29`='MO',
                                            `30`='MT',
                                            `31`='NE',
                                            `32`='NV',
                                            `33`='NH',
                                            `34`='NJ',
                                            `35`='NM',
                                            `36`='NY',
                                            `37`='NC',
                                            `38`='ND',
                                            `39`='OH',
                                            `40`='OK',
                                            `41`='OR',
                                            `42`='PA',
                                            `44`='RI',
                                            `45`='SC',
                                            `46`='SD',
                                            `47`='TN',
                                            `48`='TX',
                                            `49`='UT',
                                            `50`='VT',
                                            `51`='VA',
                                            `53`='WA',
                                            `54`='WV',
                                            `55`='WI',
                                            `56`='WY',
                                            `72`='PR')) %>% filter(state != 'PR')

mgd <- mgd %>%
  mutate(
    hisp_mult = HISP == 1,
    tmp = hisp_mult * RAC1P,
    race = dplyr::recode(tmp, `1`='White', `2`='Black', `0`='Hispanic', .default = 'Other'),
    educ_lvl = as.integer(cut(SCHL, breaks = c(0,15,17,20,21,Inf))),
    educ = dplyr::recode(educ_lvl, `1`='No High School',
                         `2`='High School',`3`='Some College',
                         `4`='College',`5`='Post Graduate'),
    marstat = dplyr::recode(MAR,
                            `1`='Married',
                            `2`='Not married',
                            `3`='Not married',
                            `4`='Not married',
                            `5`='Never married'),
    age = cut(AGEP, breaks = c(18,30,45,65,Inf), right=F)) %>% rename(weight=PWGTP)

poll_prior <- readRDS("data/poll_prior_2012.RDS") %>% select(-state_nm)

pstrat_gpd <- mgd %>% 
  mutate(
    age = as.character(age),
    female = if_else(sex == 'Female', 0.5, -0.5)
  ) %>%
  rename(gender = sex) %>%
  group_by(
    age,
    gender,
    state,
    race,
    educ,
    marstat
  ) %>%
  summarise(
    female = first(female),
    N_elig = sum(weight),
    N = n()
  ) %>% ungroup() %>%
  left_join(poll_prior, by = 'state') %>%
  filter(state != 'DC')

saveRDS(pstrat_gpd, file = 'data/pstrat_2012.RDS')

cps <- readRDS(file = 'data/cps_raw_20170111.RDS') 
cps <- cps %>% filter(AGE >= 18)
cps <- cps %>% filter(CITIZEN != 3)
cps <- cps %>% filter(!(VOTEREG %in% c(99, 98)))
cps <- cps %>% mutate(
  state = plyr::mapvalues(STATEFIP,
                          from = fips_mod$fips,
                          to = as.character(fips_mod$abb)),
  age = cut(AGE, breaks = c(18,30,45,65,Inf), right=F),
  gender = dplyr::recode(SEX,`1`='Male',`2`='Female'),
  race_vec = (HISPAN == 0) * RACE,
  race = dplyr::recode(race_vec, `100`='White', `200`='Black', `0` = 'Hispanic', .default='Other'),
  educ_cut = as.integer(cut(EDUC,breaks = c(0,72,73,110,111,Inf))),
  educ = dplyr::recode(educ_cut,
                       `1`='No High School', `2`='High School', 
                       `3`='Some College', `4`='College',
                       `5`='Post Graduate'),
  marstat_raw = dplyr::recode(MARST,
                          `1`='Married',`2`='Married',`3`='Separated',
                          `4`='Divorced',`5`='Widowed',`6`='Single',
                          `7`='Widowed'),
  marstat = dplyr::recode(marstat_raw,
                          'Married'='Married','Married'='Married','Separated'='Not married',
                          'Divorced'='Not married','Widowed'='Not married','Single'='Never married',
                          'Widowed'='Not married'),
  vote = as.integer(VOTEREG == 2)
  )

cps %>% group_by(educ, state, age, gender, race, marstat) %>%
  summarise(N = n(), vote = sum(vote), 
            did_not_vote = N - vote, 
            female = if_else(first(gender) == 'Male',-0.5,0.5)) -> cps_fit
cps_fit <- cps_fit %>% left_join(poll_prior, by = 'state') %>% filter(!is.na(state_pres_vote))
cps_fit %>% ungroup() %>% 
  dplyr::mutate(educ = as.character(educ),
                state = as.character(state), 
                age = as.character(age),
                gender = as.character(gender),
                race = as.character(race),
                marstat = as.character(marstat)) -> cps_fit

saveRDS(cps_fit, file = 'data/cps_2012_modeling_frame.RDS')