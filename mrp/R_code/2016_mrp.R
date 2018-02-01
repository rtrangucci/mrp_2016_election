############
### INFO ###
############

# EVENT: US Election 2016
# LEVEL : State
# DATA: Polling, ACS, Result

# REPRODUCIBLE STEPS:
# 1. clean data
# 2. fit model (with appropriate interactions)
# 3. poststratify using cell weights
# 4. apply an election outcome correction

################
### PREAMBLE ###
################

library(dplyr)
library(rstanarm)

#######################
### LOAD/CLEAN DATA ###
#######################

## CCES cleaning 

# set directory to parent directory!

source("R_code/functions.R")

poll_2016 <- readRDS('data/poll_2016_modeling_frame.RDS')

pref_model <- stan_glmer(
  cbind(clinton, trump) ~ 1 + female + state_pres_vote +
    (1 | state) + (1 | age) + (1 | educ) +
    (1 + state_pres_vote | race) + (1 | marstat) +
    (1 | marstat:age) + (1 | marstat:state) + (1 | marstat:race) + (1 | marstat:gender) + (1 | marstat:educ) +
    (1 | state:gender) + (1 | age:gender) + (1 | educ:gender) + (1 | race:gender) +
    (1 | state:race) + (1 | state:age) + (1 | state:educ) + 
    (1 | race:age) + (1 | race:educ) +
    (1 | age:educ) +
    (1 | state:educ:age) +
    (1 | educ:age:gender), 
  family = "binomial",
  data = poll_2016,
  iter = 1000,
  cores = 4,
  adapt_delta = 0.95
)

cps_2016 <- readRDS('data/cps_2016_modeling_frame.RDS')

turnout_model <- stan_glmer(
  cbind(vote, did_not_vote) ~ 1 + female + state_pres_vote +
    (1 | state) + (1 | age) + (1 | educ) +
    (1 + state_pres_vote | race) + (1 | marstat) +
    (1 | marstat:age) + (1 | marstat:state) + (1 | marstat:race) + (1 | marstat:gender) + (1 | marstat:educ) +
    (1 | state:gender) + (1 | age:gender) + (1 | educ:gender) + (1 | race:gender) +
    (1 | state:race) + (1 | state:age) + (1 | state:educ) + 
    (1 | race:age) + (1 | race:educ) +
    (1 | age:educ) +
    (1 | state:educ:age) +
    (1 | educ:age:gender),
  family = "binomial",
  data = cps_2016,
  iter = 1000,
  cores = 4,
  adapt_delta = 0.95
)

pstrat_2016 <- readRDS('data/pstrat_2016.RDS')
voting_preds <- posterior_linpred(turnout_model, newdata = pstrat_2016, transform = T)
pref_preds <- posterior_linpred(pref_model, newdata = pstrat_2016, transform = T)
pstrat_2016$vote_prob <- colMeans(voting_preds)
pstrat_2016$dem_prob <- colMeans(pref_preds) # mu is raw expected prob_dem

votes <- readRDS(file = 'data/results_2016.RDS')

pstrat_2016$prob_hat_turnout <- 
  correct_probs(pstrat = pstrat_2016,
                weighting = 'N_elig',
                cell_prob = 'vote_prob',
                outcome = votes,
                state_factor = 'turnout_pct'
  )

pstrat_2016$N_voters <-
  pstrat_2016$prob_hat_turnout * pstrat_2016$N_elig
pstrat_2016$prob_hat_dem <- 
  correct_probs(pstrat_2016,
                weighting = 'N_voters',
                cell_prob = 'dem_prob', 
                outcome = votes, 
                state_factor = 'pct_clinton'
  )

saveRDS(pstrat_2016, file = 'data/pstrat_2016_modeled.RDS')
