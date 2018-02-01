############
### INFO ###
############

# Code is split into education charts and income charts
# depending on pstrat frame used

################
### PREAMBLE ###
################

library(dplyr)
library(tidyr)
library(maptools)
library(rgdal)
library(USAboundaries)
library(RColorBrewer)
library(classInt)

####################
### MAPPING DATA ###
####################


remove(list = ls()[!ls() %in% c("pstrat", "year", "prefix_path")])

source("R_code/functions.R")

poststrat_cells <- readRDS(paste0('data/',pstrat))

# NEW HEADER!!!
# N_elig = orig_weight
# N_voters = weight 
# prob_hat_dem = correct_prob
# prob_hat_turnout = turnout_weight
names(poststrat_cells)[which(names(poststrat_cells) == "orig_weight")] <- "N_elig"
names(poststrat_cells)[which(names(poststrat_cells) == "weight")] <- "N_voters"
names(poststrat_cells)[which(names(poststrat_cells) %in% c("correct_probs", "correct_prob"))] <- "prob_hat_dem"
names(poststrat_cells)[which(names(poststrat_cells) == "turnout_weight")] <- "prob_hat_turnout"

if (pstrat == "pstrat_gpd_2012_age_4_cat_20170119.RDS") {
  poststrat_cells$educ <- ifelse(poststrat_cells$educ == "Post-grad", "Post Graduate", poststrat_cells$educ)
}

if (year == 2016) {
  dem_candidate <- "Clinton"
  rep_candidate <- "Trump"
}
if (year == 2012) {
  dem_candidate <- "Obama"
  rep_candidate <- "Romeny"
}

# read in shapefile
us_state_shp <- readOGR("data/shapefiles/cb_2015_us_state_20m/cb_2015_us_state_20m.shp", "cb_2015_us_state_20m",
                        verbose = FALSE)
# get rid of dc, alaska, hawaii, and puerto rico
us_state_shp = us_state_shp[-which(us_state_shp$STUSPS %in% c("AK","HI","PR", "DC")),]
# apply (albers) projection (if omitted then projection will resemble mercator - i.e. maps will look flat)
us_state_shp <- spTransform(us_state_shp, 
                            CRS("+proj=lcc +lat_1=39.78333333333333 +lat_2=38.71666666666667 +lat_0=38.33333333333334 +lon_0=-98 +x_0=400000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

# values to extract and loop over
sel <- list()
sel$educ <- c("No High School", "High School", "Some College", "College", "Post Graduate")
sel$age <- as.character(unique(poststrat_cells$age))
sel$race <- as.character(unique(poststrat_cells$race))
sel$gender <- as.character(unique(poststrat_cells$gender))
sel$age_label <- c("Under 30", "30-45", "45-65", "65+")  # needed only for the old mapping code
sel$income <- c("Under $30k", "$30-50k", "$50-100k", "Over $100k")

# WARNING: CONVERTING TO REP SHARE
poststrat_cells$prob_hat_dem <- 1 - poststrat_cells$prob_hat_dem

# info to arrange the plots in decreasing order of trump share
sel$state <- unique(poststrat_cells$state)
temp <- list()
temp$Ordering <- summarise(group_by(poststrat_cells, state),
                           mean_prob = (sum(prob_hat_dem*N_voters)/ sum(N_voters)))
temp$Ordering <- arrange(temp$Ordering, mean_prob)

sel$state_reorder <- temp$Ordering$state
# get state names from clean shapefile
us_state_shp <- readOGR("data/shapefiles/cb_2015_us_state_20m/cb_2015_us_state_20m.shp", "cb_2015_us_state_20m",
                        verbose = FALSE)
sel$state_abb_name_map <- cbind(as.character(us_state_shp$NAME),as.character(us_state_shp$STUSPS))
for (i in 1:length(sel$state_reorder)) {
  sel$state_reorder_lab[i] <- sel$state_abb_name_map[which(sel$state_abb_name_map[,2] == sel$state_reorder[i]),1]
}
# rename
sel$state_reorder_lab[which(sel$state_reorder_lab == "District of Columbia")] <- "Washington DC"

### gender chart

temp <- list()
for (gender_lab in unique(poststrat_cells$gender)) {
  temp[[gender_lab]] <- summarise(group_by(filter(poststrat_cells, race == "White"), state, educ, gender),
                                  mean_prob = (sum(prob_hat_dem*N_voters)/ sum(N_voters)))
  temp[[gender_lab]] <- filter(temp[[gender_lab]], gender == gender_lab)
  temp[[gender_lab]] <- select(temp[[gender_lab]], state, educ, mean_prob)
  temp[[gender_lab]] <- spread(temp[[gender_lab]], educ, mean_prob)
  temp[[gender_lab]] <- temp[[gender_lab]][,c("state", sel$educ)]
}

temp$Overall <- summarise(group_by(filter(poststrat_cells, race == "White"), state, educ),
                          mean_prob = (sum(prob_hat_dem*N_voters)/ sum(N_voters)))
temp$Overall <- select(temp$Overall, state, educ, mean_prob)
temp$Overall <- spread(temp$Overall, educ, mean_prob)
temp$Overall <- temp$Overall[,c("state", sel$educ)]

# info to arrange the plots in decreasing order of trump share
temp$Ordering <- summarise(group_by(poststrat_cells, state),
                           mean_prob = (sum(prob_hat_dem*N_voters)/ sum(N_voters)))
temp$Ordering <- arrange(temp$Ordering, mean_prob)

sel$state_reorder <- temp$Ordering$state
# get state names from clean shapefile
us_state_shp <- readOGR("data/shapefiles/cb_2015_us_state_20m/cb_2015_us_state_20m.shp", "cb_2015_us_state_20m",
                        verbose = FALSE)
sel$state_abb_name_map <- cbind(as.character(us_state_shp$NAME),as.character(us_state_shp$STUSPS))
for (i in 1:length(sel$state_reorder)) {
  sel$state_reorder_lab[i] <- sel$state_abb_name_map[which(sel$state_abb_name_map[,2] == sel$state_reorder[i]),1]
}
# rename
sel$state_reorder_lab[which(sel$state_reorder_lab == "District of Columbia")] <- "Washington DC"

pdf(paste0(prefix_path, "rep_support_educ_gender_white.pdf"),30,24)
layout(rbind(rbind(rep(1,9), matrix(c(seq(1:50)+1,rep(52,4)), nrow = 6, byrow = TRUE)),
             c(rep(0,5),rep(52,4)), rep(53,9)),
       widths=rep(5,9),
       heights=c(3.3, rep(5,7), 2),
       respect = T)
# title
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, paste0(rep_candidate, "'s Share of Vote by Education (White Only)"), cex = 5)
# plots
par_old <- par()$mar
for(i in 1:length(sel$state_reorder)) {
  par(mar = par_old/3)
  # females
  plot(0:4, filter(temp$Female, state == sel$state_reorder[i])[-1],
       type = "l", lwd = 5, xlab="", ylab="",
       xaxt='n', yaxt='n', xlim = c(0,4), ylim = c(0,1),
       col = "coral2")
  # males
  lines(0:4, filter(temp$Male, state == sel$state_reorder[i])[-1],
        col = "cornflowerblue", lwd = 5)
  # overall
  lines(0:4, filter(temp$Overall, state == sel$state_reorder[i])[-1],
        col = "darkgrey", lwd = 5)
  title_col <- "black"
  text(2, 0.9, sel$state_reorder_lab[i], cex = 3, col = title_col)
  if(i %in% (c(47, 48, 49, 50, 51, 43, 44, 45, 46)-1)) {
    axis(side = 1, at = 0:4, labels = sel$educ, las = 2, cex.axis = 2.5)
  }
  if(i %in% (c(2, 11, 20, 29, 38, 47)-1)) {
    axis(side = 2, at = seq(0,1, length.out = 3), labels = c("0%", "50%", "100%"), las = 1, cex.axis = 2.5, tick = FALSE)
  }
}
plot(0,0, type="n",xlab="",ylab="",axes=F)
legend(-1,0.3, c("Women","Men","Overall"), cex = 3, horiz = TRUE, text.width = c(0.3,0.3,0.25),
       col = c("coral2","cornflowerblue","darkgrey"), lty = rep(1,3), lwd = rep(5,3), bty = "n")
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0.5, paste0("Post Strat: ", pstrat), cex = 2.5, pos = 4)
dev.off()



### gender chart: male - female by education

temp$diff <- cbind(state = temp$Female$state, temp$Male[,-1] - temp$Female[,-1])

pdf(paste0(prefix_path, "rep_support_educ_gender_gap_white.pdf"),30,24)
layout(rbind(rbind(rep(1,9), matrix(c(seq(1:50)+1,rep(52,4)), nrow = 6, byrow = TRUE)),
             c(rep(0,5),rep(52,4)), rep(53,9)),
       widths=rep(5,9),
       heights=c(3.3, rep(5,7), 2),
       respect = T)
# title
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "Gender Gap by Education (Men minus Women) White Only", cex = 5)
# plots
par_old <- par()$mar
for(i in 1:length(sel$state_reorder)) {
  par(mar = par_old/3)
  # female - male
  plot(0:4, filter(temp$diff, state == sel$state_reorder[i])[-1],
       type = "l", lwd = 5, xlab="", ylab="",
       xaxt='n', yaxt='n', xlim = c(0,4), ylim = c(0,0.22),
       col = "darkgrey")
  title_col <- "black"
  text(2, 0.2, sel$state_reorder_lab[i], cex = 3, col = title_col)
  if(i %in% (c(47, 48, 49, 50, 51, 43, 44, 45, 46)-1)) {
    axis(side = 1, at = 0:4, labels = sel$educ, las = 2, cex.axis = 2.5)
  }
  if(i %in% (c(2, 11, 20, 29, 38, 47)-1)) {
    axis(side = 2, at = seq(0,0.22, length.out = 3), labels = c("0%", "11%", "22%"), las = 1, cex.axis = 2.5, tick = FALSE)
  }
}
plot(0,0, type="n",xlab="",ylab="",axes=F)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0.5, paste0("Post Strat: ", pstrat), cex = 2.5, pos = 4)
dev.off()



### gender chart: male - female by age

temp <- list()
for (gender_lab in unique(poststrat_cells$gender)) {
  temp[[gender_lab]] <- summarise(group_by(filter(poststrat_cells, race == "White"), state, age, gender),
                                  mean_prob = (sum(prob_hat_dem*N_voters)/ sum(N_voters)))
  temp[[gender_lab]] <- filter(temp[[gender_lab]], gender == gender_lab)
  temp[[gender_lab]] <- select(temp[[gender_lab]], state, age, mean_prob)
  temp[[gender_lab]] <- spread(temp[[gender_lab]], age, mean_prob)
  temp[[gender_lab]] <- temp[[gender_lab]][,c("state", sel$age)]
}

temp$diff <- cbind(state = temp$Female$state, temp$Male[,-1] - temp$Female[,-1])

pdf(paste0(prefix_path, "rep_support_age_gender_gap_white.pdf"),30,24)
layout(rbind(rbind(rep(1,9), matrix(c(seq(1:50)+1,rep(52,4)), nrow = 6, byrow = TRUE)),
             c(rep(0,5),rep(52,4)), rep(53,9)),
       widths=rep(5,9),
       heights=c(3.3, rep(5,7), 2),
       respect = T)
# title
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "Gender Gap by Age (Men minus Women) White Only", cex = 5)
# plots
par_old <- par()$mar
for(i in 1:length(sel$state_reorder)) {
  par(mar = par_old/3)
  # female - male
  plot(0:3, filter(temp$diff, state == sel$state_reorder[i])[-1],
       type = "l", lwd = 5, xlab="", ylab="",
       xaxt='n', yaxt='n', xlim = c(0,3), ylim = c(0,0.2),
       col = "darkgrey")
  title_col <- "black"
  text(1.5, 0.18, sel$state_reorder_lab[i], cex = 3, col = title_col)
  if(i %in% (c(47, 48, 49, 50, 51, 43, 44, 45, 46)-1)) {
    axis(side = 1, at = 0:3, labels = sel$age_label, las = 2, cex.axis = 2.5)
  }
  if(i %in% (c(2, 11, 20, 29, 38, 47)-1)) {
    axis(side = 2, at = seq(0, 0.2, length.out = 3), labels = c("0%", "10%", "20%"), las = 1, cex.axis = 2.5, tick = FALSE)
  }
}
plot(0,0, type="n",xlab="",ylab="",axes=F)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0.5, paste0("Post Strat: ", pstrat), cex = 2.5, pos = 4)
dev.off()
