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


# red, blue, and swing states
sel$swing <- c("ME","NH","PA","NC","FL","MI","WI","MN","NE","CO","AZ","NV") 
sel$blue <- c("HI","VT","MA","RI","CT","NJ","DE","MD","NY","VA","IL","NM","CA","OR","WA")
sel$red <- c("AK","TX","LA","MS","AL","GA","SC","TN","AR","OK","KS","MO","KY",
             "WV","OH","IN","IA","SD","ND","WY","MT","ID","UT")


############################
### VOTER TURNOUT CHARTS ###
############################

### voter turnout education/age (side-by-side) charts
plt_dat <- list()
for(i in c("red", "blue", "swing")) {
  plt_dat$age_gpd[[i]] <- summarise(group_by(filter(poststrat_cells, state %in% sel[[i]]), age),
                                    turnout = (sum(N_voters)/ sum(N_elig)))
  plt_dat$age_gpd[[i]] <- data.frame(plt_dat$age_gpd[[i]])
}

for(i in c("red", "blue", "swing")) {
  plt_dat$educ_gpd[[i]] <- summarise(group_by(filter(poststrat_cells, state %in% sel[[i]]), educ),
                                     turnout = (sum(N_voters)/ sum(N_elig)))
  plt_dat$educ_gpd[[i]] <-  data.frame(plt_dat$educ_gpd[[i]][c(3,2,5,1,4),])
}


pdf(paste0(prefix_path, "turnout_rep_support_rbs_pooled.pdf"),12,8, pointsize = 18)
layout(matrix(c(1,2,3,3,4,4), ncol = 2, byrow = TRUE),
       widths=c(5,5),
       heights=c(5,1,1),
       respect = T)
par(mar = c(5, 4.1, 4.1, 0))
# education
plot(0, 0, type = "n",
     ylim = c(0.25,0.8), xlim = c(0,4), yaxt = "n", xaxt = "n", bty = "l",
     xlab = "", ylab = "Voter Turnout (%)", cex.lab = 1.2, yaxs = "i", cex.main = 1.5,
     main = "Voter Turnout by Education")
axis(1, at = c(0,1,3), labels = c("< HS", "HS", "College"), cex.axis = 1.2)
axis(1, at = c(2,4), labels = c("Some\nCollege", "Post-\ngrad"), cex.axis = 1.2,
     mgp=c(3, 2, 0))
axis(2, at = seq(0.25, 0.8, by = 0.05), labels = paste0(seq(0.25, 0.8, by = 0.05)*100,"%"),
     cex.axis = 1, las = 2)
lines(0:4, plt_dat$educ_gpd$red$turnout, lwd = 3, col = "firebrick2")
lines(0:4, plt_dat$educ_gpd$blue$turnout, lwd = 3, col = "dodgerblue2")
lines(0:4, plt_dat$educ_gpd$swing$turnout, lwd = 3, col = "darkorchid2")
# age
plot(0, 0, type = "n",
     ylim = c(0.25,0.8), xlim = c(0,3), yaxt = "n", xaxt = "n", bty = "l",
     xlab = "", ylab = "", yaxs = "i", cex.main = 1.5,
     main = "Voter Turnout by Age")
axis(1, at = 0:3, labels = c("18-29", "30-44", "45-64", "65+"), cex.axis = 1.2)
axis(2, at = seq(0.25, 0.8, by = 0.05), labels = paste0(seq(0.25, 0.8, by = 0.05)*100,"%"),
     cex.axis = 1, las = 2)
lines(0:3, plt_dat$age_gpd$red$turnout, lwd = 3, col = "firebrick2")
lines(0:3, plt_dat$age_gpd$blue$turnout, lwd = 3, col = "dodgerblue2")
lines(0:3, plt_dat$age_gpd$swing$turnout, lwd = 3, col = "darkorchid2")
par(mar = c(0, 0, 0.1, 0))

plot(0, 0, type = "n", xlim = c(0,1), ylim = c(0.6,1), axes = FALSE, xlab = "", ylab = "")
text(0.02, 0.9, paste("Red States:", paste(sel$red, collapse = " ")), adj = 0, cex = 0.95, col = "firebrick2")
text(0.02, 0.8, paste("Battleground States:", paste(sel$swing, collapse = " ")), adj = 0, cex = 0.95, col = "darkorchid2")
text(0.02, 0.7, paste("Blue States:", paste(sel$blue, collapse = " ")), adj = 0, cex = 0.95, col = "dodgerblue2")

plot(0, 0, type = "n", xlim = c(0,1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
text(0.02, 0.9, paste0("Post Strat: ", pstrat), adj = 0, cex = 0.95)
dev.off()

### voter turnout four chart trump share by education for each age group

plt_dat <- list()
for(i in c("red", "blue", "swing")) {
  plt_dat[[i]] <- summarise(group_by(filter(poststrat_cells, state %in% sel[[i]]), age, educ),
                            turnout = (sum(N_voters)/ sum(N_elig)))
  plt_dat[[i]] <- data.frame(spread(plt_dat[[i]], educ, turnout))
  plt_dat[[i]] <- plt_dat[[i]][,c("age", "No.High.School", "High.School", "Some.College", "College", "Post.Graduate")]
}

pdf(paste0(prefix_path, "turnout_rep_support_rbs_pooled_4plot_ts.pdf"),18,6, pointsize = 18)
layout(matrix(c(1,2,3,4,5,5,0,0,6,6,0,0), ncol = 4, byrow = TRUE),
       widths=c(5,5,5,5),
       heights=c(5,1,1),
       respect = T)
par(mar = c(5, 4.1, 4.1, 0))
for(i in 1:length(sel$age)) {
  plot(0, 0, type = "n",
       ylim = c(0.15,0.85), xlim = c(0,4), yaxt = "n", xaxt = "n", bty = "l",
       xlab = "", ylab = if(i==1) "Voter Turnout (%)" else "", cex.lab = 1.2, yaxs = "i", cex.main = 1.5,
       main = sel$age_label[i])
  axis(1, at = c(0,1,3), labels = c("< HS", "HS", "College"), cex.axis = 1.2)
  axis(1, at = c(2,4), labels = c("Some\nCollege", "Post-\ngrad"), cex.axis = 1.2,
       mgp=c(3, 2, 0))
  axis(2, at = seq(0.15, 0.85, by = 0.1), labels = paste0(seq(0.15, 0.85, by = 0.1)*100,"%"),
       cex.axis = 1, las = 2)
  lines(0:4, select(filter(plt_dat$red, age == sel$age[i]),-age), lwd = 3, col = "firebrick2")
  lines(0:4, select(filter(plt_dat$blue, age == sel$age[i]),-age), lwd = 3, col = "dodgerblue2")
  lines(0:4, select(filter(plt_dat$swing, age == sel$age[i]),-age), lwd = 3, col = "darkorchid2")
}

par(mar = c(0, 0, 0.1, 0))

plot(0, 0, type = "n", xlim = c(0,1), ylim = c(0.6,1), axes = FALSE, xlab = "", ylab = "")
text(0.02, 0.9, paste("Red States:", paste(sel$red, collapse = " ")), adj = 0, cex = 0.95, col = "firebrick2")
text(0.02, 0.8, paste("Battleground States:", paste(sel$swing, collapse = " ")), adj = 0, cex = 0.95, col = "darkorchid2")
text(0.02, 0.7, paste("Blue States:", paste(sel$blue, collapse = " ")), adj = 0, cex = 0.95, col = "dodgerblue2")

plot(0, 0, type = "n", xlim = c(0,1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
text(0.02, 0.9, paste0("Post Strat: ", pstrat), adj = 0, cex = 0.95)

dev.off()
