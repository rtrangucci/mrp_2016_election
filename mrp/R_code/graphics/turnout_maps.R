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

###############
### MAPPING ###
###############

remove(list = ls()[!ls() %in% c("pstrat", "prefix_path", "year")])

# pstrat <- 'pstrat_gpd_age_4_cat_20170119.RDS'

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

# names(poststrat_cells)[which(names(poststrat_cells) == "correct_probs")] <- "correct_prob"

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

### color for turnout maps
turnout_base_colors <- c("#ffffcc", "#41b6c4", "#0c2c84")
### legend defaults to use repeatedly
vertical_legend_args <- list(colors = turnout_base_colors, num_cols = 26,
                             lab = c("0%", "50%", "100%"), at = c(1.5, 14, 26.5))
horizontal_legend_args <- list(colors =  turnout_base_colors, num_cols = 26,
                               lab = c("0%", "50%", "100%"), at = c(1, 13.8, 26), horizontal = TRUE)

### static footer description
static_desc <- paste("White indicates low voter turnout and dark purple indicates high voter turnout.")

### overall map (model checking)
temp <- NULL
temp <- summarise(group_by(poststrat_cells, state),
                  turnout = sum(N_voters) / sum(N_elig))
temp <- merge(us_state_shp, temp, by.x = "STUSPS", by.y = "state")

pdf(paste0(prefix_path, "/turnout_model_checking.pdf"),24,18)
par_old <- par()$mar
par(mar = par_old/4)
layout(matrix(c(1,2,3,0,4,0), nrow = 3, ncol = 2, byrow = FALSE),
       widths=c(8,1),
       heights=c(1,5,1),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016: Voter Turnout", cex = 5)

mapper(NA, temp$turnout, temp, 26, range = c(0.3,0.7), colors = turnout_base_colors)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, paste0(static_desc, "\nPost Strat: ", pstrat), cex = 2.5, pos = 4)
col_legend(colors = turnout_base_colors, num_cols = 26,
           lab = c("30%", "70%"), at = c(1.5, 26.5))
dev.off()


### educ x gender

temp <- list()
for(i in 1:length(sel$educ)) {
  temp[[sel$educ[i]]] <- summarise(group_by(poststrat_cells, state, educ, gender),
                                   turnout = sum(N_voters) / sum(N_elig))
  temp[[sel$educ[i]]] <- filter(temp[[sel$educ[i]]], gender == "Female", educ == sel$educ[i])
  temp[[sel$educ[i]]] <- select(temp[[sel$educ[i]]], state, educ, turnout)
  temp[[sel$educ[i]]] <- merge(us_state_shp, data.frame(temp[[sel$educ[i]]]), by.x = "STUSPS", by.y = "state")
}

pdf(paste0(prefix_path, "/turnout_women_educ_grid.pdf"),24,8)
layout(matrix(c(rep(1,5),2:6,7:11,rep(12,5),rep(13,5)), nrow = 5, ncol = 5, byrow = TRUE),
       widths=c(4,4,4,4,4),
       heights=c(1.5,1,3,1.5,1.5),
       respect = T)
par_old <- par()$mar
par(mar = par_old/6)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "Voter Turnout for Women", cex = 5)
for(i in 1:length(sel$educ)) {
  plot(0,0, type="n",xlab="",ylab="",axes=F)
  text(0,0, sel$educ[i], cex = 3)
}
mapper(NA, temp$`No High School`$turnout, temp$`No High School`, colors = turnout_base_colors)
mapper(NA, temp$`High School`$turnout, temp$`High School`, colors = turnout_base_colors)
mapper(NA, temp$`Some College`$turnout, temp$`Some College`, colors = turnout_base_colors)
mapper(NA, temp$`College`$turnout, temp$`College`, colors = turnout_base_colors)
mapper(NA, temp$`Post Graduate`$turnout, temp$`Post Graduate`, colors = turnout_base_colors)
par(mar = replace(par_old/6, 1, 0.1))
do.call(col_legend, horizontal_legend_args)
plot(0,0, type="n",xlab="",ylab="",axes=F, xlim = c(0,1))
par(mar = replace(par_old/6, 3, 0.1))
text(-0.04,0.5, paste0(static_desc, "\nPost Strat: ", pstrat), cex = 1.5, pos = 4)
dev.off()



### educ x gender x white-nonwhite

pdf(paste0(prefix_path, "/turnout_women_race_educ_grid.pdf"),20,8)
layout(cbind(c(0,0,19,20,0,0),
             matrix(c(rep(1,5),2:6,7:11,12:16,rep(17,5),rep(18,5)),nrow = 6, ncol = 5, byrow = TRUE)),
       widths=c(4,4,4,4,4,4),
       heights=c(1.5,1,3,3,1.5),
       respect = T)
par_old <- par()$mar
par(mar = par_old/6)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "Voter Turnout for Women by Race", cex = 5)
for(i in 1:length(sel$educ)) {
  plot(0,0, type="n",xlab="",ylab="",axes=F)
  text(0,0, sel$educ[i], cex = 3)
}

temp <- list()
for(i in 1:length(sel$educ)) {
  temp[[sel$educ[i]]] <- filter(poststrat_cells, race == "White")
  temp[[sel$educ[i]]] <- summarise(group_by(temp[[sel$educ[i]]], state, educ, gender),
                                   turnout = sum(N_voters) / sum(N_elig))
  temp[[sel$educ[i]]] <- filter(temp[[sel$educ[i]]], gender == "Female", educ == sel$educ[i])
  temp[[sel$educ[i]]] <- select(temp[[sel$educ[i]]], state, educ, turnout)
  temp[[sel$educ[i]]] <- merge(us_state_shp, data.frame(temp[[sel$educ[i]]]), by.x = "STUSPS", by.y = "state")
}
mapper(NA, temp$`No High School`$turnout, temp$`No High School`, colors = turnout_base_colors)
mapper(NA, temp$`High School`$turnout, temp$`High School`, colors = turnout_base_colors)
mapper(NA, temp$`Some College`$turnout, temp$`Some College`, colors = turnout_base_colors)
mapper(NA, temp$College$turnout, temp$College, colors = turnout_base_colors)
mapper(NA, temp$`Post Graduate`$turnout, temp$`Post Graduate`, colors = turnout_base_colors)

temp <- list()
for(i in 1:length(sel$educ)) {
  temp[[sel$educ[i]]] <- filter(poststrat_cells, race != "White")
  temp[[sel$educ[i]]] <- summarise(group_by(temp[[sel$educ[i]]], state, educ, gender),
                                   turnout = sum(N_voters) / sum(N_elig))
  temp[[sel$educ[i]]] <- filter(temp[[sel$educ[i]]], gender == "Female", educ == sel$educ[i])
  temp[[sel$educ[i]]] <- select(temp[[sel$educ[i]]], state, educ, turnout)
  temp[[sel$educ[i]]] <- merge(us_state_shp, data.frame(temp[[sel$educ[i]]]), by.x = "STUSPS", by.y = "state")
}
mapper(NA, temp$`No High School`$turnout, temp$`No High School`, colors = turnout_base_colors)
mapper(NA, temp$`High School`$turnout, temp$`High School`, colors = turnout_base_colors)
mapper(NA, temp$`Some College`$turnout, temp$`Some College`, colors = turnout_base_colors)
mapper(NA, temp$College$turnout, temp$College, colors = turnout_base_colors)
mapper(NA, temp$`Post Graduate`$turnout, temp$`Post Graduate`, colors = turnout_base_colors)

do.call(col_legend, horizontal_legend_args)

plot(0,0, type="n",xlab="",ylab="",axes=F, xlim = c(0,1))
par(mar = replace(par_old/6, 3, 0.1))
text(-0.04,0.5, paste0(static_desc, " Post Strat: ", pstrat), cex = 1.5, pos = 4)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "White", cex = 3)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "Non-White", cex = 3)

dev.off()

### educ x white-nonwhite

pdf(paste0(prefix_path, "/turnout_race_educ_grid.pdf"),15,11)
layout(cbind(rbind(c(0,1,1),c(0,2,3),c(8,4,5),c(9,6,7),c(0,10,10)),c(0,11,11,11,0)),
       widths=c(4,4,4,1.5),
       heights=c(1.5,1,3,3,1.5),
       respect = T)
par_old <- par()$mar
par(mar = par_old/6)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "Voter Turnout \nby White/non-White and Education", cex = 3.5)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "No College", cex = 3)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "College", cex = 3)

temp <- list()

temp$white_nocoll <- filter(poststrat_cells, race == "White", educ %in% c("No High School","High School","Some College"))
temp$white_nocoll <- summarise(group_by(temp$white_nocoll, state),
                               turnout = sum(N_voters) / sum(N_elig))
temp$white_nocoll <- merge(us_state_shp, data.frame(temp$white_nocoll), by.x = "STUSPS", by.y = "state")

temp$nowhite_nocoll <- filter(poststrat_cells, race != "White", educ %in% c("No High School","High School","Some College"))
temp$nowhite_nocoll <- summarise(group_by(temp$nowhite_nocoll, state),
                                 turnout = sum(N_voters) / sum(N_elig))
temp$nowhite_nocoll <- merge(us_state_shp, data.frame(temp$nowhite_nocoll), by.x = "STUSPS", by.y = "state")

temp$white_coll <- filter(poststrat_cells, race == "White", educ %in% c("College","Post Graduate"))
temp$white_coll <- summarise(group_by(temp$white_coll, state),
                             turnout = sum(N_voters) / sum(N_elig))
temp$white_coll <- merge(us_state_shp, data.frame(temp$white_coll), by.x = "STUSPS", by.y = "state")

temp$nowhite_coll <- filter(poststrat_cells, race != "White", educ %in% c("College","Post Graduate"))
temp$nowhite_coll <- summarise(group_by(temp$nowhite_coll, state),
                               turnout = sum(N_voters) / sum(N_elig))
temp$nowhite_coll <- merge(us_state_shp, data.frame(temp$nowhite_coll), by.x = "STUSPS", by.y = "state")

mapper(NA, temp$white_nocoll$turnout, temp$white_nocoll, colors = turnout_base_colors)
mapper(NA, temp$white_coll$turnout, temp$white_coll, colors = turnout_base_colors)
mapper(NA, temp$nowhite_nocoll$turnout, temp$nowhite_nocoll, colors = turnout_base_colors)
mapper(NA, temp$nowhite_coll$turnout, temp$nowhite_coll, colors = turnout_base_colors)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0.5,0, "White", cex = 3)

plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0.5,0, "Non-White", cex = 3)

plot(0,0, type="n",xlab="",ylab="",axes=F, xlim = c(0,1))
par(mar = replace(par_old/6, 3, 0.1))
text(-0.04,0.5, paste0(static_desc, "\nPost Strat: ", pstrat), cex = 1.5, pos = 4)

par(mar = replace(par_old/6, 1, 0.1))

do.call(col_legend, vertical_legend_args)

dev.off()

### educ x age overall

temp <- NULL
temp <- summarise(group_by(poststrat_cells, state, educ, age),
                  turnout = sum(N_voters) / sum(N_elig))
temp <- spread(temp, age, turnout)

pdf(paste0(prefix_path,"/turnout_overall_educ_age_grid.pdf"),24,18)

footer_text <- paste0(static_desc, " Post Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp, us_state_shp, "Voter Turnout by Age and Education",
         footer_text, legend_space = TRUE, colors = turnout_base_colors)
do.call(col_legend, vertical_legend_args)
dev.off()

### (educ x age) x race

temp <- list()
for(r in 1:length(sel$race)) {
  temp[[sel$race[r]]] <- summarise(group_by(poststrat_cells, state, educ, age, race),
                                   turnout = sum(N_voters) / sum(N_elig))
  temp[[sel$race[r]]] <- filter(temp[[sel$race[r]]], race == sel$race[r])
  temp[[sel$race[r]]] <- select(temp[[sel$race[r]]], state, educ, age, turnout)
  temp[[sel$race[r]]] <- spread(temp[[sel$race[r]]], age, turnout)
}

# white

pdf(paste0(prefix_path,"/turnout_white_educ_age_grid.pdf"),24,18)
footer_text <- paste0(static_desc, " Post Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp$White, us_state_shp, "Voter Turnout for Whites",footer_text,
         legend_space = TRUE, colors = turnout_base_colors)
do.call(col_legend, vertical_legend_args)
dev.off()

# black

pdf(paste0(prefix_path, "/turnout_black_educ_age_grid.pdf"),24,18)
footer_text <- paste0(static_desc, " Post Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp$Black, us_state_shp, "Voter Turnout for Blacks", footer_text,
         legend_space = TRUE, colors = turnout_base_colors)
do.call(col_legend, c(vertical_legend_args, show_missing = TRUE))
dev.off()

# hispanic

pdf(paste0(prefix_path, "/turnout_hispanic_educ_age_grid.pdf"),24,18)
footer_text <- paste0(static_desc, " Post Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp$Hispanic, us_state_shp, "Voter Turnout for Hispanics", footer_text,
         legend_space = TRUE, colors = turnout_base_colors)
do.call(col_legend, c(vertical_legend_args, show_missing = TRUE))
dev.off()

# other

pdf(paste0(prefix_path, "/turnout_other_educ_age_grid.pdf"),24,18)
footer_text <- paste0(static_desc, " Post Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp$Other, us_state_shp, "Voter Turnout for Other Races",
         footer_text, legend_space = TRUE, colors = turnout_base_colors)
do.call(col_legend, vertical_legend_args)
dev.off()



### (educ x age) x gender

temp <- list()
for(g in 1:length(sel$gender)) {
  temp[[sel$gender[g]]] <- summarise(group_by(poststrat_cells, state, educ, age, gender),
                                     turnout = sum(N_voters) / sum(N_elig))
  temp[[sel$gender[g]]] <- filter(temp[[sel$gender[g]]], gender == sel$gender[g])
  temp[[sel$gender[g]]] <- select(temp[[sel$gender[g]]], state, educ, age, turnout)
  temp[[sel$gender[g]]] <- spread(temp[[sel$gender[g]]], age, turnout)
}

# female

pdf(paste0(prefix_path, "/turnout_women_educ_age_grid.pdf"),24,18)
footer_text <- paste0(static_desc, " Post Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp$Female, us_state_shp, "Voter Turnout for Women",
         footer_text, legend_space = TRUE, colors = turnout_base_colors)
do.call(col_legend, vertical_legend_args)
dev.off()

# male 

pdf(paste0(prefix_path, "/turnout_men_educ_age_grid.pdf"),24,18)
footer_text <- paste0(static_desc, " Post Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp$Male, us_state_shp, "Voter Turnout for Men",
         footer_text, legend_space = TRUE, colors = turnout_base_colors)
do.call(col_legend, vertical_legend_args)
dev.off()

### (educ x age) x gender gap (i.e. male - female)

temp <- list()
for(g in 1:length(sel$gender)) {
  temp[[sel$gender[g]]] <- summarise(group_by(poststrat_cells, state, educ, age, gender),
                                     turnout = sum(N_voters) / sum(N_elig))
  temp[[sel$gender[g]]] <- filter(temp[[sel$gender[g]]], gender == sel$gender[g])
  temp[[sel$gender[g]]] <- select(temp[[sel$gender[g]]], state, educ, age, turnout)
  temp[[sel$gender[g]]] <- spread(temp[[sel$gender[g]]], age, turnout)
}
temp$diff <- cbind(state = temp$Female$state, educ = temp$Female$educ, temp$Male[,-c(1:2)] - temp$Female[,-c(1:2)])

pdf(paste0(prefix_path, "/turnout_gender_gap_educ_age_grid.pdf"),24,18)
footer_text <- paste0("Dark green indicates large gender gap, white indicates no gender gap, and orange indicates a negative gender gap.", "\nPost Strat: ", pstrat)
grid_map(sel$age, sel$educ, temp$diff, us_state_shp, "Voter Turnout Gender Gap (Men minus Women)",
         footer_text, colors = c("dark orange", "white", "dark green"), range = c(-0.1,0.1), legend_space = TRUE)
col_legend(colors = c("dark orange", "white", "dark green"), num_cols = 27,
           lab = c("-10%", "0%", "10%"), at = c(1.5, 14.5, 27.5))
dev.off()