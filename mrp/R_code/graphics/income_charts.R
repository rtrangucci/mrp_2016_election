#####################
### INCOME CHARTS ###
#####################

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

### race chart

temp <- list()
for (race_lab in unique(poststrat_cells$race)) {
  temp[[race_lab]] <- summarise(group_by(poststrat_cells, state, income, race),
                                mean_prob = (sum(prob_hat_dem*N_voters)/ sum(N_voters)))
  temp[[race_lab]] <- filter(temp[[race_lab]], race == race_lab)
  temp[[race_lab]] <- select(temp[[race_lab]], state, income, mean_prob)
  temp[[race_lab]] <- spread(temp[[race_lab]], income, mean_prob)
  temp[[race_lab]] <- temp[[race_lab]][,c("state", sel$income)]
}

temp$Overall <- summarise(group_by(poststrat_cells, state, income),
                          mean_prob = (sum(prob_hat_dem*N_voters)/ sum(N_voters)))
temp$Overall <- select(temp$Overall, state, income, mean_prob)
temp$Overall <- spread(temp$Overall, income, mean_prob)
temp$Overall <- temp$Overall[,c("state", sel$income)]

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

# rbind(rbind(rep(1,9), matrix(c(seq(1:50)+1,rep(0,4)), nrow = 6, byrow = TRUE)), c(rep(52,5), rep(0,4))),
pdf(paste0(prefix_path, "trump_support_income_race.pdf"),30,24)
layout(rbind(rbind(rep(1,9), matrix(c(seq(1:50)+1,rep(52,4)), nrow = 6, byrow = TRUE)),
             c(rep(0,5),rep(52,4)), rep(53,9)),
       widths=rep(5,9),
       heights=c(3.3, rep(5,7), 2),
       respect = T)
# title
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "Trump's Share of Vote by Income", cex = 5)
# plots
par_old <- par()$mar
for(i in 1:length(sel$state_reorder)) {
  par(mar = par_old/3)
  # whites
  plot(0:3, filter(temp$White, state == sel$state_reorder[i])[-1],
       type = "l", lwd = 5, xlab="", ylab="",
       xaxt='n', yaxt='n', xlim = c(0,3), ylim = c(0,1),
       col = "#FF851B")
  # blacks
  lines(0:3, filter(temp$Black, state == sel$state_reorder[i])[-1],
        col = "#111111", lwd = 5)
  # hispanics
  lines(0:3, filter(temp$Hispanic, state == sel$state_reorder[i])[-1],
        col = "#FF4136", lwd = 5)
  # others
  lines(0:3, filter(temp$Other, state == sel$state_reorder[i])[-1],
        col = "#2ECC40", lwd = 5)
  # overall
  lines(0:3, filter(temp$Overall, state == sel$state_reorder[i])[-1],
        col = "#0074D9", lwd = 5)
  title_col <- "black"
  text(1.5, 0.9, sel$state_reorder_lab[i], cex = 3, col = title_col)
  if(i %in% (c(47, 48, 49, 50, 51, 43, 44, 45, 46)-1)) {
    axis(side = 1, at = 0:3, labels = sel$income, las = 2, cex.axis = 2.5)
  }
  if(i %in% (c(2, 11, 20, 29, 38, 47)-1)) {
    axis(side = 2, at = seq(0,1, length.out = 3), labels = c("0%", "50%", "100%"), las = 1, cex.axis = 2.5, tick = FALSE)
  }
}
par(bg=NA) 
plot(0,0, type="n",xlab="",ylab="",axes=F)
legend(-1,0.5, c("White","Black","Hispanic","Other","Overall"), ncol = 3, cex = 3,
       col = c("#FF851B","#111111","#FF4136","#2ECC40","#0074D9"), lty = rep(1,5), lwd = rep(5,5), bty = "n")
# text(-1,0.5, "\n\n\n\n\n\n\n\nTrump's share of the two-party vote (in increasing order) for white voters (grey),\nblack voters (black), hispanic voters (red), other race voters (green), and overall (blue).", cex = 2.5, pos = 4)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0.5, paste0("Post Strat: ", pstrat), cex = 2.5, pos = 4)
dev.off()

### race chart country level

temp$Hispanic <- apply(temp$Hispanic[,-1], 2, mean, na.rm = TRUE)
temp$Other <- apply(temp$Other[,-1], 2, mean, na.rm = TRUE)
temp$White <- apply(temp$White[,-1], 2, mean, na.rm = TRUE)
temp$Black <- apply(temp$Black[,-1], 2, mean, na.rm = TRUE)
temp$Overall <- apply(temp$Overall[,-1], 2, mean, na.rm = TRUE)

pdf(paste0(prefix_path, "trump_support_income_race_national.pdf"),8,6)
par(mar = c(9, 5.1, 4.1, 2.1))
# whites
plot(0:3, temp$White,
     type = "l", lwd = 3, xlab="", ylab="", main = "Trump's Share of Vote by Income",
     xaxt='n', yaxt='n', xlim = c(0,3), ylim = c(0,0.75), cex.lab = 1.3, bty = "l",
     col = "#FF851B")
# blacks
lines(0:3, temp$Black,
      col = "#111111", lwd = 3)
# hispanics
lines(0:3, temp$Hispanic,
      col = "#FF4136", lwd = 3)
# others
lines(0:3, temp$Other,
      col = "#2ECC40", lwd = 3)
# overall
lines(0:3, temp$Overall,
      col = "#0074D9", lwd = 3)
axis(side = 2, at = seq(0,0.75, length.out = 4), labels = c("0%", "25%", "50%", "75%"), las = 1, cex.axis = 1, tick = FALSE, cex.axis = 1.2)
axis(side = 1, at = 0:3, labels = sel$income, las = 2, cex.axis = 1.2)
title(ylab = "Trump's Share of Vote", line = 4, cex.lab = 1.3)
title(sub = paste0("Post Strat: ", pstrat), line = 8, cex.sub = 0.6, adj = 0)
dev.off()