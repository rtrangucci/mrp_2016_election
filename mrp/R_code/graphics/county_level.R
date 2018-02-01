############
### INFO ###
############

# EVENT: US Election 2016 / 2012
# LEVEL : County
# DATA: Github (https://github.com/huffpostdata/election-2012-results,
#               https://github.com/mkearney/presidential_election_county_results_2016)

library(dplyr)
library(tidyr)
library(rgdal)
library(classInt)
library(RColorBrewer)

#####################
### PRELIMINARIES ###
#####################

# data
pres16results <- read.csv("data/county_level/pres16results.csv")
pres12results <- read.csv("data/county_level/pres12results_manual.csv")
county_facts <- read.csv("data/county_level/county_facts.csv")

# county info
county_facts <- county_facts[,c("fips","area_name","state_abbreviation",
                                "INC110213","SEX255214","RHI225214","RHI725214","RHI825214","EDU635213","EDU685213")]
names(county_facts) <- c("fips","area_name","state_abbreviation",
                         "med_hh_inc","female_per",
                         "black_per","hispanic_per","white_per",
                         "educ_hs_per","educ_bach_per")
county_facts <- county_facts[-(county_facts$state_abbreviation == ""),]
county_facts$geoid <- formatC(county_facts$fips, width = 5, format = "d", flag = "0")

# 2012

name_cols <- grep("Last.name", names(pres12results), fixed = TRUE)
for(i in 1:length(name_cols)) {
  print(names(pres12results)[name_cols[i]])
  print(table(pres12results[,names(pres12results)[name_cols[i]]]))
  cat("\n")
}
# obama/romney are in Last.name, Last.name.1, Last.name.2, Last.name.3
pres12results <- pres12results[,-c(2,6,which(names(pres12results) == "Last.name.4"):ncol(pres12results))]

block1 <- select(filter(pres12results, Last.name == "Obama" | Last.name == "Romney"),
                 c(1:8), Last.name, Votes)
block2 <- select(filter(pres12results, Last.name.1 == "Obama" | Last.name.1 == "Romney"),
                 c(1:8), Last.name.1, Votes.1)
block3 <- select(filter(pres12results, Last.name.2 == "Obama" | Last.name.2 == "Romney"),
                 c(1:8), Last.name.2, Votes.2)
block4 <- select(filter(pres12results, Last.name.3 == "Obama" | Last.name.3 == "Romney"),
                 c(1:8), Last.name.3, Votes.3)

names(block1) <- c("state", "county_num", "fips", "county_name", "office_des",
                   "precincts_rep", "total_precincts", "total_votes", "cand", "votes")
names(block2) <- c("state", "county_num", "fips", "county_name", "office_des",
                   "precincts_rep", "total_precincts", "total_votes", "cand", "votes")
names(block3) <- c("state", "county_num", "fips", "county_name", "office_des",
                   "precincts_rep", "total_precincts", "total_votes", "cand", "votes")
names(block4) <- c("state", "county_num", "fips", "county_name", "office_des",
                   "precincts_rep", "total_precincts", "total_votes", "cand", "votes")

pres12results_filt <- rbind(block1,
                            block2,
                            block3,
                            block4)

pres12results_filt <- spread(pres12results_filt, cand, votes)
pres12results_filt <- pres12results_filt[-which(pres12results_filt$state %in% c("AK","HI","PR", "DC")),]
pres12results_filt <- filter(pres12results_filt, fips != 0)

# 2016
pres16results <- pres16results[-which(is.na(pres16results$county)),]
pres16results$geoid <- formatC(as.numeric(as.character(pres16results$fips)), width = 5, format = "d", flag = "0")
pres16results <- pres16results[-which(pres16results$st %in% c("AK","HI","PR", "DC")),]

# fips county info
fips_county <- read.csv("data/county_level/fips_county.csv",
                          col.names = c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "CLASSFP"), header = FALSE)
# drop regions
drop_in_shape <- unique(fips_county$STATEFP[which(fips_county$STATE %in% c("AK","HI","PR", "DC", "AS", "GU", "MP", "UM", "VI"))])
fips_county <- fips_county[-which(fips_county$STATE %in% c("AK","HI","PR", "DC")),]
# create geoid
fips_county$GEOID <- paste0(formatC(fips_county$STATEFP, width = 2, format = "d", flag = "0"),
                            formatC(fips_county$COUNTYFP, width = 3, format = "d", flag = "0"))

# read in shapefile
us_county_shp <- readOGR("data/shapefiles/cb_2015_us_county_20m/cb_2015_us_county_20m.shp", "cb_2015_us_county_20m",
                         verbose = FALSE)
# drop regions
drop_in_shape <- formatC(unique(drop_in_shape), width = 2, format = "d", flag = "0")
us_county_shp <- us_county_shp[-which(us_county_shp$STATEFP %in% drop_in_shape),]
# apply (albers) projection (if omitted then projection will resemble mercator - i.e. maps will look flat)
us_county_shp <- spTransform(us_county_shp, 
                            CRS("+proj=lcc +lat_1=39.78333333333333 +lat_2=38.71666666666667 +lat_0=38.33333333333334 +lon_0=-98 +x_0=400000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


###################
### FORMAT DATA ###
###################

# 2016

pres16results_gpd <-select(pres16results, st, county, geoid, cand, votes, total_votes, pct)

pres16results_clinton <- filter(pres16results_gpd, cand == "Hillary Clinton")
pres16results_clinton <- pres16results_clinton[,-which(names(pres16results_clinton)=="cand")]
names(pres16results_clinton)[6] <- "clinton"  # hillary clinton's county frame

pres16results_trump <- filter(pres16results_gpd, cand == "Donald Trump")
pres16results_trump <- pres16results_trump[,-which(names(pres16results_trump)=="cand")]
names(pres16results_trump)[6] <- "trump"  # donald trump's country frame

pres16results_final <- full_join(pres16results_clinton, select(pres16results_trump, geoid, trump), by = "geoid")
pres16results_final$diff <- pres16results_final$clinton - pres16results_final$trump

# 2012

pres12results_filt$geoid <- formatC(pres12results_filt$fips, width = 5, format = "d", flag = "0")
# fips_county$geoid <- fips_county$GEOID
# pres12results_filt <- left_join(pres12results_filt, fips_county, by = "geoid")

pres12results_filt <- summarize(group_by(pres12results_filt, fips),
                                total_votes = sum(total_votes),
                                obama_count = sum(Obama),
                                romney_count = sum(Romney),
                                geoid = mean(as.numeric(geoid)))

pres12results_filt$obama   <- pres12results_filt$obama_count / pres12results_filt$total_votes
pres12results_filt$romney  <- pres12results_filt$romney_count / pres12results_filt$total_votes
pres12results_filt$diff_12 <- pres12results_filt$obama - pres12results_filt$romney

pres12results_filt$geoid <-formatC(pres12results_filt$geoid, width = 5, format = "d", flag = "0")

pres16results_final$geoid <- sub(" ", "0", pres16results_final$geoid)

# merge

pres12and16 <- left_join(pres16results_final, pres12results_filt, by = "geoid")

pres12and16$rep_swing <- pres12and16$trump - pres12and16$romney
pres12and16$dem_swing <- pres12and16$clinton - pres12and16$obama

###################
### COUNTY MAPS ###
###################

shp_to_map <- merge(us_county_shp, pres12and16, by.x = "GEOID", by.y = "geoid")

source("R_code/functions.R")

# clinton minus trump
pdf(paste0(prefix_path, "maps/county_clinton_minus_trump.pdf"),24,18)
par_old <- par()$mar
par(mar = par_old/4)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE),
       widths=c(10),
       heights=c(1,5,1),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016: Clinton minus Trump", cex = 5)
mapper(NA, shp_to_map$diff, shp_to_map)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Dark blue indicates stronger support for Hillary Clinton and dark red indicates stronger support for Donald Trump.\nColors range on a scale of -100% (red) to 100% (blue).", cex = 2.5, pos = 4)
dev.off()

# obama minus romney
pdf(paste0(prefix_path, "maps/county_obama_minus_romney.pdf"),24,18)
par_old <- par()$mar
par(mar = par_old/4)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE),
       widths=c(10),
       heights=c(1,5,1),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2012: Obama minus Romney", cex = 5)
mapper(NA, shp_to_map$diff_12, shp_to_map)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Dark blue indicates stronger support for Barack Obama and dark red indicates stronger support for Mitt Romney.\nColors range on a scale of -100% (red) to 100% (blue).", cex = 2.5, pos = 4)
dev.off()

# republican swing
pdf(paste0(prefix_path, "maps/county_rep_swing.pdf"),24,18)
par_old <- par()$mar
par(mar = par_old/4)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE),
       widths=c(10),
       heights=c(1,5,1),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016/12: Republican Swing", cex = 5)
mapper(NA, shp_to_map$rep_swing, shp_to_map,
       colors = c("lightseagreen", "white", "dark red"), range = c(-0.4,0.4))
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Dark red indicates stronger support for Donald Trump (2016) compared to Mitt Romney (2012), \nand dark green indicates stronger support for Mitt Romney compared to Donald Trump.\nColors range on a scale of -40% (green) to 40% (red).", cex = 2.5, pos = 4)
dev.off()

# democratic swing
pdf(paste0(prefix_path, "maps/county_dem_swing.pdf"),24,18)
par_old <- par()$mar
par(mar = par_old/4)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE),
       widths=c(10),
       heights=c(1,5,1),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016/12: Democratic Swing", cex = 5)
mapper(NA, shp_to_map$dem_swing, shp_to_map,
       colors = c("lightseagreen", "white", "dark blue"), range = c(-0.25,0.25))
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Dark blue indicates stronger support for Hillary Clinton (2016) compared to Barack Obama (2012), \nand dark green indicates stronger support for Barack Obama compared to Hillary Clinton.\nColors range on a scale of -25% (green) to 25% (blue).", cex = 2.5, pos = 4)
dev.off()

# democratic swing
pdf(paste0(prefix_path, "maps/county_dem_swing.pdf"),24,18)
par_old <- par()$mar
par(mar = par_old/4)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE),
       widths=c(10),
       heights=c(1,5,1),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016/12: Democratic Swing", cex = 5)
mapper(NA, shp_to_map$dem_swing, shp_to_map,
       colors = c("lightseagreen", "white", "dark blue"), range = c(-0.25,0.25))
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Dark blue indicates stronger support for Hillary Clinton (2016) compared to Barack Obama (2012), \nand dark green indicates stronger support for Barack Obama compared to Hillary Clinton.\nColors range on a scale of -25% (green) to 25% (blue).", cex = 2.5, pos = 4)
dev.off()

# voter turnout
pdf(paste0(prefix_path, "maps/county_turnout.pdf"),24,18)
par_old <- par()$mar
par(mar = par_old/4)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE),
       widths=c(10),
       heights=c(1,5,1),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016: Voter Turnout", cex = 5)
mapper(NA, shp_to_map$total_votes.x/max(shp_to_map$total_votes.x, na.rm = TRUE), shp_to_map,
       colors = c("#fde0dd", "#fa9fb5", "#c51b8a"), range = c(0,0.01))
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Dark pink indicates a higher proportion and light pink indicates a lower proprotion of voter turnout.\nColors range on a scale of 0% (light pink) to 1% (dark pink).", cex = 2.5, pos = 4)
dev.off()

####################
### COUNTY PLOTS ###
####################

pres12and16 <- left_join(pres12and16, county_facts, by = "geoid")

# faminccuts <- c(-Inf,30000,50000,100000,Inf)  # you gov consistent
faminccuts <- unname(quantile(pres12and16$med_hh_inc,seq(0,1,length.out = 4)))
# labels_faminc <- c("Under $30k","$30-50k","$50-100k","Over $100k")
labels_faminc <- c("Low","Middle","High")
pres12and16$faminc <- cut(pres12and16$med_hh_inc, breaks = faminccuts, labels = labels_faminc)

# View(filter(pres12and16, faminc == "Under $30k"))

### INCOME

# # republican swing by income
# pdf(paste0(prefix_path, "charts/county_rep_swing.pdf"),40,11, pointsize = 40)
# par(mfrow = c(1,3))
# for(i in 1:length(labels_faminc)) {
#   max_val <- max(filter(pres12and16, faminc == labels_faminc[i])$total_votes.x)
#   plot(filter(pres12and16, faminc == labels_faminc[i])$romney, filter(pres12and16, faminc == labels_faminc[i])$trump,
#        xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n",
#        xlab = "Romney vote share in 2012", ylab = "Trump vote share in 2016",
#        main = paste(labels_faminc[i], "Income Counties"))
#   symbols(filter(pres12and16, faminc == labels_faminc[i])$romney, filter(pres12and16, faminc == labels_faminc[i])$trump,
#           circles=0.2*filter(pres12and16, faminc == labels_faminc[i])$total_votes.x/max_val, inches=FALSE, add=TRUE,
#           fg = "dark grey", lwd = 3)
#   abline(0,1, col = "black")
#   axis(1, seq(0,1,length.out=3), tick = FALSE)
#   axis(2, seq(0,1,length.out=3), tick = FALSE)
# }
# dev.off()


# republican swing by income
pdf(paste0(prefix_path, "charts/county_rep_swing.pdf"),33,15)
# par(mfrow = c(1,4))
par_old <- par()$mar
par(mar = par_old/4)
layout(rbind(rep(1,3),seq(2,4),rep(5,3)),
       widths=c(25,25,25),
       heights=c(5,25,5),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016/12: Republican Swing", cex = 7)
for(i in 1:length(labels_faminc)) {
  par(mar = c(7.18, 7.38, 7.38, 3.78))
  max_val <- max(filter(pres12and16, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(pres12and16, faminc == labels_faminc[i])$romney, filter(pres12and16, faminc == labels_faminc[i])$trump,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Counties"))
  if(i == 1) {
    title(ylab = "Trump vote share in 2016", cex.lab = 3, line = 5)
  }
  title(xlab = "Romney vote share in 2012", cex.lab = 3, line = 5)
  symbols(filter(pres12and16, faminc == labels_faminc[i])$romney, filter(pres12and16, faminc == labels_faminc[i])$trump,
          circles=0.1*sqrt((filter(pres12and16, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 2)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5)
  }
}
par(mar = par_old/5)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Donald Trump's share of the 2016 vote against Mitt Romney's share of the 2012 vote at the county level.\nIncome refers to county-level median household income. Low = ($19,986, $40,295], Middle = ($40,295, $48,426], High = ($48,426, $122,238]. \nThe area of each circle is proportional to voter turnout within the income quantile.", cex = 2, pos = 4)
dev.off()



# democratic swing by income
pdf(paste0(prefix_path, "charts/county_dem_swing.pdf"),33,15)
# par(mfrow = c(1,4))
par_old <- par()$mar
par(mar = par_old/4)
layout(rbind(rep(1,3),seq(2,4),rep(5,3)),
       widths=c(25,25,25),
       heights=c(5,25,5),
       respect = T)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016/12: Democratic Swing", cex = 7)
for(i in 1:length(labels_faminc)) {
  par(mar = c(7.18, 7.38, 7.38, 3.78))
  max_val <- max(filter(pres12and16, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(pres12and16, faminc == labels_faminc[i])$obama, filter(pres12and16, faminc == labels_faminc[i])$clinton,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Counties"))
  if(i == 1) {
    title(ylab = "Clinton vote share in 2016", cex.lab = 3, line = 5)
  }
  title(xlab = "Obama vote share in 2012", cex.lab = 3, line = 5)
  symbols(filter(pres12and16, faminc == labels_faminc[i])$obama, filter(pres12and16, faminc == labels_faminc[i])$clinton,
          circles=0.1*sqrt((filter(pres12and16, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 2)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5)
  }
}
par(mar = par_old/5)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,0, "Hillary Clinton's share of the 2016 vote against Barack Obama's share of the 2012 vote at the county level.\nIncome refers to county-level median household income. Low = ($19,986, $40,295], Middle = ($40,295, $48,426], High = ($48,426, $122,238]. \nThe area of each circle is proportional to voter turnout within the income quantile", cex = 2, pos = 4)
dev.off()

### INCOME x REGION

northeast <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
midwest <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
south <- c("DE","FL","GA","MD","NC","SC","VA","WV","AL","KY","MS","TN","AR","LA","OK","TX")
west <- c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")

# faminccuts <- c(-Inf,30000,50000,100000,Inf)  # you gov consistent
faminccuts_northeast <- unname(quantile(filter(pres12and16, st %in% northeast)$med_hh_inc, seq(0,1,length.out = 4)))
faminccuts_midwest <- unname(quantile(pres12and16$med_hh_inc,seq(0,1,length.out = 4)))
faminccuts_south <- unname(quantile(pres12and16$med_hh_inc,seq(0,1,length.out = 4)))
faminccuts_west <- unname(quantile(pres12and16$med_hh_inc,seq(0,1,length.out = 4)))


# republican swing by income by region
pdf(paste0(prefix_path, "charts/county_region_rep_swing.pdf"),50,70, pointsize = 20)

layout(rbind(rep(1,3),matrix(2:13, ncol = 3, byrow = TRUE),rep(14,3)),
       widths=c(25,25,25),
       heights=c(5,25,25,25,25,5),
       respect = T)

par_old <- par()$mar
par(mar = par_old/4)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016/12: Republican Swing by Region", cex = 7)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))  # bottom, left, top, right
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Northeastern Counties"))
  if(i == 1) {
    title(ylab = "Trump vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Romney vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 6)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  }
}

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Midwestern Counties"))
  if(i == 1) {
    title(ylab = "Trump vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Romney vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 6)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  }
}

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Western Counties"))
  if(i == 1) {
    title(ylab = "Trump vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Romney vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 6)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  }
}

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Southern Counties"))
  if(i == 1) {
    title(ylab = "Trump vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Romney vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$romney, filter(reduced_dat, faminc == labels_faminc[i])$trump,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 6)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  }
}

par(mar = par_old/5)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,-0.5, "Donald Trump's share of the 2016 vote against Mitt Romney's share of the 2012 vote at the county level.\nIncome refers to county-level median household income, and the income quantiles are constructed at the regional level. \nThe area of each circle is proportional to voter turnout within the income quantile.", cex = 3, pos = 4)

dev.off()



# democratic swing by income by region
pdf(paste0(prefix_path, "charts/county_region_dem_swing.pdf"),50,70, pointsize = 20)

layout(rbind(rep(1,3),matrix(2:13, ncol = 3, byrow = TRUE),rep(14,3)),
       widths=c(25,25,25),
       heights=c(5,25,25,25,25,5),
       respect = T)

par_old <- par()$mar
par(mar = par_old/4)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(0,0, "US Election 2016/12: Democratic Swing by Region", cex = 7)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))  # bottom, left, top, right
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Northeastern Counties"))
  if(i == 1) {
    title(ylab = "Clinton vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Obama vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 2)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  }
}

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Midwestern Counties"))
  if(i == 1) {
    title(ylab = "Clinton vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Obama vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 2)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 3, line = 1.5)
  }
}

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Western Counties"))
  if(i == 1) {
    title(ylab = "Clinton vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Obama vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 2)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  }
}

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
region_dat$faminc <- cut(region_dat$med_hh_inc, breaks = faminccuts_northeast, labels = labels_faminc)
for(i in 1:length(labels_faminc)) {
  par(mar = c(9.18, 8.38, 9.38, 0))
  reduced_dat <- filter(region_dat, faminc == labels_faminc[i])
  max_val <- max(filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x)
  plot(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
       xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", type="n", xlab = "", ylab = "", cex.main = 4,
       main = paste(labels_faminc[i], "Income Southern Counties"))
  if(i == 1) {
    title(ylab = "Clinton vote share in 2016", cex.lab = 4, line = 5.5)
  }
  title(xlab = "Obama vote share in 2012", cex.lab = 4, line = 6.5)
  symbols(filter(reduced_dat, faminc == labels_faminc[i])$obama, filter(reduced_dat, faminc == labels_faminc[i])$clinton,
          circles=0.1*sqrt((filter(reduced_dat, faminc == labels_faminc[i])$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
          fg = "dark grey", lwd = 2)
  abline(0,1, col = "black")
  axis(1, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  if(i == 1) {
    axis(2, seq(0,1,length.out=3), tick = FALSE, cex.axis = 2.5, line = 1.5)
  }
}

par(mar = par_old/5)
plot(0,0, type="n",xlab="",ylab="",axes=F)
text(-1,-0.5, "Hillary Clinton's share of the 2016 vote against Donald Trumps's share of the 2012 vote at the county level.\nIncome refers to county-level median household income, and the income quantiles are constructed at the regional level. \nThe area of each circle is proportional to voter turnout within the income quantile.", cex = 3, pos = 4)

dev.off()

###########################
### BASIC SCATTER PLOTS ###
###########################

### INCOME

# overall
pdf(paste0(prefix_path, "charts/county_inc_scatter_rep_swing.pdf"),10,8)
max_val <- max(pres12and16$total_votes.x)
plot(log(pres12and16$med_hh_inc/1000), pres12and16$trump - pres12and16$romney, ylim = c(-0.4,0.25),
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Median Household Income ($1,000 on log scale)", cex.lab = 1.5,
     main = "US Election 2016/12 County Republican Swing")
symbols(log(pres12and16$med_hh_inc/1000), pres12and16$trump - pres12and16$romney,
        circles=0.1*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
dev.off()

# regional
pdf(paste0(prefix_path, "charts/county_inc_region_scatter_rep_swing.pdf"),17,5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))
region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", ylim = c(-0.4,0.25), xlim = c(2.9,5),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Median Household Income ($1,000 on log scale)", cex.lab = 1.5, cex.main = 1.5,
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type= "n", ylim = c(-0.4,0.25), xlim = c(2.9,5),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Median Household Income ($1,000 on log scale)", cex.lab = 1.5, cex.main = 1.5,
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type= "n", ylim = c(-0.4,0.25), xlim = c(2.9,5),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Median Household Income ($1,000 on log scale)", cex.lab = 1.5, cex.main = 1.5,
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type= "n", ylim = c(-0.4,0.25), xlim = c(2.9,5),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Median Household Income ($1,000 on log scale)", cex.lab = 1.5, cex.main = 1.5,
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$trump - region_dat$romney,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()



### FEMALE

pdf(paste0(prefix_path, "charts/fem_rep_swing.pdf"),10,8)
plot(pres12and16$female_per, pres12and16$trump - pres12and16$romney,
     col = "dark grey", pch = 20, type = "n", ylim = c(-0.4,0.2),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Women (%)",
     main = "US Election 2016/12 County Republican Swing")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$female_per, pres12and16$trump - pres12and16$romney,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
dev.off()

# grid

pdf(paste0(prefix_path, "charts/fem_grid_scatter_rep_swing.pdf"),15,4.5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$female_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Female (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$trump - region_dat$romney,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$female_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Female (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$trump - region_dat$romney,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$female_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Female (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$trump - region_dat$romney,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$female_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Female (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$trump - region_dat$romney,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()


### RACE

pdf(paste0(prefix_path, "charts/white_rep_swing.pdf"),8,6)
plot(pres12and16$white_per, pres12and16$trump - pres12and16$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "White (%)",
     main = "US Election 2016/12 County Republican Swing")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$white_per, pres12and16$trump - pres12and16$romney,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
dev.off()

pdf(paste0(prefix_path, "charts/black_rep_swing.pdf"),8,6)
plot(pres12and16$black_per, pres12and16$trump - pres12and16$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Black (%)",
     main = "US Election 2016/12 County Republican Swing")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$black_per, pres12and16$trump - pres12and16$romney,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
dev.off()

pdf(paste0(prefix_path, "charts/hispanic_rep_swing.pdf"),8,6)
plot(pres12and16$hispanic_per, pres12and16$trump - pres12and16$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Hispanic (%)",
     main = "US Election 2016/12 County Republican Swing")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$hispanic_per, pres12and16$trump - pres12and16$romney,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
dev.off()

# grid

pdf(paste0(prefix_path, "charts/race_grid_rep_swing.pdf"),12,17)
par(mfrow = c(4,3), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$white_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "White (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$black_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Black (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Hispanic (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$white_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "White (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$black_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Black (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Hispanic (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$white_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "White (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$black_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Black (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Hispanic (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$white_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "White (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$black_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Black (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
plot(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Hispanic (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()


### EDUC

pdf(paste0(prefix_path, "charts/educ_hs_rep_swing.pdf"),8,6)
plot(pres12and16$educ_hs_per, pres12and16$trump - pres12and16$romney,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Trump vote share (2016) - Romney vote share (2012)",
     xlab = "High school education or higher (%)",
     main = "US Election 2016/12 County Republican Swing",
     ylim = c(-0.4,0.25), cex.lab = 1.5)
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$educ_hs_per, pres12and16$trump - pres12and16$romney,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
dev.off()

# high school or higher
pdf(paste0(prefix_path, "charts/educ_hs_grid_rep_swing.pdf"),15,4.5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "High school education or higher (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "High school education or higher (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "High school education or higher (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "High school education or higher (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()

# bachelors or higher
pdf(paste0(prefix_path, "charts/educ_bach_rep_swing.pdf"),10,8)
plot(pres12and16$educ_bach_per, pres12and16$trump - pres12and16$romney,
     col = "dark grey", pch = 20, type = "n",  cex.lab = 1.5, ylim = c(-0.4,0.25),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Bachelors education or higher (%)",
     main = "US Election 2016/12 County Republican Swing")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$educ_bach_per, pres12and16$trump - pres12and16$romney,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)
dev.off()

# bachelors or higher
pdf(paste0(prefix_path, "charts/educ_bach_grid_rep_swing.pdf"),15.5,4.5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80), ylim = c(-0.4, 0.25),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Bachelors education or higher (%)",
     cex.lab = 1.4, cex.main = 1.2,
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80), ylim = c(-0.4, 0.25),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Bachelors education or higher (%)",
     cex.lab = 1.4, cex.main = 1.5,
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80), ylim = c(-0.4, 0.25),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Bachelors education or higher (%)",
     cex.lab = 1.4, cex.main = 1.5,
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80), ylim = c(-0.4, 0.25),
     ylab = "Trump vote share (2016) - Romney vote share (2012)", xlab = "Bachelors education or higher (%)",
     cex.lab = 1.4, cex.main = 1.5,
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$trump - region_dat$romney,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 0)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()



###
###
### TOTAL VOTE 2016/2012 on y-axis
###
###

# overall
pdf(paste0(prefix_path, "charts/county_inc_scatter_vote_portion.pdf"),10,8)
max_val <- max(pres12and16$total_votes.x)
plot(log(pres12and16$med_hh_inc/1000), pres12and16$total_votes.x/pres12and16$total_votes.y,
     col = "dark grey", pch = 20, type = "n", ylim = c(0.4,1.9), yaxt = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Median Household Income ($1,000 on log scale)",
     main = "US Election 2016/12")
symbols(log(pres12and16$med_hh_inc/1000), pres12and16$total_votes.x/pres12and16$total_votes.y,
        circles=0.1*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
axis(2, at = seq(0.4,1.9,0.2))
abline(h = 1)
dev.off()

# regional
pdf(paste0(prefix_path, "charts/county_inc_region_scatter_vote_portion.pdf"),15,4.5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))
region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", ylim = c(0.4,1.9), xlim = c(2.9,5), yaxt = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Median Household Income ($1,000 on log scale)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
axis(2, at = seq(0.4,1.9,0.2))
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type= "n", ylim = c(0.4,1.9), xlim = c(2.9,5), yaxt = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Median Household Income ($1,000 on log scale)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
axis(2, at = seq(0.4,1.9,0.2))
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type= "n", ylim = c(0.4,1.9), xlim = c(2.9,5), yaxt = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Median Household Income ($1,000 on log scale)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
axis(2, at = seq(0.4,1.9,0.2))
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type= "n", ylim = c(0.4,1.9), xlim = c(2.9,5), yaxt = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Median Household Income ($1,000 on log scale)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(log(region_dat$med_hh_inc/1000), region_dat$total_votes.x/region_dat$total_votes.y,
        circles=0.1*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
axis(2, at = seq(0.4,1.9,0.2))
abline(h = 1)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()


### FEMALE

pdf(paste0(prefix_path, "charts/fem_vote_portion.pdf"),10,8)
plot(pres12and16$female_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
     col = "dark grey", pch = 20, type = "n", ylim = c(0.4,1.9), yaxt = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Women (%)",
     main = "US Election 2016/12")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$female_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
axis(2, at = seq(0.4,1.9,0.2))
dev.off()

# grid

pdf(paste0(prefix_path, "charts/fem_grid_scatter_vote_portion.pdf"),15,4.5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(30,55),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Female (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(30,55),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Female (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(30,55),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Female (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(30,55),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Female (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$female_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()


### RACE

pdf(paste0(prefix_path, "charts/white_vote_portion.pdf"),8,6)
plot(pres12and16$white_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "White (%)",
     main = "US Election 2016/12")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$white_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
dev.off()

pdf(paste0(prefix_path, "charts/black_vote_portion.pdf"),8,6)
plot(pres12and16$black_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Black (%)",
     main = "US Election 2016/12")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$black_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
dev.off()

pdf(paste0(prefix_path, "charts/hispanic_vote_portion.pdf"),8,6)
plot(pres12and16$hispanic_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Hispanic (%)",
     main = "US Election 2016/12")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$hispanic_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
dev.off()

# grid

pdf(paste0(prefix_path, "charts/race_grid_vote_portion.pdf"),12,17)
par(mfrow = c(4,3), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "White (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Black (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Hispanic (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "White (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Black (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Hispanic (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "White (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Black (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Hispanic (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "White (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$white_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Black (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$black_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
plot(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", cex.lab = 1.5, cex.main = 1.5, xlim = c(0,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Hispanic (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$hispanic_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()


### EDUC

pdf(paste0(prefix_path, "charts/educ_hs_vote_portion.pdf"),8,6)
plot(pres12and16$educ_hs_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "High school education or higher (%)",
     main = "US Election 2016/12")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$educ_hs_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
dev.off()

# high school or higher
pdf(paste0(prefix_path, "charts/educ_hs_grid_vote_portion.pdf"),15,4.5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "High school education or higher (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "High school education or higher (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "High school education or higher (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(45,100),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "High school education or higher (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_hs_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()

# bachelors or higher
pdf(paste0(prefix_path, "charts/educ_bach_vote_portion.pdf"),8,6)
plot(pres12and16$educ_bach_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
     col = "dark grey", pch = 20, type = "n",
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Bachelors education or higher (%)",
     main = "US Election 2016/12")
max_val <- max(pres12and16$total_votes.x)
symbols(pres12and16$educ_bach_per, pres12and16$total_votes.x/pres12and16$total_votes.y,
        circles=3*sqrt((pres12and16$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)
dev.off()

# bachelors or higher
pdf(paste0(prefix_path, "charts/educ_bach_grid_vote_portion.pdf"),15.5,4.5)
par(mfrow = c(1,4), oma = c(0, 0, 2, 0))

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% northeast)
plot(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Bachelors education or higher (%)",
     main = "Northeastern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% midwest)
plot(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Bachelors education or higher (%)",
     main = "Midwestern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% west)
plot(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Bachelors education or higher (%)",
     main = "Western Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

region_dat <- NULL
region_dat <- filter(pres12and16, st %in% south)
plot(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
     col = "dark grey", pch = 20, type = "n", xlim = c(0, 80),
     ylab = "Total votes (2016) / Total votes (2012)", xlab = "Bachelors education or higher (%)",
     main = "Southern Counties")
max_val <- max(region_dat$total_votes.x)
symbols(region_dat$educ_bach_per, region_dat$total_votes.x/region_dat$total_votes.y,
        circles=3*sqrt((region_dat$total_votes.x/max_val)*(1/pi)), inches=FALSE, add=TRUE,
        fg = "dark grey", lwd = 1)
abline(h = 1)

mtext("US Election 2016/12", outer = TRUE, cex = 1.5)
dev.off()


###
###
###

# select(pres12and16, st, county, geoid, total_votes_2016 = total_votes.x, clinton, trump, diff, fips = fips.x, total_votes_2012 = total_votes.y, obama, romney, diff_12, rep_swing, dem_swing, med_hh_inc, female_per, black_per, hispanic_per, white_per, educ_hs_per, educ_bach_per)