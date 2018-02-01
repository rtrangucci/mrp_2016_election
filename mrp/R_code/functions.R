############
### INFO ###
############

# EVENT: US Election 2016
# LEVEL : NA
# DATA: NA

#################
### FUNCTIONS ###
#################

# plot map
mapper <- function(map_title, data_vector, shp_file, num_cols = 26, colors = c("dark blue", "white", "dark red"), range = c(0,1)) {
  breaks <- seq(range[1],range[2], length.out = num_cols + 1)
  plotclr <- colorRampPalette(colors)(num_cols)
  class <- classIntervals(data_vector, num_cols, style = "fixed", fixedBreaks = breaks)
  colcode <- findColours(class, plotclr)
  plot(shp_file, col = colcode, border = "white")
  if (length(is.na(data_vector))>0) {
    plot(shp_file[is.na(data_vector),], density = 10, col = "darkgrey", add = TRUE, border = "white")
  }
  if (!(is.na(map_title))) {
    title(map_title)
  }
}

col_legend <- function(colors, num_cols, lab, at, show_missing = FALSE, horizontal = FALSE) {
  if(show_missing == TRUE)
    n <- num_cols
  else
    n <- num_cols + 1
  plotclr <- colorRampPalette(colors)(num_cols)
  if(horizontal == FALSE) {
    plot(0,0,type="n",xlab="",ylab="",axes=F, xlim=c(-0.6,1.5), ylim=c(-0.05, 1.05), yaxs="i", xaxs="i")
    for(i in 1:n) {
      x1 <- (i-1)/n
      x2 <- i/n
      rect(-0.5, x1, 0, x2, col=plotclr[i], border=NA)
    }
    for(i in 1:length(lab)) {
      text(0.25, (at[i]-1)/n, lab[i], adj=0, cex=2.5)
    }
    if(show_missing == TRUE) {
      rect(-0.5, -1/n, 0, 0, col = "dark grey", density = 10, border=NA)
      text(0.25, -0.5/n, "Missing", adj = 0, cex = 2.5)
    }
  }
  else {
    plot(0,0,type="n",xlab="",ylab="",axes=F, xlim=c(0,1), ylim=c(-0.5,0.5), yaxs="i", xaxs="i")
    for (i in 1:n) {
      x1 <- (i-1)/n
      x2 <- i/n
      rect(x1, 0, x2, 0.5, col=plotclr[i], border=NA)
    }
    for (i in 1:length(lab)) {
      text((at[i]-1)/n, -0.25, lab[i], adj=0, cex=2.5) 
    }
  }
}

# extract list name when looping
extractor <- function(string, sp_merge) {
  which(names(sp_merge) == string)
}

# create 4x4 map layout for mapper
layout_mapper <- function(y_axis_lab, x_axis_lab, legend_space){
  if(legend_space == TRUE) {
    layout(matrix(c(30,30,30,30,30,0,
                    0,1,2,3,4,32,
                    5,10,11,12,13,32,
                    6,14,15,16,17,32,
                    7,18,19,20,21,32,
                    8,22,23,24,25,32,
                    9,26,27,28,29,32,
                    31,31,31,31,31,0), nrow = 8, ncol = 6, byrow = TRUE),
           widths=c(3,3.6,3.6,3.6,3.6,2),
           heights=c(0.8,1.3,2.5,2.5,2.5,2.5,2.5),
           respect = T)
  }
  else {
    layout(matrix(c(30,30,30,30,30,
                    0,1,2,3,4,
                    5,10,11,12,13,
                    6,14,15,16,17,
                    7,18,19,20,21,
                    8,22,23,24,25,
                    9,26,27,28,29,
                    31,31,31,31,31), nrow = 8, ncol = 5, byrow = TRUE),
           widths=c(3,3.6,3.6,3.6,3.6),
           heights=c(0.8,1.3,2.5,2.5,2.5,2.5,2.5),
           respect = T)
  }
  
  for(i in 1:length(y_axis_lab)) {
    plot(0,0, type="n",xlab="",ylab="",axes=F)
    text(0,0,y_axis_lab[i], cex = 3.3)
  }
  for(i in 1:length(x_axis_lab)) {
    plot(0,0, type="n",xlab="",ylab="",axes=F)
    text(0,0,x_axis_lab[i], cex = 3.3)
  }
}

# wrapper for layout_mapper and mapper
grid_map <- function(age_lab, income_lab, data, us_shape_file, grid_title, footer,
                     num_cols = 26, colors = c("dark blue", "white", "dark red"), range = c(0,1),
                     legend_space = FALSE) {
  par_old <- par()$mar
  layout_mapper(c("Under 30", "30-45", "45-65", "65+"), income_lab, legend_space)
  for(i in 1:length(income_lab)) {
    to_merge <- NULL
    to_merge <- filter(data, educ == income_lab[i])
    to_merge <- ungroup(to_merge)
    to_merge <- select(to_merge, which(names(to_merge) != "educ"))
    to_merge <- to_merge[!(to_merge$state %in% c("AK", "HI")),]  # drop AK and HI
    shp_to_map <- us_shape_file
    for (k in 1:length(age_lab)) {
      shp_to_map <- merge(shp_to_map, to_merge[,c(1,k+1)], by.x = "STUSPS", by.y = "state")
    }
    for(j in 1:length(age_lab)) {
      par(mar = par_old/4)  # shrink inner margin space
      mapper(map_title = NA, shp_to_map[[extractor(age_lab[j], shp_to_map)]], shp_to_map,
             num_cols, colors, range)
    }
  }
  plot(0,0, type="n",xlab="",ylab="",axes=F)
  text(0,0, grid_title, cex = 5)
  plot(0,0, type="n",xlab="",ylab="",axes=F)
  text(-1,0, footer, cex = 2.5, pos = 4)
  cat("note: warnings will arise due to missing values \n")
}

# create 4x4 map layout for mapper (for income)
layout_mapper_inc <- function(y_axis_lab, x_axis_lab){
  layout(matrix(c(25,25,25,25,25,
                  0,1,2,3,4,
                  5,9,10,11,12,
                  6,13,14,15,16,
                  7,17,18,19,20,
                  8,21,22,23,24,
                  26,26,26,26,26), nrow = 7, ncol = 5, byrow = TRUE),
         widths=c(3,3.6,3.6,3.6,3.6),
         heights=c(0.8,1.3,2.5,2.5,2.5,2.5,2.5),
         respect = T)
  
  for(i in 1:length(y_axis_lab)) {
    plot(0,0, type="n",xlab="",ylab="",axes=F)
    text(0,0,y_axis_lab[i], cex = 3.3)
  }
  for(i in 1:length(x_axis_lab)) {
    plot(0,0, type="n",xlab="",ylab="",axes=F)
    text(0,0,x_axis_lab[i], cex = 3.3)
  }
}

# wrapper for layout_mapper and mapper (for income)
grid_map_inc <- function(age_lab, income_lab, data, us_shape_file, grid_title, footer,
                     num_cols = 26, colors = c("dark blue", "white", "dark red"), range = c(0,1)) {
  par_old <- par()$mar
  layout_mapper_inc(c("Under 30", "30-45", "45-65", "65+"), income_lab)
  for(i in 1:length(income_lab)) {
    to_merge <- NULL
    to_merge <- filter(data, income == income_lab[i])
    to_merge <- ungroup(to_merge)
    to_merge <- select(to_merge, which(names(to_merge) != "income"))
    to_merge <- to_merge[!(to_merge$state %in% c("AK", "HI")),]  # drop AK and HI
    shp_to_map <- us_shape_file
    for (k in 1:length(age_lab)) {
      shp_to_map <- merge(shp_to_map, to_merge[,c(1,k+1)], by.x = "STUSPS", by.y = "state")
    }
    for(j in 1:length(age_lab)) {
      par(mar = par_old/4)  # shrink inner margin space
      mapper(map_title = NA, shp_to_map[[extractor(age_lab[j], shp_to_map)]], shp_to_map,
             num_cols, colors, range)
    }
  }
  plot(0,0, type="n",xlab="",ylab="",axes=F)
  text(0,0, grid_title, cex = 5)
  plot(0,0, type="n",xlab="",ylab="",axes=F)
  text(-1,0, footer, cex = 2.5, pos = 4)
  cat("note: warnings will arise due to missing values \n")
}

invlogit <- function(x) {
  return(plogis(x))
}
logit <- function(x) {
  return(qlogis(x))
}
calc_delta_correction <- function(delta, data_v, weights, x0) 
{
  abs(x0-sum(invlogit(logit(data_v) + delta)*weights))
}
weighted_correction <- function(data_v, weights, x0) 
{
  delta <- optimize(calc_delta_correction, interval=c(-5,5), data_v, weights, x0)$minimum
  corrected <- invlogit(logit(data_v) + delta)
  return(list(delta=delta, corrected=corrected))
}

# raking code -- should be put in YG library
# internal raking algorithm
rake0 <- function(vars, weight, maxit=100, eps=1e-7) {
  for (iter in 1:maxit) {
    convg <- 0
    for (v in vars) {
      counts <- prop.table(xtabs(weight ~ v$data))
      mult <- v$target / sum(v$target)
      stopifnot(all(counts[mult > 0] > 0))
      convg <- max(convg, sum(abs(counts - mult)))
      ok <- mult > 0
      mult[ok] <- mult[ok] / counts[ok]
      weight <- weight * mult[ as.integer(v$data) ]
    }
    if(iter > 99) {
      print(convg)
      print(counts)
      print(mult)
    }
    if (convg < eps & all(sapply(vars, function(v) sum(abs(prop.table(
      xtabs(weight ~ v$data)) - prop.table(v$target))) < eps)))
      return(weight)
  }
  return(NULL)
}
# rake to multidimensional margins
# targets is a list of tables with labeled dimnames
# data is a dataframe containing the dimensions in targets
# weight is an optional non-negative baseweight with length = nrow(data)
# rake returns a numeric vector of weights with the same mean as the baseweight
# (or one if there is no baseweight)
# no missing values are allowed for the weighting variables
rake <- function(data, targets, weight=NULL, maxit=100, eps=1e-7) {
  if (is.null(weight)) weight <- rep(1.0, nrow(data))
  stopifnot(length(weight) == nrow(data) & all(weight >= 0))
  vars <- lapply(targets, function(target) {
    x <- with(data, eval(parse(text=paste(names(dimnames(target)),
                                          collapse=":"))))
    print(target)
    freq <- sapply(strsplit(levels(x), ":"), function(args) do.call("[",
                                                                    c(list(target), as.list(args))))
    names(freq) <- levels(x)
    return(list(data=x, target=freq))
  })
  print(head(weight))
  rake0(vars, weight, maxit=100, eps=1e-7)
}

intervals_of_interest <- function(ps, group_facs, vote_reps, pref_reps, probs) { 
  
  gpd_ps <- ps %>% ungroup() %>% group_by_(.dots = group_facs)
  gp_ind <- gpd_ps %>% group_indices()
  gpd_ps$gp_ind <- gp_ind
  gp_sums <- summarise(gpd_ps, tot_pop = sum(N_elig), gp_ind = first(gp_ind))
  gp_nms <- select_(gp_sums, .dots = group_facs)
  gp_nms <- apply(gp_nms, 1, function(x) paste(x, collapse = '_'))
  
  gp_sums$lo = NA_real_
  gp_sums$med = NA_real_
  gp_sums$hi = NA_real_
  gp_num <- sort(unique(gp_ind))
  gp_map <- data.frame(nm = gp_nms, 
                       num = gp_num, stringsAsFactors = F)
  n_gps <- nrow(gp_map)
  vote_reps <- sweep(x = vote_reps, MARGIN = 2, STATS = ps$N_elig, FUN = '*')
  pref_reps <- vote_reps * pref_reps
  
  intervals_pref <- as.data.frame(matrix(NA, nrow = n_gps, ncol = 4))
  intervals_vote <- as.data.frame(matrix(NA, nrow = n_gps, ncol = 4))
  colnames(intervals_pref) <- c("lo", "mid", "hi", "mean")
  colnames(intervals_vote) <- c("lo", "mid", "hi", "mean")
  dists_vote <- list()
  dists_pref <- list()
  for (gp_i in seq_along(gp_num)) {
    gp <- gp_map[gp_i,]
    gp_n <- gp$num
    gp_nm <- gp$nm
    sel <- which(gpd_ps$gp_ind == gp_n)
    weight_tot <- sum(ps$N_elig[sel])
    sub_vote <- vote_reps[, sel]
    sub_pref <- pref_reps[, sel]
    dist <- rowSums(sub_pref) / rowSums(sub_vote)
    pref_vec <- dist
    vote_vec <- rowSums(sub_vote)/weight_tot
    intervals_pref[gp_n,] <- round(100*c(quantile(pref_vec, probs = probs),mean(pref_vec)),1)
    intervals_vote[gp_n,] <- round(100*c(quantile(vote_vec, probs = probs),mean(vote_vec)),1)
    dists_vote[[gp_nm]] <- vote_vec
    dists_pref[[gp_nm]] <- pref_vec
  }
  intervals_vote$group <- gp_map$nm
  intervals_pref$group <- gp_map$nm
  return(list(intervals_vote = intervals_vote, 
              intervals_pref = intervals_pref, 
              dists_vote = dists_vote, 
              dists_pref = dists_pref))
}

intervals_of_interest_corr <- function(ps, vote_reps, pref_reps, probs,
                                       turnout_act, vote_act) { 
  
  group_facs <- c('state')
  gpd_ps <- ps %>% ungroup() %>% group_by_(.dots = c('state'))
  gp_ind <- gpd_ps %>% group_indices()
  gpd_ps$gp_ind <- gp_ind
  gp_sums <- summarise(gpd_ps, tot_pop = sum(N_elig), gp_ind = first(gp_ind))
  gp_nms <- select_(gp_sums, .dots = group_facs)
  gp_nms <- apply(gp_nms, 1, function(x) paste(x, collapse = '_'))
  
  gp_sums$lo = NA_real_
  gp_sums$med = NA_real_
  gp_sums$hi = NA_real_
  gp_num <- sort(unique(gp_ind))
  gp_map <- data.frame(nm = gp_nms, 
                       num = gp_num, stringsAsFactors = F)
  n_gps <- nrow(gp_map)
  intervals <- as.data.frame(matrix(NA, nrow = n_gps, ncol = 4))
  colnames(intervals) <- c("lo", "mid", "hi", "mean")
  out_votes <- matrix(NA_real_,dim(vote_reps)[1], dim(vote_reps)[2])
  out_prefs <- matrix(NA_real_,dim(pref_reps)[1], dim(pref_reps)[2])
  for (gp_i in seq_along(gp_num)) {
    gp <- gp_map[gp_i,]
    gp_n <- gp$num
    gp_nm <- gp$nm
    st_out <- turnout_act %>% filter(state == gp_nm) %>% .$turnout_pct
    st_vote <- vote_act %>% filter(state == gp_nm) %>% .$dem_pct
    sel <- which(gpd_ps$gp_ind == gp_n)
    sub_vote <- vote_reps[, sel]
    sub_pref <- pref_reps[, sel]
    sub_weights <- ps$N_elig[sel] / sum(ps$N_elig[sel])
    cor_votes <- t(apply(sub_vote,1,function(x) 
                        weighted_correction(data_v = x, weights = sub_weights, x0 = st_out)$corrected))
    cor_weights <- sweep(x = cor_votes, MARGIN = 2, STATS = ps$N_elig[sel], FUN = '*')
    cor_prefs <- lapply(1:nrow(cor_votes), function(x)
                        weighted_correction(data_v = sub_pref[x,], weights = cor_weights[x,] / sum(cor_weights[x,]), x0 = st_vote)$corrected)
    cor_prefs <- do.call(rbind, cor_prefs)
    out_votes[,sel] <- cor_votes
    out_prefs[,sel] <- cor_prefs
    print(gp_nm)
  }
  return(list(votes = out_votes, prefs = out_prefs))
}

correct_probs <- function(pstrat, weighting, cell_prob, outcome, state_factor) {
  correct_prob = rep(NA,nrow(pstrat))
  for (st in as.character(unique(pstrat$state))) {
    sel_vec = pstrat$state == st
    probs = pstrat[sel_vec,cell_prob] %>% data.frame() %>% data.matrix() %>% drop()
    weights = pstrat[sel_vec,weighting] %>% data.frame() %>% data.matrix() %>% drop()
    state_outcome = outcome[as.character(outcome$state) == st,state_factor] %>% data.matrix() %>% drop()
    corrected = weighted_correction(probs, weights / sum(weights), state_outcome)$corrected
    correct_prob[sel_vec] <- corrected
  }
  return(correct_prob)
}