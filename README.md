Overview
--------

This repo contains most of the code used to do the analysis and generate the
graphs in our arXiv paper [Voting patterns in 2016: Exploration using
multilevel regression and poststratification (MRP) on pre-election
polls](https://arxiv.org/abs/1802.00842).

Description
--------------

The code used to run the multilevel regression and poststratification from
start to finish can be found
[here](https://github.com/rtrangucci/mrp_2016_election/blob/master/mrp/R_code/2016_mrp.R). All the data necessary to run the code is on the repo.

The data preparation code for polling data and CPS data are in the files
[polling_2012.R](https://github.com/rtrangucci/mrp_2016_election/blob/master/mrp/R_code/polling_2012.R) and [pums_cps_clean_2012.R](https://github.com/rtrangucci/mrp_2016_election/blob/master/mrp/R_code/pums_cps_clean_2012.R).

The graphing and mapping code can be found in the [graphics](https://github.com/rtrangucci/mrp_2016_election/tree/master/mrp/R_code/graphics) directory.

All of the MRP and data prep code requires
[rstanarm](https://CRAN.R-project.org/package=rstanarm),
[dplyr](https://CRAN.R-project.org/package=dplyr),
[plyr](https://CRAN.R-project.org/package=plyr)

