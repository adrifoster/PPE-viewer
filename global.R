#
# #########################
# Purpose: Load dependencies
# Author: Adrianna C. Foster, NSF NCAR (afoster@ucar.edu)
# Date: September, 2025
# R version 4.5.0 (2025-04-11) 'How About a Twenty-Six'
# #########################
# #########################

# load required packages
if (!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require(bslib)) install.packages("bslib", repos = "http://cran.us.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if (!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require(ggpattern)) install.packages("ggpattern", repos = "http://cran.us.r-project.org")
if (!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if (!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if (!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if (!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if (!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if (!require(ggpp)) install.packages("ggpp", repos = "http://cran.us.r-project.org")
if (!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if (!require(ncdf4)) install.packages("ncdf4", repos = "http://cran.us.r-project.org")
if (!require(terra)) install.packages("terra", repos = "http://cran.us.r-project.org")
if (!require(ggspatial)) install.packages("ggspatial", repos = "http://cran.us.r-project.org")
if (!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if (!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if (!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")

# source helper scripts
source("R/constants.R")
source("R/helpers.R")
source("R/preprocess.R")
source("R/plotting.R")