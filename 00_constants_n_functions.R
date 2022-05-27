## Header ----
# Author: Danny Colombara
# Date: May 26, 2022
# R version: 4.1.2
# Purpose: set essential file paths for all pulse survey analyses
#
# Needs rads >= v1.0.0
#

# input and output file directories on your local machine ----
  inputdir  <- "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/census_pulse/input/"
  outputdir <- "//PHDATA01/EPE_DATA/CDC COVID19 impacts eval/census_pulse/output/"

# state code used in pulse survey ----
  # this can be found in each survey week's data dictionary under the "EST_ST" variable
  my_state_code <- 53

# metropolitan statistical area code used in pulse survey ----
  # this can be found in each survey week's data dictionary under the "EST_MSA" variable
  my_msa_code <- 42660

  
# DO NOT EDIT (unless you are certain you know what you're doing!) ----
  # function to survey set weekly data ----
  survey_set_weekly <- function(pulseDT){
    weeklyDT <-
      srvyr::as_survey_rep(
        copy(pulseDT)  ,
        weights = pweight ,
        combined.weights = TRUE ,
        repweights = grep('pweight[0-9]+', names(dt), value  = T) ,
        scale = 4 / 80 ,
        rscales = rep( 1 , 80 ) ,
        mse = TRUE ,
        type = "JK1"
      )
    weeklyDT <- dtsurvey::dtrepsurvey(weeklyDT) # survey set for RADS package
    return(weeklyDT)
  }

  
  # function to survey set pooled data ----
  survey_set_pooled <- function(pulseDT){
    pooledDT <-   
      srvyr::as_survey_rep(
        copy(pulseDT)  ,
        weights = pooledNwt ,
        combined.weights = TRUE ,
        repweights = grep('pooledNwt[0-9]+', names(dt), value  = T) ,
        scale = 4 / 80 ,
        rscales = rep( 1 , 80 ) ,
        mse = TRUE ,
        type = "JK1"
      )
    pooledDT <- dtsurvey::dtrepsurvey(pooledDT) # survey set for RADS package
    return(pooledDT)
  }

  
# The end ----