## Header ----
    # Author: Danny Colombara
    # Date: May 26, 2022
    # R version: 4.1.2
    # Purpose: analyze PULSE data
    #
    # Notes: Phase 3.2 has many differences vs phase 2, 3, and 3.1 and so is analyzed separately
    #        It begins with week 34
    #
    # Needs rads >= v1.0.0

## set up ----
    pacman::p_load(data.table, openxlsx, srvyr, rads)

    # load prepped data if necessary ----
    if(!exists("svy_msa") | !exists("svy_wa")){
      eval(parse(text = httr::content(httr::GET(
        url = "https://raw.githubusercontent.com/PHSKC-APDE/svy_pulse/main/00_phase3_2_prep_survey.R",
        httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")), "text")))
    }

    # load dates for each week ----
    week_dates <- data.table::fread(httr::content(httr::GET(
      url = "https://raw.githubusercontent.com/PHSKC-APDE/svy_pulse/main/pulse_weeks_dates.csv",
      httr::authenticate(Sys.getenv("GITHUB_PAT"), "")), type = "text", encoding = "UTF-8"))
      if(grepl("-", week_dates[1]$start_date)){
        week_dates[, start_date := as.Date(start_date)]
        week_dates[, end_date := as.Date(end_date)]
      }
      if(grepl("/", week_dates[1]$start_date)){
          week_dates[, start_date := lubridate::mdy(gsub("/", "-", start_date))]
          week_dates[, end_date := lubridate::mdy(gsub("/", "-", end_date))]
      }
      week_dates[week == "43842", week := "1-12"] # Excel converts 1-12 to a January 12, so need to ensure it is the text '1-12'
      week_dates <- rads::sql_clean(week_dates)[!is.na(week)]
    
    # load variable description for vars actually used ----
    vartable <- data.table::fread(httr::content(httr::GET(
      url = "https://raw.githubusercontent.com/PHSKC-APDE/svy_pulse/main/pulse_varlist.csv",
      httr::authenticate(Sys.getenv("GITHUB_PAT"), "")), type = "text", encoding = "UTF-8"))

## Identify columns for calculations ----
    education_vars <- c("chldcare", paste0("chldimpct", 1:9))  
            
    education_vars_noncat <- c("enroll_school", "enroll_home", 
                               "ps_cancelchange", "tnum_ps") 
                               
    food_vars <- c("childfood.bin", "curfoodsuf", "freefood" ,
                   grep("^foodwhynot", names(dt), value = T), 
                   "curfoodsuf2", "foodwhynot12", "foodwhynot22", "foodwhynot32", "foodwhynot42")    
    
    health_vars <- c("down", "anxious", "worry", "interest", 
                     "phq4severe", "phq4mod_severe", "phq4anxiety", "phq4depression", 
                     "prescript", "mh_svcs", "mh_notget", 
                     "telehlth", "telechld", "prvntive")

    housing_vars <- c("tenure2", "rentcur", "mortcur", "current", "notcurrent", "confidence", "confidence.binary", "eviction", "foreclosure", "leave2mo", "leave2mo_alt")
    
    insured_vars <- c("insurance", "uninsured", "insured", "insured_employer", "insured_exchange", "insured_mcare", "insured_military")

    vaccine_vars <- c("recvdvacc", "doses", "definitevacc", "prevcovid", "whynorv1", "whynorv2", "whynorv3", "whynorv4", "whynorv5", 
                      "whynorv6", "whynorv7", "whynorv8", "whynorv9", "whynorv10", "whynorv11", "whynorv12", "whynorv13")
    
    mybyvars <- c("phase", "age4", "anywork", "disability", "edu", "ethn", "income", "kindwork", "ms", "wrklossrv", "alone", 
                  "sex_at_birth", "gender_id", "orientation", "lgbt", "lgbtq")
    
## Use rads to perform calculations ----
    # note, even though the education vars are relevant given specific conditionalities (e.g., enroll_school==1), it is not 
    # necessary to specify this in rads::calc() in the '...' ('where') statement. This is because the NA values have been properly
    # set in the data prep process. In other words, we are not subsetting to a population that is not already accounted for in the survey design.
    
    # education: by week ----
        education_weeks_wa <- rbind(
          calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
               what = education_vars, # categorical variables
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = T,
               ci = 0.90), 
          calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
               what = education_vars_noncat, # continuous variables
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = F,
               ci = 0.90))
        
        education_weeks_msa <- rbind(
          calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
               what = education_vars, # categorical vars
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = T,
               ci = 0.90), 
          calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
               what = education_vars_noncat, # continuous vars
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = F,
               ci = 0.90))
    
    # education: pooled demographics ----
        edu.cat.grid <- setDT(expand.grid(myvars = education_vars, byvars = setdiff(mybyvars, "alone")))
        edu.cont.grid <- setDT(expand.grid(myvars = education_vars_noncat, byvars = setdiff(mybyvars, "alone")))
        
        education_msa_combo <- rbind(
          # Categorical education variables
          rbindlist(lapply(X = as.list(seq(1, nrow(edu.cat.grid))), 
                           FUN = function(X){
                             # message("Education MSA combo (categorical) #", X, ": ", paste0(edu.cat.grid[X, myvars]), " x ", paste0(edu.cat.grid[X, byvars]))
                             tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use data set for pooled weeks
                                                  what = paste0(edu.cat.grid[X, myvars]),
                                                  by = paste0(edu.cat.grid[X, byvars]),
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = T,
                                                  ci = 0.90)
                             tempDT[, category := paste0(edu.cat.grid[X, byvars])]
                             setnames(tempDT, paste0(edu.cat.grid[X, byvars]), "group")
                             tempDT[, geo := "MSA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T ), 
          # Continuous education variables
          rbindlist(lapply(X = as.list(as.list(seq(1, nrow(edu.cont.grid)))),
                           FUN = function(X){
                             # message("Education MSA combo (continuous) #", X, ": ", paste0(edu.cont.grid[X, myvars]), " x ", paste0(edu.cont.grid[X, byvars]))
                             tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use data set for pooled weeks
                                                  what = paste0(edu.cont.grid[X, myvars]),
                                                  by = paste0(edu.cont.grid[X, byvars]),
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = F,
                                                  ci = 0.90)
                             tempDT[, category := paste0(edu.cont.grid[X, byvars])]
                             setnames(tempDT, paste0(edu.cont.grid[X, byvars]), "group")
                             tempDT[, geo := "MSA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T ))
        
        education_wa_combo <- rbind(
          # Categorical education variables
          rbindlist(lapply(X = as.list(seq(1, nrow(edu.cat.grid))), 
                           FUN = function(X){
                             # message("Education WA combo (categorical) #", X, ": ", paste0(edu.cat.grid[X, myvars]), " x ", paste0(edu.cat.grid[X, byvars]))
                             tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use data set for pooled weeks
                                                  what = paste0(edu.cat.grid[X, myvars]),
                                                  by = paste0(edu.cat.grid[X, byvars]),
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = T,
                                                  ci = 0.90)
                             tempDT[, category := paste0(edu.cat.grid[X, byvars])]
                             setnames(tempDT, paste0(edu.cat.grid[X, byvars]), "group")
                             tempDT[, geo := "WA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T ), 
          # Continuous education variables
          rbindlist(lapply(X = as.list(as.list(seq(1, nrow(edu.cont.grid)))),
                           FUN = function(X){
                             # message("Education WA combo (continuous) #", X, ": ", paste0(edu.cont.grid[X, myvars]), " x ", paste0(edu.cont.grid[X, byvars]))
                             tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use data set for pooled weeks
                                                  what = paste0(edu.cont.grid[X, myvars]),
                                                  by = paste0(edu.cont.grid[X, byvars]),
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = F,
                                                  ci = 0.90)
                             tempDT[, category := paste0(edu.cont.grid[X, byvars])]
                             setnames(tempDT, paste0(edu.cont.grid[X, byvars]), "group")
                             tempDT[, geo := "WA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T ))
        
    # Food security: by week ----
        food_weeks_wa <- rbind(
                          calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
                                what = c("curfoodsuf", grep("foodwhynot|childfood.bin", names(dt), value = T), "freefood"), # insufficient food, couldn't afford, afraid, stores didn't have what was wanted, free food
                                by = c("week", "phase"),
                                metrics = c("mean", "rse", "denominator", "numerator"),
                                proportion = T,
                                ci = 0.90)[, hh:= "All"], 
                          calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
                               what = grep("curfoodsuf$|foodwhynot", names(dt), value = T),
                               children == "Has children", # also calc for HH with children
                               by = c("week", "phase"),
                               metrics = c("mean", "rse", "denominator", "numerator"),
                               proportion = T,
                               ci = 0.90)[, variable := paste0(variable, "2")][, hh := "With children"])
        
        food_weeks_msa <- rbind(
                            calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
                                 what = c("curfoodsuf", grep("foodwhynot|childfood.bin", names(dt), value = T), "freefood"), # insufficient food, couldn't afford, afraid, stores didn't have what was wanted, free food
                                 by = c("week", "phase"),
                                 metrics = c("mean", "rse", "denominator", "numerator"),
                                 proportion = T,
                                 ci = 0.90)[, hh := "All"], 
                            calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
                                 what = grep("curfoodsuf$|foodwhynot", names(dt), value = T),
                                 children == "Has children", # also calc for HH with children
                                 by = c("week", "phase"),
                                 metrics = c("mean", "rse", "denominator", "numerator"),
                                 proportion = T,
                                 ci = 0.90)[, variable := paste0(variable, "2")][, hh := "With children"])
        
    # Food security: pooled demographics ----
        # Make grid of all combinations needed
        mycombos <- setDT(expand.grid(myvars = c(setdiff(mybyvars, "gender"), "children"), weekstart = min(food_weeks_wa$week) , weekend = max(food_weeks_wa$week)))
        mycombos <- mycombos[(weekstart==min(food_weeks_wa$week) & weekend==max(food_weeks_wa$week))]
        
        food_msa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(mycombos), 1)),
                                             FUN = function(X){
                                               # message(paste0("Food MSA combo: row ", X, " of ", nrow(mycombos)))
                                               tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use data set for pooled weeks
                                                                    what = c("curfoodsuf", grep("where|foodwhy", names(dt), value = T), "freefood"),
                                                                    week %in% paste0(mycombos[X, weekstart]):paste0(mycombos[X, weekend]),
                                                                    by = paste0(mycombos[X, myvars]),
                                                                    time_var = "week",
                                                                    metrics = c("mean", "rse", "denominator", "numerator"),
                                                                    proportion = T,
                                                                    ci = 0.90)
                                               tempDT[, category := mycombos[X, myvars]]
                                               setnames(tempDT, paste0(mycombos[X, myvars]), "group")
                                               tempDT[, geo := "MSA"]
                                               tempDT <- tempDT[!is.na(group)]
                                               tempDT <- tempDT[, week := paste0(mycombos[X, ]$weekstart, "-", mycombos[X, ]$weekend)]
                                             }), use.names = T )
        
        food_wa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(mycombos), 1)),
                                           FUN = function(X){
                                             # message(paste0("Food MSA combo: row ", X, " of ", nrow(mycombos)))
                                             tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use data set for pooled weeks
                                                                  what = c("curfoodsuf", grep("where|foodwhy", names(dt), value = T), "freefood"),
                                                                  week %in% paste0(mycombos[X, weekstart]):paste0(mycombos[X, weekend]),
                                                                  by = paste0(mycombos[X, myvars]),
                                                                  time_var = "week",
                                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                                  proportion = T,
                                                                  ci = 0.90)
                                             tempDT[, category := mycombos[X, myvars]]
                                             setnames(tempDT, paste0(mycombos[X, myvars]), "group")
                                             tempDT[, geo := "WA"]
                                             tempDT <- tempDT[!is.na(group)]
                                             tempDT <- tempDT[, week := paste0(mycombos[X, ]$weekstart, "-", mycombos[X, ]$weekend)]
                                           }), use.names = T )
    
    # Health: by week ----
        health_weeks_wa <- calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
                           what = health_vars,
                           by = c("week", "phase"),
                           metrics = c("mean", "rse", "denominator", "numerator"),
                           proportion = T,
                           ci = 0.90)

        health_weeks_msa <- calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
                            what = health_vars,
                            by = c("week", "phase"),
                            metrics = c("mean", "rse", "denominator", "numerator"),
                            proportion = T,
                            ci = 0.90)


    # Health: pooled demographics ----
        # Cycle through all other insurance x byvar combinations
        grid.health <- setDT(expand.grid(indicator = health_vars, byvar = mybyvars))
        grid.health <- grid.health[!(indicator %in% c("telechld", "prvntive", paste0("prvntwhy", 1:7)) & byvar == "alone")]

        health_msa_combo <- rbindlist(lapply(X = as.list( seq(1, nrow(grid.health)) ),
                                            FUN = function(X){
                                                  # message(paste0("Health MSA combo: ", paste0(grid.health[X]$indicator), ":", paste0(grid.health[X]$byvar)))
                                                  tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use data set for pooled weeks
                                                                       what = paste0(grid.health[X]$indicator),
                                                                       by = paste0(grid.health[X]$byvar),
                                                                       time_var = "week",
                                                                       metrics = c("mean", "rse", "denominator", "numerator"),
                                                                       proportion = T,
                                                                       ci = 0.90)
                                                  tempDT[, category := paste0(grid.health[X]$byvar)]
                                                  setnames(tempDT, paste0(grid.health[X]$byvar), "group")
                                                  tempDT[, geo := "MSA"]
                                                  tempDT <- tempDT[!is.na(group)]
                                             }), use.names = T )
        
        health_wa_combo  <- rbindlist(lapply(X = as.list( seq(1, nrow(grid.health)) ),
                                             FUN = function(X){
                                               # message(paste0("Health WA combo: ", paste0(grid.health[X]$indicator), ":", paste0(grid.health[X]$byvar)))
                                               tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use data set for pooled weeks
                                                                    what = paste0(grid.health[X]$indicator),
                                                                    by = paste0(grid.health[X]$byvar),
                                                                    time_var = "week",
                                                                    metrics = c("mean", "rse", "denominator", "numerator"),
                                                                    proportion = T,
                                                                    ci = 0.90)
                                               tempDT[, category := paste0(grid.health[X]$byvar)]
                                               setnames(tempDT, paste0(grid.health[X]$byvar), "group")
                                               tempDT[, geo := "WA"]
                                               tempDT <- tempDT[!is.na(group)]
                                             }), use.names = T )

    # Housing: by week ----
        housing_weeks_wa <- calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
                           what = housing_vars,
                           by = c("week", "phase"),
                           metrics = c("mean", "rse", "denominator", "numerator"),
                           proportion = T,
                           ci = 0.90)

        housing_weeks_msa <- calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
                            what = housing_vars,
                            by = c("week", "phase"),
                            metrics = c("mean", "rse", "denominator", "numerator"),
                            proportion = T,
                            ci = 0.90)

    # Housing: pooled demographics ----
        housing.grid <- setDT(expand.grid(myvars = housing_vars, byvars = mybyvars))
        
        housing_msa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(housing.grid))),
                                              FUN = function(X){
                                                # message("Housing MSA combo #", X, ": ", paste0(housing.grid[X, myvars]), " x ", paste0(housing.grid[X, byvars]))
                                                tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use data set for pooled weeks
                                                                     what = paste0(housing.grid[X, myvars]),
                                                                       ifelse(paste0(housing.grid[1, myvars]) == "eviction", rentcur == 0, phase == "3.2") &
                                                                       ifelse(paste0(housing.grid[1, myvars]) == "foreclosure", mortcur == 0, phase == "3.2") &
                                                                       ifelse(paste0(housing.grid[1, myvars]) == "leave2mo", current == 0, phase == "3.2"),
                                                                     by = paste0(housing.grid[X, byvars]),
                                                                     time_var = "week",
                                                                     metrics = c("mean", "rse", "denominator", "numerator"),
                                                                     proportion = T,
                                                                     ci = 0.90)
                                                tempDT[, category := paste0(housing.grid[X, byvars])]
                                                setnames(tempDT, paste0(housing.grid[X, byvars]), "group")
                                                tempDT[, geo := "MSA"]
                                                tempDT <- tempDT[!is.na(group)]
                                              }), use.names = T )     

        housing_wa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(housing.grid))),
                                              FUN = function(X){
                                                # message("Housing WA combo #", X, ": ", paste0(housing.grid[X, myvars]), " x ", paste0(housing.grid[X, byvars]))
                                                tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use data set for pooled weeks
                                                                     what = paste0(housing.grid[X, myvars]),
                                                                       ifelse(paste0(housing.grid[1, myvars]) == "eviction", rentcur == 0, phase == "3.2") &
                                                                       ifelse(paste0(housing.grid[1, myvars]) == "foreclosure", mortcur == 0, phase == "3.2") &
                                                                       ifelse(paste0(housing.grid[1, myvars]) == "leave2mo", current == 0, phase == "3.2"),
                                                                     by = paste0(housing.grid[X, byvars]),
                                                                     time_var = "week",
                                                                     metrics = c("mean", "rse", "denominator", "numerator"),
                                                                     proportion = T,
                                                                     ci = 0.90)
                                                tempDT[, category := paste0(housing.grid[X, byvars])]
                                                setnames(tempDT, paste0(housing.grid[X, byvars]), "group")
                                                tempDT[, geo := "WA"]
                                                tempDT <- tempDT[!is.na(group)]
                                              }), use.names = T )    

    # Insured: by week ----
        insured_weeks_wa <- calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
                                what = insured_vars,
                                by = c("week", "phase"),
                                metrics = c("mean", "rse", "denominator", "numerator"),
                                proportion = T,
                                ci = 0.90)
        
        insured_weeks_msa <- calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
                                 what = insured_vars,
                                 by = c("week", "phase"),
                                 metrics = c("mean", "rse", "denominator", "numerator"),
                                 proportion = T,
                                 ci = 0.90)
        
    # Insured: pooled demographics ----
        # Insured doesn't apply to 65+ so need calc age bins separately
        insured_msa_combo_age4 <- rads::calc(ph.data = pooledN_svy_msa, # use dataset for pooled weeks 
                                             what = c("insured", "uninsured"), 
                                             age4 != "65+" ,
                                             by = "age4", 
                                             time_var = "week", 
                                             metrics = c("mean", "rse", "denominator", "numerator"), 
                                             proportion = T, 
                                             ci = 0.90)[, category := "age4"][, geo := "MSA"]
        setnames(insured_msa_combo_age4, "age4", "group")
        
        insured_wa_combo_age4 <- rads::calc(ph.data = pooledN_svy_wa, # use dataset for pooled weeks 
                                             what = c("insured", "uninsured"), 
                                             age4 != "65+",
                                             by = "age4", 
                                             time_var = "week", 
                                             metrics = c("mean", "rse", "denominator", "numerator"), 
                                             proportion = T, 
                                             ci = 0.90)[, category := "age4"][, geo := "WA"]
        setnames(insured_wa_combo_age4, "age4", "group")
        
        # Cycle through all other insurance x byvar combinations
        insured.grid <- setDT(expand.grid(myvars = insured_vars, byvars = mybyvars))
        insured.grid <- insured.grid[!(myvars %in% c("insured", "uninsured") & byvars=="age4")]

        insured_msa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(insured.grid))),
                                              FUN = function(X){
                                                # message("Insured MSA combo #", X, ": ", paste0(insured.grid[X, myvars]), " x ", paste0(insured.grid[X, byvars]))
                                                tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use data set for pooled weeks
                                                                     what = paste0(insured.grid[X, myvars]),
                                                                     by = paste0(insured.grid[X, byvars]),
                                                                     time_var = "week",
                                                                     metrics = c("mean", "rse", "denominator", "numerator"),
                                                                     proportion = T,
                                                                     ci = 0.90)
                                                tempDT[, category := paste0(insured.grid[X, byvars])]
                                                setnames(tempDT, paste0(insured.grid[X, byvars]), "group")
                                                tempDT[, geo := "MSA"]
                                                tempDT <- tempDT[!is.na(group)]
                                              }), use.names = T )   
        
        insured_wa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(insured.grid))),
                                              FUN = function(X){
                                                # message("Insured WA combo #", X, ": ", paste0(insured.grid[X, myvars]), " x ", paste0(insured.grid[X, byvars]))
                                                tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use data set for pooled weeks
                                                                     what = paste0(insured.grid[X, myvars]),
                                                                     by = paste0(insured.grid[X, byvars]),
                                                                     time_var = "week",
                                                                     metrics = c("mean", "rse", "denominator", "numerator"),
                                                                     proportion = T,
                                                                     ci = 0.90)
                                                tempDT[, category := paste0(insured.grid[X, byvars])]
                                                setnames(tempDT, paste0(insured.grid[X, byvars]), "group")
                                                tempDT[, geo := "WA"]
                                                tempDT <- tempDT[!is.na(group)]
                                              }), use.names = T )   
        
        # combine the two MSA tables
        insured_msa_combo <- rbind(insured_msa_combo, insured_msa_combo_age4)
        
        # combine the two WA tables
        insured_wa_combo <- rbind(insured_wa_combo, insured_wa_combo_age4)

    # Vaccine: by week ----
        vaccine_weeks_wa <- calc(ph.data = svy_wa, # use data set for individual weeks (NOT POOLED weights)
                                 what = vaccine_vars,
                                 week >= 34,
                                 by = c("week", "phase"),
                                 metrics = c("mean", "rse", "denominator", "numerator"),
                                 proportion = T,
                                 ci = 0.90)
        
        vaccine_weeks_msa <- calc(ph.data = svy_msa, # use data set for individual weeks (NOT POOLED weights)
                                  what = vaccine_vars,
                                  week >= 34,
                                  by = c("week", "phase"),
                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                  proportion = T,
                                  ci = 0.90)
    
    # Vaccine: pooled demographics ----
        vac.grid <- setDT(expand.grid(myvars = vaccine_vars, byvars = mybyvars))
        vaccine_msa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(vac.grid))),
                                              FUN = function(X){
                                                # message("vaccine MSA combo #", X, ": ", paste0(vac.grid[X, myvars]), " x ", paste0(vac.grid[X, byvars]))
                                                tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use data set for pooled weeks
                                                                     what = paste0(vac.grid[X, myvars]),
                                                                     by = paste0(vac.grid[X, byvars]),
                                                                     time_var = "week",
                                                                     metrics = c("mean", "rse", "denominator", "numerator"),
                                                                     proportion = T,
                                                                     ci = 0.90)
                                                tempDT[, category := paste0(vac.grid[X, byvars])]
                                                setnames(tempDT, paste0(vac.grid[X, byvars]), "group")
                                                tempDT[, geo := "MSA"]
                                                tempDT <- tempDT[!is.na(group)]
                                              }), use.names = T )   
        
        vaccine_wa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(vac.grid))),
                                             FUN = function(X){
                                               # message("vaccine WA combo #", X, ": ", paste0(vac.grid[X, myvars]), " x ", paste0(vac.grid[X, byvars]))
                                               tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use data set for pooled weeks
                                                                    what = paste0(vac.grid[X, myvars]),
                                                                    by = paste0(vac.grid[X, byvars]),
                                                                    time_var = "week",
                                                                    metrics = c("mean", "rse", "denominator", "numerator"),
                                                                    proportion = T,
                                                                    ci = 0.90)
                                               tempDT[, category := paste0(vac.grid[X, byvars])]
                                               setnames(tempDT, paste0(vac.grid[X, byvars]), "group")
                                               tempDT[, geo := "WA"]
                                               tempDT <- tempDT[!is.na(group)]
                                             }), use.names = T )   

## Clean up functions ----
    # Generic clean up function ----
        clean_up <- function(prefix = NULL){
          # Estimates by week ----
              wkcombo <- rbind(get(paste0(prefix, '_weeks_wa'))[, geo := "WA"], 
                               get(paste0(prefix, '_weeks_msa'))[, geo := "MSA"])
              wkcombo[, phase := NULL]
              wkcombo[, category := "week"]
              wkcombo[, group := week]
              setorder(wkcombo, -geo, variable, group)

          # Estimates by demographics ----
              catcombo <- rbind(get(paste0(prefix, '_wa_combo')), 
                                get(paste0(prefix, '_msa_combo')))
              
              if(prefix == "food"){catcombo[, hh := "All"]}
              
              catcombo[, week := paste0("34-", max(na.omit(suppressWarnings(as.integer(wkcombo$week)))))]
              catcombo[category == "phase", group := "Total"]
              catcombo[category == "phase", category := "all"]    
              
          # Combine weekly and stratified estimates ----
              combo <- rbind(wkcombo, catcombo)  
              
              vars2round <- c("mean", "mean_se", "mean_lower", "mean_upper")
              
              continuous.vars <- unique(vartable[type=="continuous"]$variable)
              
              if(!prefix %in% c("food")){
                combo[!variable %in% continuous.vars, (vars2round) := lapply(.SD, function(x){rads::round2(100*x,1)}), .SDcols = vars2round]            
                combo[ variable %in% continuous.vars, (vars2round) := lapply(.SD, function(x){rads::round2(x,1)}), .SDcols = vars2round]            
                combo <- combo[, .(week, source = "Pulse", geo, variable, level, category, group, percent = mean, 
                                     se = mean_se, lower_ci = mean_lower, upper_ci = mean_upper, rse = rads::round2(rse, 1), numerator, denominator)]
                  
                  } else {
                    combo[!variable %in% continuous.vars, (vars2round) := lapply(.SD, function(x){rads::round2(100*x,1)}), .SDcols = vars2round]            
                    combo[ variable %in% continuous.vars, (vars2round) := lapply(.SD, function(x){rads::round2(x,1)}), .SDcols = vars2round]  
                    combo <- combo[, .(week, source = "Pulse", geo, variable, level, category, group, percent = mean, 
                                     lower_ci = mean_lower, upper_ci = mean_upper, sample_n = NA_integer_, se = mean_se, rse = rads::round2(rse, 1), numerator, denominator, hh)]}

              setorder(combo, -geo, variable, category, group)
              
          # Format identical to Lin Song's output ----
              combo <- combo[!(variable=="freefood" & level == "No")]
              combo <- combo[is.na(level) | !level %in% c("< 1/2 prior 7 days", "No", "Not insufficient")]
              combo <- combo[variable %in% get(paste0(prefix, '_vars'))]

              combo[, variable := gsub("foodsufrsn", "foodwhynot", variable)]
              combo[, variable := gsub("wherefree", "where", variable)]
              combo[category %in% c("anywork", "wrkloss", "expctloss"), group := paste0(category, "_", tolower(group))]
              combo[category == "age4", category := "age"]
              combo[category == "all", category := "total"]
              combo[category == "edu", category := "educ"]
              combo[category == "ethn", category := "race"]
              combo[category == "gender", category := "sex"]
              combo[category == "ms", category := "marital"]
              combo[group=="Other/Multiracial", group := "Other or multiracial"]
              combo[group=="Total", group := "demo_all"]
              if(!prefix %in% c("food")){
                combo[group == "Bachelor degree", group := "Bachelor's degree"]
                combo[group == "Widowed/Divorced/Separated", group := "Widowed/divorced/separated"]
                combo[, group := gsub(" alone$", "", group)]
              } 
              if(!prefix %in% c("food", "education", "housing", "insured")){
                combo[, level := NULL]
              } 
              
              
          # Add label for phase ----
              combo[, phase := "3.2"]
              
          # Add suppression and caution flags ----
              combo[, suppress := 0][denominator < 50, suppress := 1]
              combo[, caution := 0][rse >= 30, caution := 1]
              
          # Append to previous phase data if it exists ----    
              if(prefix == "education"){previousdt <- setDT(openxlsx::read.xlsx(paste0(outputdir, "education/pulse_results.xlsx"), sheet = 'pulse'))}
              if(prefix == "food"){previousdt <- setDT(openxlsx::read.xlsx(paste0(outputdir, "food_security/pulse_results.xlsx"), sheet = 'pulse'))}
              if(prefix == "health"){previousdt <- setDT(openxlsx::read.xlsx(paste0(outputdir, "behavioral_health/pulse_results.xlsx"), sheet = 'pulse'))}
              if(prefix == "housing"){previousdt <- setDT(openxlsx::read.xlsx(paste0(outputdir, "housing/pulse_results.xlsx"), sheet = 'pulse'))}
              if(prefix == "insured"){previousdt <- setDT(openxlsx::read.xlsx(paste0(outputdir, "health_insurance/pulse_results.xlsx"), sheet = 'pulse'))}
              if(prefix == "vaccine"){previousdt <- setDT(openxlsx::read.xlsx(paste0(outputdir, "vaccination/pulse_results.xlsx"), sheet = 'pulse'))}
              
              previousdt <- previousdt[phase != "3.2"] # drop data from phase 3.2 that will be replaced
              
              combo <- rbind(combo, previousdt, fill = T)
              
          # Ensure there are no negative lower limits ----
              combo[lower_ci < 0, lower_ci := 0]

          # Return the object ----
              return(combo)
        }
        
    # Generic dictionary function ----
        create_dict <- function(mydt = NULL){
          mydt_name <- deparse(substitute(mydt))
          dict <- copy(mydt)
          
          # Identify column type ----
              temp.vartype <- data.table(varname = names(sapply(dict, class)), vartype = sapply(dict, class))
              temp.binary <- data.table(varname = names(sapply(dict,function(x) { all(na.omit(x) %in% 0:1) })), binary = sapply(dict,function(x) { all(na.omit(x) %in% 0:1) }))
              dict <- merge(temp.vartype, temp.binary, by = "varname")
              dict[vartype %in% c("numeric", "integer") & binary == TRUE, vartype := "binary"]
              dict[, binary := NULL]
              dict[, varname := factor(varname, levels = names(mydt))]
              setorder(dict, varname)
              
          # Ascribe variable names ----
              dict[varname == 'week', desc := 'Week(s) of estimate']
              dict[varname == 'source', desc := 'Original data source']
              dict[varname == 'geo', desc := 'Geography']
              dict[varname == 'variable', desc := 'Indicators of interest']
              dict[varname == 'level', desc := 'Factor levels for variables']
              dict[varname == 'category', desc := 'Stratification variable']
              dict[varname == 'group', desc := 'Strata']
              dict[varname == 'percent', desc := 'Estimate (survey weighted)']
              dict[varname == 'se', desc := 'Standard error']
              dict[varname == 'lower_ci', desc := 'Lower 90% limit of estimate']
              dict[varname == 'upper_ci', desc := 'Upper 90% limit of estimate']
              dict[varname == 'sample_n', desc := 'Sample size']
              dict[varname == 'hh', desc := 'Household type']
              dict[varname == 'phase', desc := 'HH Pulse Survey Phase']
              
          # Identify value_coding for key variables ----
          mydtcopy <- copy(mydt)
          for(var in names(mydt)[3:ifelse(mydt_name %in% c("food"), 7, 6)]){
            mydtcopy[, paste0(var) := factor(get(var))]
            dict[varname==var, values := paste(levels(mydtcopy[[var]]), collapse = ", ")]
          }
              
         # return object ----
            return(dict)
        }
        
    # Generic Excel workbook creator ----
        create_excel <- function(prefix = NULL, varcall = NULL){
          wb <- createWorkbook() # create Excel shell
          addWorksheet(wb, "pulse")
          writeDataTable(wb, sheet = "pulse", get(paste0(prefix))[variable %in% vartable[viz %in% varcall]$variable], colNames = TRUE, rowNames = FALSE)
          addWorksheet(wb, "variable")
          writeDataTable(wb, sheet = "variable", vartable[viz %in% varcall], colNames = TRUE, rowNames = FALSE)              
          addWorksheet(wb, "week")
          writeDataTable(wb, sheet = "week", week_dates, colNames = TRUE, rowNames = FALSE)
          addWorksheet(wb, "dictionary")
          writeDataTable(wb, sheet = "dictionary", get(paste0(prefix, "_dict")), colNames = TRUE, rowNames = FALSE)
          return(wb)
        }
        
## Clean up & save to Excel ----
    # education data ----
        education_vars <- c(education_vars, education_vars_noncat) # need to create a complete list of all education variables
        education <- clean_up("education")
        education_dict <- create_dict(education)
        education_wb <- create_excel(prefix = "education", varcall = c("education")) 
        
        saveWorkbook(education_wb, file = paste0(outputdir, "education/pulse_results.xlsx"), overwrite = TRUE) # h
        
        removeWorksheet(education_wb, sheet = "pulse")
        addWorksheet(education_wb, sheet = "pulse")
        writeDataTable(education_wb, sheet = "pulse", education[phase=="3.2"], colNames = TRUE, rowNames = FALSE)
        saveWorkbook(education_wb, file = paste0(outputdir, "education/pulse_phase3_2_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE) # has phase 3.2 results only
        

    # food security data ----
        food <- clean_up("food")
        food_dict <- create_dict(food)
        food_wb <- create_excel(prefix = "food", varcall = c("food_security")) 
        
        saveWorkbook(food_wb, file = paste0(outputdir, "food_security/pulse_results.xlsx"), overwrite = TRUE) 
        
        removeWorksheet(food_wb, sheet = "pulse")
        addWorksheet(food_wb, sheet = "pulse")
        writeDataTable(food_wb, sheet = "pulse", food[phase=="3.2"], colNames = TRUE, rowNames = FALSE)
        saveWorkbook(food_wb, file = paste0(outputdir, "food_security/pulse_phase3_2_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE) # has phase 3.2 results only
        
    # health / behavioral health data ----
        health <- clean_up("health")
        health_dict <- create_dict(health)
        health_wb <- create_excel(prefix = "health", varcall = c("behavioral_health")) 
        
        saveWorkbook(health_wb, file = paste0(outputdir, "behavioral_health/pulse_results.xlsx"), overwrite = TRUE) 
        
        removeWorksheet(health_wb, sheet = "pulse")
        addWorksheet(health_wb, sheet = "pulse")
        writeDataTable(health_wb, sheet = "pulse", health[phase=="3.2"], colNames = TRUE, rowNames = FALSE)
        saveWorkbook(health_wb, file = paste0(outputdir, "behavioral_health/pulse_phase3_2_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE) # has phase 3.2 results only
        
    # housing ----
        housing <- clean_up("housing")
        housing_dict <- create_dict(housing)
        housing_wb <- create_excel(prefix = "housing", varcall = c("housing")) 
        
        saveWorkbook(housing_wb, file = paste0(outputdir, "housing/pulse_results.xlsx"), overwrite = TRUE) 
        
        removeWorksheet(housing_wb, sheet = "pulse")
        addWorksheet(housing_wb, sheet = "pulse")
        writeDataTable(housing_wb, sheet = "pulse", housing[phase=="3.2"], colNames = TRUE, rowNames = FALSE)
        saveWorkbook(housing_wb, file = paste0(outputdir, "housing/pulse_phase3_2_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE) # has phase 3.2 results only      
        
        
    # insured data ----
        insured <- clean_up("insured")
        insured_dict <- create_dict(insured)
        insured_wb <- create_excel(prefix = "insured", varcall = c("insurance")) 
        
        saveWorkbook(insured_wb, file = paste0(outputdir, "health_insurance/pulse_results.xlsx"), overwrite = TRUE)
        
        removeWorksheet(insured_wb, sheet = "pulse")
        addWorksheet(insured_wb, sheet = "pulse")
        writeDataTable(insured_wb, sheet = "pulse", insured[phase=="3.2"], colNames = TRUE, rowNames = FALSE)
        saveWorkbook(insured_wb, file = paste0(outputdir, "health_insurance/pulse_phase3_2_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE) # has phase 3.2 results only

    # vaccine data ----
        vaccine <- clean_up("vaccine")
        vaccine_dict <- create_dict(vaccine)
        vaccine_wb <- create_excel(prefix = "vaccine", varcall = c("vaccine")) 
        
        saveWorkbook(vaccine_wb, file = paste0(outputdir, "vaccination/pulse_results.xlsx"), overwrite = TRUE)
        
        removeWorksheet(vaccine_wb, sheet = "pulse")
        addWorksheet(vaccine_wb, sheet = "pulse")
        writeDataTable(vaccine_wb, sheet = "pulse", vaccine[phase=="3.2"], colNames = TRUE, rowNames = FALSE)
        saveWorkbook(vaccine_wb, file = paste0(outputdir, "vaccination/pulse_phase3_2_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE) # has phase 3.2 results only
        
        
## The end ----
        rm(list=ls()) # clear memory to ensure appropriate data is loaded / prepped if run all phases sequentially
