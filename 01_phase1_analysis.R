## Header ----
    # Author: Danny Colombara
    # Date: May 26, 2022
    # R version: 4.1.2
    # Purpose: analyse PULSE data
    #
    # Notes: All analyses for Pulse Survey Phase 1
    #

## set up ----
    pacman::p_load(data.table, openxlsx, srvyr, rads)

    # load prepped data if necessary ----
    if(!exists("svy_msa") | !exists("svy_wa")){
      eval(parse(text = httr::content(httr::GET(
        url = "https://raw.githubusercontent.com/PHSKC-APDE/pulse_survey/main/00_phase1_prep_survey.R",
        httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")), "text")))
    }

    # load dates for each week ----
    week_dates <- data.table::fread(httr::content(httr::GET(
      url = "https://raw.githubusercontent.com/PHSKC-APDE/pulse_survey/main/pulse_weeks_dates.csv",
      httr::authenticate(Sys.getenv("GITHUB_PAT"), "")), type = "text", encoding = "UTF-8"))
      week_dates[, start_date := lubridate::mdy(gsub("/", "-", start_date))]
      week_dates[, end_date := lubridate::mdy(gsub("/", "-", end_date))]
    
    # load variable description for vars actually used ----
    vartable <- data.table::fread(httr::content(httr::GET(
      url = "https://raw.githubusercontent.com/PHSKC-APDE/pulse_survey/main/pulse_varlist.csv",
      httr::authenticate(Sys.getenv("GITHUB_PAT"), "")), type = "text", encoding = "UTF-8"))

## Identify columns for calculations ----
    education_vars <- c("enroll_school", "enroll_home", 
                        "teach_cancel", "teach_online", "teach_paper", "teach_other", "teach_nochange", # teaching modality for those taught in school (enroll_school==1)
                        "compavail", "compavail.binary", "intrntavail", "intrntavail.binary", "nocomp", # only when enroll_school==1
                        "comp_school", "comp_family", "comp_other", # only when compavail != "Never"
                        "intrnt_school", "intrnt_family", "intrnt_other") # only when intrntavail != "Never
      
    education_vars_noncat <- c("tschlhrs", "tstdy_hrs") # only when enroll_school==1 (these are continuous vars)
      
    food_vars <- c("curfoodsuf", "curfoodsuf2", "freefood", 
                   "foodsufrsn1", "foodsufrsn12", "foodsufrsn2","foodsufrsn22", "foodsufrsn3", "foodsufrsn32", 
                   "foodsufrsn4", "foodsufrsn42", "foodsufrsn5", "foodsufrsn52", 
                   "wherefree1", "wherefree2", "wherefree3", "wherefree4", "wherefree5", "wherefree6", "wherefree7")    
  
    health_vars <- c("down", "anxious", "worry", "interest")

    insured_vars <- c("insurance", "uninsured", "insured", "insured_employer", "insured_exchange", "insured_mcare", "insured_military", "delay", "notget")

    mybyvars <- c("phase", "age4", "anywork", "edu", "ethn", "expctloss", "gender", "income", "kindwork", "ms", "wrkloss")
    
## Use rads to perform calculations ----
    # note, even though the education vars are relevant given specific conditionalities (e.g., enroll_school==1), it is not 
    # necessary to specify this in rads::calc() in the '...' ('where') statement. This is because the NA values have been properly
    # set in the data prep process. In other words, we are not subsetting to a population that is not already accounted for in the survey design.
    
    # education: by week ----
        education_weeks_wa <- rbind(
          calc(ph.data = svy_wa, # use dataset for individual weeks (NOT POOLED weights)
               what = education_vars, # categorical variables
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = T,
               ci = 0.90), 
          calc(ph.data = svy_wa, # use dataset for individual weeks (NOT POOLED weights)
               what = education_vars_noncat, # continous variables
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = F,
               ci = 0.90))
        
        education_weeks_msa <- rbind(
          calc(ph.data = svy_msa, # use dataset for individual weeks (NOT POOLED weights)
               what = education_vars, # categorical vars
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = T,
               ci = 0.90), 
          calc(ph.data = svy_msa, # use dataset for individual weeks (NOT POOLED weights)
               what = education_vars_noncat, # continous vars
               by = c("week", "phase"),
               metrics = c("mean", "rse", "denominator", "numerator"),
               proportion = F,
               ci = 0.90))
    
    
    # education: weeks 1-12: demographics ----
        education_msa_combo <- rbind(
          # Categorical education variables
          rbindlist(lapply(X = as.list(mybyvars),
                           FUN = function(X){
                             tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use dataset for pooled weeks
                                                  what = education_vars,
                                                  week >= 1 & !is.na(get(X)),
                                                  by = X,
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = T,
                                                  ci = 0.90)
                             tempDT[, category := X]
                             setnames(tempDT, X, "group")
                             tempDT[, geo := "MSA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T), 
          # Continuous education variables
          rbindlist(lapply(X = as.list(mybyvars),
                           FUN = function(X){
                             tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use dataset for pooled weeks
                                                  what = education_vars_noncat,
                                                  week >= 1 & !is.na(get(X)),
                                                  by = X,
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = F,
                                                  ci = 0.90)
                             tempDT[, category := X]
                             setnames(tempDT, X, "group")
                             tempDT[, geo := "MSA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T))
        education_msa_combo[, week := "1-12"] # do this manually b/c RADS is too smart and only records the weeks actually available, not the min and max
        
        education_wa_combo <- rbind(
          # Categorical education variables
          rbindlist(lapply(X = as.list(mybyvars),
                           FUN = function(X){
                             tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use dataset for pooled weeks
                                                  what = education_vars,
                                                  week >= 1 & !is.na(get(X)),
                                                  by = X,
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = T,
                                                  ci = 0.90)
                             tempDT[, category := X]
                             setnames(tempDT, X, "group")
                             tempDT[, geo := "WA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T), 
          # Continuous education variables
          rbindlist(lapply(X = as.list(mybyvars),
                           FUN = function(X){
                             tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use dataset for pooled weeks
                                                  what = education_vars_noncat,
                                                  week >= 1 & !is.na(get(X)),
                                                  by = X,
                                                  time_var = "week",
                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                  proportion = F,
                                                  ci = 0.90)
                             tempDT[, category := X]
                             setnames(tempDT, X, "group")
                             tempDT[, geo := "WA"]
                             tempDT <- tempDT[!is.na(group)]
                           }), use.names = T))
        education_wa_combo[, week := "1-12"] # do this manually b/c RADS is too smart and only records the weeks actually available, not the min and max
        
    # Food security: by week ----
        food_weeks_wa <- rbind(
                          calc(ph.data = svy_wa, # use dataset for individual weeks (NOT POOLED weights)
                                what = c("curfoodsuf", grep("foodsufrsn", names(dt), value = T), "freefood"), # insufficient food, couldn't afford, afraid, stores didn't have what was wanted, free food
                                by = c("week", "phase"),
                                metrics = c("mean", "rse", "denominator", "numerator"),
                                proportion = T,
                                ci = 0.90)[, hh:= "All"], 
                          calc(ph.data = svy_wa, # use dataset for individual weeks (NOT POOLED weights)
                               what = c("curfoodsuf", grep("foodsufrsn", names(dt), value = T)),
                               children == "Has children", # also calc for HH with children
                               by = c("week", "phase"),
                               metrics = c("mean", "rse", "denominator", "numerator"),
                               proportion = T,
                               ci = 0.90)[, variable := paste0(variable, "2")][, hh := "With children"])
        
        food_weeks_msa <- rbind(
                            calc(ph.data = svy_msa, # use dataset for individual weeks (NOT POOLED weights)
                                 what = c("curfoodsuf", grep("foodsufrsn|where", names(dt), value = T), "freefood"), # insufficient food, couldn't afford, afraid, stores didn't have what was wanted, free food
                                 by = c("week", "phase"),
                                 metrics = c("mean", "rse", "denominator", "numerator"),
                                 proportion = T,
                                 ci = 0.90)[, hh := "All"], 
                            calc(ph.data = svy_msa, # use dataset for individual weeks (NOT POOLED weights)
                                 what = c("curfoodsuf", grep("foodsufrsn", names(dt), value = T)),
                                 children == "Has children", # also calc for HH with children
                                 by = c("week", "phase"),
                                 metrics = c("mean", "rse", "denominator", "numerator"),
                                 proportion = T,
                                 ci = 0.90)[, variable := paste0(variable, "2")][, hh := "With children"])
        
    # Food security: weeks 1-4, 5-8, 9-12: demographics ----
        # Make grid of al combinations needed
        mycombos <- setDT(expand.grid(myvars = c(setdiff(mybyvars, "gender"), "children"), weekstart = c(1) , weekend = c(12)))
        mycombos <- mycombos[(weekstart==1 & weekend==12)]
        
        food_msa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(mycombos), 1)),
                                             FUN = function(X){
                                               tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use dataset for pooled weeks
                                                                    what = c("curfoodsuf", grep("where", names(dt), value = T), "freefood"),
                                                                    week %in% paste0(mycombos[X, ]$weekstart):paste0(mycombos[X, ]$weekend),
                                                                    by = paste0(mycombos[X, ]$myvars),
                                                                    time_var = "week",
                                                                    metrics = c("mean", "rse", "denominator", "numerator"),
                                                                    proportion = T,
                                                                    ci = 0.90)
                                               tempDT[, category := mycombos[X, ]$myvars]
                                               setnames(tempDT, paste0(mycombos[X, ]$myvars), "group")
                                               tempDT[, geo := "MSA"]
                                               tempDT <- tempDT[!is.na(group)]
                                               tempDT <- tempDT[, week := paste0(mycombos[X, ]$weekstart, "-", mycombos[X, ]$weekend)]
                                             }), use.names = T)
        
        food_wa_combo <- rbindlist(lapply(X = as.list(seq(1, nrow(mycombos), 1)),
                                           FUN = function(X){
                                             tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use dataset for pooled weeks
                                                                  what = c("curfoodsuf", grep("where", names(dt), value = T), "freefood"),
                                                                  week %in% paste0(mycombos[X, ]$weekstart):paste0(mycombos[X, ]$weekend),
                                                                  by = paste0(mycombos[X, ]$myvars),
                                                                  time_var = "week",
                                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                                  proportion = T,
                                                                  ci = 0.90)
                                             tempDT[, category := mycombos[X, ]$myvars]
                                             setnames(tempDT, paste0(mycombos[X, ]$myvars), "group")
                                             tempDT[, geo := "WA"]
                                             tempDT <- tempDT[!is.na(group)]
                                             tempDT <- tempDT[, week := paste0(mycombos[X, ]$weekstart, "-", mycombos[X, ]$weekend)]
                                           }), use.names = T)
        
    # Health: by week ----
        health_weeks_wa <- calc(ph.data = svy_wa, # use dataset for individual weeks (NOT POOLED weights)
                           what = health_vars,
                           by = c("week", "phase"),
                           metrics = c("mean", "rse", "denominator", "numerator"),
                           proportion = T,
                           ci = 0.90)

        health_weeks_msa <- calc(ph.data = svy_msa, # use dataset for individual weeks (NOT POOLED weights)
                            what = health_vars,
                            by = c("week", "phase"),
                            metrics = c("mean", "rse", "denominator", "numerator"),
                            proportion = T,
                            ci = 0.90)


    # Health: weeks 1-12: demographics ----
        health_msa_combo <- rbindlist(lapply(X = as.list(mybyvars),
                                           FUN = function(X){
                                             tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use dataset for pooled weeks
                                                                  what = health_vars,
                                                                  week >= 1,
                                                                  by = X,
                                                                  time_var = "week",
                                                                  metrics = c("mean", "rse", "denominator", "numerator"),
                                                                  proportion = T,
                                                                  ci = 0.90)
                                             tempDT[, category := X]
                                             setnames(tempDT, X, "group")
                                             tempDT[, geo := "MSA"]
                                             tempDT <- tempDT[!is.na(group)]
                                           }), use.names = T )
        health_msa_combo[, week := "1-12"] # do this manually b/c RADS is too smart and only records the weeks actually available, not the min and max

        health_wa_combo <- rbindlist(lapply(X = as.list(mybyvars),
                                            FUN = function(X){
                                              tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use dataset for pooled weeks
                                                                   what = health_vars,
                                                                   week >= 1,
                                                                   by = X,
                                                                   time_var = "week",
                                                                   metrics = c("mean", "rse", "denominator", "numerator"),
                                                                   proportion = T,
                                                                   ci = 0.90)
                                              tempDT[, category := X]
                                              setnames(tempDT, X, "group")
                                              tempDT[, geo := "WA"]
                                              tempDT <- tempDT[!is.na(group)]
                                            }), use.names = T )
        health_wa_combo[, week := "1-12"] # do this manually b/c RADS is too smart and only records the weeks actually available, not the min and max
        
        
    # Insured: by week ----
        insured_weeks_wa <- calc(ph.data = svy_wa, # use dataset for individual weeks (NOT POOLED weights)
                                what = insured_vars,
                                by = c("week", "phase"),
                                metrics = c("mean", "rse", "denominator", "numerator"),
                                proportion = T,
                                ci = 0.90)
        
        insured_weeks_msa <- calc(ph.data = svy_msa, # use dataset for individual weeks (NOT POOLED weights)
                                 what = insured_vars,
                                 by = c("week", "phase"),
                                 metrics = c("mean", "rse", "denominator", "numerator"),
                                 proportion = T,
                                 ci = 0.90)
        
        
    # Insured: weeks 1-12: demographics ----
        # Insured doesn't apply to 65+ so need calc age bins separately
        insured_msa_combo_age4 <- rads::calc(ph.data = pooledN_svy_msa, # use dataset for pooled weeks 
                                             what = c("insured", "uninsured"), 
                                             week >= 1 & age4 != "65+",
                                             by = c("age4"), 
                                             time_var = "week", 
                                             metrics = c("mean", "rse", "denominator", "numerator"), 
                                             proportion = T, 
                                             ci = 0.90)[, category := "age4"][, geo := "MSA"]
        setnames(insured_msa_combo_age4, "age4", "group")
        
        insured_wa_combo_age4 <- rads::calc(ph.data = pooledN_svy_wa, # use dataset for pooled weeks 
                                             what = c("insured", "uninsured"), 
                                             week >= 1 & age4 != "65+",
                                             by = "age4", 
                                             time_var = "week", 
                                             metrics = c("mean", "rse", "denominator", "numerator"), 
                                             proportion = T, 
                                             ci = 0.90)[, category := "age4"][, geo := "WA"]
        setnames(insured_wa_combo_age4, "age4", "group")
        
        # Cycle through all other insurance x byvar combinations
        grid.insured <- setDT(expand.grid(indicator = insured_vars, byvar = mybyvars))
        grid.insured <- setDF(grid.insured[!(indicator %in% c("insured", "uninsured") & byvar=="age4")])

        insured_msa_combo <- rbindlist(lapply(X = as.list( seq(1, nrow(grid.insured)) ), 
                                              FUN = function(X){print(paste("Processing row", X, "of", nrow(grid.insured), "in 'grid.insured'(", grid.insured[X, 1], ":", grid.insured[X, 2], ")"))
                                                tempDT <- rads::calc(ph.data = pooledN_svy_msa, # use dataset for pooled weeks 
                                                                     what = paste0(grid.insured[X, 1]), 
                                                                     week >= 1 & !is.na(get(paste0(grid.insured[X, 2]))), # skip when byvar is NA
                                                                     by = paste0(grid.insured[X, 2]), 
                                                                     time_var = "week", 
                                                                     metrics = c("mean", "rse", "denominator", "numerator"), 
                                                                     proportion = T, 
                                                                     ci = 0.90)
                                                tempDT[, category := paste0(grid.insured[X, 2])]
                                                setnames(tempDT, paste0(grid.insured[X, 2]), "group")
                                                tempDT[, geo := "MSA"]
                                                tempDT <- tempDT[!is.na(group)]
                                              }), use.names = T )
        
        insured_wa_combo <- rbindlist(lapply(X = as.list( seq(1, nrow(grid.insured)) ), 
                                              FUN = function(X){print(paste("Processing row", X, "of", nrow(grid.insured), "in 'grid.insured'(", grid.insured[X, 1], ":", grid.insured[X, 2], ")"))
                                                tempDT <- rads::calc(ph.data = pooledN_svy_wa, # use dataset for pooled weeks 
                                                                     what = paste0(grid.insured[X, 1]), 
                                                                     week >= 1 & !is.na(get(paste0(grid.insured[X, 2]))), # skip when byvar is NA
                                                                     by = paste0(grid.insured[X, 2]), 
                                                                     time_var = "week", 
                                                                     metrics = c("mean", "rse", "denominator", "numerator"), 
                                                                     proportion = T, 
                                                                     ci = 0.90)
                                                tempDT[, category := paste0(grid.insured[X, 2])]
                                                setnames(tempDT, paste0(grid.insured[X, 2]), "group")
                                                tempDT[, geo := "WA"]
                                                tempDT <- tempDT[!is.na(group)]
                                              }), use.names = T )
        
        # combine the two MSA tables
        insured_msa_combo <- rbind(insured_msa_combo, insured_msa_combo_age4)
        insured_msa_combo[, week := "1-12"] # do this manually b/c RADS is too smart and only records the weeks actually available, not the min and max
        
        insured_wa_combo <- rbind(insured_wa_combo, insured_wa_combo_age4)
        insured_wa_combo[, week := "1-12"] # do this manually b/c RADS is too smart and only records the weeks actually available, not the min and max
        
        
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
              
              catcombo[category == "phase", group := "Total"]
              catcombo[category == "phase", category := "all"]    
              
          # Combine weekly and stratified estimates ----
              combo <- rbind(wkcombo, catcombo)  
              
              vars2round <- c("mean", "mean_se", "mean_lower", "mean_upper")
              
              if(!prefix %in% c("food")){
                combo[!variable %in% c("tstdy_hrs", "tschlhrs"), (vars2round) := lapply(.SD, function(x){rads::round2(100*x,1)}), .SDcols = vars2round]            
                combo[ variable %in% c("tstdy_hrs", "tschlhrs"), (vars2round) := lapply(.SD, function(x){rads::round2(x,1)}), .SDcols = vars2round]            
                combo <- combo[, .(week, source = "Pulse", geo, variable, level, category, group, percent = mean, 
                                     se = mean_se, lower_ci = mean_lower, upper_ci = mean_upper, rse = rads::round2(rse, 1), numerator, denominator)]
                  
                  } else {
                    combo[!variable %in% c("tstdy_hrs", "tschlhrs"), (vars2round) := lapply(.SD, function(x){rads::round2(100*x,1)}), .SDcols = vars2round]            
                    combo[ variable %in% c("tstdy_hrs", "tschlhrs"), (vars2round) := lapply(.SD, function(x){rads::round2(x,1)}), .SDcols = vars2round]  
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
              if(!prefix %in% c("food", "education", "insured")){
                combo[, level := NULL]
              } 
              
          # Add label for phase ----
              combo[, phase := 1]

          # Ensure there are no negative lower limits ----
              combo[lower_ci < 0, lower_ci := 0]    
              
          # Add suppression and caution flags ----
              combo[, suppress := 0][denominator < 50, suppress := 1]
              combo[, caution := 0][rse >= 30, caution := 1]
              
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
        
## Clean up & save to output directory ----
    # education data ----
        education_vars <- c(education_vars, education_vars_noncat) # need to create a complet list of all education variables
        education <- clean_up("education")
        education_dict <- create_dict(education)
        education_wb <- create_excel(prefix = "education", varcall = c("education")) 
        
        saveWorkbook(education_wb, file = paste0(outputdir, "education/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE)
        
        file.copy(from = paste0(outputdir, "education/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), 
                  to = paste0(outputdir, "education/pulse_results.xlsx"), overwrite = T)
        
    # food security data ----
        food <- clean_up("food")
        food_dict <- create_dict(food)
        food_wb <- create_excel(prefix = "food", varcall = c("food_security")) 
        
        saveWorkbook(food_wb, file = paste0(outputdir, "food_security/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE)
        
        
        file.copy(from = paste0(outputdir, "food_security/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), 
                  to = paste0(outputdir, "food_security/pulse_results.xlsx"), overwrite = T)

    # health / behavioral health data ----
        health <- clean_up("health")
        health_dict <- create_dict(health)
        health_wb <- create_excel(prefix = "health", varcall = c("behavioral_health")) 
        
        saveWorkbook(health_wb, file = paste0(outputdir, "behavioral_health/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE)
            
        file.copy(from = paste0(outputdir, "behavioral_health/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), 
                  to = paste0(outputdir, "behavioral_health/pulse_results.xlsx"), overwrite = T)
            
      
    # insured data ----
        insured <- clean_up("insured")
        insured_dict <- create_dict(insured)
        insured_wb <- create_excel(prefix = "insured", varcall = c("insurance")) 
        
        saveWorkbook(insured_wb, file = paste0(outputdir, "health_insurance/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), overwrite = TRUE)
        
        file.copy(from = paste0(outputdir, "health_insurance/pulse_phase1_results_", gsub("-", "_", Sys.Date()), ".xlsx"), 
                  to = paste0(outputdir, "health_insurance/pulse_results.xlsx"), overwrite = T)
        
## The end ----
        rm(list=ls()) # clear memory to ensure appropriate data is loaded / prepped if run all phases sequentially
