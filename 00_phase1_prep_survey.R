## Header ----
    # Author: Danny Colombara
    # Date: May 26, 2022
    # R version: 4.1.2
    # Purpose: append and prep all individual PULSE survey files for analysis
    #
    # Notes: PAGE 11 https://www2.census.gov/programs-surveys/demo/technical-documentation/hhp/Phase2_Source_and_Accuracy-Week_14.pdf
    #        Users may want to pool estimates over multiple weeks by creating averages for estimates
    #        with small sample sizes. For pooled estimates, where two or more weeks of data are
    #        combined to make one estimate for a longer time period, one would divide the unit-level
    #        weights that formed x-hat0 and xi (for each of the 80 replicate weights) for each week by the
    #        number of weeks that are combined.  Then, form 80 replicate pooled estimates, xhati_pooled
    #        and the estimate, xhat0_pooled. Then use the pooled estimates in formula (1) to calculate
    #        the pooled variance for the item of interest.
    #
    #        So, I will pool across the 12 weeks and divide the replicate weights by 12

## set up ----
    rm(list=ls())
    pacman::p_load(data.table, srvyr, rads)
    source("https://raw.githubusercontent.com/PHSKC-APDE/svy_pulse/main/00_constants_n_functions.R")
    setwd(paste0(inputdir, "phase1_unzipped/"))
    Sys.setenv(TZ='America/Los_Angeles') # set time zone
    
## load survey & replicate weights ----
    svy <- rbindlist(lapply(list.files(pattern = "^pulse2020_puf_"),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    rep <- rbindlist(lapply(list.files(pattern = "^pulse2020_repwgt_"),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)

## Prepare data ----
    # merge survey and weights and clean ----
        dt <- merge(svy, rep, by = c("SCRAM", "WEEK"), all = T)
        # rm(rep, svy)

    # clean ----
        setnames(dt, tolower(names(dt)) ) # make all colnames lowercase

        for(col in names(dt)) set(dt, i=which(dt[[col]]==-99), j=col, value=NA) # replace all -99 with NA
        for(col in names(dt)) set(dt, i=which(dt[[col]]==-88), j=col, value=NA) # replace all -88 with NA

        dt[ms %in% c(2, 3, 4), ms := 2]

        for(blah in c(grep("hlthins", names(dt), value = T), "delay", "notget") ){print(blah); dt[get(paste0(blah)) == 2, (paste0(blah)) := 0]}
        for(blah in c("down", "anxious", "worry", "interest")){print(blah); dt[get(paste0(blah)) %in% 1:2, (paste0(blah)) := 0]; dt[get(paste0(blah)) %in% 3:4, (paste0(blah)) := 1]}

        dt[prifoodsuf %in% 1:2, prifoodsuf := 0]
        dt[prifoodsuf %in% 3:4, prifoodsuf := 1]
        
    # create variables ----
        # demographics ----
            dt[, age := 2020-tbirth_year] # generate age
            
            dt[thhld_numkid == 0, children := 0]
            dt[thhld_numkid %in% 1:7, children := 1]
            
            dt[, edu := eeduc]
            dt[edu==1, edu := 2][edu==4, edu := 5] # collapse less than HS into some HS & some college into Associate's
    
            dt[, ethn := rrace][rhispanic == 2, ethn := 5] # create hispanic as race
            
        # education ----
            # enrollment
                dt[, enroll_school := enroll1][is.na(enroll1), enroll_school := 0] # hh with child enrolled in public or private school
                dt[, enroll_home := enroll2][is.na(enroll2), enroll_home := 0] # hh with child home schooled
                dt[, enroll_none := enroll3][is.na(enroll3), enroll_none := 0] # hh without child in school
                dt[(is.na(enroll1) & is.na(enroll2) & is.na(enroll3)), c("enroll_school", "enroll_home", "enroll_none") := NA] # properly set NA values

            # teaching modality
                dt[, teach_cancel := teach1][is.na(teach1), teach_cancel := 0] # only enroll1==1
                dt[, teach_online := teach2][is.na(teach2), teach_online := 0] # only enroll1==1
                dt[, teach_paper := teach3][is.na(teach3), teach_paper := 0] # only enroll1==1
                dt[, teach_other := teach4][is.na(teach4), teach_other := 0] # only enroll1==1
                dt[, teach_nochange := teach5][is.na(teach5), teach_nochange := 0] # only enroll1==1
                dt[(is.na(teach1) & is.na(teach2) & is.na(teach3) & is.na(teach4) & is.na(teach5)), 
                    grep("teach_", names(dt), value = T) := NA]

            # No computer / no internet
                dt[, compavail.binary := factor(compavail, levels = 1:5, labels = c("Available", "Available", "Not available", "Not available", "Not available"))] # only enroll1==1
                dt[, compavail := factor(compavail, levels = 1:5, labels = c("Always", "Usually", "Sometimes", "Rarely", "Never"))] # only enroll1==1
                #dt[is.na(enroll_school) | enroll_school == 0, c("compavail", "compavail.binary") := NA]
                
                dt[, intrntavail.binary := factor(intrntavail, levels = 1:5, labels = c("Available", "Available", "Not available", "Not available", "Not available"))] # only enroll1==1
                dt[, intrntavail := factor(intrntavail, levels = 1:5, labels = c("Always", "Usually", "Sometimes", "Rarely", "Never"))] # only enroll1==1
                #dt[is.na(enroll_school) | enroll_school == 0, c("intrntavail", "intrntavail.binary", "nocomp") := NA]
                
                dt[!is.na(compavail) & !is.na(intrntavail), nocomp := "Available"] 
                dt[compavail %in% c("Sometimes", "Rarely", "Never"), nocomp := "Not available"]                 
                dt[intrntavail %in% c("Sometimes", "Rarely", "Never"), nocomp := "Not available"] 
                dt[, nocomp := factor(nocomp)]
                #dt[is.na(enroll_school) | enroll_school == 0, c("nocomp") := NA]

                dt[, comp_school := comp1][is.na(comp1), comp_school := 0] # only compavail != "Never"
                dt[, comp_family := comp2][is.na(comp2), comp_family := 0] # only compavail != "Never"
                dt[, comp_other := comp3][is.na(comp3), comp_other := 0] # only compavail != "Never"
                dt[(is.na(comp1) & is.na(comp2) & is.na(comp3)), grep("comp_", names(dt), value = T) := NA]
                
                dt[, intrnt_school := intrnt1][is.na(intrnt1), intrnt_school := 0] # only intrntavail != "Never"
                dt[, intrnt_family := intrnt2][is.na(intrnt2), intrnt_family := 0] # only intrntavail != "Never"
                dt[, intrnt_other := intrnt3][is.na(intrnt3), intrnt_other := 0] # only intrntavail != "Never"
                dt[(is.na(intrnt1) & is.na(intrnt2) & is.na(intrnt3)), grep("intrnt_", names(dt), value = T) := NA]
                
                summary(dt[]$tschlhrs) # hrs contact with teacher in last 7 days (only enroll1==1)
                summary(dt[]$tstdy_hrs) # hrs studied on own in last 7 days (only enroll1==1)
                

        # food security ----
            # reasons for insufficient food
            for(i in grep("^foodsufrsn[0-9]$", names(dt), value = T)){
              dt[is.na(get(paste0(i))) & curfoodsuf %in% c(2, 3, 4), paste0(i) := 0] # set NA to zero when stated that had insufficient food
            }
            
            # collapse children not eating enought
            dt[childfood %in% 1:2, childfood := 1]
            dt[childfood %in% 3, childfood := 0]
            dt[children == 0, childfood := NA]
            
            # collapse food confidence into less than moderately confident and greater than or equal moderately confident
            dt[foodconf %in% 1:2, foodconf2 := 1]
            dt[foodconf %in% 3:4, foodconf2 := 0]
            
            # current sufficient food ... must be after foodsufrsn[0-9] b/c used in definint the latter
            dt[curfoodsuf %in% 1:2, curfoodsuf := 0]
            dt[curfoodsuf %in% 3:4, curfoodsuf := 1]
            
            # where received free food
            for(i in grep("^wherefree[0-9]$", names(dt), value = T)){
              dt[is.na(get(paste0(i))) & freefood %in% c(1), paste0(i) := 0] # set NA to zero when stated that had insufficient food
            }
            
            # total $ spent on food in last 7 days
            dt[, tspndtotal := tspndfood + tspndprpd] 
            
        # healthcare / insurance ----
          # 100% exact Census / CDC definition
            dt[, insurance := "Not reported"]
            dt[hlthins1==1 | hlthins2==1 | hlthins5 ==1, insurance := "Private"]
            dt[hlthins3==1 | hlthins4==1 | hlthins6 ==1, insurance := "Public"]
            dt[(hlthins1==1 | hlthins2==1 | hlthins5 ==1) & (hlthins3==1 | hlthins4==1 | hlthins6 ==1), insurance := "Public & Private"]
            dt[hlthins1==0 & hlthins2==0 & hlthins3 ==0 & hlthins4==0 & hlthins5==0 & hlthins6==0, insurance := "Uninsured"]
            dt[, insurance := factor(insurance)]
            
            # Simpler insurance / not insured definition based on Census ... 19-64 to match ACS working age adult bin
            dt[insurance %in% c("Private", "Public", "Public & Private"), insured := "Yes"]
            dt[insurance %in% "Uninsured", insured := "No"]
            dt[!age %in% 19:64, insured := NA]
            dt[, insured := factor(insured)]
            
            dt[insurance %in% c("Private", "Public", "Public & Private"), uninsured := "No"]
            dt[insurance %in% "Uninsured", uninsured := "Yes"]
            dt[!age %in% 19:64, uninsured := NA]     
            dt[, uninsured := factor(uninsured)]
            
            dt[, insured_employer := hlthins1]
            dt[, insured_exchange := hlthins2]
            dt[, insured_mcare := hlthins3]
            dt[, insured_military := hlthins5] # start with TRICARE
            dt[hlthins6 == 1, insured_military := 1] # add on VA
          
        # misc ----
            dt[, c("msa", "washington") := 0]
            dt[est_st == 53, washington := 1]
            dt[est_msa == 42660, msa := 1] # actually seattle-tacoma-bellevue metro area
            
            dt[, phase := 1]

    # create pooled weights for all weeks combined ----
        myweights <- grep("ght", names(dt), value = T)
        dt[, (gsub("pweight", "pooledNwt", myweights)) := lapply(.SD, function(X){X/12}), .SDcols = myweights] # divide by 12 because 12 weeks in phase1

    # create factor labels ----
        dt[, agechi := cut(age, c(17, 24, 44, 64, 74, 120), labels = c("18-24", "25-44", "45-64", "65-74", "75+"))]
        dt[, agegp := cut(age, c(17, 29, 39, 49, 59, 69, 79, 120), labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))]
        dt[, age4 := cut(age, c(17, 24, 44, 64, 120), labels = c("18-24", "25-44", "45-64", "65+"))]
        dt[, anxious := factor(anxious, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]
        dt[, childfood := factor(childfood, levels = 0:1, labels = c("Not insufficient", "Insufficient"))]
        dt[, children := factor(children, levels= 0:1, labels = c("No children", "Has children"))]
        dt[, curfoodsuf := factor(curfoodsuf, levels = 0:1, labels = c("Not insufficient", "Insufficient"))]
        dt[, down := factor(down, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]
        dt[, edu := factor(edu, levels = c(2, 3, 5, 6, 7), labels = c("<HS", "HS Grad/GED", "Some college/Associate degree","Bachelor degree","Graduate degree"))]
        dt[, ethn := factor(ethn, levels = 1:5 , labels = c("White alone", "Black alone", "Asian alone", "Other/Multiracial", "Hispanic") )]
        dt[, foodconf := factor(foodconf, levels = 1:4, labels = c("Not at all confident", "Somewhat confident", "Moderately confident", "Very confident"))]
        dt[, foodconf2 := factor(foodconf2, levels = 0:1, labels = c("< moderately confident", ">= moderately confident"))]
        dt[, gender := factor(egender, levels = 1:2, labels = c("Male", "Female"))]
        dt[, income := factor(income, levels = c(1:8), labels = c("<$25K", "<$35K", "<$50K", "<$75K", "<$100K", "<$150K", "<$200K", "$200K+") )]
        dt[, interest := factor(interest, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]
        dt[, kindwork := factor(kindwork, levels = c(1:5), labels = c("Government", "Private", "Non-profit", "Self-employed", "Family business") )]
        dt[, prifoodsuf := factor(prifoodsuf, levels = 0:1, labels = c("Not insufficient", "Insufficient"))]
        dt[, ms := factor(ms, levels = c(1, 2, 5), labels = c("Married", "Widowed/Divorced/Separated", "Never married") )]
        dt[, rrace := factor(rrace, levels = 1:4 , labels = c("White alone", "Black alone", "Asian alone", "Other or multiracial") )]
        dt[, rhispanic := factor(rhispanic, levels = 1:2, labels = c("No", "Yes"))] # YES, I confirmed that 1=No & 2=Yes.
        dt[, worry := factor(worry, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]

        noyesvars <- c("delay", "notget", "hlthins1")
        dt[, (noyesvars) := lapply(.SD, function(X){factor(X, levels = 0:1, labels = c("No", "Yes") )}), .SDcols = noyesvars]

        yesnovars <- c("wrkloss", "expctloss", "anywork", "freefood")
        dt[, (yesnovars) := lapply(.SD, function(X){factor(X, levels = 1:2, labels = c("Yes", "No") )}), .SDcols = yesnovars]

    # check that factors processed properly ----
        # for(i in c("age4", "agechi", "agegp", "anxious", "anywork", "childfood", "compavail.binary", "curfoodsuf", "delay", "down", "edu", "ethn", "expctloss", 
        #            "foodconf", "foodconf2", "gender", "hlthins1", "income", "interest", "intrntavail.binary", "kindwork", "ms", "notget", "prifoodsuf", 
        #            "rhispanic", "rrace", "uninsured", "insurance", "worry", "wrkloss")){print(i); print( table(dt[, ..i]))}

## Survey Set for WA & MSA ----
    # Washington ----
        svy_wa <- survey_set_weekly(dt[washington==1])
        pooledN_svy_wa <- survey_set_pooled(dt[washington==1])
        
    # MSA ----
        svy_msa <- survey_set_weekly(dt[msa==1])
        pooledN_svy_msa <- survey_set_pooled(dt[msa==1])
        
## The end ----
