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
    #        SO, in the end, I will pool across the 12 weeks and divide the replicate weights by 12
    #        However, until we get to 12 weeks, it will be pooled by n weeks, so replicate weights will be divided by 'n'
    #
    #
    # Notes: Phase 3 is identical to Phase 2 in every way, so the analysis for week 2 & 3 will be combined

## set up ----
    rm(list=ls())
    pacman::p_load(data.table, srvyr, rads)

## load survey & replicate weights ----
    source("https://raw.githubusercontent.com/PHSKC-APDE/pulse_survey/main/00_constants_n_functions.R")
    setwd(paste0(inputdir, "phase2_unzipped/"))
    svy_phase2 <- rbindlist(lapply(list.files(pattern = "^pulse2020_puf_"),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    rep_phase2 <- rbindlist(lapply(list.files(pattern = "^pulse2020_repwgt_"),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)

    source("https://raw.githubusercontent.com/PHSKC-APDE/pulse_survey/main/00_constants_n_functions.R")
    setwd(paste0(inputdir, "phase3_unzipped/"))
    keepers <- list.files(pattern = "_repwgt")
    svy_phase3 <- rbindlist(lapply(gsub("_repwgt", "", keepers),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    rep_phase3 <- rbindlist(lapply(keepers,FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    
    svy <- rbind(svy_phase2, svy_phase3, fill = T)
    rep <- rbind(rep_phase2, rep_phase3)
    
## Prepare data ----
    # merge survey and weights and clean ----
        dt <- merge(svy, rep, by = c("SCRAM", "WEEK"), all = T)
        # rm(rep, svy)

    # clean ----
        setnames(dt, tolower(names(dt)) ) # make all colnames lowercase
    
        for(col in grep("hlthins", names(dt), value = T)) set(dt, i=which(dt[[col]]%in% c(-88, -99)), j=col, value=10) # replace all -88 with 10 (arbitrary #) so it it not changed to NA below
        for(col in names(dt)) set(dt, i=which(dt[[col]]==-99), j=col, value=NA) # replace all -99 with NA
        for(col in names(dt)) set(dt, i=which(dt[[col]]==-88), j=col, value=NA) # replace all -88 with NA

    # create variables ----
        # demographics ----
            dt[, age := 2020-tbirth_year] # generate age
    
            dt[ms %in% c(2, 3, 4), ms := 2] # marital status
            
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
                
            # time
                dt[, schlhrs := factor(schlhrs, levels = 1:4, labels = c("None", "1 day", "2-3 days", "4 or more days"))]
                # summary(dt[]$schlhrs) # hrs contact with teacher in last 7 days (only enroll1==1)
                # summary(dt[]$tstdy_hrs) # hrs studied on own in last 7 days (only enroll1==1)
                dt[, tch_hrs := factor(tch_hrs, levels = 1:4, labels = c("Much less", "A little bit less", "A little bit more", "Much more"))]

            # post-secondary
                # summary(dt[]$tnum_ps) # num in hh planning to take fall classes before covid 
                
                dt[tnum_ps > 0, ps_occupational := 0][psplans1==1, ps_occupational := 1] 
                dt[tnum_ps > 0, ps_associate := 0][psplans2==1, ps_associate := 1] 
                dt[tnum_ps > 0, ps_bachelor := 0][psplans3==1, ps_bachelor := 1] 
                dt[tnum_ps > 0, ps_graduate := 0][psplans4==1, ps_graduate := 1] 
                dt[tnum_ps > 0, ps_other := 0][psplans5==1, ps_other := 1] 
                dt[tnum_ps > 0, ps_nocredential := 0][psplans6==1, ps_nocredential := 1] 
                
                dt[tnum_ps > 0, ps_chng_none := 0][pschng1==1, ps_chng_none := 1] 
                dt[tnum_ps > 0, ps_chng_cancel := 0][pschng2==1, ps_chng_cancel := 1] 
                dt[tnum_ps > 0, ps_chng_format := 0][pschng3==1, ps_chng_format := 1] 
                dt[tnum_ps > 0, ps_chng_fewer := 0][pschng4==1, ps_chng_fewer := 1] 
                dt[tnum_ps > 0, ps_chng_more := 0][pschng5==1, ps_chng_more := 1] 
                dt[tnum_ps > 0, ps_chng_diffsch := 0][pschng6==1, ps_chng_diffsch := 1] 
                dt[tnum_ps > 0, ps_chng_diffcert := 0][pschng7==1, ps_chng_diffcert := 1] 
                
                dt[tnum_ps > 0, ps_why_getcovid := 0][pswhychg1==1, ps_why_getcovid := 1] 
                dt[tnum_ps > 0, ps_why_covidcare := 0][pswhychg2==1, ps_why_covidcare := 1] 
                dt[tnum_ps > 0, ps_why_lostcare := 0][pswhychg3==1, ps_why_lostcare := 1] 
                dt[tnum_ps > 0, ps_why_chngcontent := 0][pswhychg4==1, ps_why_chngcontent := 1] 
                dt[tnum_ps > 0, ps_why_chngaid := 0][pswhychg5==1, ps_why_chngaid := 1] 
                dt[tnum_ps > 0, ps_why_campuslife := 0][pswhychg6==1, ps_why_campuslife := 1] 
                dt[tnum_ps > 0, ps_why_uncertclass := 0][pswhychg7==1, ps_why_uncertclass := 1] 
                dt[tnum_ps > 0, ps_why_chngincome := 0][pswhychg8==1, ps_why_chngincome := 1] 
                dt[tnum_ps > 0, ps_why_other := 0][pswhychg9==1, ps_why_other := 1] 
                

                dt[tnum_ps >0, ps_cancelchange := 0] # denominator for adults whose higher ed plans canceled of changed
                dt[ps_chng_cancel==1|ps_chng_format==1|ps_chng_fewer==1|ps_chng_more==1|ps_chng_diffsch==1|ps_chng_diffcert==1 
                   , ps_cancelchange := 1]
                
                dt[, grep("^psplans|^pschng|^pswhychg", names(dt), value = TRUE) := NULL]
                
        # food security ----
            # Prior sufficient food   
            dt[prifoodsuf %in% 1:2, prifoodsuf := 0] # enough food
            dt[prifoodsuf %in% 3:4, prifoodsuf := 1] # sometimes or often not enough to eat   
            
            # reasons for insufficient food
            for(i in grep("^foodsufrsn[0-9]$", names(dt), value = T)){
              dt[is.na(get(paste0(i))) & curfoodsuf %in% c(2, 3, 4), paste0(i) := 0] # set NA to zero when stated that had insufficient food
            }
            
            # collapse children not eating enough
            dt[childfood %in% 1:2, childfood.bin := 1]
            dt[childfood %in% 3, childfood.bin := 0]
            dt[children == 0, childfood.bin := NA]
            
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
          for(blah in c(grep("hlthins", names(dt), value = T), "delay", "notget") ){dt[get(paste0(blah)) == 2, (paste0(blah)) := 0]}
            
            
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
          
          dt[, insured_employer := hlthins1][hlthins1 == 10, insured_employer := NA]
          dt[, insured_exchange := hlthins2][hlthins2 == 10, insured_exchange := NA]
          dt[, insured_mcare := hlthins3][hlthins3 == 10, insured_mcare := NA]
          dt[, insured_military := hlthins5][hlthins5 == 10, insured_military := NA] # start with TRICARE
          dt[hlthins6 == 1, insured_military := 1] # add on VA
          
          # mental health
          for(blah in c("prescript", "mh_svcs", "mh_notget") ){dt[get(blah) == 2, (paste0(blah)) := 0]} # Q 38a, 38b, 38c
          
          
        # housing ----
          # Tenure as a factor ----
            # create 'tenure2' below
          
          # Current rent OR mortgage ----
            for(blah in c("rentcur", "mortcur") ){dt[get(paste0(blah)) == 2, (paste0(blah)) := 0]}
            dt[, current := rentcur][!is.na(mortcur), current := mortcur]
          
          # NOT current rent or mortgage ----
            dt[current==0, notcurrent := 1][current==1, notcurrent := 0]
          
          # Confidence in in paying next rent / mortgage ----
            # create confidence below
          
          # Eviction within 2 months ----
            # create eviction below ... make binary extremely/very = likely, somewhat/not at all = unlikely
          
          # Foreclosure within 2 months ----
            # create foreclosure below ... make binary extremely/very = likely, somewhat/not at all = unlikely
          
          # Forced to leave within 2 months (AMONG THOSE NOT CURRENT) ----
            # Due to eviction OR foreclosure, create here, set as factor below
            # factor version will be binary .... extremely/very = likely, somewhat/not at all = unlikely
            dt[, leave2mo := evict][!is.na(forclose), leave2mo := forclose]
            
          # Forced to leave within 2 months (AMONG THOSE WHO ANSWERED TENURE QUESTION) ----
            # Only those who answered tenure question could answer follow-up housing questions
            # Will have to assume that those who have tenure and didn't answer eviction/foreclosure
            # because they were current on payments ... they must be (4) Not at all likely
            dt[, leave2mo_alt := leave2mo]
            dt[!is.na(tenure) & is.na(leave2mo_alt), leave2mo_alt := 4]
            
        # Mental Health ----
            dt[, phq4total := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)]
            dt[, phq4severe := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)][phq4severe < 9, phq4severe := 0][phq4severe >= 9, phq4severe := 1]
            dt[, phq4mod_severe := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)][phq4mod_severe < 6, phq4mod_severe := 0][phq4mod_severe >= 6, phq4mod_severe := 1]
            dt[, phq4anxiety := (anxious - 1) + (worry - 1)][phq4anxiety < 3, phq4anxiety := 0][phq4anxiety >= 3, phq4anxiety := 1]
            dt[, phq4depression := (interest - 1) + (down - 1)][phq4depression < 3, phq4depression := 0][phq4depression >= 3, phq4depression := 1]
            
            for(blah in c("down", "anxious", "worry", "interest")){dt[get(paste0(blah)) %in% 1:2, (paste0(blah)) := 0]; dt[get(paste0(blah)) %in% 3:4, (paste0(blah)) := 1]}
            
        # Vaccine (weeks 22+ only) ----      
          for(blah in c("recvdvacc", "doses") ){dt[get(blah) == 2, (paste0(blah)) := 0]} # QV1 & QV2
          dt[recvdvacc == 0, doses := 0]  # change requested by Lin Song
          dt[getvacc == 1 & week >= 22, definitevacc := 1][getvacc %in% 2:4 & week >= 22, definitevacc := 0] # QV3
          for(col in grep("whynot[0-9]", names(dt), value = T)){dt[week >= 22 & getvacc %in% 2:4 & is.na(get(col)), paste0(col) := 0]} # QV4
          for(col in grep("whynotb[0-9]", names(dt), value = T)){dt[week >= 22 & whynot3 == 1 & is.na(get(col)), paste0(col) := 0]} # QV5
          dt[hadcovid == 1 & week >= 22, prevcovid := 1][hadcovid %in% 2:3 & week >= 22, prevcovid := 0] # QV6
          
        # misc ----
            dt[, c("msa", "washington") := 0]
            dt[est_st == 53, washington := 1]
            dt[est_msa == 42660, msa := 1] # actually seattle-tacoma-bellevue metro area
            
            dt[, phase := 2]
            
            dt[, constant := 1]

    # create pooled weights for all weeks combined ----
        myweights <- grep("pweight", names(dt), value = T) # just get person level weights since currently (11/5/2020) do not need household level estimates
        dt[, (gsub("pweight", "pooledNwt", myweights)) := lapply(.SD, function(X){X/length(unique(dt$week))}), .SDcols = myweights] # divide by # of weeks in phase 2 thus far << used for pooled estimation

    # create factor labels ----
        dt[, agechi := cut(age, c(17, 24, 44, 64, 74, 120), labels = c("18-24", 
                                                                       "25-44", 
                                                                       "45-64", 
                                                                       "65-74", 
                                                                       "75+"))]
        dt[, agegp := cut(age, c(17, 29, 39, 49, 59, 69, 79, 120), labels = c("18-29", 
                                                                              "30-39", 
                                                                              "40-49", 
                                                                              "50-59", 
                                                                              "60-69", 
                                                                              "70-79", 
                                                                              "80+"))]
        dt[, age4 := cut(age, c(17, 24, 44, 64, 120), labels = c("18-24", 
                                                                 "25-44", 
                                                                 "45-64", 
                                                                 "65+"))]
        dt[, anxious := factor(anxious, levels = 0:1, labels = c("< 1/2 prior 7 days", 
                                                                 ">= 1/2 prior 7 days"))]
        dt[, childfood.bin := factor(childfood.bin, levels = 0:1, labels = c("Never true", 
                                                                     "Often/sometimes true"))]
        dt[, children := factor(children, levels= 0:1, labels = c("No children", 
                                                                  "Has children"))]
        dt[, confidence := factor(mortconf, levels = 1:5, labels = c("No confidence", 
                                                                     "Slight confidence", 
                                                                     "Moderate confidence", 
                                                                     "High confidence", 
                                                                     "Payment is / will be deferred"))]
        dt[, confidence.binary := factor(mortconf, levels = 1:5, labels = c("Slight confidence / No confidence / Payment deferred", 
                                                                     "Slight confidence / No confidence / Payment deferred", 
                                                                     "High confidence /Moderate confidence", 
                                                                     "High confidence /Moderate confidence", 
                                                                     "Slight confidence / No confidence / Payment deferred"))]
        dt[, curfoodsuf := factor(curfoodsuf, levels = 0:1, labels = c("Not insufficient", 
                                                                       "Insufficient"))]
        dt[, down := factor(down, levels = 0:1, labels = c("< 1/2 prior 7 days", 
                                                           ">= 1/2 prior 7 days"))]
        dt[, edu := factor(edu, levels = c(2, 3, 5, 6, 7), labels = c("<HS", 
                                                                      "HS Grad/GED", 
                                                                      "Some college/Associate degree",
                                                                      "Bachelor degree",
                                                                      "Graduate degree"))]
        dt[, ethn := factor(ethn, levels = 1:5 , labels = c("White alone", 
                                                            "Black alone", 
                                                            "Asian alone", 
                                                            "Other/Multiracial", 
                                                            "Hispanic") )]
        dt[, eviction := factor(evict, levels = 1:4, labels = c("Likely",
                                                                "Likely",
                                                                "Unlikely", 
                                                                "Unlikely"))]
        dt[, foodconf := factor(foodconf, levels = 1:4, labels = c("Not at all confident", 
                                                                   "Somewhat confident", 
                                                                   "Moderately confident", 
                                                                   "Very confident"))]
        dt[, foodconf2 := factor(foodconf2, levels = 0:1, labels = c("< moderately confident", 
                                                                     ">= moderately confident"))]
        dt[, foreclosure := factor(forclose, levels = 1:4, labels = c("Likely",
                                                                      "Likely",
                                                                      "Unlikely", 
                                                                      "Unlikely"))]
        dt[, gender := factor(egender, levels = 1:2, labels = c("Male", 
                                                                "Female"))]
        dt[, income := factor(income, levels = c(1:8), labels = c("<$25K", 
                                                                  "<$35K", 
                                                                  "<$50K", 
                                                                  "<$75K", 
                                                                  "<$100K", 
                                                                  "<$150K", 
                                                                  "<$200K", 
                                                                  "$200K+") )]
        dt[, interest := factor(interest, levels = 0:1, labels = c("< 1/2 prior 7 days", 
                                                                   ">= 1/2 prior 7 days"))]
        dt[, kindwork := factor(kindwork, levels = c(1:5), labels = c("Government", 
                                                                      "Private", 
                                                                      "Non-profit", 
                                                                      "Self-employed", 
                                                                      "Family business") )]
        dt[, leave2mo := factor(leave2mo, levels = 1:4, labels = c("Likely",
                                                                   "Likely",
                                                                   "Unlikely", 
                                                                   "Unlikely"))]
        
        dt[, leave2mo_alt := factor(leave2mo_alt, levels = 1:4, labels = c("Likely",
                                                                   "Likely",
                                                                   "Unlikely", 
                                                                   "Unlikely"))]
        
        dt[, prifoodsuf := factor(prifoodsuf, levels = 0:1, labels = c("Not insufficient", 
                                                                       "Insufficient"))]
        dt[, ms := factor(ms, levels = c(1, 2, 5), labels = c("Married", 
                                                              "Widowed/Divorced/Separated", 
                                                              "Never married") )]
        dt[, rrace := factor(rrace, levels = 1:4 , labels = c("White alone", 
                                                              "Black alone", 
                                                              "Asian alone", 
                                                              "Other or multiracial") )]
        dt[, rhispanic := factor(rhispanic, levels = 1:2, labels = c("No", 
                                                                     "Yes"))] # YES, I confirmed that 1=No & 2=Yes.
        dt[, tenure2 := factor(tenure, levels = 1:4, labels = c("Owned free & clear", 
                                                                "Owned with a mortgage or loan", 
                                                                "Rented", 
                                                                "Occupied without payment"))]
        dt[, worry := factor(worry, levels = 0:1, labels = c("< 1/2 prior 7 days", 
                                                             ">= 1/2 prior 7 days"))]

        noyesvars <- c("delay", "notget", "hlthins1", "phq4mod_severe", "phq4severe", "phq4anxiety", "phq4depression", "recvdvacc", 
                       "doses", "definitevacc", "prescript", "mh_svcs", "mh_notget", 
                       grep("whynot[0-9]", names(dt), value = T), grep("whynotb[0-9]", names(dt), value = T), "prevcovid")
        dt[, (noyesvars) := lapply(.SD, function(X){factor(X, levels = 0:1, labels = c("No", "Yes") )}), .SDcols = noyesvars]

        yesnovars <- c("wrkloss", "expctloss", "anywork", "freefood")
        dt[, (yesnovars) := lapply(.SD, function(X){factor(X, levels = 1:2, labels = c("Yes", "No") )}), .SDcols = yesnovars]

    # check that factors processed properly ----
        # for(i in c("age4", "agechi", "agegp", "anxious", "anywork", "childfood", "curfoodsuf", "delay", "down", "edu", "ethn", "expctloss", 
        #            "foodconf", "foodconf2", "gender", "hlthins1", "income", "interest", "kindwork", "ms", "notget", "prifoodsuf", 
        #            "rhispanic", "rrace", "uninsured", "insurance", "worry", "wrkloss", "intrntavail.binary", "intrntavail", "compavail.binary", "compavail",
        #            "tenure2", "confidence", "confidence.binary", "eviction", "foreclosure", "leave2mo")){print(i); print( table(dt[[i]]))}

## Survey Set for WA & MSA ----
    # Washington ----
        svy_wa <- survey_set_weekly(dt[washington==1])
        pooledN_svy_wa <- survey_set_pooled(dt[washington==1])

    # MSA ----
        svy_msa <- survey_set_weekly(dt[msa==1])
        pooledN_svy_msa <- survey_set_pooled(dt[msa==1])
        
## The end ----
