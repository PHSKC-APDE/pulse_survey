## Header ----
    # Author: Danny Colombara
    # Date: May 26, 2022
    # R version: 4.1.2
    # Purpose: append and prep all individual PULSE 3.3 survey files for analysis
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
    # Notes: Phase 3.3 began with week 40 (12/1/21-12/13/21) and is scheduled to continue until February 7, 2022.
    #        Rather than surveying every week, Phase 3.3 switched a "two weeks on, two weeks off" sampling scheme.
    #        Phase 3.3 introduced new questions re: boosters and vaccine brand. Transportation and unemployment
    #        insurance questions returned to the survey. Since it differs from previous phases, it will be analyzed
    #        separately.
    # 

## set up ----
    rm(list=ls())
    pacman::p_load(data.table, srvyr, rads)

## load survey & replicate weights ----
    source("https://raw.githubusercontent.com/PHSKC-APDE/svy_pulse/main/00_constants_n_functions.R")
    setwd(paste0(inputdir, "phase3_3_unzipped/"))
    
    keepers <- list.files(pattern = "_repwgt")
    svy <- rbindlist(lapply(gsub("_repwgt", "", keepers),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    rep <- rbindlist(lapply(keepers,FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    
## Prepare data ----
    # merge survey and weights and clean ----
        dt <- merge(svy, rep, by = c("SCRAM", "WEEK"), all = T)

    # clean ----
        setnames(dt, tolower(names(dt)) ) # make all colnames lowercase
    
        for(col in grep("hlthins", names(dt), value = T)) set(dt, i=which(dt[[col]]%in% c(-88, -99)), j=col, value=10) # replace all -88 with 10 (arbitrary #) so it it not changed to NA below
        for(col in names(dt)) set(dt, i=which(dt[[col]]==-99), j=col, value=NA) # replace all -99 with NA
        for(col in names(dt)) set(dt, i=which(dt[[col]]==-88), j=col, value=NA) # replace all -88 with NA

    # create variables ----
        # childcare ----
            dt[, chldcare := factor(chldcare, levels = 1:2, labels = c("Yes", "No"))]
            dt[is.na(kids_lt5y & is.na(kids_5_11y)), chldcare := NA] # limit to the universe

            for(p in paste0("chldimpct", 1:9)){
              dt[is.na(get(p)) & chldcare=="Yes", paste0(p) := 0]
              dt[, paste0(p) := factor(get(p), levels = 0:1, labels = c("No", "Yes"))]
            }
    
        # demographics ----
            dt[, age := 2020-tbirth_year] # generate age
    
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
            
            dt[, alone := 0][thhld_numper == 1, alone := 1] # living alone, which is associated with mental health
            dt[, alone := factor(alone, levels = 0:1, labels = c("No", "Yes"))]
            
            dt[thhld_numkid == 0, children := 0]
            dt[thhld_numkid %in% 1:7, children := 1]
            dt[, children := factor(children, levels= 0:1, labels = c("No children", 
                                                                      "Has children"))]
            
            dt[, edu := eeduc]
            dt[edu==1, edu := 2][edu==4, edu := 5] # collapse less than HS into some HS & some college into Associate's
            dt[, edu := factor(edu, levels = c(2, 3, 5, 6, 7), labels = c("<HS", 
                                                                          "HS Grad/GED", 
                                                                          "Some college/Associate degree",
                                                                          "Bachelor degree",
                                                                          "Graduate degree"))]            
            
            dt[, ethn := rrace][rhispanic == 2, ethn := 5] # create hispanic as race
            dt[, ethn := factor(ethn, levels = 1:5 , labels = c("White alone", 
                                                                "Black alone", 
                                                                "Asian alone", 
                                                                "Other/Multiracial", 
                                                                "Hispanic") )]       
            
            dt[, gender_id := factor(genid_describe, levels = 1:4, labels = c("Male", "Female", "Transgender", "None of these"))]
            
            dt[, income := factor(income, levels = c(1:8), labels = c("<$25K", 
                                                                      "<$35K", 
                                                                      "<$50K", 
                                                                      "<$75K", 
                                                                      "<$100K", 
                                                                      "<$150K", 
                                                                      "<$200K", 
                                                                      "$200K+") )]            
            
            dt[, kindwork := factor(kindwork, levels = c(1:5), labels = c("Government", 
                                                                          "Private", 
                                                                          "Non-profit", 
                                                                          "Self-employed", 
                                                                          "Family business") )]
            
            dt[ms %in% c(2, 3, 4), ms := 2] # marital status
            dt[, ms := factor(ms, levels = c(1, 2, 5), labels = c("Married", 
                                                                  "Widowed/Divorced/Separated", 
                                                                  "Never married") )]
            

            dt[, orientation := factor(sexual_orientation, levels = 1:5, labels = c("Gay or Lesbian", "Straight", "Bisexual", "Something else", "I don't know"))]
            
            dt[, rrace := factor(rrace, levels = 1:4 , labels = c("White alone", 
                                                                  "Black alone", 
                                                                  "Asian alone", 
                                                                  "Other or multiracial") )]
            
            dt[, rhispanic := factor(rhispanic, levels = 1:2, labels = c("No", "Yes"))] # YES, I confirmed that 1=No & 2=Yes.
            
            dt[, sex_at_birth := factor(egenid_birth, levels = 1:2, labels = c("Male", "Female"))] # sex at birth
            
        # disability ----
            # - From Lin Song on 6/17
            # disability: create a disability “demographic” variable based on the four disability questions 
            # (seeing, hearing, remembering, and mobility) with responses  
            # 1 (No – no difficulty) and 2 (Yes – some difficulty) as No and 
            # 3 (Yes – a lot of difficulty) and 4 (Cannot do at all) as Yes 
            # (any Yes as Yes, the rest as No for overall disability).  
            
            dt[!is.na(seeing) & !is.na(hearing) & !is.na(remembering) & !is.na(mobility), disability := "No"] # can only know disability status if it is not missing. Set No as default
            for(dis in c("seeing", "hearing", "mobility", "remembering")){dt[get(dis) %in% 3:4, disability := "Yes"]}
            
    
            dt[, hearing := factor(hearing, levels = 1:4, labels = c("No difficulty", 
                                                                    "Some difficulty", 
                                                                    "A lot of difficulty", 
                                                                    "Cannot do at all"))]      
            
            dt[, mobility := factor(mobility, levels = 1:4, labels = c("No difficulty", 
                                                                       "Some difficulty", 
                                                                       "A lot of difficulty", 
                                                                       "Cannot do at all"))]
            
            dt[, remembering := factor(remembering, levels = 1:4, labels = c("No difficulty", 
                                                                             "Some difficulty", 
                                                                             "A lot of difficulty", 
                                                                             "Cannot do at all"))]
            
            dt[, seeing := factor(seeing, levels = 1:4, labels = c("No difficulty", 
                                                                   "Some difficulty", 
                                                                   "A lot of difficulty", 
                                                                   "Cannot do at all"))]
            
        # education ----
            # enrollment ----
                dt[, enroll_school := rowSums(.SD, na.rm = T), .SDcols = c("tenrollpub", "tenrollprv")] # hh with child enrolled in public or private school
                dt[enroll_school >=1, enroll_school := 1]
                dt[, enroll_home := tenrollhmsch][is.na(enroll_home), enroll_home := 0] # hh with child home schooled
                dt[enroll_home >=1, enroll_home := 1]
                dt[, enroll_none := enrollnone][is.na(enroll_none), enroll_none := 0] # hh without child in school
                dt[thhld_numkid == 0, c("enroll_school", "enroll_home", "enroll_none") := NA] # properly set NA values

            # teaching modality ----
                # teach1-8 & hybrid not available in phase 3.2
                
            # No computer / no internet ----
                # computer & internet availability not present in Phase 3.2
                
            # time ----
                # schlhrs not available in Phase 3.2

            # school food ----
                # 'schlfood' not available in Phase 3.2

                dt[schlfdhlp1 == 2, schlfdhlp1 := 0] # picked up food from school or other location
                dt[schlfdhlp2 == 2, schlfdhlp2 := 0] # received ebt card
                dt[schlfdhlp3 == 2, schlfdhlp3 := 0] # ate meals onsite
                dt[schlfdhlp4 == 2, schlfdhlp4 := 0] # meals delivered
                
                # dt[is.na(schlfood) | schlfood !=1 , c("schlfdhlp1", "schlfdhlp2", "schlfdhlp3", "schlfdhlp4") := NA]
                
            # post-secondary ----
                # summary(dt[]$tnum_ps) # num in hh planning to take fall classes before covid 
                
                dt[tnum_ps > 0, ps_chng_none := 0][tnum_ps > 0 & pschng1==1, ps_chng_none := 1] 
                dt[tnum_ps > 0, ps_chng_cancel := 0][tnum_ps > 0 & pschng2==1, ps_chng_cancel := 1] 
                dt[tnum_ps > 0, ps_chng_format := 0][tnum_ps > 0 & pschng3==1, ps_chng_format := 1] 
                dt[tnum_ps > 0, ps_chng_fewer := 0][tnum_ps > 0 & pschng4==1, ps_chng_fewer := 1] 
                dt[tnum_ps > 0, ps_chng_more := 0][tnum_ps > 0 & pschng5==1, ps_chng_more := 1] 
                dt[tnum_ps > 0, ps_chng_diffsch := 0][tnum_ps > 0 & pschng6==1, ps_chng_diffsch := 1] 
                dt[tnum_ps > 0, ps_chng_diffcert := 0][tnum_ps > 0 & pschng7==1, ps_chng_diffcert := 1] 
                
                dt[tnum_ps >0, ps_cancelchange := 0] # denominator for adults whose higher ed plans canceled of changed
                dt[ps_chng_cancel==1|ps_chng_format==1|ps_chng_fewer==1|ps_chng_more==1|ps_chng_diffsch==1|ps_chng_diffcert==1 
                   , ps_cancelchange := 1]     
                
                dt[, ps_why_getcovid := 0][pswhychg1==1, ps_why_getcovid := 1] 
                dt[, ps_why_covidcare := 0][pswhychg2==1, ps_why_covidcare := 1] 
                dt[, ps_why_lostcare := 0][pswhychg3==1, ps_why_lostcare := 1] 
                dt[, ps_why_chngcontent := 0][pswhychg4==1, ps_why_chngcontent := 1] 
                dt[, ps_why_chngaid := 0][pswhychg5==1, ps_why_chngaid := 1] 
                dt[, ps_why_campuslife := 0][pswhychg6==1, ps_why_campuslife := 1] 
                dt[, ps_why_uncertclass := 0][pswhychg7==1, ps_why_uncertclass := 1] 
                dt[, ps_why_chngincome := 0][pswhychg8==1, ps_why_chngincome := 1] 
                dt[, ps_why_other := 0][pswhychg9==1, ps_why_other := 1] 
                
                whycols <- c("ps_why_getcovid", "ps_why_covidcare", "ps_why_lostcare", "ps_why_chngcontent", "ps_why_chngaid", 
                             "ps_why_campuslife", "ps_why_uncertclass", "ps_why_chngincome", "ps_why_other")
                dt[is.na(pschng2) & is.na(pschng3) & is.na(pschng4) & is.na(pschng5) & is.na(pschng6) & is.na(pschng7), (whycols) := NA]

                dt[, grep("^psplans|^pschng|^pswhychg", names(dt), value = TRUE) := NULL]
                
        # employment ----
          dt[, anywork := factor(anywork, levels = 1:2, labels = c("Yes", "No"))]
          dt[, wrklossrv := factor(wrklossrv, levels = 1:2, labels = c("Yes", "No"))]
          # expctloss not in Pulse 3.2
                
        # food security ----
            # reasons for insufficient food
            for(i in 1:4){dt[, paste0("foodwhynot", i) := get(paste0("foodrsnrv", i))]}    
                
            for(i in grep("^foodwhynot[0-9]$", names(dt), value = T)){
              dt[is.na(get(paste0(i))) & curfoodsuf %in% c(2, 3, 4), paste0(i) := 0] # set NA to zero when stated that had insufficient food
            }
            
            # collapse children not eating enough
            dt[childfood %in% 1:2, childfood.bin := 1]
            dt[childfood %in% 3, childfood.bin := 0]
            dt[children == 0, childfood.bin := NA]
            
            # current sufficient food ... must be after foodsufrsn[0-9] b/c used in defining the latter
            dt[curfoodsuf %in% 1:2, curfoodsuf := 0]
            dt[curfoodsuf %in% 3:4, curfoodsuf := 1]
            
            dt[, curfoodsuf := factor(curfoodsuf, levels = 0:1, labels = c("Not insufficient", "Insufficient"))]
            
            # total $ spent on food in last 7 days
              # not available in Phase 3.2

            dt[, childfood.bin := factor(childfood.bin, levels = 0:1, labels = c("Never true", "Often/sometimes true"))]
            
            dt[, freefood := factor(freefood, levels = 1:2, labels = c("Yes", "No"))]
            
        # health ----
            # Insurance ----
              for(blah in c(grep("hlthins", names(dt), value = T)) ){dt[get(paste0(blah)) == 2, (paste0(blah)) := 0]} # delay & notget not in 3.2

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
          
            # Mental Health ----
                for(blah in c("prescript", "mh_svcs", "mh_notget") ){dt[get(blah) == 2, (paste0(blah)) := 0]} # Q 38a, 38b, 38c
          
                dt[, phq4total := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)]
                dt[, phq4severe := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)][phq4severe < 9, phq4severe := 0][phq4severe >= 9, phq4severe := 1]
                dt[, phq4mod_severe := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)][phq4mod_severe < 6, phq4mod_severe := 0][phq4mod_severe >= 6, phq4mod_severe := 1]
                dt[, phq4anxiety := (anxious - 1) + (worry - 1)][phq4anxiety < 3, phq4anxiety := 0][phq4anxiety >= 3, phq4anxiety := 1]
                dt[, phq4depression := (interest - 1) + (down - 1)][phq4depression < 3, phq4depression := 0][phq4depression >= 3, phq4depression := 1]
                
                dt[, c("phq4severe", "phq4mod_severe", "phq4anxiety", "phq4depression") := lapply(.SD, function(X){
                  factor(X, levels = 0:1, labels = c("No", "Yes") )}), .SDcols = c("phq4severe", "phq4mod_severe", "phq4anxiety", "phq4depression")]
                
                for(blah in c("anxious", "down", "interest", "worry")){dt[get(paste0(blah)) %in% 1:2, (paste0(blah)) := 0]; dt[get(paste0(blah)) %in% 3:4, (paste0(blah)) := 1]}
                dt[, anxious := factor(anxious, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]
                dt[, down := factor(down, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]
                dt[, interest := factor(interest, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]
                dt[, worry := factor(worry, levels = 0:1, labels = c("< 1/2 prior 7 days", ">= 1/2 prior 7 days"))]
                
                dt[, mh_notget := factor(mh_notget, levels = 0:1, labels = c("No", "Yes"))]
                dt[, mh_svcs := factor(mh_svcs, levels = 0:1, labels = c("No", "Yes"))]
                
                dt[, prescript := factor(prescript, levels = 0:1, labels = c("No", "Yes"))]
                
            # Telehealth ----
                dt[, telehlth := factor(telehlth, levels = 1:2, labels = c("Yes", "No"))]
                dt[, telechld := factor(telechld, levels = 1:2, labels = c("Yes", "No"))]
                
            # Preventive care ----
                dt[, prvntive  := factor(prvntive, levels = 1:3, labels = c("No", "Yes", "Yes"))]
                
                # prvntwhy1-7 not in Phase 3.2

        # housing ----
          # Tenure as a factor ----
            dt[, tenure2 := factor(tenure, levels = 1:4, labels = c("Owned free & clear", 
                                                                    "Owned with a mortgage or loan", 
                                                                    "Rented", 
                                                                    "Occupied without payment"))]
      
          # Current rent OR mortgage ----
            for(blah in c("rentcur", "mortcur") ){dt[get(paste0(blah)) == 2, (paste0(blah)) := 0]}
            dt[, current := rentcur][!is.na(mortcur), current := mortcur]
          
          # NOT current rent or mortgage ----
            dt[current==0, notcurrent := 1][current==1, notcurrent := 0]
          
          # Confidence in in paying next rent / mortgage ----
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
            
          # Eviction within 2 months ----
            # create eviction below ... make binary extremely/very = likely, somewhat/not at all = unlikely
            dt[, eviction := factor(evict, levels = 1:4, labels = c("Likely",
                                                                    "Likely",
                                                                    "Unlikely", 
                                                                    "Unlikely"))]
          # Foreclosure within 2 months ----
            # create foreclosure below ... make binary extremely/very = likely, somewhat/not at all = unlikely
            dt[, foreclosure := factor(forclose, levels = 1:4, labels = c("Likely",
                                                                          "Likely",
                                                                          "Unlikely", 
                                                                          "Unlikely"))]
            
          # Forced to leave within 2 months (AMONG THOSE NOT CURRENT) ----
            # Due to eviction OR foreclosure, create here, set as factor below
            # factor version will be binary .... extremely/very = likely, somewhat/not at all = unlikely
            dt[, leave2mo := evict][!is.na(forclose), leave2mo := forclose]
            dt[, leave2mo := factor(leave2mo, levels = 1:4, labels = c("Likely",
                                                                       "Likely",
                                                                       "Unlikely", 
                                                                       "Unlikely"))]
            
          # Forced to leave within 2 months (AMONG THOSE WHO ANSWERED TENURE QUESTION) ----
            # Only those who answered tenure question could answer follow-up housing questions
            # Will have to assume that those who have tenure and didn't answer eviction/foreclosure
            # because they were current on payments ... they must be (4) Not at all likely
            dt[, leave2mo_alt := evict][!is.na(forclose), leave2mo_alt := forclose]
            dt[!is.na(tenure) & is.na(leave2mo_alt), leave2mo_alt := 4]
            dt[, leave2mo_alt := factor(leave2mo_alt, levels = 1:4, labels = c("Likely",
                                                                               "Likely",
                                                                               "Unlikely", 
                                                                               "Unlikely"))]

        # Vaccine ----      
          # QV1
          dt[, recvdvacc := factor(recvdvacc, levels = 1:2, labels = c("Yes", "No"))]

          # QV 2 (previously dosesrv, changed to NUMDOSES in Phase 3.3)
          dt[, numdoses := factor(numdoses, 
                                  levels = 1:5, 
                                  labels = c("1 vaccination", "2 vaccinations", 
                                             "3 vaccinations", "4 vaccinations", 
                                             "Don't know"))]
          dt[, dosesrv := NA]
          dt[, doses := NA]
          
          # QV3
          dt[getvacrv == 1, definitevacc := 1][getvacrv %in% 2:5 , definitevacc := 0] 
          dt[, definitevacc := factor(definitevacc, levels = 0:1, labels = c("No", "Yes"))]
          
          # QV4 
          for(col in grep("whynorv[0-9]", names(dt), value = T)){
            dt[(getvacrv %in% 2:5) & is.na(get(col)), paste0(col) := 0]
            dt[, paste0(col) := factor(get(paste0(col)), levels = 0:1, labels = c("No", "Yes"))]
          }   

          # QV5 not asked in Pulse 3.2
          
          # Brand
          dt[, brand := factor(brand, 
                                    levels = 1:6, 
                                    labels = c("Pfizer-Biontech", "Moderna", "Johnson and Johnson (Janssen)",
                                               "One of the brands that requires two initial shots, but not sure which brand", 
                                               "None of these brands", "Don’t know"))]
          
          # KIDDOSES
          dt[, kiddoses := factor(kiddoses, 
                                  levels = 1:3, 
                                  labels = c("Yes", "No", "Don't know"))]
          
          # KIDWHYNO
          for(col in grep("kidwhyno[0-9]", names(dt), value = T)){
            dt[(kidgetvac %in% 2:5) & is.na(get(col)), paste0(col) := 0]
            dt[, paste0(col) := factor(get(paste0(col)), levels = 0:1, labels = c("No", "Yes"))]
          }   
          
          # KIDGETVAC
          dt[, kidgetvac := factor(kidgetvac, 
                                   levels = 1:6, 
                                   labels = c("Definitely get the children a vaccine", 
                                              "Probably get the children a vaccine", 
                                              "Be unsure about getting the children a vaccine", 
                                              "Probably NOT get the children a vaccine", 
                                              "Definitely NOT get the children a vaccine", 
                                              "I do not know the plans for vaccination of the children aged 5-17 in my household"))]
          
          
          
      # COVID history ----
          dt[hadcovid == 1, prevcovid := 1][hadcovid %in% 2:3, prevcovid := 0] # QV6
          dt[, prevcovid := factor(prevcovid, levels = 0:1, labels = c("No", "Yes"))]
          
        # Misc ----
            dt[, c("msa", "washington") := 0]
            dt[est_st == 53, washington := 1]
            dt[est_msa == 42660, msa := 1] # actually seattle-tacoma-bellevue metro area
            
            dt[, phase := 3.3]
            
            dt[, constant := 1]

    # create pooled weights for all weeks combined ----
        myweights <- grep("pweight", names(dt), value = T) # just get person level weights since currently (12/30/2021) do not need household level estimates
        
        dt[, gsub("pweight", "pooledNwt", myweights) := lapply(.SD, function(X){X/length(unique(dt$week))}), .SDcols = myweights]
            

## Survey Set for WA & MSA ----
    # Washington ----
        svy_wa <- survey_set_weekly(dt[washington==1])
        pooledN_svy_wa <- survey_set_pooled(dt[washington==1])

    # MSA ----
        svy_msa <- survey_set_weekly(dt[msa==1])
        pooledN_svy_msa <- survey_set_pooled(dt[msa==1])

## The end ----
