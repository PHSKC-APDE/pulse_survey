## Header ----
    # Author: Danny Colombara
    # Date: October 18, 2022
    # R version: 4.2.1
    # Purpose: append and prep all individual PULSE 3.6 survey files for analysis
    #
    # Notes: PAGE 12 https://www2.census.gov/programs-surveys/demo/technical-documentation/hhp/Phase3-4_Source_and_Accuracy_Week43.pdf
    #        Users may want to pool estimates over multiple weeks by creating averages for estimates
    #        with small sample sizes. For pooled estimates, where two or more weeks of data are
    #        combined to make one estimate for a longer time period, one would divide the unit-level
    #        weights that formed x-hat0 and xi (for each of the 80 replicate weights) for each week by the
    #        number of weeks that are combined.  Then, form 80 replicate pooled estimates, xhati_pooled
    #        and the estimate, xhat0_pooled. Then use the pooled estimates in formula (1) to calculate
    #        the pooled variance for the item of interest.
    #
    #        SO, in the end, we can pool across 'n' weeks as long as we divide the replicate weights by 'n' 
    #
    # Notes: Phase 3.6 began with week 49 (14-Sep-2022 <> 26-Sep-2022) and is scheduled to continue two additional survey weeks.
    #        Like, Phases 3.3, 3.4, & 3.5, Phase 3.6 has a "two weeks on, two weeks off" sampling scheme.
    #        Many questions were added, changed, and deleted when compared to 3.5. When possible, attempts
    #        were made to keep indicators consistent across time. No attempt was made to incorporate every new
    #        question
    # 
    #        We removed the following indicators for Phase 3.6
    #         chldcare < Childcare impacts due to pandemic
    #         chldimpct <- Childcare impact
    #         schlhrs <- Child educational format
    #         all post-secondary variables (ps_xxx)
    #         telehlth
    #         telechld
    #         All vaccine vars

## Set up ----
    # rm(list=ls())
    pacman::p_load(data.table, srvyr, rads)
    Sys.setenv(TZ='America/Los_Angeles') # set time zone
    
## Load survey & replicate weights ----
    source("https://raw.githubusercontent.com/PHSKC-APDE/svy_pulse/main/00_constants_n_functions.R")
    setwd(paste0(inputdir, "phase3_6_unzipped/"))
    
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
            # dt[, chldcare := factor(chldcare, levels = 1:2, labels = c("Yes", "No"))]
            # dt[is.na(kids_lt5y & is.na(kids_5_11y)), chldcare := NA] # limit to the universe

            # for(p in paste0("chldimpct", 1:9)){
            #   dt[is.na(get(p)) & chldcare=="Yes", paste0(p) := 0]
            #   dt[, paste0(p) := factor(get(p), levels = 0:1, labels = c("No", "Yes"))]
            # }
    
    
    
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
            
            # Census Bureau definition of LGBT
            dt[as.character(sex_at_birth) == as.character(gender_id) & 
                 orientation == "Straight", 
               lgbt := "Non-LGBT"]
            dt[as.character(sex_at_birth) != as.character(gender_id) |
                 orientation %in% c("Gay or Lesbian", "Bisexual") |
                 gender_id == "Transgender", 
               lgbt := "LGBT"]
            
            # APDE definition of LGBTQ
            dt[, lgbtq := lgbt]
            dt[lgbtq=="Non-LGBT", lgbtq := "Non-LGBTQ"]
            dt[lgbtq=="LGBT", lgbtq := "LGBTQ"]
            dt[gender_id == "None of these" | 
                 orientation %in% c("Something else"), 
               lgbtq := "LGBTQ"]        
            
        # disability ----
            # - From Lin Song on 6/17
            # disability: create a disability “demographic” variable based on the four disability questions 
            # (seeing, hearing, remembering, and mobility) with responses  
            # 1 (No – no difficulty) and 2 (Yes – some difficulty) as No and 
            # 3 (Yes – a lot of difficulty) and 4 (Cannot do at all) as Yes 
            # (any Yes as Yes, the rest as No for overall disability).  
            
            dt[!is.na(seeing) & 
                 !is.na(hearing) & 
                 !is.na(remembering) & 
                 !is.na(mobility) & 
                 !is.na(selfcare) & 
                 !is.na(understand), disability := "No"] # can only know disability status if it is not missing. Set No as default
            for(dis in c("seeing", "hearing", "mobility", "remembering", "selfcare", "understand")){
              dt[get(dis) %in% 3:4, disability := "Yes"]
              dt[, paste0(dis) := factor(get(dis), levels = 1:4, labels = c("No difficulty", 
                                                                             "Some difficulty", 
                                                                             "A lot of difficulty", 
                                                                             "Cannot do at all"))] 
              }

        # education ----
            # enrollment ----
                dt[, enroll_school := rowSums(.SD, na.rm = T), .SDcols = c("tenrollpub", "tenrollprv")] # hh with child enrolled in public or private school
                dt[enroll_school >=1, enroll_school := 1]
                dt[, enroll_home := tenrollhmsch][is.na(enroll_home), enroll_home := 0] # hh with child home schooled
                dt[enroll_home >=1, enroll_home := 1]
                dt[, enroll_none := enrollnone][is.na(enroll_none), enroll_none := 0] # hh without child in school
                dt[thhld_numkid == 0, c("enroll_school", "enroll_home", "enroll_none") := NA] # properly set NA values

            # teaching modality ----
                # teach1-8 & hybrid not available in phase 3.2+
                
            # Receipt of K-12 education in last 7 days ----
                setnames(dt, 'rcveduc1', 'teach_35_school_in_person')
                setnames(dt, 'rcveduc2', 'teach_35_school_realtime')
                setnames(dt, 'rcveduc3', 'teach_35_school_asynchronous')
                setnames(dt, 'rcveduc4', 'teach_35_school_paper')
                setnames(dt, 'rcveduc5', 'teach_35_own_materials')
                setnames(dt, 'rcveduc6', 'teach_35_none_closed')
                setnames(dt, 'rcveduc7', 'teach_35_none_sick')
                setnames(dt, 'rcveduc8', 'teach_35_none_break')
                setnames(dt, 'rcveduc9', 'teach_35_other')
                
                for(ii in grep("teach_35_", names(dt), value = T)){
                  dt[(tenrollpub > 0 | tenrollprv > 0) & is.na(get(ii)), paste0(ii) := 0]
                }
                
            # No computer / no internet ----
                # computer & internet availability not present in Phase 3.2+
                
            # time ----
                # dt[, schlhrs := factor(schlhrs, levels = 1:4, labels = c("None", "1 day", "2-3 days", "4 or more days"))]
                # dt[is.na(enroll_school) | enroll_school == 0, c("schlhrs") := NA]

            # school food ----
                dt[schlfdhlp1 == 2, schlfdhlp1 := 0] # picked up food from school or other location
                dt[schlfdhlp2 == 2, schlfdhlp2 := 0] # received ebt card
                dt[schlfdhlp3 == 2, schlfdhlp3 := 0] # ate meals onsite
                dt[schlfdhlp4 == 2, schlfdhlp4 := 0] # meals delivered
                
            # post-secondary ----
                # ended with Phase 3.5 
 
        # employment ----
          dt[, anywork := factor(anywork, levels = 1:2, labels = c("Yes", "No"))]
          dt[, wrklossrv := factor(wrklossrv, levels = 1:2, labels = c("Yes", "No"))]
          # expctloss not in Pulse 3.2+
                
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
                # prescript, mh_svcs, mh_notget not available as of Phase 3.5
                # for(blah in c("prescript", "mh_svcs", "mh_notget") ){dt[get(blah) == 2, (paste0(blah)) := 0]} # Q 38a, 38b, 38c
                # dt[, mh_notget := factor(mh_notget, levels = 0:1, labels = c("No", "Yes"))]
                # dt[, mh_svcs := factor(mh_svcs, levels = 0:1, labels = c("No", "Yes"))]
                # dt[, prescript := factor(prescript, levels = 0:1, labels = c("No", "Yes"))]
              
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
                
                # Children's mental health (question HLTH14) added for Phase 3.5
                for(ii in grep("kidbhvr[0-9]", names(dt), value = T)){
                  dt[thhld_numkid > 0 & is.na(get(ii)), paste0(ii) := 0]
                  dt[, paste0(ii) := factor(get(ii), levels = 0:1, labels = c("No", "Yes"))]
                }
                
                
            # Telehealth ----
                # ended with Phase 3.5
                
            # Preventive care ----
                # prvntive ended with Phase 3.4
                # prvntwhy1-7 not in Phase 3.2

        # housing ----
          # Tenure as a factor ----
            dt[, tenure2 := factor(tenure, levels = 1:4, labels = c("Owned free & clear", 
                                                                    "Owned with a mortgage or loan", 
                                                                    "Rented", 
                                                                    "Occupied without payment"))]
      
          # Monthly rent ----
            # this is new for Phase 3.5, no need for modification because it is continuous
            setnames(dt, "trentamt", "rent_monthly")
                
            # this is new for Phase 3.5
            dt[, rent_change := factor(rentchng, 
                                       levels = c(1:6), 
                                       labels = c("Rent did not change", 
                                                  "Rent decreased", 
                                                  "Rent increased by <$100", 
                                                  "Rent increased by $100-$249", 
                                                  "Rent increased by $250-$500", 
                                                  "Rent increased by more than $500"))]
            dt[rentchng %in% 1:6, rent_change.gt250 := 0]
            dt[rentchng %in% 5:6, rent_change.gt250 := 1]
                
          # Current rent OR mortgage ----
            for(blah in c("rentcur", "mortcur") ){dt[get(paste0(blah)) == 2, (paste0(blah)) := 0]}
            dt[, current := rentcur][!is.na(mortcur), current := mortcur]
          
          # NOT current rent or mortgage ----
            dt[current==0, notcurrent := 1][current==1, notcurrent := 0]
          
          # Confidence in in paying next rent / mortgage ----
            # mortconf ended Phase 3.4

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
          # No longer extracted because not requested by CD EPI team
            
        # COVID history ----
          # After Phase 3.4, HADCOVID & PRECOVID are no longer available
  
      # Misc ----
          dt[, c("msa", "washington") := 0]
          dt[est_st == 53, washington := 1]
          dt[est_msa == 42660, msa := 1] # actually seattle-tacoma-bellevue metro area
          
          dt[, phase := 3.6]
          
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

## Tidy environment ----
  rm(list = grep("^pooledN|^svy_|^dt$|inputdir|outputdir|my_state_code|my_msa_code|phase_number|phase.number|first.week", ls(), value = T, invert = T)) 
        
## The end ----
