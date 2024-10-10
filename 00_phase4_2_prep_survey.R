## Header ----
    # Author: Danny Colombara
    # Date: October 9, 2024
    # R version: 4.4.1
    # Purpose: append and prep all individual PULSE 4_2 survey files for analysis
    #
    # Notes: PAGE 13 https://www2.census.gov/programs-surveys/demo/technical-documentation/hhp/Phase3-8_Source_and_Accuracy_Week55.pdf
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
    # Notes: Phase 4_2 began on July 23, 2024 and will continue for two cycles (8-9) until September 16, 2024. 
    #
    #        ---ANNOYING VARS---
    #           * 
    #
    #        ---CHANGED VARS---
    #           * RGENID_DESCRIBE replaces GENID_DESCRIBE ... include Nonbinary/I use a different term
    #           * SEXUAL_ORIENTATION_RV replaces SEXUAL_ORIENTATION ... includes 'I use a different term'
    #           * support4_rv ... changed coding from 0:4 to 1:5 (but labels are same)
    #        ---DROPPED VARS---
    #           * support1exp ... In a typical week, how often do you text or message with family, friends, or neighbors? 
    #


## Set up ----
    # rm(list=ls())
    pacman::p_load(data.table, srvyr, rads)
    Sys.setenv(TZ='America/Los_Angeles') # set time zone
    this_phase <- '4_2'
    
## Load survey & replicate weights ----
    source(here::here('00_constants_n_functions.R'))
    
    phase.file.path <- file.path(paste0(inputdir, "phase", this_phase, "_unzipped/"))

    keepers <- list.files(path = phase.file.path, 
                          pattern = "_repwgt", 
                          full.names = TRUE)
    
    svy <- rbindlist(lapply(gsub("_repwgt", "", keepers),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    rep <- rbindlist(lapply(keepers,FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    
## Prepare data ----
    # merge survey and weights and clean ----
        dt <- merge(svy, rep, by = c("SCRAM", "CYCLE"), all = T)

    # clean ----
        setnames(dt, tolower(names(dt)) ) # make all colnames lowercase
    
        for(col in grep("hlthins", names(dt), value = T)) set(dt, i=which(dt[[col]]%in% c(-88, -99)), j=col, value=10) # replace all -88 with 10 (arbitrary #) so it it not changed to NA below
        for(col in names(dt)) set(dt, i=which(dt[[col]]==-99), j=col, value=NA) # replace all -99 with NA, Question seen but not selected
        for(col in names(dt)) set(dt, i=which(dt[[col]]==-88), j=col, value=NA) # replace all -88 with NA, Missing, did not report
        for(col in names(dt)) set(dt, i=which(dt[[col]]=='M'), j=col, value=NA) # replace all 'M' with NA, Missing, did not report
    
    # Set integers as integers (needed bc Census started using 'M', rather than -88, for missing) ----
        for(myint in c(
                       # education
                          'tenrollhmsch', 'tenrollpub', 'tenrollprv', 
                          grep('rcveduc[0-9]', names(dt), value = T), 
                       # behavioral health
                          'anxious', 'worry', 'interest', 'down', 
                       # food security
                          'curfoodsuf', grep('foodrsnrv[0-9]', names(dt), value = T), 
                          'childfood', 'freefood', 
                       # housing
                          'rentcur', 'mortcur', 
                       # health insurance
                          grep('hlthins[0-9]', names(dt), value = T))){
          dt[, paste0(myint) := as.integer(get(myint))]
        }
    
    # create variables ----
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
            dt[thhld_numkid %in% 1:40, children := 1]
            dt[, children := factor(children, levels= 0:1, labels = c("No children", 
                                                                      "Has children"))]
            
            dt[, edu := eeduc]
            dt[edu==1, edu := 2][edu==4, edu := 5] # collapse less than HS into some HS & some college into Associate's
            dt[, edu := factor(edu, levels = c(2, 3, 5, 6, 7), labels = c("<HS", 
                                                                          "HS Grad/GED", 
                                                                          "Some college/Associate degree",
                                                                          "Bachelor degree",
                                                                          "Graduate degree"))]            
            
            dt[, ethn := rrace][rhispanic == 2, ethn := 5] # create Hispanic as race
            dt[, ethn := factor(ethn, levels = 1:5 , labels = c("White alone", 
                                                                "Black alone", 
                                                                "Asian alone", 
                                                                "Other/Multiracial", 
                                                                "Hispanic") )]       
            
            dt[, gender_id := factor(rgenid_describe, levels = 1:4, 
                                     labels = c("Male", 
                                                "Female", 
                                                "Transgender", 
                                                "Nonbinary/I use a different term"))]
            
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
            

            dt[, orientation := factor(sexual_orientation_rv, levels = 1:4, 
                                       labels = c("Gay or Lesbian", 
                                                  "Straight, that is not gay or lesbian", 
                                                  "Bisexual", 
                                                  "I use a different term"))]
            
            dt[, rrace := factor(rrace, levels = 1:4 , labels = c("White alone", 
                                                                  "Black alone", 
                                                                  "Asian alone", 
                                                                  "Other or multiracial") )]
            
            dt[, rhispanic := factor(rhispanic, levels = 1:2, labels = c("No", "Yes"))] # YES, I confirmed that 1=No & 2=Yes.
            
            dt[, sex_at_birth := factor(egenid_birth, levels = 1:2, labels = c("Male", "Female"))] # sex at birth
            
            # Census Bureau definition of LGBT
            dt[as.character(sex_at_birth) == as.character(gender_id) & 
                 orientation == "Straight, that is not gay or lesbian", 
               lgbt := "Non-LGBT"]
            dt[as.character(sex_at_birth) != as.character(gender_id) |
                 orientation %in% c("Gay or Lesbian", "Bisexual", 'I use a different term') |
                 gender_id == "Transgender", 
               lgbt := "LGBT"]
            # dt[, .N, .(lgbt, orientation, sex_at_birth, gender_id)]
            
            # APDE definition of LGBTQ
            dt[, lgbtq := lgbt]
            dt[lgbtq=="Non-LGBT", lgbtq := "Non-LGBTQ"]
            dt[lgbtq=="LGBT", lgbtq := "LGBTQ"]
            dt[gender_id == "Nonbinary/I use a different term" | 
                 orientation %in% c("I use a different term"), 
               lgbtq := "LGBTQ"]        
            
        # disability ----
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
                
            # Receipt of K-12 education in last 7 days (no longer beginning with 3.9)----
                # setnames(dt, 'rcveduc1', 'teach_35_school_in_person')
                # setnames(dt, 'rcveduc2', 'teach_35_school_realtime')
                # setnames(dt, 'rcveduc3', 'teach_35_school_asynchronous')
                # setnames(dt, 'rcveduc4', 'teach_35_school_paper')
                # setnames(dt, 'rcveduc5', 'teach_35_own_materials')
                # setnames(dt, 'rcveduc6', 'teach_35_none_closed')
                # setnames(dt, 'rcveduc7', 'teach_35_none_sick')
                # setnames(dt, 'rcveduc8', 'teach_35_none_break')
                # setnames(dt, 'rcveduc9', 'teach_35_other')
                
                for(ii in grep("teach_35_", names(dt), value = T)){
                  dt[(tenrollpub > 0 | tenrollprv > 0) & is.na(get(ii)), paste0(ii) := 0]
                }
                
            # No computer / no internet ----
                # computer & internet availability not present in Phase 3.2+
                
            # time ----
                # dt[, schlhrs := factor(schlhrs, levels = 1:4, labels = c("None", "1 day", "2-3 days", "4 or more days"))]
                # dt[is.na(enroll_school) | enroll_school == 0, c("schlhrs") := NA]

            # school food ----
                dt[schlfdhlp_rv1 == 2, schlfdhlp1 := 0] # free meals at school
                dt[schlfdhlp_rv2 == 2, schlfdhlp_rv2 := 0] # reduced price meals at school
                dt[schlfdhlp_rv3 == 2, schlfdhlp_rv3 := 0] # full price meals at school
                dt[schlfdhlp_rv4 == 2, schlfdhlp_rv4 := 0] # pick up free meals at school or other location
                dt[schlfdhlp_rv5 == 2, schlfdhlp_rv5 := 0] # received EBT card
                dt[schlfdhlp_rv6 == 2, schlfdhlp_rv6 := 0] # eat free meals at location other than school
                dt[schlfdhlp_rv7 == 2, schlfdhlp_rv7 := 0] # have free meals delivered
                dt[schlfdhlp_rv8 == 2, schlfdhlp_rv8 := 0] # none of the above

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

            # federal benefits
            dt[!(is.na(fdbenefit1) & is.na(fdbenefit2) & is.na(fdbenefit3)), fdbenefitsnap := 0][fdbenefitsnap ==0 & fdbenefit1 == 1, fdbenefitsnap := 1]
            dt[!(is.na(fdbenefit1) & is.na(fdbenefit2) & is.na(fdbenefit3)), fdbenefitwic := 0][fdbenefitwic ==0 & fdbenefit2 == 1, fdbenefitwic := 1]
            dt[!(is.na(fdbenefit1) & is.na(fdbenefit2) & is.na(fdbenefit3)), fdbenefitnone := 0][fdbenefitnone == 0 & fdbenefit3 == 1, fdbenefitnone := 1]
            dt[, c('fdbenefit3', 'fdbenefit2', 'fdbenefit1') := NULL]
            
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
          
            # Medicaid ----
                # dropped as of Phase 4, Cycle 1
              
            # Mental Health ----
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
                
                # Children's mental health questions added for Phase 3.9
                dt[, mhlth_need := factor(mhlth_need,
                                          levels = 1:3, 
                                          labels = c('Yes, all children needed mental health treatment', 
                                                     'Yes, some but not all children needed mental health treatment', 
                                                     'No, none of the children needed mental health treatment'))]
                dt[, mhlth_get := factor(mhlth_get, 
                                 levels = 1:3, 
                                 labels = c('Yes, all children who needed treatment received it', 
                                            'Yes, but only some children who needed treatment received it', 
                                            'No, none of the children who needed treatment received it'))]
                dt[, mhlth_satisfd := factor(mhlth_satisfd, 
                                 levels = 1:3, 
                                 labels = c('Satisfied with all of the mental health treatment the children received', 
                                            'Satisfied with some but not all of the mental health treatment the children received', 
                                            'Not satisfied with the mental health treatment the children received'))]
                dt[, mhlth_diffclt := factor(mhlth_diffclt, 
                                 levels = 1:5, 
                                 labels = c('Not difficult', 
                                            'Somewhat difficult',
                                            'Very difficult',
                                            'Unable to get treatment due to difficulty',
                                            'Did not try to get treatment'))]
                
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
            # setnames(dt, "trentamt", "rent_monthly") # not available since 3.9
                
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
            dt[, foreclosure := factor(foreclose, levels = 1:4, labels = c("Likely",
                                                                          "Likely",
                                                                          "Unlikely", 
                                                                          "Unlikely"))]
            
          # Forced to leave within 2 months (AMONG THOSE NOT CURRENT) ----
            # Due to eviction OR foreclosure, create here, set as factor below
            # factor version will be binary .... extremely/very = likely, somewhat/not at all = unlikely
            dt[, leave2mo := evict][!is.na(foreclose), leave2mo := foreclose]
            dt[, leave2mo := factor(leave2mo, levels = 1:4, labels = c("Likely",
                                                                       "Likely",
                                                                       "Unlikely", 
                                                                       "Unlikely"))]
            
          # Forced to leave within 2 months (AMONG THOSE WHO ANSWERED TENURE QUESTION) ----
            # Only those who answered tenure question could answer follow-up housing questions
            # Will have to assume that those who have tenure and didn't answer eviction/foreclosure
            # because they were current on payments ... they must be (4) Not at all likely
            dt[, leave2mo_alt := evict][!is.na(foreclose), leave2mo_alt := foreclose]
            dt[!is.na(tenure) & is.na(leave2mo_alt), leave2mo_alt := 4]
            dt[, leave2mo_alt := factor(leave2mo_alt, levels = 1:4, labels = c("Likely",
                                                                               "Likely",
                                                                               "Unlikely", 
                                                                               "Unlikely"))]
          # Pressure to move (starting Phase 3.9) ----
            # identify whether answered any of the HSE7rev (movewhy) questions so that
            # can have proper denominator among respondents
            for(numby in 1:8){
              dt[!is.na(get(paste0('movewhy', numby))), whynot_denominator := 1]}

            # now encode zero values when there was a response to HSE7rev
            for(numby in 1:8){
              dt[whynot_denominator == 1 & is.na(get(paste0('movewhy', numby))), 
                 paste0('movewhy', numby) := 0]}
            
            dt[, movewhy1 := factor(movewhy1, level = 0:1, label = c('No', 'Yes'))]
            dt[, movewhy2 := factor(movewhy2, level = 0:1, label = c('No', 'Yes'))]
            dt[, movewhy3 := factor(movewhy3, level = 0:1, label = c('No', 'Yes'))]
            dt[, movewhy4 := factor(movewhy4, level = 0:1, label = c('No', 'Yes'))]
            dt[, movewhy5 := factor(movewhy5, level = 0:1, label = c('No', 'Yes'))]
            dt[, movewhy6 := factor(movewhy6, level = 0:1, label = c('No', 'Yes'))]
            dt[, movewhy7 := factor(movewhy7, level = 0:1, label = c('No', 'Yes'))]
            dt[, movewhy8 := factor(movewhy8, level = 0:1, label = c('No', 'Yes'))]
            dt[, moved := factor(moved, levels = 1:2, labels = c("No", "Yes"))]
            
        # Social support ----
          # added Phase 4, Cycle 1
            # How often do you get the social and emotional support you need?
              dt[, social1 := factor(social1, 
                                     levels = 1:5, 
                                     labels = c('Always', 'Usually', 'Sometimes', 'Rarely', 'Never'))]
            
            # How often do you feel lonely?
              dt[, social2 := factor(social2, 
                                     levels = 1:5, 
                                     labels = c('Always', 'Usually', 'Sometimes', 'Rarely', 'Never'))]
              
            # In a typical week, how often do you talk on the telephone with family, friends, or neighbors?
              dt[, support1 := factor(support1, 
                                     levels = 1:4, 
                                     labels = c('Less than once a week', '1 or 2 times a week', 
                                                '3 or 4 times a week', '5 or more times a week'))]      
              
            # How often do you get together with friends or relatives?
              dt[, support2 := factor(support2, 
                                     levels = 1:4, 
                                     labels = c('Less than once a week', '1 or 2 times a week', 
                                                '3 or 4 times a week', '5 or more times a week'))]
            
            # How often do you attend church or religious services?
              dt[, support3 := factor(support3, 
                                     levels = 1:4, 
                                     labels = c('Less than once a week', '1 or 2 times a week', 
                                                '3 or 4 times a week', '5 or more times a week'))] 
              
            # How often do you attend meetings of the clubs or organizations you belong to?
              dt[, support4_rv := factor(support4_rv, 
                                     levels = 1:5, 
                                     labels = c('I do not belong to a group', 
                                                'Never or less than once a year', 
                                                '1 to 3 times per year', 
                                                '4 to 11 times a year', 
                                                '12 or more times per year'))]              
              
      # Misc ----
          dt[, c("msa", "washington") := 0]
          dt[est_st == 53, washington := 1]
          dt[est_msa == 42660, msa := 1] # actually seattle-tacoma-bellevue metro area
          
          dt[, phase := this_phase]
          
          dt[, constant := 1]

    # create pooled weights for all weeks combined ----
        myweights <- grep("pweight", names(dt), value = T) # just get person level weights since currently (12/30/2021) do not need household level estimates
        
        dt[, gsub("pweight", "pooledNwt", myweights) := lapply(.SD, function(X){X/length(unique(dt$cycle))}), .SDcols = myweights]
            

## Survey Set for WA & MSA ----
    # Washington ----
        svy_wa <- survey_set_weekly(dt[washington==1])
        pooledN_svy_wa <- survey_set_pooled(dt[washington==1]) 
        
    # MSA ----
        svy_msa <- survey_set_weekly(dt[msa==1])
        pooledN_svy_msa <- survey_set_pooled(dt[msa==1]) 

## Tidy environment ----
  rm(list = grep("^pooledN|^svy_|^dt$|inputdir|outputdir|my_state_code|my_msa_code|phase_number|phase.number|first.cycle", ls(), value = T, invert = T)) 
        
## The end ----
