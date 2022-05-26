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
    # Notes: Phase 3_1 differs from Phase 2 & 3 and it is also a different stage of the pandemic, so it will be analyzed separately
    # 

## set up ----
    rm(list=ls())
    pacman::p_load(data.table, srvyr, rads)

## load survey & replicate weights ----
    source("https://raw.githubusercontent.com/PHSKC-APDE/pulse_survey/main/00_filepaths.R")
    setwd(paste0(inputdir, "phase3_1_unzipped/"))

    keepers <- list.files(pattern = "_repwgt")
    svy <- rbindlist(lapply(gsub("_repwgt", "", keepers),FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    rep <- rbindlist(lapply(keepers,FUN=function(mycsv){print(mycsv); fread(mycsv)}), use.names = T, fill = T)
    
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
        # childcare ----
            dt[, chldcare := factor(chldcare, levels = 1:2, labels = c("Yes", "No"))]
    
            for(p in paste0("chldimpct", 1:9)){
              dt[is.na(get(p)) & chldcare=="Yes", paste0(p) := 0]
              dt[, paste0(p) := factor(get(p), levels = 0:1, labels = c("No", "Yes"))]
            }
    
        # demographics ----
            dt[, age := 2020-tbirth_year] # generate age
    
            dt[ms %in% c(2, 3, 4), ms := 2] # marital status
            
            dt[thhld_numkid == 0, children := 0]
            dt[thhld_numkid %in% 1:7, children := 1]
            
            dt[, edu := eeduc]
            dt[edu==1, edu := 2][edu==4, edu := 5] # collapse less than HS into some HS & some college into Associate's
    
            dt[, ethn := rrace][rhispanic == 2, ethn := 5] # create hispanic as race
            
            dt[, alone := 0][thhld_numper == 1, alone := 1] # living alone, which is associated with mental health
            
        # disability ----
            # - From Lin Song on 6/17
            # disability: create a disability “demographic” variable based on the four disability questions 
            # (seeing, hearing, remembering, and mobility) with responses  
            # 1 (No – no difficulty) and 2 (Yes – some difficulty) as No and 
            # 3 (Yes – a lot of difficulty) and 4 (Cannot do at all) as Yes 
            # (any Yes as Yes, the rest as No for overall disability).  
            
            dt[!is.na(seeing) & !is.na(hearing) & !is.na(remembering) & !is.na(mobility), disability := "No"] # can only know disability status if it is not missing. Set No as default
            for(dis in c("seeing", "hearing", "mobility", "remembering")){dt[get(dis) %in% 3:4, disability := "Yes"]}
            
        # education ----
            # enrollment ----
                dt[, enroll_school := rowSums(.SD, na.rm = T), .SDcols = c("tenrollpub", "tenrollprv")] # hh with child enrolled in public or private school
                dt[enroll_school >=1, enroll_school := 1]
                dt[, enroll_home := tenrollhmsch][is.na(enroll_home), enroll_home := 0] # hh with child home schooled
                dt[enroll_home >=1, enroll_home := 1]
                dt[, enroll_none := enrollnone][is.na(enroll_none), enroll_none := 0] # hh without child in school
                dt[thhld_numkid == 0, c("enroll_school", "enroll_home", "enroll_none") := NA] # properly set NA values

            # teaching modality ----
                dt[, teach_31_inperson := teach1][is.na(teach1), teach_31_inperson := 0] # only enroll_school >= 1
                dt[, teach_31_online_live := teach2][is.na(teach2), teach_31_online_live := 0] # only enroll_school >= 1
                dt[, teach_31_asynchronous := teach3][is.na(teach3), teach_31_asynchronous := 0] # only enroll_school >= 1
                dt[, teach_31_paper := teach4][is.na(teach4), teach_31_paper := 0] # only enroll_school >= 1
                dt[, teach_31_own_material := teach5][is.na(teach5), teach_31_own_material := 0] # only enroll_school >= 1
                dt[, teach_31_none_closed := teach6][is.na(teach6), teach_31_none_closed := 0] # only enroll_school >= 1
                dt[, teach_31_none_sick := teach7][is.na(teach7), teach_31_none_sick := 0] # only enroll_school >= 1
                dt[, teach_31_other := teach8][is.na(teach8), teach_31_other := 0] # only enroll_school >= 1
                dt[hybrid == 1, teach_31_hybrid := hybrid] # only enroll_school >= 1
                dt[hybrid == 2, teach_31_hybrid := 0] # only enroll_school >= 1
                dt[is.na(enroll_school) | enroll_school == 0, grep("teach_", names(dt), value = T) := NA]

            # No computer / no internet ----
                dt[, compavail.binary := factor(compavail, levels = 1:5, labels = c("Available", "Available", "Not available", "Not available", "Not available"))] # only enroll1==1
                dt[, compavail := factor(compavail, levels = 1:5, labels = c("Always", "Usually", "Sometimes", "Rarely", "Never"))] # only enroll1==1
                dt[is.na(enroll_school) | enroll_school == 0, c("compavail", "compavail.binary") := NA]
                
                dt[, intrntavail.binary := factor(intrntavail, levels = 1:5, labels = c("Available", "Available", "Not available", "Not available", "Not available"))] # only enroll1==1
                dt[, intrntavail := factor(intrntavail, levels = 1:5, labels = c("Always", "Usually", "Sometimes", "Rarely", "Never"))] # only enroll1==1
                dt[is.na(enroll_school) | enroll_school == 0, c("intrntavail", "intrntavail.binary") := NA]
                
                dt[!is.na(compavail.binary) & !is.na(intrntavail.binary), nocomp := "Available"] 
                dt[intrntavail.binary == "Not available" | compavail.binary == "Not available", nocomp := "Not available"]                 
                dt[is.na(enroll_school) | enroll_school == 0, c("nocomp") := NA]
                dt[, nocomp := factor(nocomp)]
                
                dt[, intrnt_school := intrntrv1][is.na(intrntrv1), intrnt_school := 0] # only intrntavail != "Never"
                dt[, intrnt_family := intrntrv2][is.na(intrntrv2), intrnt_family := 0] # only intrntavail != "Never"
                dt[, intrnt_other := intrntrv3][is.na(intrntrv3), intrnt_other := 0] # only intrntavail != "Never"
                dt[, intrnt_notathome := intrntrv4][is.na(intrntrv4), intrnt_notathome := 0] # only intrntavail != "Never"
                dt[!intrntavail %in% c("Always",  "Usually", "Sometimes", "Rarely"), c("intrnt_school", "intrnt_family", "intrnt_other", "intrnt_notathome") := NA]
                
            # time ----
                dt[, schlhrs := factor(schlhrs, levels = 1:4, labels = c("None", "1 day", "2-3 days", "4 or more days"))]
                dt[is.na(enroll_school) | enroll_school == 0, c("schlhrs") := NA]
                
            # school food ----
                dt[schlfood == 2, schlfood := 0] # food assistance from school in last 7 days
                dt[is.na(enroll_school) | enroll_school == 0, c("schlfood") := NA]
                
                dt[schlfdhlp1 == 2, schlfdhlp1 := 0][is.na(schlfdhlp1), schlfdhlp1 := 0] # picked up food from school or other location
                dt[schlfdhlp2 == 2, schlfdhlp2 := 0][is.na(schlfdhlp2), schlfdhlp2 := 0] # received ebt card
                dt[schlfdhlp3 == 2, schlfdhlp3 := 0][is.na(schlfdhlp3), schlfdhlp3 := 0] # ate meals onsite
                dt[schlfdhlp4 == 2, schlfdhlp4 := 0][is.na(schlfdhlp4), schlfdhlp4 := 0] # meals delivered
                
                dt[is.na(schlfood) | schlfood !=1 , c("schlfdhlp1", "schlfdhlp2", "schlfdhlp3", "schlfdhlp4") := NA]
                
                
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
            
            # total $ spent on food in last 7 days
            dt[, tspndtotal := tspndfood + tspndprpd] 
            
        # health ----
            # Insurance ----
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
          
            # Mental Health ----
                for(blah in c("prescript", "mh_svcs", "mh_notget") ){dt[get(blah) == 2, (paste0(blah)) := 0]} # Q 38a, 38b, 38c
          
                dt[, phq4total := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)]
                dt[, phq4severe := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)][phq4severe < 9, phq4severe := 0][phq4severe >= 9, phq4severe := 1]
                dt[, phq4mod_severe := (anxious - 1) + (worry - 1) + (interest - 1) + (down - 1)][phq4mod_severe < 6, phq4mod_severe := 0][phq4mod_severe >= 6, phq4mod_severe := 1]
                dt[, phq4anxiety := (anxious - 1) + (worry - 1)][phq4anxiety < 3, phq4anxiety := 0][phq4anxiety >= 3, phq4anxiety := 1]
                dt[, phq4depression := (interest - 1) + (down - 1)][phq4depression < 3, phq4depression := 0][phq4depression >= 3, phq4depression := 1]
                
                for(blah in c("down", "anxious", "worry", "interest")){dt[get(paste0(blah)) %in% 1:2, (paste0(blah)) := 0]; dt[get(paste0(blah)) %in% 3:4, (paste0(blah)) := 1]}
                
            # Telehealth ----
                dt[, telehlth := factor(telehlth, levels = 1:2, labels = c("Yes", "No"))]
                dt[, telechld := factor(telechld, levels = 1:2, labels = c("Yes", "No"))]
                
            # Preventive care ----
                dt[, prvntive  := factor(prvntive, levels = 1:2, labels = c("Yes", "No"))]
                
                for(p in paste0("prvntwhy", 1:7)){
                     dt[is.na(get(p)) & prvntive=="Yes", paste0(p) := 0]
                     dt[, paste0(p) := factor(get(p), levels = 0:1, labels = c("No", "Yes"))]
                }
  
                
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
            
        # Vaccine ----      
          for(blah in c("recvdvacc", "doses") ){dt[get(blah) == 2, (paste0(blah)) := 0]} # QV1 & QV2
          dt[getvacrv == 1, definitevacc := 1][getvacrv %in% 2:5 , definitevacc := 0] # QV3
          for(col in grep("whynot[0-9]", names(dt), value = T)){dt[(doses == 0 | getvacrv %in% 2:5) & is.na(get(col)), paste0(col) := 0]} # QV4
          for(col in grep("whynotb[0-9]", names(dt), value = T)){dt[whynot3 == 1 & is.na(get(col)), paste0(col) := 0]} # QV5
          dt[hadcovid == 1, prevcovid := 1][hadcovid %in% 2:3, prevcovid := 0] # QV6
          
        # Misc ----
            dt[, c("msa", "washington") := 0]
            dt[est_st == 53, washington := 1]
            dt[est_msa == 42660, msa := 1] # actually seattle-tacoma-bellevue metro area
            
            dt[, phase := 3.1]
            
            dt[, constant := 1]

    # create pooled weights for all weeks combined ----
        myweights <- grep("pweight", names(dt), value = T) # just get person level weights since currently (11/5/2020) do not need household level estimates
        
        dt[, gsub("pweight", "pooledNwt", myweights) := lapply(.SD, function(X){X/length(unique(dt$week))}), .SDcols = myweights]
            
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

        dt[, foreclosure := factor(forclose, levels = 1:4, labels = c("Likely",
                                                                      "Likely",
                                                                      "Unlikely", 
                                                                      "Unlikely"))]
        dt[, gender := factor(egender, levels = 1:2, labels = c("Male", 
                                                                "Female"))]
        
        dt[, hearing := factor(hearing, levels = 1:4, labels = c("No difficulty", 
                                                                "Some difficulty", 
                                                                "A lot of difficulty", 
                                                                "Cannot do at all"))]
        
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
        
        dt[, mobility := factor(mobility, levels = 1:4, labels = c("No difficulty", 
                                                                 "Some difficulty", 
                                                                 "A lot of difficulty", 
                                                                 "Cannot do at all"))]
        
        dt[, ms := factor(ms, levels = c(1, 2, 5), labels = c("Married", 
                                                              "Widowed/Divorced/Separated", 
                                                              "Never married") )]
        
        dt[, remembering := factor(remembering, levels = 1:4, labels = c("No difficulty", 
                                                                 "Some difficulty", 
                                                                 "A lot of difficulty", 
                                                                 "Cannot do at all"))]
        
        dt[, rrace := factor(rrace, levels = 1:4 , labels = c("White alone", 
                                                              "Black alone", 
                                                              "Asian alone", 
                                                              "Other or multiracial") )]
        dt[, rhispanic := factor(rhispanic, levels = 1:2, labels = c("No", 
                                                                     "Yes"))] # YES, I confirmed that 1=No & 2=Yes.
        
        dt[, seeing := factor(seeing, levels = 1:4, labels = c("No difficulty", 
                                                                "Some difficulty", 
                                                                "A lot of difficulty", 
                                                                "Cannot do at all"))]
        
        dt[, tenure2 := factor(tenure, levels = 1:4, labels = c("Owned free & clear", 
                                                                "Owned with a mortgage or loan", 
                                                                "Rented", 
                                                                "Occupied without payment"))]
        dt[, worry := factor(worry, levels = 0:1, labels = c("< 1/2 prior 7 days", 
                                                             ">= 1/2 prior 7 days"))]

        noyesvars <- c("alone", "delay", "notget", "hlthins1", "phq4mod_severe", "phq4severe", "phq4anxiety", "phq4depression", "recvdvacc", 
                       "doses", "definitevacc", "prescript", "mh_svcs", "mh_notget", 
                       grep("whynot[0-9]", names(dt), value = T), grep("whynotb[0-9]", names(dt), value = T), "prevcovid")
        dt[, (noyesvars) := lapply(.SD, function(X){factor(X, levels = 0:1, labels = c("No", "Yes") )}), .SDcols = noyesvars]

        yesnovars <- c("wrklossrv", "expctloss", "anywork", "freefood")
        dt[, (yesnovars) := lapply(.SD, function(X){factor(X, levels = 1:2, labels = c("Yes", "No") )}), .SDcols = yesnovars]

## Survey Set for WA & MSA ----
    # Washington ----
        pooledN_svy_wa3_1 <-  # PHASE 3.1 in Washington
          srvyr::as_survey_rep(
            copy(dt[washington==1])  ,
            weights = pooledNwt ,
            combined.weights = TRUE ,
            repweights = grep('pooledNwt[0-9]+', names(dt), value  = T) ,
            scale = 4 / 80 ,
            rscales = rep( 1 , 80 ) ,
            mse = TRUE ,
            type = "JK1"
          )
        pooledN_svy_wa3_1 <- dtsurvey::dtrepsurvey(pooledN_svy_wa3_1) # survey set for RADS
        
        svy_wa3_1 <-  # PHASE 3.1 in Washington (for individual weeks)
          srvyr::as_survey_rep(
            copy(dt[washington==1])  ,
            weights = pweight ,
            combined.weights = TRUE ,
            repweights = grep('pweight[0-9]+', names(dt), value  = T) ,
            scale = 4 / 80 ,
            rscales = rep( 1 , 80 ) ,
            mse = TRUE ,
            type = "JK1"
          )
        
        svy_wa3_1 <- dtsurvey::dtrepsurvey(svy_wa3_1) # survey set for RADS
        
    # MSA ----
        pooledN_svy_msa3_1 <-   # PHASE 3.1 in Seattle-Tacoma-Bellevue
          srvyr::as_survey_rep(
            copy(dt[msa==1])  ,
            weights = pooledNwt ,
            combined.weights = TRUE ,
            repweights = grep('pooledNwt[0-9]+', names(dt), value  = T) ,
            scale = 4 / 80 ,
            rscales = rep( 1 , 80 ) ,
            mse = TRUE ,
            type = "JK1"
          )
        pooledN_svy_msa3_1 <- dtsurvey::dtrepsurvey(pooledN_svy_msa3_1) # survey set for RADS
        
        svy_msa3_1 <-   # PHASE 3.1 in Seattle-Tacoma-Bellevue (for individual weeks)
          srvyr::as_survey_rep(
            copy(dt[msa==1])  ,
            weights = pweight ,
            combined.weights = TRUE ,
            repweights = grep('pweight[0-9]+', names(dt), value  = T) ,
            scale = 4 / 80 ,
            rscales = rep( 1 , 80 ) ,
            mse = TRUE ,
            type = "JK1"
          )
        svy_msa3_1 <- dtsurvey::dtrepsurvey(svy_msa3_1) # survey set for RADS
        
## The end ----
