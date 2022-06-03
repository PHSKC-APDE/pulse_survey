# svy_pulse

Analyze US [Census Bureau](https://www.census.gov/en.html) [Household Pulse Survey](https://www.census.gov/programs-surveys/household-pulse-survey.html) Public Use File ([PUF](https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html)) data in R

Code last updated June 3, 2022 and includes data through Phase 3, week 45 (April 27 - May 9, 2022).

# Description

This repository contains R code that was used by [Public Health — Seattle & King County](https://kingcounty.gov/depts/health.aspx) to analyze the Census Bureau's [Household Pulse Survey](https://www.census.gov/programs-surveys/household-pulse-survey.html) for use in our COVID response and to produce our [Economic, social, and overall health impacts dashboard](https://kingcounty.gov/depts/health/covid-19/data/impacts.aspx). 

The [Household Pulse Survey](https://www.census.gov/programs-surveys/household-pulse-survey.html) is an *experimental*  rapid response survey developed to assess household experiences during the COVID-19 pandemic. It was designed to collect data regarding a variety of household factors including, but not limited to: childcare, education, employment, food security, housing, and vaccinations. The survey is designed to produce estimates for those 18+ years old living within household units. Estimates can be generated at three geographic levels: (1) national, (2) 50 states + Washington, DC, and (3) the 15 largest Metropolitan Statistical Areas (MSAs). 

The survey has evolved over time, with each iteration designated as a `Phase` and each survey sample within a Phase designated a `Week`. Please note that survey weeks do not correspond to calendar weeks and that survey questions and the survey methodology have evolved over time.   

      | Phase | Dates               | Week    |
      | ----- | ------------------- | ------- |
      | 1     | Apr 2020 – Jul 2020 | 1 – 12  |
      | 2     | Aug 2020 – Oct 2020 | 13 – 17 |
      | 3     | Oct 2020 – Mar 2021 | 18 – 27 |
      | 3.1   | Apr 2021 – Jul 2021 | 28 – 33 |
      | 3.2   | Jul 2021 – Oct 2021 | 34 – 39 |
      | 3.3   | Dec 2021 - Feb 2022 | 40 – 42 |
      | 3.4   | Mar 2022 - May 2022 | 43 – 45 |
      | 3.5   | May 2022 - ???      | 46 - ?? |

# Use cases

The primary use is to prepare data for the analyses needed for the [Economic, social, and overall health impacts dashboard](https://kingcounty.gov/depts/health/covid-19/data/impacts.aspx). 

In addition, we hope that it can serve as a template to facilitate analyses for other health departments and interested parties. 

## Data inputs
All data are downloaded from the Pulse Survey [Public Use File (PUF)](https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html) website. Downloaded files are unzipped and stored together with other weeks from the same phase. The files are not modified in any way. Here is a snapshot of our file organization: 

        ├── census_pulse
        |   ├── input
        │   |   ├── phase1_unzipped
        │   |   │   ├── pulse2020_repwgt_puf_01.csv
        │   |   │   ├── pulse2020_puf_01.csv
        │   |   │   ├── pulse2020_repwgt_puf_02.csv
        │   |   │   ├── pulse2020_puf_02.csv
        │   |   │   ├── ...
        │   |   │   ├── pulse2020_repwgt_puf_12.csv
        │   |   │   ├── pulse2020_puf_12.csv
        │   |   ├── phase2_unzipped
        │   |   │   ├── pulse2020_repwgt_puf_13.csv
        │   |   │   ├── pulse2020_puf_13.csv
        │   |   │   ├── pulse2020_repwgt_puf_14.csv
        │   |   │   ├── pulse2020_puf_14.csv
        │   |   │   ├── ...
        │   |   │   ├── pulse2020_repwgt_puf_17.csv
        │   |   │   ├── pulse2020_puf_17.csv
        │   |   ├── phase3_unzipped
        │   |   ├── ...
        │   |   ├── phase3_4_unzipped

## Data outputs
Depending on the survey Phase, the code will produce Excel files for up to six topical directories (see diagram below). In each directory there will be a file named `pulse_results.xlsx` that contains estimates for each individual week as well as estimates for each phase as whole. Excel files are also generated for each individual Phase with a date stamp as part of the file name so that you always have a historical record of your previous runs. 

Note that the output labeled `pulse_phase2_results_YYYY_MM_DD.xlxs` contains estimates for phases 2 & 3 combined because the surveys are 100% identical in structure.

        ├── census_pulse
        |   ├── output
        │   |   ├── behavioral_health
        │   |   │   ├── pulse_results.xlsx
        │   |   │   ├── pulse_phase1_results_2022_01_25.xlsx
        │   |   │   ├── pulse_phase1_results_2021_07_13.xlsx
        │   |   │   ├── pulse_phase2_results_2022_01_24.xlsx
        │   |   │   ├── ...
        │   |   │   ├── pulse_phase3_4_results_2022_05_18.xlsx
        │   |   ├── education
        │   |   │   ├── ...
        │   |   ├── food_security
        │   |   │   ├── ...
        │   |   ├── health_insurance
        │   |   │   ├── ...
        │   |   ├── housing
        │   |   │   ├── ...
        │   |   ├── vaccination
        │   |   │   ├── ...

Each Excel workbook contains the following worksheets:
1. **pulse**: 
   * the results table, with columns for week, source, geo (WA or Seattle MSA), variable (e.g., insurance), level (e.g., Private, Public, Uninsured), category (e.g., age), group (e.g., 18-24 yrs, 25-44 yrs, 45-64 yrs, etc.), percent, standard error, lower confidence interval, upper confidence interval, relative standard error, numerator, denominator, phase, suppression, and caution. 
   * *Note!* The geo column will have 'WA' and 'MSA' as values even if you change the codes to select different geographies (see 'Set up your programming environment' below). This does not mean that the calculations were performed for Washington State.
2. **variable**: 
   * a description of all the variables used in the pulse worksheet
3. **dictionary**: 
   * a description of the columns used in the pulse worksheet
4. **week**: 
   * a record of all the survey phases, weeks, and their corresponding calendar dates

## Protocol

### Before you begin!
This code generates estimates for specific variables stratified by demographics that are of importance to [Public Health — Seattle & King County](https://kingcounty.gov/depts/health.aspx). If you want to recreate the same analysis for your specific state or metropolitan statistical area (MSA), you're in luck! However, if you want to change this code for additional variables or demographics, you will need to dive deep into the relevant survey instruments and their documentation. The necessary information is provided on the [Pulse Survey website](https://www.census.gov/programs-surveys/household-pulse-survey.html). Unfortunately, we do not have capacity to help others adapt this code in this way. 

### Set up your file directories
  - Create the file directory structure expected by this code, which is given in in the **data inputs** and **data outputs** sections above. E.g., somewhere on your computer create a directory named `census_pulse` and create two sub-directories called `input` and `output`, and then create the directories that nest within `input` and `output`. 
  - Go to the [Public Use Files](https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html) webpage and download the CSV version of the zip file for each week. 
  - Unzip the contents of each zip file into the appropriate phase directory. E.g., the contents of `HPS_Week36_PUF_CSV.zip` would be unzipped into `census_pulse/input/phase3_2_unzipped`. 
  
### Set up your programming environment
  - Install necessary packages in R with the following lines of code
    - `install.packages(c("data.table", "srvyr", "openxlsx", "pacman", "remotes"))`
    - `remotes::install_github('PHSKC-APDE/rads', auth_token = NULL)`
  - Clone this Git repository to your local machine. If you use Git Bash you can type the following: 
    - `git clone https://github.com/PHSKC-APDE/svy_pulse.git`
  - Open your local copy of [00_constants_n_functions.R](https://github.com/PHSKC-APDE/svy_pulse/blob/main/00_constants_n_functions.R) and update the following:
    - the complete file paths leading to your `input` and `output` folders. 
      - Note that R file paths use *forward leaning slashes*, i.e., `/`.  
    - the numeric value for your state's code (`my_state_code`), which can be found in any Pulse Survey data dictionary under `EST_ST`
    - the numeric value for your Metropolitan Statistical Area code (`my_msa_code`), which can be found in any Pulse Survey data dictionary under `EST_MSA`

### Run the code
There are three ways you can run the code. Regardless of which way you choose, the analysis will take a while so plan to take lunch or work on other projects while the code is running. 
  
1. Batch
   * definitely the easiest / fastest method
   * will run the code for every Phase and week and store all the results neatly in your output folder 
     * open your local copy of `_run_all_pulse_analyses.R`
     * edit the first part of each file path to point to where you cloned the R code on to your local machine. 
       * E.g., replace `"https://raw.githubusercontent.com/PHSKC-APDE/svy_pulse/main/"` with `"C:/code/svy_pulse/"`
     * run the entire script
     * wait for *a long* time (it takes ~1.5 hours on my machine)
     * get your results
2. One phase at a time
   * suggest using this method when you want to refresh your estimates to include an additional week in the most recent phase
     * run a phase specific `01_phase##_analysis.R` file, e.g., `01_phase3_4_analysis.R`, etc.
     * Note, these files will automatically call the proper data prep files 
3. Step by step
   * useful for trouble shooting when you are adding new variables to your analysis or an writing code for an entire new Phase
     * run the appropriate data prep code, e.g., `00_phase3_5_prep_survey.R` and check for error messages
     * when the data prep is working properly, run the corresponding analysis code, e.g., `01_phase3_5_analysis.R`

  
# Available analytic categories & Data dictionary
Here is a simplified table of demographic stratification variables used in our analyses. Note that some are only used for particular topical analyses in particular phases.

| category       | group                         | category\_description                                               |
| -------------- | ----------------------------- | ------------------------------------------------------------------- |
| age            | 18-24                         | age groups                                                          |
| age            | 25-44                         |                                                                     |
| age            | 45-64                         |                                                                     |
| age            | 65+                           |                                                                     |
| alone          | No                            | live alone                                                          |
| alone          | Yes                           |                                                                     |
| anywork        | anywork\_no                   | employed in last 7 days                                             |
| anywork        | anywork\_yes                  |
| children       | No children                   | has children                                                        |
| children       | Has children                  |
| disability     | No                            | disability binary based on seeing, hearing, remembering, & mobility |
| disability     | Yes                           |                                                                     |
| educ           | <HS                           | education level                                                     |
| educ           | Bachelor's degree             |
| educ           | Graduate degree               |
| educ           | HS Grad/GED                   |
| educ           | Some college/Associate degree |
| expctloss      | expctloss\_no                 | expect loss of income in next 4 weeks due to pandemic               |
| expctloss      | expctloss\_yes                |
| gender\_id     | Female                        | gender                                                              |
| gender\_id     | Male                          |                                                                     |
| gender\_id     | None of these                 |
| gender\_id     | Transgender                   |
| income         | $200K+                        | income categories                                                   |
| income         | <$100K                        |                                                                     |
| income         | <$150K                        |                                                                     |
| income         | <$200K                        |                                                                     |
| income         | <$25K                         |                                                                     |
| income         | <$35K                         |                                                                     |
| income         | <$50K                         |                                                                     |
| income         | <$75K                         |                                                                     |
| kindwork       | Family business               | type of employer                                                    |
| kindwork       | Government                    |
| kindwork       | Non-profit                    |
| kindwork       | Private                       |                                                                     |
| kindwork       | Self-employed                 |
| marital        | Married                       | marital status                                                      |
| marital        | Never married                 |
| marital        | Widowed/divorced/separated    |
| orientation    | Bisexual                      | sexual orientation                                                  |
| orientation    | Gay or Lesbian                |
| orientation    | I don't know                  |
| orientation    | Something else                |
| orientation    | Straight                      |                                                                     |
| race           | Asian                         | race / ethnicity                                                    |
| race           | Black                         |                                                                     |
| race           | Hispanic                      |                                                                     |
| race           | Other or multiracial          |
| race           | White                         |                                                                     |
| sex            | Female                        | sex                                                                 |
| sex            | Male                          |                                                                     |
| sex\_at\_birth | Female                        | sex at birth (to distinguish from gender)                           |
| sex\_at\_birth | Male                          |                                                                     |
| wrkloss        | wrkloss\_no                   | experienced loss of employment income in last 4 weeks               |
| wrkloss        | wrkloss\_yes                  |
| wrklossrv      | No                            | experienced loss of employment income in last 4 weeks               |
| wrklossrv      | Yes                           |                                                                     |

The file [`pulse_varlist.csv`](https://github.com/PHSKC-APDE/svy_pulse/blob/main/pulse_varlist.csv) file is a dictionary of all the variables that will be output by this code. It includes selected variables created by the Census Bureau and derivative variables created by the `00_phase##_prep_survey.R` code. I suggest viewing it on your local machine since GitHub may only show the first few columns. A summary is provided here for your convenience.

| variable                 | topic              | description                                                                                                                                                                                                                                                                       |
| ------------------------ | ------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| anxious                  | behavioral\_health | Nervous, axious, or on edge                                                                                                                                                                                                                                                       |
| down                     | behavioral\_health | Felt down, depressed, or hopeless                                                                                                                                                                                                                                                 |
| interest                 | behavioral\_health | Little interest or pleasure in doing things                                                                                                                                                                                                                                       |
| mh\_notget               | behavioral\_health | Last 4 weeks, did you need counseling or therapy from a mental health professional but DID NOT GET it for any reason?                                                                                                                                                             |
| mh\_svcs                 | behavioral\_health | Last 4 weeks, receive counseling/therapy from mental health professional?                                                                                                                                                                                                         |
| phq4anxiety              | behavioral\_health | PHQ-4 score for worry & anxiety only, total points >= 3                                                                                                                                                                                                                           |
| phq4depression           | behavioral\_health | PHQ-4 score for interest & down only, total points >= 3                                                                                                                                                                                                                           |
| phq4mod\_severe          | behavioral\_health | PHQ-4 score of 6-12                                                                                                                                                                                                                                                               |
| phq4severe               | behavioral\_health | PHQ-4 score of 9-12                                                                                                                                                                                                                                                               |
| phq4total                | behavioral\_health | PHQ-4 Total score (BUT for last 7 days, not last two weeks)                                                                                                                                                                                                                       |
| prescript                | behavioral\_health | Last 4 weeks, take any prescription meds to help with emotions, concentration, behavior, or mental health?                                                                                                                                                                        |
| prvntive                 | behavioral\_health | At any time in the last 12 months, did any children in the household miss, delay or skip any PREVENTIVE check-ups because of the coronavirus pandemic?                                                                                                                            |
| prvntwhy1                | behavioral\_health | Reasons children missed or delayed preventive visits: Health care provider’s location was closed due to the coronavirus pandemic                                                                                                                                                  |
| prvntwhy2                | behavioral\_health | Reasons children missed or delayed preventive visits: Health care provider’s location was open but had limited appointments due to the coronavirus pandemic                                                                                                                       |
| prvntwhy3                | behavioral\_health | Reasons children missed or delayed preventive visits: Parent, adult caregiver, or child was concerned about going to the health care provider’s location due to the coronavirus pandemic                                                                                          |
| prvntwhy4                | behavioral\_health | Reasons children missed or delayed preventive visits: This child no longer had health insurance or had a change in health insurance due to the coronavirus pandemic                                                                                                               |
| prvntwhy5                | behavioral\_health | Reasons children missed or delayed preventive visits: Someone in the household was ill with the coronavirus                                                                                                                                                                       |
| prvntwhy6                | behavioral\_health | Reasons children missed or delayed preventive visits: Someone in the household had been in contact with someone who was ill with the coronavirus                                                                                                                                  |
| prvntwhy7                | behavioral\_health | Reasons children missed or delayed preventive visits: None of the above                                                                                                                                                                                                           |
| telechld                 | behavioral\_health | At any time in the last 4 weeks, did any children in the household have an appointment with a doctor, nurse, or other health professional by video or by phone?                                                                                                                   |
| telehlth                 | behavioral\_health | At any time in the last 4 weeks, did you have an appointment with a doctor, nurse, or other health professional by video or by phone?                                                                                                                                             |
| worry                    | behavioral\_health | Not being able to stop or control worrying                                                                                                                                                                                                                                        |
| chldcare                 | education          | At any time in the last 4 weeks, were any children in the household unable to attend daycare or another childcare arrangement because of the coronavirus pandemic?  Please include before school care, after school care, and all other forms of childcare that were unavailable. |
| chldimpct1               | education          | Childcare impact - unpaid leave                                                                                                                                                                                                                                                   |
| chldimpct2               | education          | Childcare impact - used paid leave                                                                                                                                                                                                                                                |
| chldimpct3               | education          | Childcare impact - cut hours                                                                                                                                                                                                                                                      |
| chldimpct4               | education          | Childcare impact - left job                                                                                                                                                                                                                                                       |
| chldimpct5               | education          | Childcare impact - lost job                                                                                                                                                                                                                                                       |
| chldimpct6               | education          | Childcare impact - did not look for job                                                                                                                                                                                                                                           |
| chldimpct7               | education          | Childcare impact - supervised children while working                                                                                                                                                                                                                              |
| chldimpct8               | education          | Childcare impact - other                                                                                                                                                                                                                                                          |
| chldimpct9               | education          | Childcare impact - None of the above                                                                                                                                                                                                                                              |
| comp\_family             | education          | Computer or digital device provided by someone in the household or the family or is the child's                                                                                                                                                                                   |
| comp\_other              | education          | Computer or digital device provided by another source                                                                                                                                                                                                                             |
| comp\_school             | education          | Computer or digital device provided by school or school district                                                                                                                                                                                                                  |
| compavail                | education          | Computer available for educational purposes                                                                                                                                                                                                                                       |
| compavail.binary         | education          | Computer available for educational purposes (binary: always, usually  vs. never, rarely, sometimes)                                                                                                                                                                               |
| enroll\_home             | education          | HH with child who is home schooled                                                                                                                                                                                                                                                |
| enroll\_school           | education          | HH with child enrolled in public or private school                                                                                                                                                                                                                                |
| intrnt\_family           | education          | Internet paid for by someone in the household or family                                                                                                                                                                                                                           |
| intrnt\_family           | education          | Internet paid for by someone in the household or family                                                                                                                                                                                                                           |
| intrnt\_notathome        | education          | Internet service not available in the home                                                                                                                                                                                                                                        |
| intrnt\_other            | education          | Internet paid for another source                                                                                                                                                                                                                                                  |
| intrnt\_other            | education          | Internet paid for another source                                                                                                                                                                                                                                                  |
| intrnt\_school           | education          | Internet paid for by child's school or school district                                                                                                                                                                                                                            |
| intrnt\_school           | education          | Internet paid for by child's school or school district                                                                                                                                                                                                                            |
| intrntavail              | education          | Internet available for educational purposes                                                                                                                                                                                                                                       |
| intrntavail.binary       | education          | Internet available for educational purposes (binary: always, usually  vs. never, rarely, sometimes)                                                                                                                                                                               |
| nocomp                   | education          | Never, rarely, or sometimes had computer access AND/OR internet access                                                                                                                                                                                                            |
| ps\_cancelchange         | education          | Adults with higher eduction plans canceled or changed due to COVID                                                                                                                                                                                                                |
| ps\_chng\_cancel         | education          | Changes to post-secondary education plans: plans for classes this term cancelled                                                                                                                                                                                                  |
| ps\_chng\_diffcert       | education          | Changes to post-secondary education plans: taking classes for different kind of certificate or degree                                                                                                                                                                             |
| ps\_chng\_diffsch        | education          | Changes to post-secondary education plans: taking classes at a different institution                                                                                                                                                                                              |
| ps\_chng\_fewer          | education          | Changes to post-secondary education plans: taking fewer classes                                                                                                                                                                                                                   |
| ps\_chng\_format         | education          | Changes to post-secondary education plans: classes are in a different format (e.g., online)                                                                                                                                                                                       |
| ps\_chng\_more           | education          | Changes to post-secondary education plans: taking more classes                                                                                                                                                                                                                    |
| ps\_chng\_none           | education          | Changes to post-secondary education plans: no changes                                                                                                                                                                                                                             |
| ps\_why\_campuslife      | education          | Reason post-secondary plans changed: changes to campus life                                                                                                                                                                                                                       |
| ps\_why\_chngaid         | education          | Reason post-secondary plans changed: changes to financial aid                                                                                                                                                                                                                     |
| ps\_why\_chngcontent     | education          | Reason post-secondary plans changed: institution changed content or class format                                                                                                                                                                                                  |
| ps\_why\_chngincome      | education          | Reason post-secondary plans changed: not able to pay due to pandemic related income change                                                                                                                                                                                        |
| ps\_why\_covidcare       | education          | Reason post-secondary plans changed: caring for someone with COVID                                                                                                                                                                                                                |
| ps\_why\_getcovid        | education          | Reason post-secondary plans changed: had COVID or was concerned about getting COVID                                                                                                                                                                                               |
| ps\_why\_lostcare        | education          | Reason post-secondary plans changed: caring for others whose care arrangements were disrupted                                                                                                                                                                                     |
| ps\_why\_other           | education          | Reason post-secondary plans changed: other                                                                                                                                                                                                                                        |
| ps\_why\_uncertclass     | education          | Reason post-secondary plans changed: uncertain how classes / program might change                                                                                                                                                                                                 |
| schlhrs                  | education          | Children's live virtual contact with teacher in last 7 days (among enroll\_school==1)                                                                                                                                                                                             |
| tch\_hrs                 | education          | Hours spent on all teaching activities with children in last 7 days                                                                                                                                                                                                               |
| teach\_31\_asynchronous  | education          | Method of instruction in last 7 days: on their own using school's online material                                                                                                                                                                                                 |
| teach\_31\_hybrid        | education          | Child(ren) currently receiving a combination of in-person and other arrangements                                                                                                                                                                                                  |
| teach\_31\_inperson      | education          | Method of instruction in last 7 days: in person                                                                                                                                                                                                                                   |
| teach\_31\_none\_closed  | education          | Method of instruction in last 7 days: none because school was closed                                                                                                                                                                                                              |
| teach\_31\_none\_sick    | education          | Method of instruction in last 7 days: none because children were sick                                                                                                                                                                                                             |
| teach\_31\_online\_live  | education          | Method of instruction in last 7 days: virtual/online in real time                                                                                                                                                                                                                 |
| teach\_31\_other         | education          | Method of instruction in last 7 days: other                                                                                                                                                                                                                                       |
| teach\_31\_own\_material | education          | Method of instruction in last 7 days: on their own with materials not from school                                                                                                                                                                                                 |
| teach\_31\_paper         | education          | Method of instruction in last 7 days: on their own using school's paper material                                                                                                                                                                                                  |
| teach\_cancel            | education          | Classes normally taught in person were cancelled                                                                                                                                                                                                                                  |
| teach\_nochange          | education          | Classes normally taught in person were not changed because schools did not close.                                                                                                                                                                                                 |
| teach\_online            | education          | Classes normally taught in person moved online (among enroll\_school==1)                                                                                                                                                                                                          |
| teach\_other             | education          | Classes normally taught in person changed some other other way (among enroll\_school==1)                                                                                                                                                                                          |
| teach\_paper             | education          | Classes normally taught in person moved to distance learning using paper (among enroll\_school==1)                                                                                                                                                                                |
| tnum\_ps                 | education          | Number planning to take post-secondary classes                                                                                                                                                                                                                                    |
| tstdy\_hrs               | education          | Children's hours studying on their own last 7 days (among enroll\_school==1)                                                                                                                                                                                                      |
| childfood.bin            | food\_security     | Children not eating enough because we couldn't afford enough food (in last 7 days)                                                                                                                                                                                                |
| curfoodsuf               | food\_security     | Food sometimes or often not enough to eat for the last 7 days                                                                                                                                                                                                                     |
| curfoodsuf2              | food\_security     | Food sometimes or often not enough to eat for the last 7 days among households with children                                                                                                                                                                                      |
| foodconf                 | food\_security     | Not at all/somewhat confident my household will be able to afford the kinds of food we need for the next 4 weeks                                                                                                                                                                  |
| foodconf2                | food\_security     | Not at all/somewhat confident my household will be able to afford the kinds of food we need for the next 4 weeks (binary version)                                                                                                                                                 |
| foodwhynot1              | food\_security     | Couldn't afford to buy more                                                                                                                                                                                                                                                       |
| foodwhynot12             | food\_security     | Couldn't afford to buy more among households with children                                                                                                                                                                                                                        |
| foodwhynot2              | food\_security     | Couldn't get out to buy food                                                                                                                                                                                                                                                      |
| foodwhynot22             | food\_security     | Couldn't get out to buy food among households with children                                                                                                                                                                                                                       |
| foodwhynot3              | food\_security     | Afraid or didn't want to go out to buy food                                                                                                                                                                                                                                       |
| foodwhynot32             | food\_security     | Afraid or didn't want to go out to buy food among households with children                                                                                                                                                                                                        |
| foodwhynot4              | food\_security     | Couldn't get groceries or meals delivered to me                                                                                                                                                                                                                                   |
| foodwhynot42             | food\_security     | Couldn't get groceries or meals delivered to me among households with children                                                                                                                                                                                                    |
| foodwhynot5              | food\_security     | The stores didn't have the food I wanted                                                                                                                                                                                                                                          |
| foodwhynot52             | food\_security     | The stores didn't have the food I wanted among households with children                                                                                                                                                                                                           |
| freefood                 | food\_security     | Did you or anyone in your household get free groceries or a free meal in the last 7 days?                                                                                                                                                                                         |
| tspndtotal               | food\_security     | Total spendt on food in last 7 days                                                                                                                                                                                                                                               |
| where1                   | food\_security     | Free food through school or other program for children                                                                                                                                                                                                                            |
| where2                   | food\_security     | Free food from food pantry or food bank                                                                                                                                                                                                                                           |
| where3                   | food\_security     | Free food from home delivery service                                                                                                                                                                                                                                              |
| where4                   | food\_security     | Free food from church or other religious organization                                                                                                                                                                                                                             |
| where5                   | food\_security     | Free food from shelter/soup kitchen                                                                                                                                                                                                                                               |
| where6                   | food\_security     | Free food from community organization                                                                                                                                                                                                                                             |
| where7                   | food\_security     | Free food from family, friend, neighbors                                                                                                                                                                                                                                          |
| confidence               | housing            | Confidence that the household will be able to pay the next rent or mortgage payment on time                                                                                                                                                                                       |
| confidence.binary        | housing            | Confidence that the household will be able to pay the next rent or mortgage payment on time                                                                                                                                                                                       |
| current                  | housing            | Is this household currently caught up on housing payments (rent or mortgage)?                                                                                                                                                                                                     |
| eviction                 | housing            | Likelihood that your household will have to leave this home or apartment within the next two months because of eviction                                                                                                                                                           |
| foreclosure              | housing            | Likelihood that your household will have to leave this home or apartment within the next two months because of foreclosure                                                                                                                                                        |
| leave2mo                 | housing            | Likelihood that your household will have to leave this home or apartment within the next two months because of eviction or foreclosure (AMONG NOT CURRENT)                                                                                                                        |
| leave2mo\_alt            | housing            | Likelihood that your household will have to leave this home or apartment within the next two months because of eviction or foreclosure (AMONG ALL ADULTS)                                                                                                                         |
| mortcur                  | housing            | Is this household currently caught up on mortgage payments?                                                                                                                                                                                                                       |
| notcurrent               | housing            | Household is NOT currently caught up on housing payments (rent or mortgage)?                                                                                                                                                                                                      |
| rentcur                  | housing            | Is this household currently caught up on rent payments?                                                                                                                                                                                                                           |
| tenure2                  | housing            | Is your home or apartment . . .?                                                                                                                                                                                                                                                  |
| delay                    | insurance          | delayed getting medical care in the last 4 weeks because of the coronavirus pandemic                                                                                                                                                                                              |
| insurance                | insurance          | CDC/Census definition of insured and uninsured                                                                                                                                                                                                                                    |
| insured                  | insurance          | had health insurance                                                                                                                                                                                                                                                              |
| insured\_employer        | insurance          | had insurance through a current or former employer or union                                                                                                                                                                                                                       |
| insured\_exchange        | insurance          | had insurance purchased directly from an insurance company, including marketplace coverage                                                                                                                                                                                        |
| insured\_mcare           | insurance          | had Medicare                                                                                                                                                                                                                                                                      |
| insured\_military        | insurance          | had TRICARE or other military health care OR VA (including those who have ever used or enrolled for VA health care)                                                                                                                                                               |
| notget                   | insurance          | did not get needed medical care in the last 4 weeks because of the coronavirus pandemic                                                                                                                                                                                           |
| uninsured                | insurance          | lacked health insurance                                                                                                                                                                                                                                                           |
| definitevacc             | vaccine            | Once a vaccine to prevent COVID-19 is available, intend to definitely get a vaccine                                                                                                                                                                                               |
| doses                    | vaccine            | Received (or plan to receive) all required COVID-19  vaccine doses                                                                                                                                                                                                                |
| prevcovid                | vaccine            | Doctor or provider told that respondent have COVID-19                                                                                                                                                                                                                             |
| recvdvacc                | vaccine            | Received a COVID-19 vaccine                                                                                                                                                                                                                                                       |
| whynot1                  | vaccine            | No vaccine because concerned about side effects                                                                                                                                                                                                                                   |
| whynot10                 | vaccine            | No vaccine because I don't trust the government                                                                                                                                                                                                                                   |
| whynot11                 | vaccine            | No vaccine because  . . . other                                                                                                                                                                                                                                                   |
| whynot2                  | vaccine            | No vaccine because don't know if vaccine will work                                                                                                                                                                                                                                |
| whynot3                  | vaccine            | No vaccine because I don't believe I need a vaccine                                                                                                                                                                                                                               |
| whynot4                  | vaccine            | No vaccine because I don't like vaccines                                                                                                                                                                                                                                          |
| whynot5                  | vaccine            | No vaccine because my doctor has not recommended it                                                                                                                                                                                                                               |
| whynot6                  | vaccine            | No vaccine because I plan to wait and see if it is safe and may get it later                                                                                                                                                                                                      |
| whynot7                  | vaccine            | No vaccine because I think other people need it more than I do right now                                                                                                                                                                                                          |
| whynot8                  | vaccine            | No vaccine because I am concerned about the cost of a COVID-19 vaccine                                                                                                                                                                                                            |
| whynot9                  | vaccine            | No vaccine because I don't trust COVID-19 vaccines                                                                                                                                                                                                                                |
| whynotb1                 | vaccine            | Believe I don't need a vaccine because I already had COVID-19                                                                                                                                                                                                                     |
| whynotb2                 | vaccine            | Believe I don't need a vaccine because I am not a member of a high-risk group                                                                                                                                                                                                     |
| whynotb3                 | vaccine            | Believe I don't need a vaccine because I plan to use masks or other precautions instead                                                                                                                                                                                           |
| whynotb4                 | vaccine            | Believe I don't need a vaccine because I don't believe COVID-19 is a serious illness                                                                                                                                                                                              |
| whynotb5                 | vaccine            | Believe I don't need a vaccine because I don't think vaccines are beneficial                                                                                                                                                                                                      |
| whynotb6                 | vaccine            | Believe I don't need a vaccine because . . . other                                                                                                                                                                                                                                |
| numdoses                 | vaccine            | Number of doses received                                                                                                                                                                                                                                                          |
| brand                    | vaccine            | Brand of first vaccination                                                                                                                                                                                                                                                        |
| getvacrv                 | vaccine            | Intention on getting vaccine                                                                                                                                                                                                                                                      |
| whynorv1                 | vaccine            | Why not get vaccine ... I am concerned about possible side effects of a COVID-19 vaccine                                                                                                                                                                                          |
| whynorv2                 | vaccine            | Why not get vaccine ... I don’t know if a COVID-19 vaccine will protect me                                                                                                                                                                                                        |
| whynorv3                 | vaccine            | Why not get vaccine ... I don’t believe I need a COVID-19 vaccine                                                                                                                                                                                                                 |
| whynorv4                 | vaccine            | Why not get vaccine ... My doctor has not recommended it                                                                                                                                                                                                                          |
| whynorv5                 | vaccine            | Why not get vaccine ... I plan to wait and see if it is safe and may get it later                                                                                                                                                                                                 |
| whynorv6                 | vaccine            | Why not get vaccine ... I am concerned about the cost of a COVID-19 vaccine                                                                                                                                                                                                       |
| whynorv7                 | vaccine            | Why not get vaccine ... I don’t trust COVID-19 vaccines                                                                                                                                                                                                                           |
| whynorv8                 | vaccine            | Why not get vaccine ... I don’t trust the government                                                                                                                                                                                                                              |
| whynorv9                 | vaccine            | Why not get vaccine ... I don't think COVID-19 is that big of a threat                                                                                                                                                                                                            |
| whynorv10                | vaccine            | Why not get vaccine ... It's hard for me to get a COVID-19 vaccine                                                                                                                                                                                                                |
| whynorv11                | vaccine            | Why not get vaccine ... I experienced side effects from the dose of COVID-19 vaccine I received                                                                                                                                                                                   |
| whynorv12                | vaccine            | Why not get vaccine ... I believe one dose is enough to protect me                                                                                                                                                                                                                |
| whynorv13                | vaccine            | Why not get vaccine ... other                                                                                                                                                                                                                                                     |
| kiddoses                 | vaccine            | Children received or plan for all doses                                                                                                                                                                                                                                           |
| kiddoses\_5\_11y         | vaccine            | Children received or plan for all doses … ages 5-11                                                                                                                                                                                                                               |
| kiddoses\_12\_17y        | vaccine            | Children received or plan for all doses … ages 12-17                                                                                                                                                                                                                              |
| kidgetvac                | vaccine            | Intention on getting children vaccine                                                                                                                                                                                                                                             |
| kidgetvac\_5\_11y        | vaccine            | Intention on getting children vaccine … ages 5-11                                                                                                                                                                                                                                 |
| kidgetvac\_12\_17y       | vaccine            | Intention on getting children vaccine … ages 12-17                                                                                                                                                                                                                                |
| kidwhyno1                | vaccine            | Why not get children vaccinated ... Concern about possible side effects of a COVID-19 vaccine for children                                                                                                                                                                        |
| kidwhyno2                | vaccine            | Why not get children vaccinated ... Plan to wait and see if it is safe and may get it later                                                                                                                                                                                       |
| kidwhyno3                | vaccine            | Why not get children vaccinated ... Not sure if a COVID-19 vaccine will work for children                                                                                                                                                                                         |
| kidwhyno4                | vaccine            | Why not get children vaccinated ... Don’t believe children need a COVID-19 vaccine                                                                                                                                                                                                |
| kidwhyno5                | vaccine            | Why not get children vaccinated ... The children in this household are not members of a high-risk group                                                                                                                                                                           |
| kidwhyno6                | vaccine            | Why not get children vaccinated ... The children’s doctor has not recommended it                                                                                                                                                                                                  |
| kidwhyno7                | vaccine            | Why not get children vaccinated ... Other people need it more than the children in this household do right now                                                                                                                                                                    |
| kidwhyno8                | vaccine            | Why not get children vaccinated ... Concern about missing work to have the children vaccinated                                                                                                                                                                                    |
| kidwhyno9                | vaccine            | Why not get children vaccinated ... Unable to get a COVID-19 vaccine for children in this household                                                                                                                                                                               |
| kidwhyno10               | vaccine            | Why not get children vaccinated ... Parents or guardians in this household do not vaccinate their children                                                                                                                                                                        |
| kidwhyno11               | vaccine            | Why not get children vaccinated ... Don’t trust COVID-19 vaccines                                                                                                                                                                                                                 |
| kidwhyno12               | vaccine            | Why not get children vaccinated ... Don’t trust the government                                                                                                                                                                                                                    |
| kidwhyno13               | vaccine            | Why not get children vaccinated ... Concern about the cost of a COVID-19 vaccine                                                                                                                                                                                                  |
| kidwhyno14               | vaccine            | Why not get children vaccinated ... other                                                                                                                                                                                                                                         |

Finally, when you unzipped each week's zip file into the relevant `census_pulse/input/phase##_unzipped` directory, you saved Census Bureau data dictionaries in addition to the survey data and the replicate weights. Use these dictionaries, in conjunction with the survey instruments and technical documentation, to identify or develop indicators not used in our code. 

# Special considerations / notes
## ¡Survey set the data!
If you run this code as is (only modifying the file paths and identifiers for the state and MSA), your data will be survey set and all analyses will fully account for the survey design by using the replicate weights. 

If you choose to write your own code from scratch or want to perform an ad hoc analysis, it is critical that you survey set the data! For those who have worked [American Community Survey PUMS data](https://www.census.gov/programs-surveys/acs/microdata.html), the survey setting process should be familiar. However, if you wish to pool across multiple survey weeks (as we do within each Phase), you will have to create new weights for the pooled estimates. To do this, you would divide the replicate weights by the number of weeks that you are pooling together. See [page 11 of this technical document](https://www2.census.gov/programs-surveys/demo/technical-documentation/hhp/Phase2_Source_and_Accuracy-Week_14.pdf) for details. 

Here are examples for how to survey set the data: 

        svy_weekly <- 
          srvyr::as_survey_rep(
            MyDT,
            weights = pweight ,
            combined.weights = TRUE ,
            repweights = grep('pweight[0-9]+', names(dt), value  = T) ,
            scale = 4 / 80 ,
            rscales = rep( 1 , 80 ) ,
            mse = TRUE ,
            type = "JK1"
          )
        svy_weekly <- dtsurvey::dtrepsurvey(svy_weekly) # survey set for RADS
        
        svy_pooled <-
          srvyr::as_survey_rep(
            MyDT,
            weights = pooledNwt ,
            combined.weights = TRUE ,
            repweights = grep('pooledNwt[0-9]+', names(dt), value  = T) ,
            scale = 4 / 80 ,
            rscales = rep( 1 , 80 ) ,
            mse = TRUE ,
            type = "JK1"
          )
        svy_pooled <- dtsurvey::dtrepsurvey(svy_pooled) # survey set for RADS
        

## Analyzing survey data with the [`rads`](https://github.com/PHSKC-APDE/rads/) package
If you look at our code, you will see that we use the [`rads`](https://github.com/PHSKC-APDE/rads/) package, a custom suite of tools developed for Seattle & King County public health analyses. While some of the `rads` tools will only work within in our network, the `calc()` function can be used by anyone for analysis of survey and administrative (i.e., count) data. Unless you're already a whiz with the R [survey](https://cran.r-project.org/web/packages/survey/index.html) or [srvyr](https://cran.r-project.org/web/packages/srvyr/index.html) packages, I highly recommend using [RADS](https://github.com/PHSKC-APDE/rads) for analyzing survey data. If you're interested in learning more, check out the examples in the [`calc()` wiki](https://github.com/PHSKC-APDE/rads/wiki/calc).
