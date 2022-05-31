# pulse_survey

Analyze US [Census Bureau](https://www.census.gov/en.html) [Household Pulse Survey](https://www.census.gov/programs-surveys/household-pulse-survey.html) Public Use File ([PUF](https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html)) data in R

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
    - `git clone https://github.com/PHSKC-APDE/pulse_survey.git`
  - Open your local copy of [00_constants_n_functions.R](https://github.com/PHSKC-APDE/pulse_survey/blob/main/00_constants_n_functions.R) and update the following:
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
       * E.g., replace `"https://raw.githubusercontent.com/PHSKC-APDE/pulse_survey/main/"` with `"C:/code/pulse_survey/"`
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

  
# Data dictionary
The file [`pulse_varlist.csv`](https://github.com/PHSKC-APDE/pulse_survey/blob/main/pulse_varlist.csv) file is a dictionary of all the variables that will be output by this code. It includes selected variables created by the Census Bureau and derivative variables created by the `00_phase##_prep_survey.R` code. I suggest viewing it on your local machine since GitHub may only show the first few columns.

When you unzipped each week's zip file into the relevant `census_pulse/input/phase##_unzipped` directory, you saved Census Bureau data dictionaries in addition to the survey data and the replicate weights. Use these dictionaries, in conjunction with the survey instruments and technical documentation, to identify or develop additional indicators. 

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
