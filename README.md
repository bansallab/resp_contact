# Data and code for "Characterizing US contact patterns relevant to respiratory transmission from a pandemic to baseline: Analysis of a large cross-sectional survey"

This repository provides the data and source code for the following study: Juliana C Taube, Zachary Susswein, Vittoria Colizza, Shweta Bansal. "Characterizing US contact patterns relevant to respiratory transmission from a pandemic to baseline: Analysis of a large cross-sectional survey." https://doi.org/10.1101/2024.04.26.24306450

## Estimates (`estimates/`)
Pandemic and baseline estimates of contact at the county-week scale are provided. Estimates are also provided dis-aggregated by age, gender, race/ethnicity, and setting of contact. The contact setting file has fewer columns so that it is small enough to upload here.

Columns of interest:
- `contact_fit` has modeled pandemic estimates of mean contact
- `non_hh_contacts` has observed pandemic mean contact
- `scale_baseline` has inferred baseline estimates of mean contact 

## Data (`data/`)
Reference files that may be required to run the code, including fips and state crosswalk files, urban/rural classifications, etc. are in `data/input/`.
* `Average_Household_Size_and_Population_Density_-_County.csv` contains population density for each fips code (from https://covid19.census.gov/datasets/USCensus::average-household-size-and-population-density-county/explore?location=4.945434%2C0.315550%2C1.99&showTable=true)
* `census_regions.csv` delineates which states are in which census regions (from https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv)
* `co-est2021-alldata.csv` contains population estimates for each county (from https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/CO-EST2021-ALLDATA.pdf)
* `COVID_county_vacc_data_dataverse.csv` contains county level vaccination coverage (from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BFRIKI)
* `covs_updated_for_inla_6_10.csv` contains county and state policy data from the COVID-AMP Project and the CDC
* `HHS_regions.csv` contains information on what HHS region each county is in 
* `NCHSURCodes2013.xlsx` contains urban-rural classifications for each fips code (from https://www.cdc.gov/nchs/data_access/urban_rural.htm)
* `nyt-us-counties-rolling-avg-2020.csv`, `nyt-us-counties-rolling-avg-2021.csv`,  `nyt-us-national-rolling-avg.csv`, `nyt-us-states-rolling-avg.csv` contain COVID-19 case incidence data from the New York Times at different spatial scales (from https://github.com/nytimes/covid-19-data/blob/master/rolling-averages/us.csv)
* `OxCGRT_compact_subnational_v1.csv` contains Oxford Stringency Index data for each state (from https://github.com/OxCGRT/covid-policy-dataset/tree/main/data)
* `state_and_county_fips_master.csv` contains each county's corresponding state and name
* `2020_US_Region_Mobility_Report.csv`, `2021_US_Region_Mobility_Report.csv`, `2022_US_Region_Mobility_Report.csv` contain Google mobility data
* `Trips_by_Distance_20241202.csv` is too large to upload to Github but can be downloaded from the Bureau of Transportation Statistics: https://data.bts.gov/Research-and-Statistics/Daily-Mobility-Statistics/w96p-f2qv/about_data
* 

Intermediate data files with aggregated raw data are provided so that the GAMs and linear regression models can be reproduced.
* `group_means_rake` provides raw weighted county-week means of contact data truncated at 72 contacts aggregated and disaggregated by age, gender, race, and setting
* `output/mobility_19_20_fall_ratio_new_norm.csv` contains ratios of Safegraph Social Distancing mobility metrics for the fall of 2019 and 2020
* `output/normal_gamma2_72trunc_m1/fitted_predictions.csv` provides contact estimates as a result of spatiotemporal GAMs for the main text analysis

## Code (`scripts/`)
Scripts to clean data, rake and aggregate survey responses, run spatiotemporal GAM regression models, baseline linear regression models, and reproduce figures. Scripts for analyzing individual responses are provided for reproducibility but will not run without the original individual-level data (see Individual Data section below). File names briefly describe the purpose of each script. Main figures 1 and 2 are produced by `09_baseline_regression.R`. 

## Individual Data
Individual CTIS survey responses cannot be shared by the authors, but researchers can visit https://cmu-delphi.github.io/delphi-epidata/symptom-survey/data-access.html if they would like to enter an agreement for data usage with CMU Delphi.  
