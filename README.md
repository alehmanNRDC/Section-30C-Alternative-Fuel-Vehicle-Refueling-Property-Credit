# EV Tax Credit Flag Script

This script identifies adjacent low-population census tracts as defined by 45D(e)(4) that, according to statute, should qualify for the Section 30C Alternative Fuel Vehicle Refueling Property Credit. This script shows how adjacent census tracts can be identified for program eligibility quickly and effectively. This script performs the following:

1. Pulls census tract population data.
2. Identifies low-population census tracts (population less than 2000).
3. Uses spacial joins to identify low-population tracts that are adjacent to at least one low-income tract, made available via Argonne National Lab (https://anl.app.box.com/s/kuybn61o5afa2a8x3knqu02bfgxd0wfg).
4. Outputs a CSV file, Eligibility_Flags.csv, with flagged census tracts that are low-population and adjacent to low-income and Empowerment Zone information made available via HUD (https://hudgis-hud.opendata.arcgis.com/datasets/1101a6c1e2364302b70485ca99fc7e69_3/about).

## Requirements
This script should be run in an R environment. It requires the following R packages:

- quarto
- sf
- tidyverse
- mapview
- dplyr
- rjson
- jsonlite
- tigris
- tidycensus
- data.table
- viridis
- fs
- here
- osmdata
- sfnetworks

## Files
### Data Inputs
1. 30C all tracts.csv: A CSV file listing census tracts with specific IDs from Argonne National Lab (https://anl.app.box.com/s/kuybn61o5afa2a8x3knqu02bfgxd0wfg).
2. Empowerment_Zones_and_Enterprise_Communities_4155431696566095439.geojson: GeoJSON file from HUD containing labes for "Empowerment Zone" status (https://hudgis-hud.opendata.arcgis.com/datasets/1101a6c1e2364302b70485ca99fc7e69_3/about)

### Data Outputs
1. Eligibility_Flags.csv: A CSV file generated by the script containing census tracts with population flags, NMTC adjacency information, and HUD empowerment status. Columns are as follows: 
   
   A) tract	- 11-Digit Tract ID (Note: need to maintain leading zeros)
   B) state -	Non-abbreviated state name
   C) county - Non-abbreviated county name
   D) nonurb	- 1, if census tract as defined as "non-urban" in the Treasury/ANL dataset, 0 if not
   E) nmtc - 1, if census tract as defined as "low-income" in the Treasury/ANL dataset, 0 if not
   F) ACS_pop - total population of the census tract as determined from the 2020 ACS
   G) pop_flag	- 1 if ACS_pop is under 2000, 0 if greater than 2000
   H) adj_nmtc_flag - total number of low-income tracts that are adjacent to the census tract
   I)	hud_distinction - census tract status as defined by HUD. Values are NA, Empowerment Zone (relevant to low population tracts), Enterprise Zone, Enterprise Community, or Renewal Community. 


