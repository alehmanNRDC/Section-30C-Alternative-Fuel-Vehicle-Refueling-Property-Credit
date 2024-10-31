# EV tax credit flag script 
# updated 6-27-24 by AL

# package upload -----

packages <- c(
  'quarto',
  'sf',
  'tidyverse',
  'mapview',
  'dplyr',
  'rjson',  
  'jsonlite',
  'tigris',
  'tidycensus',
  'data.table',
  'viridis',
  'fs',
  'here',
  'osmdata',
  'sfnetworks',
  'ggplot2',
  "readr", 
  "osmdata",
  'dodgr'
)

if(!require(pacman)) install.packages('pacman')
pacman::p_load(packages,character.only = T)


# upload files ------
# 30c all tracts file & save everything as a character
# saving as a character is needed to preserve the tract IDs correctly
DOEcensusTract <- read.csv(here( "30C all tracts.csv"), 
                 colClasses = c("character", "character", "character", "character", "character", "character"))

#upload hud file 
hudEmpowermentZones<-st_read(here( "Empowerment_Zones_and_Enterprise_Communities_4155431696566095439.geojson"))%>%
  st_make_valid()# fixes the unclosed polygons 


# Pull tract population using ACS package ---- 
# A. Loop through each census tract in USA and retrieve population data
# census API can't output information nationally at the tract level, so need to make subsets (county and state) and loop through them

ctys <- counties(cb = TRUE) # make a list of counties

state_codes <- unique(fips_codes$state_code)[1:51] # a vector containing the unique FIPS codes for each state.

# this function that iterates through the FIPS
# get_act is used on state/county subsets to get population (by tract) 
populationACS <- map_df(state_codes, function(state_code) { # start with a state/ DC
  state <- filter(ctys, STATEFP == state_code) # makes a list of all the counties
  county_codes <- state$COUNTYFP
  get_acs(geography = "tract", 
          variables = "B01003_001",
          state = state_code, 
          county = county_codes, 
          geometry = TRUE)
}) 


# join census and treasury info (by tract ID) -----
populationACS<-populationACS[,c(1,4)] # remove unneeded columns 
df<-merge(DOEcensusTract,populationACS,by.x="tract", by.y="GEOID", all.x = T)

# flags: low pop  ----
# is pop less than 2k ?
df$pop_flag <- ifelse(df$estimate < 2000, 1, 0)

# flags: does it touch at least 1 nmtc (low income) ----
# A. Perform a spatial self-join to find a list of pairs of all touching tracts

#i. prep for join 
df$nmtc <- as.numeric(df$nmtc) # turn to a number 
df<-df[df$state != "Puerto", ] # PR doesnt have ACS data, removing this
lowIncomeTracts  <- df[df$nmtc == 1, ] #only keep tracts flagged as NMTC


# ii. make the dfs spatial objects
lowIncomeTracts<-st_sf(lowIncomeTracts)
df<-st_sf(df)

# iii.  join, resultant df has all the combos of tracts that touch
listAdjTracts <- st_join(df, lowIncomeTracts, join = st_touches)

# B. count how many nmtc flagged tracts each tract touches 
# in the pair list, x tract (.x) are the tracts of focus, y tracts (.y) are the touched / neighboring tracts
# this portion takes ~15 mins to run


adjNMTCtotal <- listAdjTracts %>%
  filter(nmtc.y>0)%>% # if y tract is 0, remove (0 means the x tract doesn't touch any low income tracts)
  group_by(tract.x) %>% # organize results by unique x tracts 
  summarize(adj_nmtc_flag = sum(nmtc.y)) %>% # counts how many nmtc flagged y tracts each x tract touches 
  ungroup()


# hud empowerment zones status by tract -----
hudEmpowermentZones <- st_transform(hudEmpowermentZones, st_crs(adjNMTCtotal)) # get matching crs, prep for join 
geoJoin<-st_join(adjNMTCtotal, hudEmpowermentZones)


geoJoin1 <-geoJoin %>%
  filter(FIPS == tract.x)

geoJoin1<-geoJoin1[,c(1,7)] # only need to keep tract ID and "type" column from the HUD shape file



# join NMTC count and empowerment zone info with original, DOE file -----
df <- st_drop_geometry(df)
geoJoin1 <- st_drop_geometry(geoJoin1)
adjNMTCtotal <-st_drop_geometry(adjNMTCtotal)


adjFlags<- left_join(df, adjNMTCtotal , by = c("tract" = "tract.x")) # add the adj flag to the original 30c listing
flags_hud<-left_join(adjFlags, geoJoin1, by = c("tract" = "tract.x")) 

flags_hud <- st_drop_geometry(flags_hud)
flags_hud<- flags_hud[,c(-6)]
names(flags_hud)[names(flags_hud) == 'estimate'] <- 'ACS_pop'
names(flags_hud)[names(flags_hud) == 'TYPE'] <- 'hud_distiction'


# export to csv ----
write_csv(flags_hud, here( "Eligibility_Flags.csv"))

