### LODES app setup

library(tidyverse)
library(sf)
library(janitor)
library(tidycensus)
library(tigris)
library(mapview)
library(viridis)
library(patchwork)
library(leaflet)
library(leafsync)
library(gt)
library(lehdr) #https://github.com/jamgreen/lehdr

options(tigris_use_cache = TRUE)
options(scipen = 999)

#var_list<-load_variables(2020,'acs5')

#census var ideas
mapTheme <- function(base_size = 8, title_size = 10, small_size = 6) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text( colour = "black", hjust = 0.5),
    plot.subtitle=element_text(colour = "black", hjust = 0.5, face="italic"),
    plot.caption=element_text( colour = "black", hjust = 0.5),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(colour="transparent", fill="transparent"),
    legend.key.size = unit(0.25, "cm"))
}

#B08126_001 MEANS OF TRANSPORTATION TO WORK BY INDUSTRY
census_api_key("746ea8916547306ae2abf2aafe059e1a1b70b98a")


#more census variables should be added
vars <- c("B23001_001")

year <- 2019


dvrpc_pacounties <- c('Bucks','Philadelphia', 'Chester','Montgomery', 'Delaware')
dvrpc_njcounties <- c('Camden', 'Gloucester', 'Burlington','Mercer')
#delaware

msa_acs <- rbind(
  get_acs(geography = "tract",
    year = year,
    variables = vars,
    geometry = TRUE,
    state = "PA",
    county = dvrpc_pacounties,
    output = "wide"),
  get_acs(geography = "tract",
    year = year,
    variables = vars,
    geometry = TRUE,
    state = "NJ",
    county = dvrpc_njcounties,
    output = "wide")) %>% 
  st_transform(crs = 4326) %>% 
  mutate(
    tract_area = st_area(.)
  )
  
  
  
  
#residential area characteristics
msa_rac_load  <- rbind(
  grab_lodes(state = "pa", 
            year = year, 
            #version = "LODES8", 
            lodes_type = "rac", 
            job_type = "JT01",
            segment = "S000", 
            state_part = "main", 
            agg_geo = "tract"),
  grab_lodes(state = "nj", 
             year = year, 
             #version = "LODES8", 
             lodes_type = "rac", 
             job_type = "JT01",
             segment = "S000", 
             state_part = "main", 
             agg_geo = "tract"))




msa_rac <- msa_rac_load %>% 
  rename(total_jobs_rac = C000,
         age_29_or_younger_rac = CA01,
         age_30_to_54_rac = CA02,
         age_55_or_older_rac = CA03,
         monthly_income_1250_or_less_rac = CE01,
         monthly_income_1251_to_3333_rac = CE02,
         monthly_income_3334_or_more_rac = CE03,
         agriculture_rac = CNS01, # agriculture, forestry, fishing, hunting / NAICS11
         mining_rac = CNS02, # mining, oil/gas extraction / MAICS21
         utilities_rac = CNS03, # utilities / NAICS22
         construction_rac = CNS04, # construction / NAICS23
         manufacturing_rac = CNS05, # manufacturing /NAICS31 and NAICS33
         wholesaleTrade_rac = CNS06, # wholesale trade / NAICS42
         retailTrade_rac = CNS07, # retail trade / 44/46
         transportationWarehousing_rac = CNS08, # transportation and warehousing / 48, 49
         informationTech_rac = CNS09, # information /51
         financeInsurance_rac = CNS10, # finance and insurance /52
         realEstate_rac = CNS11, # real estate /53
         techServices_rac = CNS12, # professional, scientific, and tech services /54
         management_rac = CNS13, # management (enterprise) /55
         wasteRemediation_rac = CNS14, # waste / remediation /56
         educational_rac = CNS15, # education /61
         healthcareSocial_rac = CNS16, # healthcare/social services /62
         artsEntertainmentRec_rac = CNS17, # arts/entertainment/rec /71
         foodServices_rac = CNS18, # food services /72
         otherJobs_rac = CNS19, # other /81
         publicAdmin_rac = CNS20, # public admin /92
         white_rac = CR01,
         black_rac = CR02,
         native_american_rac = CR03,
         asian_rac = CR04,
         pacific_rac = CR05,
         mixed_rac = CR07,
         not_hispanic_rac = CT01,
         hispanic_rac = CT02,
         male_rac = CS01,
         female_rac = CS02,
         underHS_rac = CD01,
         HS_rac = CD02,
         associate_rac = CD03,
         bachProfessional_rac = CD04)


# choose your variable

var_list <- colnames(msa_rac)[4:ncol(msa_rac)] %>% sub(pattern = '_rac',replacement = '')




msa_rac_sf <- left_join(msa_acs, msa_rac, by = c('GEOID' = 'h_tract')) %>% 
  mutate_at(vars(paste0(var_list,'_rac')), ~ as.numeric(. / tract_area)*1000000 )




#Work area characteristics
msa_wac_load  <- rbind(
  grab_lodes(state = "pa", 
             year = year, 
             #version = "LODES8", 
             lodes_type = "wac", 
             job_type = "JT01",
             segment = "S000", 
             state_part = "main", 
             agg_geo = "tract"),
  grab_lodes(state = "nj", 
             year = year, 
             #version = "LODES8", 
             lodes_type = "wac", 
             job_type = "JT01",
             segment = "S000", 
             state_part = "main", 
             agg_geo = "tract"))

msa_wac <- msa_wac_load %>% 
  rename(total_jobs_wac = C000,
         age_29_or_younger_wac = CA01,
         age_30_to_54_wac = CA02,
         age_55_or_older_wac = CA03,
         monthly_income_1250_or_less_wac = CE01,
         monthly_income_1251_to_3333_wac = CE02,
         monthly_income_3334_or_more_wac = CE03,
         agriculture_wac = CNS01, # agriculture, forestry, fishing, hunting / NAICS11
         mining_wac = CNS02, # mining, oil/gas extwaction / MAICS21
         utilities_wac = CNS03, # utilities / NAICS22
         construction_wac = CNS04, # construction / NAICS23
         manufacturing_wac = CNS05, # manufacturing /NAICS31 and NAICS33
         wholesaleTrade_wac = CNS06, # wholesale trade / NAICS42
         retailTrade_wac = CNS07, # retail trade / 44/46
         transportationWarehousing_wac = CNS08, # transportation and warehousing / 48, 49
         informationTech_wac = CNS09, # information /51
         financeInsurance_wac = CNS10, # finance and insurance /52
         realEstate_wac = CNS11, # real estate /53
         techServices_wac = CNS12, # professional, scientific, and tech services /54
         management_wac = CNS13, # management (enterprise) /55
         wasteRemediation_wac = CNS14, # waste / remediation /56
         educational_wac = CNS15, # education /61
         healthcareSocial_wac = CNS16, # healthcare/social services /62
         artsEntertainmentRec_wac = CNS17, # arts/entertainment/rec /71
         foodServices_wac = CNS18, # food services /72
         otherJobs_wac = CNS19, # other /81
         publicAdmin_wac = CNS20, # public admin /92
         white_wac = CR01,
         black_wac = CR02,
         native_american_wac = CR03,
         asian_wac = CR04,
         pacific_wac = CR05,
         mixed_wac = CR07,
         not_hispanic_wac = CT01,
         hispanic_wac = CT02,
         male_wac = CS01,
         female_wac = CS02,
         underHS_wac = CD01,
         HS_wac = CD02,
         associate_wac = CD03,
         bachProfessional_wac = CD04) 

msa_wac_sf <- left_join(msa_acs, msa_wac, by = c('GEOID' = 'w_tract')) %>% na.omit()%>% 
  mutate_at(vars(paste0(var_list,'_wac')), ~ as.numeric(. / tract_area)*1000000 )

# phl_wac_sf <- msa_wac_sf %>% filter(grepl("Philadelphia County", NAME))
# phl_rac_sf <- msa_rac_sf %>% filter(grepl("Philadelphia County", NAME))

#%>% filter(grepl("Philadelphia County", NAME))
# get difference between # of employees vs # of jobs in tract
#ppl who live there - ppl who work there
# phl_econ1 <- phl_econ %>% mutate(
#   total_jobs_diff = total_jobs_rac - total_jobs_wac ,
#   age_29_or_younger_diff = age_29_or_younger_rac- age_29_or_younger_wac,
#   age_30_to_54_diff = age_30_to_54_rac-age_30_to_54_wac,
#   age_55_or_older_diff = age_55_or_older_rac-age_55_or_older_wac,
#   monthly_income_1250_or_less_diff = monthly_income_1250_or_less_rac -monthly_income_1250_or_less_wac ,
#   monthly_income_1251_to_3333_diff = monthly_income_1251_to_3333_rac -monthly_income_1251_to_3333_wac ,
#   monthly_income_3334_or_more_diff = monthly_income_3334_or_more_rac-monthly_income_3334_or_more_wac,
#   agriculture_diff = agriculture_rac -agriculture_wac , # agriculture, forestry, fishing, hunting / NAICS11
#   mining_diff = mining_rac-mining_wac, # mining, oil/gas extraction / MAICS21
#   utilities_diff = utilities_rac-utilities_wac, # utilities / NAICS22
#   construction_diff = construction_rac-construction_wac, # construction / NAICS23
#   manufacturing_diff = manufacturing_rac-manufacturing_wac, # manufacturing /NAICS31 and NAICS33
#   wholesaleTrade_diff = wholesaleTrade_rac-wholesaleTrade_wac, # wholesale trade / NAICS42
#   retailTrade_diff = retailTrade_rac-retailTrade_wac, # retail trade / 44/46
#   transportationWarehousing_diff = transportationWarehousing_rac-transportationWarehousing_wac, # transportation and warehousing / 48, 49
#   informationTech_diff = informationTech_rac-informationTech_wac, # information /51
#   financeInsurance_diff = financeInsurance_rac-financeInsurance_wac, # finance and insurance /52
#   realEstate_diff = realEstate_rac-realEstate_wac, # real estate /53
#   techServices_diff = techServices_rac-techServices_wac, # professional, scientific, and tech services /54
#   management_diff = management_rac-management_wac, # management (enterprise) /55
#   wasteRemediation_diff = wasteRemediation_rac-wasteRemediation_wac, # waste / remediation /56
#   educational_diff = educational_rac-educational_wac, # education /61
#   healthcareSocial_diff = healthcareSocial_rac-healthcareSocial_wac, # healthcare/social services /62
#   artsEntertainmentRec_diff = artsEntertainmentRec_rac-artsEntertainmentRec_wac, # arts/entertainment/rec /71
#   foodServices_diff = foodServices_rac-foodServices_wac, # food services /72
#   otherJobs_diff = otherJobs_rac-otherJobs_wac, # other /81
#   publicAdmin_diff = publicAdmin_rac-publicAdmin_wac, # public admin /92
#   white_diff = white_rac-white_wac,
#   black_diff = black_rac-black_wac,
#   native_american_diff = native_american_rac-native_american_wac,
#   asian_diff = asian_rac-asian_wac,
#   pacific_diff = pacific_rac-pacific_wac,
#   mixed_race_diff = mixed_race_rac-mixed_race_wac,
#   not_hispanic_diff = not_hispanic_rac-not_hispanic_wac,
#   hispanic_diff = hispanic_rac-hispanic_wac,
#   male_diff = male_rac-male_wac,
#   female_diff = female_rac-female_wac,
#   underHS_diff = underHS_rac-underHS_wac,
#   HS_diff = HS_rac-HS_wac,
#   associate_diff = associate_rac-associate_wac,
#   bachProfessional_diff = bachProfessional_rac-bachProfessional_wac)

## viz


### leaflet, rip ggplot and patchwork

var <-'informationTech'


binpal <- colorNumeric(palette = "YlGnBu", domain = msa_wac_sf$informationTech_wac)

wac_leaf<-leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -75.161802, lat = 39.957673, zoom = 11) %>%
  addPolygons(
    data = msa_wac_sf,
    fillColor =   ~binpal(msa_wac_sf$informationTech_wac),
    color = "lightgrey",
    weight = 1,
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8
    )) 

###

binpal_rac <- colorBin("Reds", msa_rac_sf$informationTech_rac, 8, pretty = T)

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -75.161802, lat = 39.957673, zoom = 11) %>%
  addPolygons(
    data = msa_wac_sf,
    fillColor =  ~binpal(informationTech_wac),
    color = "lightgrey",
    weight = 1,
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8
    )) %>%
  addLegend(data = msa_wac_sf, position = "bottomright", pal = binpal, values = ~informationTech_wac,
            title = "informationTech_wac",
            opacity = 1
  )



### histogram to figure color pallete breaks

cc <- msa_wac_sf %>% filter(GEOID == 42101000402)
cc_val <- cc %>% pull(paste0(var,'_wac')) 



var <-'informationTech'
var_wac <- paste0(var,'_wac')
var_wac_col <- msa_wac_sf[[var_wac]] 

label_interval <- function(breaks) {
  paste0("(", breaks[1:length(breaks) - 1], " - ", breaks[2:length(breaks)], ")")
}


color_breaks <- c(0, 1,10,5000,15000,30000)

binpal <- colorFactor("Blues", cut(msa_wac_sf[[var_wac]], breaks = color_breaks, labels = label_interval(color_breaks)))



leaflet(msa_wac_sf) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -75.161802, lat = 39.957673, zoom = 11) %>%
  addPolygons(
    fillColor =  ~binpal(cut(var_wac_col, breaks = color_breaks, labels = label_interval(color_breaks))),
    color = "lightgrey",
    weight = 1,
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8
    )) %>%
  addLegend(position = "bottomright", pal = binpal, values = ~cut(var_wac_col, breaks = color_breaks, labels = label_interval(color_breaks)),
            title = "informationTech_wac",
            opacity = 1
  )




wac_table <- msa_wac_sf %>% st_drop_geometry() %>% 
  mutate(
    group_var = cut(var_wac_col, breaks = color_breaks, labels = label_interval(color_breaks))
  ) %>% group_by(group_var) %>% 
  summarise(n = n()) 

wac_table %>% gt() %>% 
  data_color(columns = group_var, colors = 'Reds')


ggplot(msa_wac_sf %>% filter(GEOID != 42101000402))+
  geom_histogram(aes_string(
    x = var_wac,
    color = cut(var_wac_col[var_wac_col != cc_val], breaks = color_breaks ),
    ))+
  geom_jitter(aes_string(x = var_wac, y = -100, color = cut(var_wac_col[var_wac_col != cc_val], breaks = color_breaks )),height = 100)+
  scale_color_brewer(palette = 'Reds', na.value = 'black')+
  labs(x = paste0(var,' Job Locations per m^2'), color = 'breaks',
       subtitle = paste0('Center City ', var, ' workers per m^2: ', cc_val%>% round()))


### remove cc from histogram, but geom_text value
### get all 3 graphs to respond to 'var'
### get onto shiny to decide color breaks
### add wilmington
### map details
### yep