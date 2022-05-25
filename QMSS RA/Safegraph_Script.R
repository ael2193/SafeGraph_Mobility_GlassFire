
################################################################################################
# IMPORT PACKAGES 
## #############################################################################################
library(tidyverse)
library(sp)
library(sf)
library(foreign)
library(zipzcta)
library(lubridate)
library(plotly)
library(ipumsr)
library(png)

################################################################################################
# SET WORKING DIRECTORY 
## #############################################################################################
setwd("/Volumes/22/SafeGraph_Mobility_GlassFire/QMSS RA")

################################################################################################
# READ IN DATA
## #############################################################################################

## Read in June Baseline Data 
june_alt <- read_csv("data/safegraph_data_purchase_apr-18-2022/your_data_apr_18_2022_1041am.csv.gz")

## Read in August Target Data 
august_data_1 <- read_csv("data/safegraph_data_purchase_mar-30-2022/your_data_mar_30_2022_0224pm.csv.gz")
august_data_2 <- read_csv("data/safegraph_data_purchase_apr-11-2022/your_data_apr_11_2022_0723pm.csv.gz")

## Bind August Data 
august <- bind_rows(august_data_1, august_data_2)


## Read in Perimeter Data (Glassfire) Sept 27 - Oct 20
glass_perim <- read.dbf('data/perim_buffer_data/glassfireperim/glassfireperim.dbf')


## Read in Data (Buffer - 10 KM outside of Perimeter)
buffer_perim <- read.dbf('data/perim_buffer_data/bufferminusperim/bufferminusperim.dbf')

## 2nd Buffer (10 km outside of first buffer)
buffer2 <- read.dbf('data/perim_buffer_data/buffer2_april6/buffer2.dbf')

## 2 Buffer and Perimeter (Spatial data of the both buffers and the GF Perimeter)
buffer2_perim <- read.dbf('data/perim_buffer_data/buffer2_perim_april6/buffer2_perim.dbf')

## Grouping by Glassfire, Buffer1, Buffer2 + Changing ZCTA to Vecotor
gfperimvec <- as.vector(glass_perim$ZCTA)
buffer1_vec <- as.vector(buffer_perim$ZCTA_2)
buffer2_vec <- as.vector(buffer2$ZCTA)
buffer2_perim_vec <- as.vector(buffer2_perim$ZCTA)


################################################################################################
# READ IN IMAGES
## #############################################################################################
pp1 <- readPNG("California_Map.png")
pp2 <- readPNG("Cali_Map_Zoom.png")


################################################################################################
# CREATE EXCLUSIVE ZCTA DATA BY BUFFER + PERIMETER
## #############################################################################################


## Creating ZCTA Exclusive to Buffer 2 (Buffer 2- Buffer 1) 
cf_bf2 <- as.vector(unlist(sapply(buffer2_vec[!duplicated(buffer2_vec)], 
                      function(item, tab1, tab2) {
                          rep(item,
                              tab1[item] - ifelse(item %in% names(tab2), tab2[item], 0))
                       }, tab1=table(buffer2_vec), tab2=table(buffer1_vec))))



## Creating ZCTA Exclusive to Buffer 1 (Buffer 1- GF Perimeter) 
cf_bf1 <- as.vector(unlist(sapply(buffer1_vec[!duplicated(buffer1_vec)], 
                      function(item, tab1, tab2) {
                          rep(item,
                              tab1[item] - ifelse(item %in% names(tab2), tab2[item], 0))
                       }, tab1=table(buffer1_vec), tab2=table(gfperimvec))))


################################################################################################
# DATA CLEANING
## #############################################################################################

## August Target Data by Normalized ZCTA
august <- august %>%
  mutate(postal_code = as.character(postal_code)) %>%
  inner_join(., zipzcta, by = c("postal_code" = "zip"))

## June Baseline Data by Normalized ZCTA
june_alt <- june_alt %>%
  mutate(postal_code = as.character(postal_code)) %>%
  inner_join(., zipzcta, by = c("postal_code" = "zip"))

## Find ZCTA That is not in August/June Data
not_zcta <- setdiff(august$zcta,buffer2_perim_vec)
not_zcta <- append(not_zcta, 93012)

## Create Not in Function (Opposite of %in%)
`%notin%` <- Negate(`%in%`)

## Filter out Observations with ZCTA Not in BF1, BF2, GF (June + August)
august <- august %>%
  filter(zcta %in% buffer2_perim_vec)

june_alt <- june_alt %>%
  filter(zcta %in% buffer2_perim_vec)

## Categorize by BF2, BF1, GF by ZCTA Vector
august <- august %>%
  mutate(fire_cat = case_when(zcta %in% cf_bf2 ~ 'BF2', 
                              zcta %in% cf_bf1 ~ 'BF1',
                              TRUE ~ 'GF')) %>%
  mutate(date_range_start = as.POSIXct(strptime(date_range_start, "%Y-%m-%d"))) %>%
  mutate(date_range_end = as.POSIXct(strptime(date_range_end, "%Y-%m-%d"))) %>%
  filter(date_range_start != "2020-08-17") %>%
  mutate(june = case_when(TRUE ~ 'August'))


## Categorize by BF2, BF1, GF by ZCTA Vector
june_alt <- june_alt %>%
  mutate(fire_cat = case_when(zcta %in% cf_bf2 ~ 'BF2', 
                              zcta %in% cf_bf1 ~ 'BF1',
                              TRUE ~ 'GF')) %>%
  mutate(date_range_start = as.POSIXct(strptime(date_range_start, "%Y-%m-%d"))) %>%
  mutate(date_range_end = as.POSIXct(strptime(date_range_end, "%Y-%m-%d"))) %>%
  filter(date_range_start != "2020-08-17") %>%
  mutate(june = case_when(TRUE ~ 'June'))

## RBIND August and June_Alt
com <- bind_rows(august, june_alt)

## Create Baseline Data-set (June) by Individual ZCTA (Not used in EDA)
june_pct_base_zcta <- june_alt %>%
  mutate(week = as.POSIXct(strptime(date_range_start, "%Y-%m-%d"))) %>%
  mutate(week = as_date(week)) %>%
  group_by(zcta, week, fire_cat) %>%
  arrange(zcta, by.groups = TRUE) %>%
  summarize(sum_visits = sum(raw_visit_counts), .groups = 'drop') %>%
  mutate(pct_chg = round((sum_visits - lag(sum_visits))/lag(sum_visits),2)) %>%
  group_by(zcta) %>%
  summarize(pct_chg_june_z = round(median(pct_chg, na.rm = TRUE),2))

## Create Baseline Data-set (June) by Fire Category between Data (Used in EDA)
june_pct_base_fcat <- june_alt %>%
  mutate(week = as.POSIXct(strptime(date_range_start, "%Y-%m-%d"))) %>%
  mutate(week = as_date(week)) %>%
  group_by(zcta, week, fire_cat) %>%
  arrange(zcta, by.groups = TRUE) %>%
  summarize(sum_visits = sum(raw_visit_counts), .groups = 'drop') %>%
  mutate(pct_chg = round((sum_visits - lag(sum_visits))/lag(sum_visits),2)) %>%
  group_by(fire_cat) %>%
  summarize(pct_chg_june_f = round(median(pct_chg, na.rm = TRUE),2))

## Create Combined August Dataset 
aug_combined <- august

## Join June Baseline Data to August Target Data (Individual ZCTA) - Not used in EDA
aug_combined <- august %>%
  inner_join(., june_pct_base_zcta, by = c("zcta" = "zcta"))

## Join June Baseline Data to August Target Data (fire Category)
aug_combined <- aug_combined %>%
  inner_join(., june_pct_base_fcat, by = c('fire_cat' = 'fire_cat'))


################################################################################################
# READ IN EXTERNAL DATA (RUCA, PM25, POVERTY, INDUSTRY)
## #############################################################################################

#### Not used in EDA
rural_ipums <- read_csv("data/urban_rural_ipums_2010/nhgis0006_ds172_2010_zcta.csv")

rural_ipums <- rural_ipums %>%
  mutate(ZCTA = substring(NAME, 7)) %>%
  select(ZCTA, Rural_Dummy)


aug_combined <- aug_combined %>%
  inner_join(., rural_ipums, by = c("zcta" = "ZCTA")) 

aug_combined <- aug_combined %>%
  mutate(Rural_Dummy_I = case_when(Rural_Dummy == 1 ~ 'Rural', TRUE ~ 'Urban'))

###


## RUCA - Rural Dataset (https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes.aspx)
ruca <- readxl::read_xlsx("data/RUCA2010zipcode_Cali.xlsx")


ruca <- ruca %>%
  inner_join(., zipzcta, by = c("ZIP_CODE" = "zip")) %>%
  select(-po_name, -state) 

ruca <- ruca %>%
  filter(zcta %notin% not_zcta)

## Grouping Rural Numerics by RUCA By Categorization C
rural_ruca <- c(4.0, 4.2, 5.0, 5.2, 6.0, 6.1, 7.0, 7.2, 7.3, 
                7.4, 8.0, 8.2, 8.3, 8.4, 9.0, 9.1, 9.2, 10.0, 
                10.2, 10.3, 10.4, 10.5,10.6)

ruca <- ruca %>%
  filter(zcta %in% buffer2_perim_vec) %>%
  mutate(Rural_Dummy_R = case_when(RUCA2 %in% rural_ruca ~ 'Rural', TRUE ~ 'Urban')) 


ruca <- ruca %>%
  select(zcta, Rural_Dummy_R)

## Join into August Data
aug_ex <- aug_combined %>%
  inner_join(., ruca, by = c("zcta" = "zcta")) 

## Create Percent Change Dataset in August Relative to June
pct_chg_aug <- aug_ex %>%
  mutate(week = as.POSIXct(strptime(date_range_start, "%Y-%m-%d"))) %>%
  mutate(week = as_date(week)) %>%
  group_by(zcta, week, Rural_Dummy_I,Rural_Dummy_R, fire_cat, pct_chg_june_z, pct_chg_june_f) %>%
  arrange(zcta, Rural_Dummy, by.groups = TRUE) %>%
  summarize(sum_visits = sum(raw_visit_counts), .groups = 'drop') %>%
  mutate(pct_chg = round((sum_visits - lag(sum_visits))/lag(sum_visits),2)) %>%
  select(zcta, week, Rural_Dummy_I, Rural_Dummy_R, fire_cat, 
         sum_visits, pct_chg, pct_chg_june_z, pct_chg_june_f) %>%
  mutate(pct_chg_mon_z = round(((pct_chg - pct_chg_june_z) / abs(pct_chg_june_z)),2)) %>%
  mutate(pct_chg_mon_f = round(((pct_chg - pct_chg_june_f) / abs(pct_chg_june_f)),2))



## Read in and Cleaining PM25 Data
load('data/wfPM25_imp_2020.rdata')

glass <- glass %>% 
  mutate(bins = case_when(
                          PM25 <= 40 ~ 'Satisfactory',
                          PM25 > 40 ~ 'Poor')) 

glass <- glass %>%
  mutate(zip = as.character(zip)) %>%
  inner_join(., zipzcta, by = c("zip" = "zip")) %>%
  select(zip, date, PM25, wf_pm25_imp, year, bins, zcta)

## Subset Glass by ZCTA and by Date 
subset_glass <- glass %>%
  filter(zcta %in% buffer2_perim_vec) %>%
  filter(date >= '2020-06-22' & date <= '2020-10-12')




## Read in Poverty IPUMS Data 

poverty_ipums <- read_csv("data/poverty_ipums_2015_2019/poverty_zcta.csv")

## ZCTA Unique to California
#zcta_cali <- as.vector(unique(pct_chg_aug$zcta))

## Mutate Variables to Categorize by Quartile 

###  Find quartile of Pct_Agg Poverty and Industry IPUMS Data

poverty_ipums <- poverty_ipums %>% 
  mutate(ZCTA = substring(ZCTA_NAME, 7)) %>%
  filter(ZCTA %in% buffer2_perim_vec) %>%
  mutate(Pct_Agg = as.numeric(Pct_Agg)) %>%
  mutate(Pct_Agg_P = round(Pct_Agg, 2)) %>%
  mutate(Poverty_Dummy_75 = case_when(Pct_Agg >= 11.415 ~ 'Pov>75', TRUE ~ 'Pov<75')) %>%
  ## Sum of ZCTA that have (Counts of under 0.5 and 0.5 - 0.99) / Total Count 
  ## Find Quartile - Those that are in top 25% are dummy coded as Poverty in Data
  mutate(Poverty_Dummy_50 = case_when(Pct_Agg >= 8.260 ~ 'Pov>50', TRUE ~ 'Pov<50')) %>%
  select(ZCTA, Pct_Agg_P, Poverty_Dummy_50, Poverty_Dummy_75)



## Inner join to Main Percent Change August
pct_chg_aug <- pct_chg_aug %>%
  inner_join(., poverty_ipums, by = c("zcta" = "ZCTA")) 



## Industry Ipums (Fishing, Agriculture)

industry_ipums <- read_csv("data/industry_ipums_2015_2019/industry_ipums.csv")


industry_ipums <- industry_ipums %>%
  mutate(ZCTA = substring(NAME_E, 7)) %>%
  filter(ZCTA %in% buffer2_perim_vec) %>%
  mutate(Pct_Agg = as.numeric(Pct_Agg)) %>%
  mutate(Pct_Agg = round(Pct_Agg, 2)) %>% 
  mutate(Pct_Agg_I = ifelse(is.na(Pct_Agg), 0, Pct_Agg)) %>%
  select(ZCTA, Pct_Agg_I) %>%
  mutate(Agr_Dummy_75 = case_when(Pct_Agg_I >= 5.725 ~ 'Agr>75', TRUE ~ 'Agr<75')) %>%
  ## Sum of ZCTA that have (Counts of under 0.5 and 0.5 - 0.99) / Total Count 
  ## Find Quartile - Those that are in top 25% are dummy coded as Industry in Data
  mutate(Agr_Dummy_50 = case_when(Pct_Agg_I >= 3.405 ~ 'Agr>50', TRUE ~ 'Agr<50')) %>%
  select(ZCTA, Pct_Agg_I,Agr_Dummy_50, Agr_Dummy_75)


## Inner join to Main Percent Change August
pct_chg_aug <- pct_chg_aug %>%
  inner_join(., industry_ipums, by = c("zcta" = "ZCTA")) 

## Descriptives + Adding External

com <- com %>%
  inner_join(., ruca, by = c('zcta' = 'zcta')) %>%
  inner_join(., industry_ipums, by = c('zcta' = 'ZCTA')) %>%
  inner_join(., poverty_ipums, by = c('zcta' = 'ZCTA')) %>%
  distinct(Agr_Dummy_75, Poverty_Dummy_75, Rural_Dummy_R, zcta, fire_cat) %>%
  inner_join(., com, by = c('zcta' = 'zcta')) %>%
  mutate(fire_cat = fire_cat.x)



