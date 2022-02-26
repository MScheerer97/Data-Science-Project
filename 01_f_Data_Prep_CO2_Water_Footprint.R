##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


#### Data Project - CO2 and Water Footprint Data Acquisition 
## SU-EATABLE LIFE: a comprehensive database of carbon and water footprints of 
## food commodities     

## Citation: Petersson, Tashina; Secondi, Luca; Magnani, Andrea; Antonelli, Marta; 
# Dembska, Katarzyna; Valentini, Riccardo; et al. (2021): 
# SU-EATABLE LIFE: a comprehensive database of carbon and water footprints of 
# food commodities. figshare. Dataset. https://doi.org/10.6084/m9.figshare.13271111.v2 

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("readxl")) install.packages("readxl")


library(dplyr)
library(stringr)
library(readxl)

# Data Acquisition --------------------------------------------------------

### Download file from website and access the two sheets of interest for both data

url <- "https://figshare.com/ndownloader/files/27921765"
temp = tempfile(fileext = ".xlsx")
download.file(url, destfile = temp, mode='wb')

# Carbon Footprint data extraction

co2_data <- read_excel(temp, sheet = "SEL CF for users") 
colnames(co2_data) <- make.names(names(co2_data))

co2_data <- co2_data %>%
  filter(str_detect(Food.commodity.TYPOLOGY, "FRUIT|VEGETABLE"), 
         !str_detect(Food.commodity.TYPOLOGY, "JUICE")) %>% 
  select(Food.commodityITEM, Carbon.Footprint.kg.CO2eq.kg.or.l.of.food.ITEM, 
         Food.commodity.TYPOLOGY) %>% 
  rename(Item = Food.commodityITEM, 
         Carbon_Footprint = Carbon.Footprint.kg.CO2eq.kg.or.l.of.food.ITEM, 
         Food_Type = Food.commodity.TYPOLOGY)

# Water Footprint data extraction
water_data <- read_excel(temp, sheet = "SEL WF for users")
colnames(water_data) <- make.names(names(water_data))

water_data <- water_data %>%
  filter(str_detect(Food.commodity.TYPOLOGY, "FRUIT|VEGETABLE", 
         !str_detect(Food.commodity.TYPOLOGY, "JUICE"))) %>% 
  select(Food.commodityITEM, Water.Footprint.liters.water.kg.o.liter.of.food.ITEM, 
         Food.commodity.TYPOLOGY) %>%
  rename(Item = Food.commodityITEM, 
         Water_Footprint = Water.Footprint.liters.water.kg.o.liter.of.food.ITEM, 
         Food_Type = Food.commodity.TYPOLOGY)

# Add manually researched information to data frame
add_item <- read_excel("data/Additional_CO2_Water_Footprints.xlsx", 
                       sheet = "Footprint")

footprint <- full_join(co2_data, water_data, by = "Item") %>% 
  rbind(., add_item) %>% 
  select(-("Food_Type.y")) %>% 
  rename(Food_Type = Food_Type.x) %>% 
  filter(!str_detect(Food_Type, "FROZEN|CANNED|IMPORTED|DRIED")) %>% 
  mutate(Item = toupper(str_replace_all(Item, "[[:punct:]]", ""))) %>%
  mutate(Item = toupper(str_replace_all(Item, "[[:space:]]g$|[[:space:]]G$", "")) )

saveRDS(footprint, "output/footprint.rds")
write.csv(footprint, "output/footprint.csv")



































