# load necessary packages
library(sf)
library(tidyverse)
library(bslib)
library(shiny)
library(leaflet)
library(plotly)

# load necessary files from webscrap.R (links need to be updated in webscrap.R)
# source("r/webscrap.R") # this saves necessary CSV files in data folder

## load shape files
sr <- st_read("data/mmr_polbnda_adm1_250k_mimu_1.shp",stringsAsFactors = F)
dst <- st_read("data/mmr_polbnda_adm2_250k_mimu.shp",stringsAsFactors = F)
tsp <- st_read("data/mmr_polbnda_adm3_250k_mimu_1.shp",stringsAsFactors = F)

### simplify the shape files for efficient processing
sr <- st_simplify(sr, dTolerance = 0.02)
dst <- st_simplify(dst, dTolerance = 0.02)
tsp <- st_simplify(tsp, dTolerance = 0.02)

## regional level files
### load data file
data <- read.csv("data/idp_data.csv", header = TRUE, sep = ",")
### rename SR names to match with shape file
data$region <- case_when(
  data$region == "Ayeyawady" ~ "Ayeyarwady",
  data$region == "NayPyiTaw" ~ "Nay Pyi Taw",
  TRUE ~ data$region)
### join undp data
undp_data <- read.csv("data/undp_data.csv", header = TRUE, sep = ",")
data <- data %>% left_join(undp_data, by = "region")

### join geometry to data
data_sr <- data %>% left_join(sr, by = c("region" = "ST")) %>%
  select(-c(ST_PCODE, ST_RG,OBJECTID,ST_MMR,PCode_V))
### change to shape file
data_sr <- st_as_sf(data_sr)

## district level file
### load data file
data1 <- read.csv("data/acled_civ.csv", header = TRUE, sep = ",")
### join geometry to data1
#### preapare shape file to include point data for each district poloygon
dst_coor <- dst %>%
  st_centroid() %>% # convert to points
  st_coordinates() # get coordinates
#### preapare data frame to join 
dst_join <- data.frame(
  DT_PCODE = dst$DT_PCODE,  # Assuming 'DT_PCODE' is an attribute in 'dst'
  geometry = dst$geometry,  # Retain the original geometry column
  DT = dst$DT,              # Include district names column
  long = dst_coor[, 1],    # Longitude of centroids
  lat = dst_coor[, 2])      # Latitude of centroids

#### join data frame to include geometry
##### join according to pcode if available
data1_pcode <- data1 %>% filter (`Admin2.Pcode` != "NA" & # if there is valid Pcode
                                   Admin2 != "Kokang Self-Administered Zone") %>% # different pcodes, so exclude
  left_join(dst_join, by = c("Admin2.Pcode" = "DT_PCODE")) %>%
  select (-DT)
#### As they are "NA" in Pcode of ACLED data, we will join by district names
data1_na <- data1 %>% filter (`Admin2.Pcode`=="NA" | # if there is NA in pcode
                                Admin2 %in% "Kokang Self-Administered Zone") %>% # 
  left_join(dst_join, by = c("Admin2" = "DT")) %>%
  select(-DT_PCODE)
#### combine the two
data_dst <- rbind(data1_pcode, data1_na)

#### a summary table for district and year
data_dst_yr <- data_dst %>%
  group_by(Admin1, Admin2, Year) %>%
  summarise(Events = sum(Events, na.rm = TRUE),
            Fatalities = sum(Fatalities, na.rm = TRUE),
            geometry = first(geometry),
            long = first(long),
            lat = first(lat)) %>%
  ungroup() %>%
  mutate(Admin1 = case_when(
    Admin1 %in% c("Bago-East", "Bago-West") ~ "Bago",
    Admin1 %in% c("Shan-East", "Shan-North", "Shan-South") ~ "Shan",
    TRUE ~ Admin1),  # Default case for Admin1 mapping
    Year = as.numeric(Year))  # Convert Year column to numeric
## transform data frame as sf object
data_dst_sp <- st_as_sf(data_dst_yr, coords = c("long", "lat"), crs = 4326)

## township level file
### load data file
harp_data <- read.csv("data/harp_data.csv", header = TRUE, sep = ",")
### preapare MIMU data file to match with harp data
tsp$TS_PCODE <- case_when(
  tsp$TS_PCODE == "MMR013046" ~ "MMR013008",
  tsp$TS_PCODE == "MMR013047" ~ "MMR013008",
  TRUE ~ tsp$TS_PCODE)
### join geometry to data
data_tsp <- tsp %>% left_join(harp_data, by = c("TS_PCODE" = "Township.Pcode")) %>%
  select(-c(OBJECTID,ST_PCODE,DT_PCODE,TS_PCODE,TS_MMR,PCode_V,
            State.Region.Pcode,State.Region.Name,District.Pcode,District.Name,Township.Name))
### change to shape file
data_tsp <- st_as_sf(data_tsp)

## create objects to put into UI
## create a new color pallet
my_palette1 <- colorRampPalette(c("green", "yellow", "red"))
## Plot the map with the specified color palette

### value box
vb1 <- value_box(
  title = "No. of IDPs",
  value = sum(data_sr$idp_pop,na.rm = TRUE),
  showcase = shiny::icon("person-walking-arrow-right"),
  showcase_layout = "top right",
  theme = value_box_theme(bg = "#FF6E00", fg = "white"),
  p(paste("Region with highest IDP:",data_sr$region[which.max(data_sr$idp_pop)])),
  p(paste("Before coup:",round(sum(data_sr$conflict,na.rm = TRUE),0))))

vb2 <- value_box(
  title = "Proportion returned",
  value = paste(round((sum(data_sr$idp_return,na.rm = TRUE)/sum(data_sr$idp_pop,na.rm = TRUE))*100,1),"%"),
  showcase = shiny::icon("person-walking-arrow-loop-left"),
  showcase_layout = "top right",
  theme = value_box_theme(bg = "#00C04B", fg = "white"),
  p(paste("Total returned to date:", sum(data_sr$idp_return,na.rm = TRUE))))

### cards
## page 1
cd1 <- card(
  height = 300,
  card_header("Summary information about regions"),
  card_body(
    DT::dataTableOutput ("table", height = "100%")),
  card_footer("Source: Population (UNFPA, 2019), IDPs (UNHCR, 2024),
              and Increased poverty (UNDP, 2024)"))

cd2 <- card(
  height = 300,
  card_header("Regional situation map"),
  card_body(
    leafletOutput("map1", height = "100%"),
    card_footer("Source: Increased poverty (UNDP, 2024)")))

cd3 <- card(
  height = 300,
  card_header("Total population and Number of IDPs"),
  card_body(
    #plot1, height = "100%"), # replaced
    plotlyOutput("plot1", height = "100%")),
  card_footer("Source: Population (UNFPA, 2019) and IDPs (UNHCR, 2024)"))

cd4 <- card(
  card_header("Selected parameters"),
  card_body(
    ## display r output objects
    textOutput("selected_region"),
    textOutput("pop_minmax"),
    textOutput("idp_minmax")))

cd5 <- card(
  height = 300,
  card_header("IDP population and increased poverty status"),
  card_body(
    plotlyOutput("plot3", height = "100%")),
  card_footer("Source: Population (UNFPA, 2019) and Increased poverty (UNDP, 2024)"))

## page 2
pg2_cd1 <- card(
  card_header("Civilian fatalities during specific period"),
  card_body(
    plotlyOutput("plot2", height = "400px"),
    leafletOutput("map2", height = "600px"),
    #   leafletOutput("map3", height = "600px"),
    card_footer("Source: ACLED, 2024")))

#pg2_cd2 <- card(
#  card_header("Time trend of civilian fatalities"),
#  card_body(
#    plotlyOutput("plot2", height = "300px"),
#  card_footer("Source: ACLED"))

### sidebar
### page 1
### filter objects
sidebar1 <- accordion(
  accordion_panel(
    "Affected region", # filter item
    helpText("Select region"), # instruction
    icon = shiny::icon("location-dot"),
    checkboxGroupInput("region", label = h4("Region"), 
                       choices = unique(data_sr$region), selected = unique(data_sr$region)),
    open = FALSE
  ),
  accordion_panel(
    "Affected size", 
    helpText("Customize the population range"),
    icon = shiny::icon("sliders"),
    sliderInput("pop", "Population", min = 0, max = max(data_sr$total), value = c(0, max(data_sr$total))),
    sliderInput("idp", "IDP population", min = 0, max = max(data_sr$idp_pop,na.rm = TRUE), value = 0),
    open = FALSE
  )
)

### page 2
sidebar2 <- absolutePanel(
  bottom = 10, left = 10,
  ### select years
  selectInput("year_from", "Select start year:", 
              choices = 2014:as.numeric(format(Sys.Date(),"%Y")),
              selected = min(data_dst_yr$Year)),
  selectInput("year_to", "Select end year:", 
              choices = 2014:as.numeric(format(Sys.Date(),"%Y")),
              selected = max(data_dst_yr$Year)),
  ### select regions
  checkboxGroupInput("region_pg2", label = h4("Region"),
                     choices = unique(data_dst_yr$Admin1), selected = unique(data_dst_yr$Admin1))
)

### page 3
sidebar3 <- absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 80, left = "auto", right = 40, bottom = "auto",
                          width = 330, height = "auto", h4("Customize the map"),
                          style = "background-color: rgba(255, 255, 255, 0.8); padding: 10px;",
                          selectizeInput("region_pg3", "Select region", choices = unique(data_tsp$ST),
                                         selected = unique(data_tsp$ST), multiple = TRUE),
                          sliderInput("vulscore","Vulnerability band", min = 0, max = 8, value = c(0,8)),
                          sliderInput("vulpop", "Vulnerable population", min = 0, max = max(data_tsp$Approximate.Vulnerable.Population,na.rm = TRUE), value = 0),
                          HTML("Source: HARP")
)
