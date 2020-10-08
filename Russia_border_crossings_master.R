library(tidyverse)
library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(ggimage)
library(scales)

# Reading the World polygons and ISO-3166 values
World <- read_sf("data-raw/World_Adj.shp") %>%
  select("ISO_A3", "geometry")

# Subsetting Russia
Russia <- World %>% 
  filter(ISO_A3 == "RUS")

# Reading the RUB/EUR FX rates
# https://excelrates.com/historical-exchange-rates/EUR-RUB
Euro <- read_csv("data-raw/RUB_EUR.csv",
                 col_types = cols(Date = col_date("%m/%d/%y"))
)

Pal_blue <- colorRampPalette(colors =c("#f2f5fb", "#0039A6")) # Russian flag blue color gradients
Pal_red <- colorRampPalette(colors=c("#fceae9", "#D52B1E")) # Russian flag red color gradients
Pal_balance <- colorRampPalette(colors=c("#D52B1E", "#FFFFFF", "#0039A6"))

# Reading inbound data file and pivoting wider
In_raw <- read_csv("data-raw/Russia_in_master.csv",
                   col_types = cols(Date = col_date("%m/%d/%Y"))) %>%
  select(-Type) %>%
  rename(Country = ENG) %>% 
  arrange_at(c("Date", "Trans")) %>% 
  pivot_wider(
    names_from = "Trans",
    values_from = "Value"
  ) %>% 
  mutate(
    Total = Car + Air + Rail + Ship + Walk
  ) %>% 
  select(-c("ISO_A2","M49")) %>% 
  filter(Country != "Russia") %>%
  mutate(Year = as.integer(year(Date))) %>%
  mutate(Month = as.character(month(Date, label = T, abbr = T))) 

Rus_in <- In_raw %>% 
  group_by(Date) %>% 
  summarise(Total_Rus = sum(Total),
            Total_Rus_Air = sum(Air),
            Total_Rus_Car = sum(Car),
            Total_Rus_Rail = sum(Rail),
            Total_Rus_Ship = sum(Ship),
            Total_Rus_Walk = sum(Walk))

# Reading outgoing data file and pivoting wider
Out_raw <- read_csv("data-raw/Russia_out_master.csv",
                    col_types = cols(Date = col_date("%m/%d/%Y"))) %>%
  select(-Type) %>%
  rename(Country = ENG) %>% 
  arrange_at(c("Date", "Trans")) %>% 
  pivot_wider(
    names_from = "Trans",
    values_from = "Value"
  ) %>% 
  mutate(
    Total = Car + Air + Rail + Ship + Walk
  ) %>% 
  select(-c("ISO_A2","M49")) %>% 
  filter(Country != "Russia") %>%
  mutate(Year = as.integer(year(Date))) %>%#
  mutate(Month = as.character(month(Date, label = T, abbr = T))) 
# %>% 
#   mutate(Color = Color_out)

Rus_out <- Out_raw %>% 
  group_by(Date) %>% 
  summarise(Total_Rus = sum(Total),
            Total_Rus_Air = sum(Air),
            Total_Rus_Car = sum(Car),
            Total_Rus_Rail = sum(Rail),
            Total_Rus_Ship = sum(Ship),
            Total_Rus_Walk = sum(Walk))

In <- merge(In_raw, Rus_in, by = "Date") %>%
  # select(-c(Air, Car, Rail, Ship, Walk)) %>%
  group_by(Date) %>%
  mutate(Sort = rank(-Total,
                     ties.method = "last"))

Out <- merge(Out_raw, Rus_out, by = "Date") %>%
  # select(-c(Air, Car, Rail, Ship, Walk)) %>%
  group_by(Date) %>%
  mutate(Sort = rank(-Total,
                     ties.method = "last"))

Bal_raw <- merge(In_raw, Out_raw, by = c("ISO_A3", "Date")) %>%
  mutate(
    Air = Air.x - Air.y,
    Car = Car.x - Car.y,
    Rail = Rail.x - Rail.y,
    Ship = Ship.x - Ship.y,
    Walk = Walk.x - Walk.y,
    Total = Total.x - Total.y
  ) %>%
  select(
    "ISO_A3",
    "Date",
    "Country.x",
    "Air",
    "Car",
    "Rail",
    "Ship",
    "Walk",
    "Total",
    "Total.x",
    "Total.y"
  ) %>%
  rename(Country = Country.x,
         In = Total.x,
         Out = Total.y) %>%
  mutate(Year = as.integer(year(Date))) %>%
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>%
  # mutate(Color = ifelse(Total > 0, 
  #                       Color_in,
  #                       Color_out)
  # ) %>%
  as_tibble()

Rus_bal <- Bal_raw %>% 
  group_by(Date) %>% 
  summarise(Total_Rus = sum(Total),
            Total_Rus_Air = sum(Air),
            Total_Rus_Car = sum(Car),
            Total_Rus_Rail = sum(Rail),
            Total_Rus_Ship = sum(Ship),
            Total_Rus_Walk = sum(Walk))

Bal <- merge(Bal_raw, Rus_bal, by = "Date") %>% 
  group_by(Date) %>% 
  mutate(Total_in = sum(In)) %>% 
  mutate(Total_out = sum(Out))

Bal_pos <- Bal %>%
  group_by(Date) %>%
  mutate(Sort = rank(-Total, 
                     ties.method = "last")
  )

Bal_neg <- Bal %>%
  group_by(Date) %>%
  mutate(Sort = rank(Total, 
                     ties.method = "last")
  )

Bal_full <- rbind(Bal_pos, Bal_neg)

# Reading Abkhazia & S.Ossetia population file
Pop_A_SO <- read_csv("data-raw/XAB_XOS_pop.csv")

# Reading World population file
Pop_WDI <-
  read_csv("data-raw/API_SP.POP.TOTL_DS2_en_csv_v2_1217749.csv", skip = 4) %>%
  # select(-X65) %>%
  pivot_longer(
    cols = c(5:last_col()),
    names_to = "Year",
    values_to = "Pop",
    values_ptypes = c(integer, double)
  ) %>%
  rename("ISO_A3" = "Country Code",) %>%
  select("ISO_A3",
         "Year",
         "Pop") %>%
  rbind(Pop_A_SO) %>% # Adding Abkhazia & South Ossetia
  as_tibble()

Pop_WDI$Year <- as.integer(Pop_WDI$Year)

# Creating inbound  by population
In_by_pop <- In_raw %>%
  merge(Pop_WDI, by = c("ISO_A3", "Year")) %>%
  mutate(Total_by_pop = Total*100/Pop) %>%
  drop_na() %>% 
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>% 
  select(
    ISO_A3,
    Date,
    Month,
    Year,
    Country,
    Total,
    Total_by_pop,
    Air,
    Car,
    Rail,
    Ship,
    Walk,
    Pop
  )

In_by_pop_on_date <- In_by_pop %>% 
  group_by(Date) %>% 
  summarise(Pop_on_date = sum(Pop),
            Total_in_on_date = sum(Total),
            Total_air_on_date = sum(Air),
            Total_car_on_date = sum(Car),
            Total_rail_on_date = sum(Rail),
            Total_ship_on_date = sum(Ship),
            Total_walk_on_date = sum(Walk)
  ) %>% 
  mutate(Total_Rus = Total_in_on_date*100/Pop_on_date,
         Total_Rus_Air = Total_air_on_date*100/Pop_on_date,
         Total_Rus_Car = Total_car_on_date*100/Pop_on_date,
         Total_Rus_Rail = Total_rail_on_date*100/Pop_on_date,
         Total_Rus_Ship = Total_ship_on_date*100/Pop_on_date,
         Total_Rus_Walk = Total_walk_on_date*100/Pop_on_date,
  ) %>% 
  merge(In_by_pop, by = "Date") %>% 
  mutate(Total = Total_by_pop
         # Color = Color_in
  ) %>% 
  select(
    ISO_A3,
    Country,
    Date,
    Month,
    Year,
    Total,
    Total_Rus,
    Total_Rus_Air,
    Total_Rus_Car,
    Total_Rus_Rail,
    Total_Rus_Ship,
    Total_Rus_Walk
  ) %>% 
  group_by(Date) %>%
  mutate(Sort = rank(-Total, 
                     ties.method = "last")
  )

Unique_dates <- c(unique(In_by_pop$Date)) %>% 
  sort()

#Creating new date sequence
Dates_new <- c(Date = seq(as.Date(min(Unique_dates)), as.Date(max(Unique_dates)), by = 91))

Switch <- tibble(Unique_dates, Dates_new) %>% 
  rename(Date = Unique_dates,
         Date_n = Dates_new)

# Reading the texting file for info boxes
Texting <- read_csv("data-raw/Texting.csv")


#### Creating UI elements ####

Aggregation <- radioButtons(
  "aggr",
  h5("Select totals type"),
  choices = c("Quarterly", "Cumulative"),
  selected = "Quarterly",
  inline = T
)

Direction <- selectInput(
  "dir",
  h5("Select border crossing type"),
  choices = c("Into Russia",
              "Into Russia - by populaiton",
              "Out of Russia",
              "Crossing balance"
              ),
  selected = "Into Russia"
)

Radio_number <- radioButtons(
  "number",
  h5("Select No. of countries"),
  choices = c(1,5,10,20,30,50),
  selected = 10,
  inline = T
)

Slider_time <- sliderInput(
  "date",
  h5("Select quarter end"),
  min = min(Dates_new),
  max = max(Dates_new),
  value = min(Dates_new),
  step = 91,
  timeFormat = "%b-%Y",
  ticks = T,
  animate = animationOptions(interval = 2000)
)

Provider_list <- c(
  "Esri.WorldShadedRelief",
  "CartoDB.DarkMatter",
  "CartoDB.VoyagerNoLabels",
  "Esri.WorldGrayCanvas",
  "Esri.WorldPhysical",
  "NASAGIBS.ViirsEarthAtNight2012",
  "Stamen.Watercolor",
  "Stamen.TerrainBackground"
)

Provider <- selectInput(
  "prov",
  h5("Select map view"),
  choices = Provider_list,
)

Author <- h6(
  "Coding and design (c) ",
  a("Maxim Kobzev", href = "https://www.linkedin.com/in/maxim-kobzev/"),
  style = "font-family: Segoe Script !important;",
  align = "center"
)

# Creating top countries list

Top_q_header <- htmlOutput("top_q_text")
Top_q_header_in <- htmlOutput(("top_q_text_in"))
Top_q_header_out <- htmlOutput(("top_q_text_out"))

Top_cum_header <- htmlOutput("top_cum_text")
Top_cum_header_in <- htmlOutput("top_cum_text_in")
Top_cum_header_out <- htmlOutput("top_cum_text_out")

Top_q_sgl <- fluidRow(
  style = "overflow-y: scroll; max-height: 66vh !important;",
  box(width = 8,
      htmlOutput("top_q_cntr")),
  box(width = 4,
      htmlOutput("top_q_nmbr"))
)

Top_cum_sgl <- fluidRow(
  style = "overflow-y: scroll; max-height: 66vh !important;",
  box(width = 8,
      htmlOutput("top_cum_cntr")),
  box(width = 4,
      htmlOutput("top_cum_nmbr"))
)

Top_q_pos_in <- fluidRow(
  style = "overflow-y: scroll; max-height: 28vh !important;",
  box(width = 8,
      htmlOutput("top_q_cntr_in")),
  box(width = 4,
      htmlOutput("top_q_nmbr_in"))
)

Top_cum_pos_in <- fluidRow(
  style = "overflow-y: scroll; max-height: 28vh !important;",
  box(width = 8,
      htmlOutput("top_cum_cntr_in")),
  box(width = 4,
      htmlOutput("top_cum_nmbr_in"))
)

Top_q_pos_out <- fluidRow(
  style = "overflow-y: scroll; max-height: 28vh !important;",
  box(width = 8,
      htmlOutput("top_q_cntr_out")),
  box(width = 4,
      htmlOutput("top_q_nmbr_out"))
)

Top_cum_pos_out <- fluidRow(
  style = "overflow-y: scroll; max-height: 28vh !important;",
  box(width = 8,
      htmlOutput("top_cum_cntr_out")),
  box(width = 4,
      htmlOutput("top_cum_nmbr_out"))
)

Country_panel <- column(
  width = 12,
  conditionalPanel(
    condition = "input.aggr == 'Quarterly'",
    conditionalPanel(
      condition = "input.dir != 'Crossing balance'",
                     box(width = NULL,
                         # Top_q_header,
                         Top_q_sgl)),
    conditionalPanel(
      condition = "input.dir == 'Crossing balance'",
      box(
        width = NULL,
        Top_q_header_in,
        Top_q_pos_in,
        br(),
        Top_q_header_out,
        Top_q_pos_out
      )
    )
  ),
  conditionalPanel(
    condition = "input.aggr == 'Cumulative'",
    conditionalPanel(
      condition = "input.dir != 'Crossing balance'",
                     box(width = NULL,
                         Top_cum_header,
                         Top_cum_sgl)),
    conditionalPanel(
      condition = "input.dir == 'Crossing balance'",
      box(
        width = NULL,
        Top_cum_header_in,
        Top_cum_pos_in,
        br(),
        Top_cum_header_out,
        Top_cum_pos_out
      )
    )
  )
)

Totals_text <- box(
  width = 12,
conditionalPanel(condition = "input.aggr == 'Quarterly'",
                 htmlOutput("totals_txt_q")),
conditionalPanel(condition = "input.aggr == 'Cumulative'",
                 htmlOutput("totals_txt_cum"))
)

Summary_aggr <- htmlOutput("summ_aggr")
Summary_dir <- htmlOutput("summ_dir")
Summary_date <- htmlOutput("Mnth_Year")

Countries_text <- box(
  width = 12,
  conditionalPanel(condition = "input.aggr == 'Quarterly'",
                   htmlOutput("cntr_txt_q")),
  conditionalPanel(condition = "input.aggr == 'Cumulative'",
                   htmlOutput("cntr_txt_cum"))
)

src = "www/russia-flag.png"

YOY_box_height <- "7.75vh"
Chart_trans_height <- "60vh"

Totals_panel_cum <- column(
  12,
  box(
    width = NULL,
    valueBoxOutput("total_value_cum",
                   width = NULL),
    plotOutput("ch_by_trans",
               height = Chart_trans_height)
  )
)

Totals_panel_q <- column(
  12,
  box(
    width = NULL,
    valueBoxOutput("total_value_q",
                   width = NULL),
    plotOutput("ch_total",
               height = YOY_box_height)
  ),
  box(
    width = NULL,
    valueBoxOutput("total_trend",
                   width = NULL),
    plotOutput("ch_yoy",
               height = YOY_box_height)
  ),
  box(
    width = NULL,
    valueBoxOutput("FX_value",
                   width = NULL),
    plotOutput("ch_fx",
               height = YOY_box_height)
  ),
  box(
    width = NULL,
    valueBoxOutput("FX_YOY",
                   width = NULL),
    plotOutput("ch_fx_yoy",
               height = YOY_box_height)
  )
)

# Creating source info
Source <- h5("Source: Border Service of the Russian Federation", 
   a("(incoming /", href = "https://fedstat.ru/indicator/38479"),
   a(" outgoing)", href = "https://fedstat.ru/indicator/38480"),
   align = "center"
)

Map_group_list <- c("Shaded Releif",
                    "Dark Matter",
                    "Voyager",
                    "Gray Canvas",
                    "Physical",
                    "Earth at Night",
                    "Watercolor",
                    "Terrain")

# Creating base leaflet map
basemap <- leaflet(options = leafletOptions(zoomq = 0.1,  
                                            # For setting the non-integer zoom intervals
                                            zoomDelta = 0.1)
                   ) %>%
  addProviderTiles("Esri.WorldShadedRelief", 
                   group = "Shaded Releif") %>%
  addProviderTiles("CartoDB.DarkMatter", 
                   group = "Dark Matter") %>%
  addProviderTiles("CartoDB.VoyagerNoLabels", 
                   group = "Voyager") %>%
  addProviderTiles("Esri.WorldGrayCanvas", 
                   group = "Gray Canvas") %>%
  addProviderTiles("Esri.WorldPhysical", 
                   group = "Physical") %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", 
                   group = "Earth at Night") %>%
  addProviderTiles("Stamen.Watercolor", 
                   group = "Watercolor") %>%
  addProviderTiles("Stamen.TerrainBackground", 
                   group = "Terrain") %>% 
  addLayersControl(
    baseGroups = Map_group_list
  ) %>% 
  addPolygons(data = Russia,
              smoothFactor = 0.3,
              fillOpacity = 0.8,
              dashArray = "1",
              color = "gray",
              weight = 1,
              opacity = 0.8,
              fillColor = "#98FB98") %>% 
  setView(36.0, 45.0, 2.5)

#### UI part as such ####

Header <-   column(12,
                   box(
                     width = NULL,
                     h1("Border crossings with Russian Federation, 2010 - 2020",
                        HTML("&emsp;", "&emsp;"),
                     img(src = "russia-flag.png", height = "30vh"),
                        align = "center")
                   ))

Summary <- box(
  width = NULL,
  box(width = 12,
      Summary_aggr),
  box(width = 12,
      Summary_dir),
  box(width = 12,
      Summary_date)
)

Controls <- column(
  12,
  column(2,
         Aggregation,
         align = "center"),
  column(2,
         Direction,
         align = "center"),
  column(3,
         Slider_time,
         align = "center"),
  column(2,
         Radio_number,
         align = "center"),
  column(3,
         Summary
  )
)

Totals <- column(3,
                 box(
                   width = 12,
                   Totals_text,
                   conditionalPanel(condition = "input.aggr == 'Quarterly'",
                                    Totals_panel_q),
                   conditionalPanel(condition = "input.aggr == 'Cumulative'",
                                    Totals_panel_cum)
                 ))

Table <- column(4,
       box(width = 12,
           Country_panel)
       )

Maps <-column(8,
  tags$style(type = "text/css",
             "#map {height: 62vh !important;} "),
  box(width = 12,
      leafletOutput("map", width = "100%"),
      Source,
      Author)
)

Countries <- box(
  width = 9,
  column(
    width = 12,
    fluidRow(box(
      width = 12,
      Countries_text)
    ),
    Maps,
    Table)
)

ui <- fluidPage(
  theme = "slate.css",
  tags$head(includeCSS("styles-2.css")),
  useShinydashboard(),
  Header,
  Controls,
  Totals,
  Countries
)

#### UI as such end ####

#### Server function ####
server <- function(input, output, session) {

  No_of_countries <- reactive({
    as.integer(input$number)
  })
  
  # Selecting direction
  Direction_full <- reactive({
    if (input$dir == "Out of Russia") {
      Out
    } else {
      if (input$dir == "Into Russia") {
        In
      } else {
        if (input$dir == "Into Russia - by populaiton") {
          In_by_pop_on_date
        } else {
          Bal_full
          }
      }
    }
  })

# Sorting out the dataframe by No. of countries
  Direction_sorted <- reactive({
    req(input$dir)
    Direction_full() %>%
      filter(Sort <= No_of_countries()) %>%
      merge(Switch, by = "Date") %>%
      merge(Euro, by = "Date") %>%
      select(-Date) %>%
      merge(World, by = "ISO_A3") %>%
      st_as_sf %>%
      filter(Date_n == input$date) %>%
      arrange(Total)
  })
  
  Master_sorted <- reactive({
    req(input$dir)
    Direction_full() %>%
      filter(Sort <= No_of_countries()) %>%
      merge(Switch, by = "Date") %>%
      merge(Euro, by = "Date") %>%
      select(-Date)
  })
  
  # Creating master tibble for value boxes / charts
  Master_box <- reactive({
    Master_sorted() %>%
      select(Date_n, 
             Total_Rus,
             Total_Rus_Air,
             Total_Rus_Car,
             Total_Rus_Rail,
             Total_Rus_Ship,
             Total_Rus_Walk,
             RUB_EUR) %>%
      unique() %>%
      arrange(Date_n) %>%
      mutate(
        Total_Rus_LY = lag(Total_Rus, 4),
        Total_Rus_YOY = (((Total_Rus - Total_Rus_LY)/abs(Total_Rus_LY))*100 - 1),
        RUB_EUR_LY = lag(RUB_EUR, 4),
        RUB_EUR_YOY = 100*(RUB_EUR - RUB_EUR_LY)/RUB_EUR_LY
      ) %>%
      select(Date_n, 
             Total_Rus, 
             Total_Rus_YOY,
             Total_Rus_Air,
             Total_Rus_Car,
             Total_Rus_Rail,
             Total_Rus_Ship,
             Total_Rus_Walk,
             RUB_EUR, 
             RUB_EUR_YOY) %>%
      replace_na(list(
        Total_Rus_LY = 0,
        Total_Rus_YOY = 0,
        RUB_EUR_LY = 0,
        RUB_EUR_YOY = 0
      ))
  })
  
  Master_chart <- reactive({
    Master_box() %>% 
    filter(Date_n <= input$date)
  })
  
  # Creating master tibble for great circles
  Master_frame <- reactive ({
    req(input$date)
    req(input$dir)
    Master_sorted() %>%
      filter(Date_n == input$date)
  })
  
  Top_q <- reactive({
    req(input$dir)
    Master_frame() %>% 
      filter(Sort <= No_of_countries()) %>% 
      arrange(desc(Total)) %>% 
      select(Date_n, Sort, Country, Total)
  })
  
  Top_q_in <- reactive({
    req(input$dir)
    Top_q() %>% 
    filter(Total >= 0)
  })
  
  Top_q_out <- reactive ({
    req(input$dir)
    Top_q() %>%
      filter(Total < 0) %>%
      arrange(Sort)
  })

  # Creating accumulated incoming top list master tibble
  Total_cum_func <- function(Sign) {
    Direction_full() %>%
      merge(Switch, by = "Date") %>%
      select(-Date) %>%
      filter(Date_n <= input$date) %>%
      group_by(Country) %>%
      mutate(Total_country = sum(Total)) %>%
      select(Date_n, Month, Year, Country, ISO_A3, Total_country) %>%
      group_by(Date_n) %>%
      unique() %>%
      mutate(Sort = rank(Sign*Total_country,
                         ties.method = "last")) %>%
      rename(Total = Total_country) %>% 
      filter(Sort <= No_of_countries()) %>%
      filter(Date_n == input$date) %>%
      merge(World, by = "ISO_A3") %>% 
      arrange(Sort) %>% 
      st_as_sf()
  }
  
  Total_cum_pos <- reactive({
    req(input$dir)
    Total_cum_func(-1)
  })
  
  Total_cum_neg <- reactive({
    req(input$dir)
    Total_cum_func(1)
  })
  
  
  Color_select_q <- reactive ({
    if (input$dir == "Crossing balance") {
        Pal_balance(nrow(Direction_sorted()))
      } else {
        if (input$dir != "Out of Russia") {
          Pal_blue(nrow(Direction_sorted()))
        } else {
          Pal_red(nrow(Direction_sorted()))
        }
      }
  })

  Color_select_cum <- reactive ({ 
      if (input$dir == "Crossing balance") {
        Pal_balance(nrow(Total_cum_pos()) +
                      nrow(Total_cum_neg())
                    )
      } else {
        if (input$dir != "Out of Russia") {
          Pal_blue(nrow(Total_cum_pos()))
        } else {
          Pal_red(nrow(Total_cum_pos()))
        }
      }
  })
  
  Master_frame_geo_cum <- reactive ({
    req(input$dir)
    if(input$dir == "Crossing balance") {
      rbind(Total_cum_pos(), Total_cum_neg()) %>%
        arrange(Total) %>%
        mutate(Color = Color_select_cum())
    } else {
      Total_cum_pos() %>% 
      arrange(Total) %>%
        mutate(Color = Color_select_cum())
    }
  })
  
  Master_frame_geo_q <- reactive ({
    req(input$dir)
    Direction_sorted() %>%
      mutate(Color = Color_select_q())
  })
  
  #Selecting the geo master frame for drawing the map
  
  Master_frame_geo <- reactive({
    if(input$aggr == "Quarterly") {
      Master_frame_geo_q()
    } else {
      Master_frame_geo_cum()
    }
  })
  
  Quarter_month <- reactive ({
    unique(Master_frame()$Month[Master_frame()$Date_n == input$date])
  })
  
  Quarter_year <- reactive ({
    unique(Master_frame()$Year[Master_frame()$Date_n == input$date])
  })
  
  Aggr_type <- reactive({
    HTML(paste0(
      h3(
        "Totals: ",
        input$aggr
        # align = "center"
      )
    ))
  })
  
  Dir_type <- reactive({
    HTML(paste0(
      h3(
        "Crossing: ",
        input$dir
        # align = "center"
      )
    ))
  })
  
  Mon_Yr <- reactive({
    HTML(paste0(
      h3(
        # Texting$Top_countries[Texting$Direction == input$dir],
        "Quarter ending: ",
        Quarter_month(),
        "-",
        Quarter_year()
        # align = "center"
      )
    ))
  })
  
  Ttl_text_q <- reactive({
    HTML(paste0(h3("Totals, ",
                   Texting$Header_q[Texting$Direction == input$dir],
                   align = "center")))
  })
  
  Ttl_text_cum <- reactive({
    HTML(paste0(h3("Totals, ",
                   Texting$Header_cum[Texting$Direction == input$dir],
                   align = "center")))
  })
  
  Country_text_q <- reactive({
    HTML(paste0(
      h3("Top", input$number, " countries, ",
                   Texting$Header_q[Texting$Direction == input$dir],
                   align = "center")))
  })
  
  Country_text_cum <- reactive({
    HTML(paste0(
      h3("Top", input$number, " countries, ",
         Texting$Header_cum[Texting$Direction == input$dir],
         align = "center")))
  })
  
  q_value <- reactive({
    Master_box()$Total_Rus[Master_box()$Date_n == input$date]
  })
  
  cum_value <- reactive({
    Master_chart() %>% 
      select(Total_Rus) %>% 
      unique() %>% 
      sum()
  })
  
  cum_value_air <- reactive({
    Master_chart() %>% 
      select(Total_Rus_Air) %>% 
      unique() %>% 
      sum()
  })
  
  cum_value_car <- reactive({
    Master_chart() %>% 
      select(Total_Rus_Car) %>% 
      unique() %>% 
      sum()
  })
  
  cum_value_rail <- reactive({
    Master_chart() %>% 
      select(Total_Rus_Rail) %>% 
      unique() %>% 
      sum()
  })
  
  cum_value_ship <- reactive({
    Master_chart() %>% 
      select(Total_Rus_Ship) %>% 
      unique() %>% 
      sum()
  })
  
  cum_value_walk <- reactive({
    Master_chart() %>% 
      select(Total_Rus_Walk) %>% 
      unique() %>% 
      sum()
  })
  
  Info_box_value_q <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(q_value(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(q_value(), big.mark = ",")
    }
  })
  
  Info_box_value_cum <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(cum_value(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(cum_value(), big.mark = ",")
    }
  })
  
  Info_box_value_cum_air <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(cum_value_air(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(cum_value_air(), big.mark = ",")
    }
  })
  
  Info_box_value_cum_car <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(cum_value_car(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(cum_value_car(), big.mark = ",")
    }
  })
  
  Info_box_value_cum_rail <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(cum_value_rail(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(cum_value_rail(), big.mark = ",")
    }
  })
  
  Info_box_value_cum_ship <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(cum_value_ship(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(cum_value_ship(), big.mark = ",")
    }
  })
  
  Info_box_value_cum_walk <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(cum_value_walk(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(cum_value_walk(), big.mark = ",")
    }
  })
  
  # Composing the text for Total info box
  Info_box_text_total_q <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir])
    } else {
      ifelse(
        q_value() < 0,
        paste0("Outgoing surplus ('out' > 'in')"),
        paste0("Incoming surplus ('in' > 'out')")
      )
    }
  })
  
  # Composing the text for Total info box
  Info_box_text_total_cum <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir], ", total")
    } else {
      ifelse(
        cum_value() < 0,
        paste0("Outgoing surplus ('outgoing' > 'incoming'), total"),
        paste0("Incoming surplus ('incoming' > 'outgoing'), total")
      )
    }
  })
  
  Info_box_text_total_cum_air <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir], ", air")
    } else {
      ifelse(
        cum_value_air() < 0,
        paste0("Outgoing surplus ('outgoing' > 'incoming'), air"),
        paste0("Incoming surplus ('incoming' > 'outgoing'), air")
      )
    }
  })
  
  Info_box_text_total_cum_car <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir], ", car")
    } else {
      ifelse(
        cum_value_car() < 0,
        paste0("Outgoing surplus ('outgoing' > 'incoming'), car"),
        paste0("Incoming surplus ('incoming' > 'outgoing'), car")
      )
    }
  })
  
  Info_box_text_total_cum_rail <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir], ", rail")
    } else {
      ifelse(
        cum_value_rail() < 0,
        paste0("Outgoing surplus ('outgoing' > 'incoming'), rail"),
        paste0("Incoming surplus ('incoming' > 'outgoing'), rail")
      )
    }
  })
  
  Info_box_text_total_cum_ship <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir], ", ship")
    } else {
      ifelse(
        cum_value_ship() < 0,
        paste0("Outgoing surplus ('outgoing' > 'incoming'), ship"),
        paste0("Incoming surplus ('incoming' > 'outgoing'), ship")
      )
    }
  })
  
  Info_box_text_total_cum_walk <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir], ", walk")
    } else {
      ifelse(
        cum_value_walk() < 0,
        paste0("Outgoing surplus ('outgoing' > 'incoming'), walk"),
        paste0("Incoming surplus ('incoming' > 'outgoing'), walk")
      )
    }
  })
  
  Box_q_total <- reactive({
    valueBox(
      paste0(
        Info_box_value_q()
        ),
      Info_box_text_total_q(),
      icon = icon(
        ifelse(input$dir %in% c("Into Russia", "Into Russia - by populaiton"), 
               "sign-in-alt",
               ifelse(input$dir == "Crossing balance",
                      "exchange-alt",
                      "sign-out-alt"
               )
        )
      ),
      color = ifelse(
        Info_box_value_q() == 0,
        "purple",
        ifelse(Info_box_value_q() > 0, "green", "red")
      )
    )
  })
  
  Box_cum_total <- reactive ({
    valueBox(
    paste0(
      Info_box_value_cum()
    ),
    Info_box_text_total_cum(),
    icon = icon(
      ifelse(input$dir %in% c("Into Russia", "Into Russia - by populaiton"), 
             "sign-in-alt",
             ifelse(input$dir == "Crossing balance",
                    "exchange-alt",
                    "sign-out-alt"
             )
      )
    ),
    color = ifelse(Info_box_value_cum() > 0, 
                   "green", 
                   "red")
  )
})
  
  Box_cum_air_total <- reactive ({
    valueBox(
      paste0(
        Info_box_value_cum_air()
      ),
      Info_box_text_total_cum_air(),
      icon = icon(
        ifelse(input$dir %in% c("Into Russia", "Into Russia - by populaiton"), 
                         "plane-arrival",
               ifelse(input$dir == "Crossing balance",
                      "plane",
                      "plane-departure"
                      )
        )
      ),
      color = ifelse(Info_box_value_cum_air() > 0, 
                     "green", 
                     "red")
    )
  })
  
  Box_cum_car_total <- reactive ({
    valueBox(
      paste0(
        Info_box_value_cum_car()
      ),
      Info_box_text_total_cum_car(),
      icon = icon(
        "car"
      ),
      color = ifelse(Info_box_value_cum_car() > 0, 
                     "green", 
                     "red")
    )
  })
  
  Box_cum_rail_total <- reactive ({
    valueBox(
      paste0(
        Info_box_value_cum_rail()
      ),
      Info_box_text_total_cum_rail(),
      icon = icon(
        "train"
      ),
      color = ifelse(Info_box_value_cum_rail() > 0, 
                     "green", 
                     "red")
    )
  })
  
  Box_cum_ship_total <- reactive ({
    valueBox(
      paste0(
        Info_box_value_cum_ship()
      ),
      Info_box_text_total_cum_ship(),
      icon = icon(
        "ship"
      ),
      color = ifelse(Info_box_value_cum_ship() > 0, 
                     "green", 
                     "red")
    )
  })
  
  Box_cum_walk_total <- reactive ({
    valueBox(
      paste0(
        Info_box_value_cum_walk()
      ),
      Info_box_text_total_cum_walk(),
      icon = icon(
        "walking"
      ),
      color = ifelse(Info_box_value_cum_walk() > 0, 
                     "green", 
                     "red")
    )
  })
  
  q_value_YOY <- reactive({
    Master_box()$Total_Rus_YOY[Master_box()$Date_n == input$date]
  })
  
  
  Info_box_YOY <- reactive({
      round(q_value_YOY(), 2)
  })
  
  # Composing the text for YOY info box
  Info_box_text_YOY <- reactive({
    if (q_value_YOY() == 0) {
      paste0("")
    } else {
      if (input$dir != "Crossing balance") {
        paste0(Texting$Box_YOY[Texting$Direction == input$dir])
      } else {
        ifelse(
          q_value_YOY() < 0,
          paste0("YOY balance change (in favor of more outgoing)"),
          paste0("YOY balance change (in favor of more incoming)")
        )
      }
    }
  })
  
  # Composing the text for top countries info box
  Text_box_countries <- reactive({
    paste0(
      "Top ", 
      No_of_countries(),
      " countries, ",
      Quarter_month(),
      "-",
      Quarter_year()
    )
  })
  
  Text_box_countries_in <- reactive({
    paste0(
      "Incoming surplus"
    )
  })
  
  Text_box_countries_out <- reactive({
    paste0(
      "Outgoing surplus"
    )
  })
  
  # Composing the text or all-time top countries info box
  Text_box_countries_cum <- reactive({
    HTML(
      paste0(
      "Mar-2010 through ",
      Quarter_month(),
      "-",
      Quarter_year()
      )
    )
  })
  
  Text_box_countries_cum_in <- reactive({
    HTML(
      paste0(
      "Mar-2010 through ",
      Quarter_month(),
      "-",
      Quarter_year(),
      ", incoming surplus"
    )
    )
  })
  
  Text_box_countries_cum_out <- reactive({
    HTML(
      paste0(
      "Mar-2010 through ",
      Quarter_month(),
      "-",
      Quarter_year(),
      ", outgoing surplus"
    )
    )
  })
  
  Box_YOY <- reactive({
      valueBox(
        ifelse(q_value_YOY() == 0,
               paste0(""),
        paste0(Info_box_YOY(), "%")),
        Info_box_text_YOY(),
        icon = icon(ifelse(
          Info_box_YOY() == 0,
          "",
          ifelse(Info_box_YOY() > 0,
                 "arrow-up",
                 "arrow-down")
        )),
        color = ifelse(
          Info_box_YOY() == 0,
          "purple",
          ifelse(Info_box_YOY() > 0, "green", "red")
        )
      )
  })

# Creating the themes for bar charts
  
  Theme_chart <- theme(
    panel.background = element_rect(fill = "#484d51"),
    plot.background = element_rect(fill = "#484d51", color = "#484d51"),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_blank()
  )
  
  Theme_chart_trans <- theme(
    panel.background = element_rect(fill = "#484d51"),
    plot.background = element_rect(fill = "#484d51", color = "#484d51"),
    # axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    plot.title = element_blank()
  )

  Theme_axes <- theme(
    axis.text.x = element_text(color = "#aaaaaa"),
    axis.text.y = element_text(color = "#aaaaaa"),
    axis.title.y = element_text(color = "#aaaaaa")
  )
  
  Theme_axes_trans <- theme(
    axis.text.x = element_text(color = "#aaaaaa",
                               # family = "Segoe UI",
                               face = "bold",
                               size = 14),
    axis.text.y = element_text(color = "#aaaaaa", 
                               # family = "Segoe UI", 
                               face = "bold", 
                               size = 24),
    axis.title.y = element_text(color = "#aaaaaa")
  )
  
  # Creating theme elements for Y axis texts
  Theme_YOY <- reactive ({
    if (q_value_YOY() == 0) {
    }
    else {
      Theme_axes
    }
  })
  
  Scale_name <- reactive ({
    if(input$dir != "Into Russia - by populaiton") 
      {
    "Million"
    } else {
    "Percent"
    }
  })
  
  Scale_div <- reactive({
    if(input$dir != "Into Russia - by populaiton") 
    {1000000} else {1}
  })
  
  Round_factor <- reactive({
    if(input$dir != "Into Russia - by populaiton") 
    {1} else {2}
  })
  
  # Scaling the Y axis by c(min, mean, max)
  Scale_seq <- reactive ({
    c(
      round(min(Master_chart()$Total_Rus / Scale_div()), Round_factor()),
      round(max(Master_chart()$Total_Rus / Scale_div()), Round_factor()),
      round(mean(Master_chart()$Total_Rus / Scale_div()), Round_factor())
    )
  })
  
  Scale_seq_trans <- reactive ({
    c(
      min(Trans_values()),
      max(Trans_values()),
      mean(Trans_values())
    )
  })
  
  Scale_Total <- reactive ({
    scale_y_continuous(name = Scale_name(),
                       breaks = Scale_seq())
  })
  
  #Creating the transport split data frame
  Trans_values <- reactive({
    c(cum_value_air(),
      cum_value_car(),
      cum_value_rail(),
      cum_value_ship(),
      cum_value_walk()
    )
  })
  
  Trans_bar_master <- reactive({
    data_frame(Trans = c("Air", "Car", "Rail", "Ship", "Walk") ,
               Value = Trans_values(),
               Icon = c("www/plane.png", 
                        "www/car.png", 
                        "www/train.png", 
                        "www/ship.png", 
                        "www/walk.png"))
  })
  
  
# Creating the bar chart for total trend
Box_chart_total <- reactive({
  ggplot(Master_chart()) +
    geom_line(
      stat = "identity",
      size = 1.5,
      color = ifelse(Master_chart()$Total_Rus >= 0,
                     "MediumSeaGreen",
                     "#ff5050"),
      aes(x = Date_n,
          y = Total_Rus/Scale_div())
    ) +
    Theme_chart + 
    Theme_axes +
    Scale_Total()
})

# Creating labels for transport split (cumulative)
Labels_trans <- reactive({
  if(input$dir != "Into Russia - by populaiton") {
    scales::label_number_si()
  } else {
    scales::comma
  }
})

Max_value <- reactive({abs(max(Trans_bar_master()$Value))})
Min_value <- reactive({abs(min(Trans_bar_master()$Value))})

Val_selector <- reactive({
  if(abs(max(Trans_bar_master()$Value)) > abs(min(Trans_bar_master()$Value))) {
    max(Trans_bar_master()$Value)
  } else {
    min(Trans_bar_master()$Value)
  }
})

Offset_dist <- reactive({
  max(abs(c(Max_value()*0.01, Min_value()*0.01)))
})

Offset_icon <- reactive({
  Offset_dist()*7
})

Scale_disp <-  reactive({
  if(input$dir != "Into Russia - by populaiton") {
    1e-6
  } else {1}
})

Suffix_disp <- reactive({
  if(input$dir != "Into Russia - by populaiton") {
    "M"
  } else {"%"}
})

Val_disp <- reactive({
  ifelse(Trans_bar_master()$Value == Val_selector(),
  label_comma(scale = Scale_disp(),
              accuracy = 0.01,
              suffix = Suffix_disp())(Val_selector()),
  "")
})

My_pal <- c("#8b008b", "#cc00cc", "#ff1aff", "#b300b3", "#4d004d")

Chart_trans <- reactive({
  ggplot(Trans_bar_master(),
         aes(
           x = reorder(Trans, Value),
           y = Value,
           fill = Trans
         )) +
    scale_fill_viridis_d(option = "E", direction = 1) +
    # scale_fill_manual(values = My_pal) +
    geom_col(width = 0.8) +
    geom_text(
      aes(y = Val_selector(),
          label = Val_disp()),
      hjust = ifelse(Trans_bar_master()$Value > 0,
                     "right",
                     "left"),
      size = 8,
      fontface = "bold",
      color = "#aaaaaa",
    ) +
    coord_flip() +
    geom_image(
      aes(
        x = Trans,
        y = ifelse(Trans_bar_master()$Value > 0,-Offset_icon(),
                   Offset_icon()),
        image = Icon
      ),
      color = "#aaaaaa",
      asp = 0.7,
      size = 0.11
    ) +
    guides(fill = F) +
    Theme_chart_trans +
    Theme_axes_trans +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = Scale_name(),
                       breaks = Scale_seq_trans(),
                       labels = Labels_trans())
})
  
  # Creating the bar chart for YOY trend
  Box_chart_YOY <- reactive({
    ggplot(Master_chart(),
           aes(x = Date_n,
               y = Total_Rus_YOY)) +
      geom_col(fill = ifelse(Master_chart()$Total_Rus_YOY >= 0, 
                             "MediumSeaGreen", 
                             "#ff5050")
               ) + 
      Theme_chart + 
      Theme_YOY() +
      scale_y_continuous(name = "%%",
        breaks = seq(
        round(min(Master_chart()$Total_Rus_YOY), 0),
        round(max(Master_chart()$Total_Rus_YOY), 0),
        (round(max(Master_chart()$Total_Rus_YOY) - min(Master_chart()$Total_Rus_YOY)))
      )
      )
  })
  
  q_value_FX_YOY <- reactive({
    Master_box()$RUB_EUR_YOY[Master_box()$Date_n == input$date]
  })
  
  # Creating an FX value for FX value box
  FX_value <- reactive({
    format(round(Master_box()$RUB_EUR[Master_box()$Date_n == input$date], 2), big.mark = ",")
  })
  
  # Creating an FX value for FX value box
  FX_value_YOY <- reactive({
    format(round(q_value_FX_YOY(), 2), big.mark = ",")
  })
  
  # Composing the text for FX info box
  FX_text <- paste0("RUB/EUR exchange rate")
  
  # Composing the text for FX YOY info box
  FX_text_YOY <- reactive({
    if(q_value_FX_YOY() == 0) {
      paste0("")
    } else {
    paste0("YOY change, RUB/EUR exchange rate")
    }
  })
  
  Box_FX_total <- reactive({
    valueBox(
      paste0(FX_value()),
      FX_text,
      icon = icon("euro-sign"),
      color = "yellow")
  })
  
  Box_FX_YOY <- reactive({
    valueBox(
      ifelse(q_value_FX_YOY() == 0,
             paste0(""),
             paste0(FX_value_YOY(), "%")),
      FX_text_YOY(),
      icon = icon(ifelse(
        FX_value_YOY() == 0,
        "",
        ifelse(FX_value_YOY() > 0,
               "arrow-up",
               "arrow-down")
      )),
      color = ifelse(
        FX_value_YOY() == 0,
        "purple",
        ifelse(FX_value_YOY() > 0, "green", "red")
      )
    )
  })

  # Creating the chart for EUR/RUB FX rate
  Box_chart_FX <- reactive({
    ggplot(Master_chart()) +
      geom_line(stat = "identity",
                color = "#e6c300",
                size = 1.5,
                aes(x = Date_n,
                    y = RUB_EUR)) +
      Theme_chart +
      Theme_axes +
      scale_y_continuous(name = "RUB/EUR",
                         breaks = seq(
        round(min(Master_chart()$RUB_EUR), 1),
        round(max(Master_chart()$RUB_EUR), 1),
        (round(max(Master_chart()$RUB_EUR) - min(Master_chart()$RUB_EUR)))/2
      )
      )
  })
  
  # Creating the chart for EUR/RUB FX YOY
  Box_chart_FX_YOY <- reactive({
    ggplot(Master_chart(),
           aes(x = Date_n,
               y = RUB_EUR_YOY)) +
      geom_col(fill = ifelse(Master_chart()$RUB_EUR_YOY >= 0, 
                             "MediumSeaGreen", 
                             "#ff5050")
      ) + 
      Theme_chart + 
      Theme_YOY() +
      scale_y_continuous(name = "%%",
                         breaks = seq(
                           round(min(Master_chart()$RUB_EUR_YOY), 0),
                           round(max(Master_chart()$RUB_EUR_YOY), 0),
                           (round(max(Master_chart()$RUB_EUR_YOY) - min(Master_chart()$RUB_EUR_YOY)))
                         )
      )
  })
  
  
  # Creating ranked numbers' and countries' list
  Top_q_country <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_q(),
        paste0(Top_q()$Sort, 
               ". ", 
               Top_q()$Country
               ),
        collapse = "<br/>"
      )
    )
  })
  
  Top_q_country_in <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_q(),
        paste0(Top_q_in()$Sort, 
               ". ", 
               Top_q_in()$Country
        ),
        collapse = "<br/>"
      )
    )
  })
  
  Top_q_country_out <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_q(),
        paste0(Top_q_out()$Sort, 
               ". ", 
               Top_q_out()$Country
        ),
        collapse = "<br/>"
      )
    )
  })
  
  # Creating ranked numbers' and countries' accumulated list
  Top_cum_country <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_cum(),
        paste0(
          # "<B>",
               Total_cum_pos()$Sort, 
               ". ", 
               Total_cum_pos()$Country
               # "</B>"
        ),
        collapse = "<br/>"
      )
    )
  })
  
  
  Top_cum_number <- reactive({
    if(input$dir == "Into Russia - by populaiton") {
      HTML(
        paste0(
          # Box_bkgr_color_cum(),
          paste0(round(Total_cum_pos()$Total, 2), " %"),
          collapse = "<br/>"
        )
      )
    } else {
      HTML(
        paste0(
          # Box_bkgr_color_cum(),
          List_value_cum(),
          collapse = "<br/>"
        )
      )
    }
  })
  
  Top_cum_country_in <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_cum(),
        paste0(
          # "<B>",
               Total_cum_pos()$Sort, 
               ". ", 
               Total_cum_pos()$Country
               # "</B>"
        ),
        collapse = "<br/>"
      )
    )
  })
  
  Top_cum_country_out <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_cum(),
        paste0(
          # "<B>",
               Total_cum_neg()$Sort, 
               ". ", 
               Total_cum_neg()$Country
               # "</B>"
        ),
        collapse = "<br/>"
      )
    )
  })
  
  Top_cum_number_in <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_cum(),
        List_value_cum_in(),
        collapse = "<br/>"
      )
    )
  })
  
  Top_cum_number_out <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_cum(),
        List_value_cum_out(),
        collapse = "<br/>"
      )
    )
  })
  
  List_value_q <- reactive ({
    paste0(
      # Box_bkgr_color_q(),
           format(Top_q()$Total, big.mark = ","))
  })
  
  List_value_q_in <- reactive ({
    paste0("<span style = color:#80c3ff>", format(Top_q_in()$Total, big.mark = ","), "</span>")
  })
  
  List_value_q_out <- reactive ({
    paste0("<span style = color:#ff8266>", format(-Top_q_out()$Total, big.mark = ","), "</span>")
  })
  
  List_value_cum <- reactive ({
    paste0(format(Total_cum_pos()$Total, big.mark = ","))
  })
  
  # Creating the top accumulated incoming: dividing by 2 to avoid doubling when summarizing totals (same country has 1 pos and 1 neg rank)
  List_value_cum_in <- reactive ({
    paste0("<span style = color:#80c3ff>", format(Total_cum_pos()$Total/2, big.mark = ","), "</span>")
  })
  
  List_value_cum_out <- reactive ({
    paste0("<span style = color:#ff8266>", format(-Total_cum_neg()$Total/2, big.mark = ","), "</span>")
  })
  
  # Creating values' list
  Top_q_number <- reactive({
    if(input$dir == "Into Russia - by populaiton") {
    HTML(
      paste0(
        # Box_bkgr_color_q(),
        paste0(round(Top_q()$Total, 2), " %"),
        collapse = "<br/>"
      )
    )
    } else {
      HTML(
        paste0(
          # Box_bkgr_color_q(),
          List_value_q(),
          collapse = "<br/>"
        )
      )
    }
  })
  
  Top_q_number_in <- reactive({
      HTML(
        paste0(
          # Box_bkgr_color_q(),
          List_value_q_in(),
          collapse = "<br/>")
      )
  })
  
  Top_q_number_out <- reactive({
    HTML(
      paste0(
        # Box_bkgr_color_q(),
        List_value_q_out(),
        collapse = "<br/>")
    )
  })

# Creating the top countries title
 Top_q_text <- reactive({
   HTML(paste0(
     h5(Text_box_countries(),
                  align = "center")
               )
        )
 })
 
 Top_q_text_in <- reactive({
   HTML(paste0(
     h5(Text_box_countries_in(),
                  align = "center")
   )
   )
 })
 
 Top_q_text_out <- reactive({
   HTML(paste0(
     h5(Text_box_countries_out(),
                  align = "center")
   )
   )
 })

 # Creating the all-time countries title
 Top_cum_text <- reactive({
   HTML(paste0(
     h5(Text_box_countries_cum(),
                  align = "center")
   )
   )
 })
 
 Top_cum_text_in <- reactive({
   HTML(paste0(
     h5(Text_box_countries_cum_in(),
                  align = "center")
   )
   )
 })
 
 Top_cum_text_out <- reactive({
   HTML(paste0(
     h5(Text_box_countries_cum_out(),
                  align = "center")
   )
   )
 })
 

 # Producing the outputs
 output$top_q_text <- renderUI(Top_q_text())
 output$top_q_text_in <- renderUI(Top_q_text_in())
 output$top_q_text_out <- renderUI(Top_q_text_out())
 output$top_q_cntr <- renderUI(Top_q_country())
 output$top_q_nmbr <- renderUI(Top_q_number())
 output$top_q_cntr_in <- renderUI(Top_q_country_in())
 output$top_q_nmbr_in <- renderUI(Top_q_number_in())
 output$top_q_cntr_out <- renderUI(Top_q_country_out())
 output$top_q_nmbr_out <- renderUI(Top_q_number_out())
 output$top_cum_text <- renderUI(Top_cum_text())
 output$top_cum_text_in <- renderUI(Top_cum_text_in())
 output$top_cum_text_out <- renderUI(Top_cum_text_out())
 output$top_cum_cntr <- renderUI(Top_cum_country())
 output$top_cum_nmbr <- renderUI(Top_cum_number())
 output$top_cum_cntr_in <- renderUI(Top_cum_country_in())
 output$top_cum_nmbr_in <- renderUI(Top_cum_number_in())
 output$top_cum_cntr_out <- renderUI(Top_cum_country_out())
 output$top_cum_nmbr_out <- renderUI(Top_cum_number_out())
 output$totals_txt_q <- renderUI(Ttl_text_q())
 output$totals_txt_cum <- renderUI(Ttl_text_cum())
 output$summ_aggr <- renderUI(Aggr_type())
 output$summ_dir <- renderUI(Dir_type())
 output$Mnth_Year <- renderUI(Mon_Yr())
 output$cntr_txt_q <- renderUI(Country_text_q())
 output$cntr_txt_cum <- renderUI(Country_text_cum())
 output$total_value_q <- renderValueBox({Box_q_total()})
 output$total_value_cum <- renderValueBox({Box_cum_total()})
 output$total_value_cum_air <- renderValueBox({Box_cum_air_total()})
 output$total_value_cum_car <- renderValueBox({Box_cum_car_total()})
 output$total_value_cum_rail <- renderValueBox({Box_cum_rail_total()})
 output$total_value_cum_ship <- renderValueBox({Box_cum_ship_total()})
 output$total_value_cum_walk <- renderValueBox({Box_cum_walk_total()})
 output$total_trend <- renderValueBox({Box_YOY()})
 output$ch_total <- renderPlot({Box_chart_total()})
 output$ch_by_trans <- renderPlot({Chart_trans()})
 output$ch_yoy <- renderPlot({Box_chart_YOY()})
 output$FX_value <- renderValueBox({Box_FX_total()})
 output$FX_YOY <- renderValueBox({Box_FX_YOY()})
 output$ch_fx <- renderPlot({Box_chart_FX()})
 output$ch_fx_yoy <- renderPlot({Box_chart_FX_YOY()})
 output$Source_out <- renderUI({tagList(Out_link)})
 output$Source_in <- renderUI({tagList(In_link)})
 
 output$picture <-renderText({c('<img src="',src,'">')})
  
  Balance_sign <- reactive ({
    if_else(Master_frame_geo()$Total > 0, 
            "incoming surplus",
            "outgoing surplus")
  })

  
  # Creating labels for countries' hover-ups
  Country_hover <- reactive({
    if (input$dir == "Crossing balance") {
      sprintf(
        "<strong> %s, </strong> %s: <br/> 
        %.1fK border crossings <br/>
        quarter ending: <br/>
        %s",
        Master_frame_geo()$Country,
        # Master_frame_geo()$In / 1000,
        # Master_frame_geo()$Country,
        # Master_frame_geo()$Out / 1000,
        Balance_sign(),
        abs(Master_frame_geo()$Total) / 1000,
        paste0(Master_frame_geo()$Month, "-", Master_frame_geo()$Year)
      ) %>%
        lapply(htmltools::HTML)
    } else {
      if (input$dir == "Into Russia - by populaiton") {
        sprintf(
        "<strong> %.2f </strong> percent of population of <br/>
        <strong> %s </strong> <br/>
        crossed a border with Russia <br/>
        at least once, quarter ending: <br/>
        %s",
          abs(Master_frame_geo()$Total),
        Master_frame_geo()$Country,
          paste0(Master_frame_geo()$Month,  "-", Master_frame_geo()$Year)
        ) %>%
          lapply(htmltools::HTML)
      } else {
        if (input$dir == "Into Russia") {
          sprintf(
            "Citizens of <strong>%s: </strong> <br/>
        %.1fK incoming border crossings <br/>
        quarter ending: <br/>
        %s",
            Master_frame_geo()$Country,
            abs(Master_frame_geo()$Total) / 1000,
            paste0(Master_frame_geo()$Month, "-", Master_frame_geo()$Year)
          ) %>%
            lapply(htmltools::HTML)
        } else {
          sprintf(
            "Russian citizens to <strong>%s: </strong> <br/>
        %.1fK outgoing border crossings <br/>
        quarter ending: <br/>
        %s",
            Master_frame_geo()$Country,
            abs(Master_frame_geo()$Total) / 1000,
            paste0(Master_frame_geo()$Month, "-", Master_frame_geo()$Year)
          ) %>%
            lapply(htmltools::HTML)
        }
      }
    }
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    basemap
  })
  
  # Creating a function for redrawing the maps
  Countries <- function(map) {
    map %>%
      addPolygons(
        data = Master_frame_geo(),
        group = "countries",
        smoothFactor = 0.1,
        fillColor = ~ Master_frame_geo()$Color,
        fillOpacity = 1,
        dashArray = "1",
        color = "#aaa",
        weight = 1,
        opacity = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = F
        ),
        label = Country_hover(),
        labelOptions = labelOptions(
          style = list(
            "border" = 0,
            "font-weight" = "normal",
            "color" = "#aaa",
            "background-color" = "#484d51",
            "line-height" = "14px",
            "padding" = "5px 5px",
            "border-radius" = 0
          ),
          textsize = "14px",
          direction = "auto"
        )
      )
  }
  
  # Observing the totals type
  observeEvent(input$aggr, {
    leafletProxy("map", session) %>%
      clearGroup(group = c("countries" 
                           # "lines"
      )) %>%
      clearMarkers() %>% 
      Countries 
    # %>% Lines
  })
  
  # Observing the changes of direction
  observeEvent(input$dir, {
    leafletProxy("map", session) %>%
      clearGroup(group = c("countries" 
                           # "lines"
                           )) %>%
      clearMarkers() %>% 
      Countries 
    # %>% Lines
  })
  
  # Observing the changes of dates
  observeEvent(input$date, {
    leafletProxy("map", session) %>%
      clearGroup(group = c("countries"
                           # "lines"
                           )) %>%
      clearMarkers() %>% 
      Countries 
    # %>% 
    #   Lines
  })
  
  # Observing the changes in No.of countries
  observeEvent(input$number, {
      leafletProxy("map", session) %>%
      clearGroup(group = c("countries"
                           # "lines"
                           )) %>%
      clearMarkers() %>% 
      Countries 
    # %>%
    #   Lines
  })
  
  # Observing the provider tiles selection
  observeEvent(input$prov, {
    leafletProxy("map", session) %>%
      clearGroup(group = c("countries"
                           # "lines"
                           )) %>%
      clearMarkers() %>% 
      addProviderTiles(input$prov) %>%
      Countries
  })
 
### Server part end ####   
  
}

shinyApp(ui, server)