install.packages("shiny")
install.packages("leaflet")
install.packages("dplyr")
install.packages("geosphere")
install.packages("plyr")
install.packages("sf")

# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(geosphere)
library(plyr)
library(sf)
library(ggplot2)

# Load the datasets
airports <- read.csv("C:/Users/josef/Documents/projects/ShinyDashboard/FlightMap/data/airports.csv", header=FALSE, sep=",")
routes <- read.csv("C:/Users/josef/Documents/projects/ShinyDashboard/FlightMap/data/routes.csv", header=FALSE, sep=",")
airlines <- read.csv("C:/Users/josef/Documents/projects/ShinyDashboard/FlightMap/data/airlines.csv", header=FALSE, sep=",") %>% dplyr::select(V2, V4)

# Rename columns for clarity
colnames(routes) <- c("Airline_code", "c2", "origin", "c4", "destination", "c6", "c7", "c8", "c9")

# Merge datasets
all_data0 <- dplyr::select(routes, Airline_code, origin, destination) %>%
  dplyr::left_join(airports[, c("V3", "V4", "V5", "V7", "V8")], by = c("origin" = "V5")) %>%
  dplyr::left_join(airports[, c("V3", "V4", "V5", "V7", "V8")], by = c("destination" = "V5")) %>%
  dplyr::left_join(airlines, by = c("Airline_code" = "V4")) %>%
  mutate(city = paste(V3.x, ", ", V4.x, sep = ""))

# Rename columns
colnames(all_data0) <- c("Airline_code", "origin", "destination", "city1", "country", "lat_ori", "lng_ori", "city_dest", "country_dest", "lat_dest", "lng_dest", "airline", "city")

# Convert lat/lng columns to numeric
all_data0$lat_ori <- as.numeric(all_data0$lat_ori)
all_data0$lng_ori <- as.numeric(all_data0$lng_ori)
all_data0$lat_dest <- as.numeric(all_data0$lat_dest)
all_data0$lng_dest <- as.numeric(all_data0$lng_dest)

# Remove rows with missing values
all_data <- na.omit(all_data0)

# Correct city names with special characters
all_data$city <- dplyr::recode(all_data$city,
                               "Ã–stersund, Sweden" = "Oestersund, Sweden",
                               "Ã„ngelholm, Sweden" = "Aengelholm, Sweden",
                               "Ã‡orlu, Turkey" = "Corlu, Turkey")

# Define UI
ui <- fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 60, right = 10,
                selectInput("origin", "Origin",
                            selected = "Brisbane, Australia",
                            levels(factor(all_data$city))),
                style = "opacity: 0.65; z-index: 1000;")
)

# Define server logic
server <- function(input, output, session) {
  
  Dataframe2 <- reactive({
    data <- all_data[all_data$city %in% input$origin,]
    data
  })
  
  output$map <- renderLeaflet({
    data <- Dataframe2()
    
    df2 <- gcIntermediate(as.matrix(data[, c("lng_ori", "lat_ori")]),
                          as.matrix(data[, c("lng_dest", "lat_dest")]),
                          n = 100,
                          addStartEnd = TRUE,
                          sp = TRUE,
                          breakAtDateLine = FALSE)
    df2 <- as(df2, "SpatialLinesDataFrame")
    df2.ff <- fortify(df2)
    
    data$id <- as.character(c(1:nrow(data))) 
    gcircles <- merge(df2.ff, data, all.x = TRUE, by = "id")
    
    if (data$lng_ori[1] > 0) {
      center <- data$lng_ori[1]
    } else {
      center <- data$lng_ori[1] + 360
    }
    
    gcircles$long.recenter <- ifelse(gcircles$long < center - 180, gcircles$long + 360, gcircles$long)
    data$long.ori.recenter <- ifelse(data$lng_ori < center - 180, data$lng_ori + 360, data$lng_ori)
    data$long.dest.recenter <- ifelse(data$lng_dest < center - 180, data$lng_dest + 360, data$lng_dest)
    
    test_line <- sf::st_as_sf(gcircles, coords = c("long.recenter", "lat")) %>%
      dplyr::group_by(id, piece) %>%
      dplyr::summarize(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()
    
    test_line2 <- dplyr::left_join(test_line, data)
    
    labels <- sprintf(
      "<strong>%s, </strong> %s",
      data$city_dest, data$country_dest) %>% lapply(htmltools::HTML)
    
    m <- leaflet(data = test_line2) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Carto DB Positron") %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Carto DB dark") %>%
      addPolylines(weight = 1, opacity = 0.5, color = "#820a0a", label = ~airline) %>%
      addCircleMarkers(data = data, lng = ~long.ori.recenter, lat = ~lat_ori, radius = 0.5, fillOpacity = 0.1,
                       weight = 2, opacity = 0.1, color = "red") %>%
      addCircleMarkers(data = data, lng = ~long.dest.recenter, lat = ~lat_dest, radius = 1, label = labels) %>%
      addLayersControl(baseGroups = c("Carto DB Positron", "Carto DB dark"))
    m
  })
}

# Run the application 
shinyApp(ui = ui, server = server)








