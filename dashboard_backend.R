library(shiny)
library(leaflet)
library(dplyr)
library(geosphere)
library(plyr)
library(ggplot2)
library(sp)
library(maps)

# Load data
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header=F, sep=",")
flights <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat", header=F, sep=",")
airlines <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat", header=F, sep=",") %>% dplyr::select(V2, V4)

# Define column names
colnames(flights) <- c("Airline_code", "c2", "origin", "c4", "destination", "c6", "c7", "c8","c9")
colnames(airports) <- c("Airport_ID", "Name", "City", "Country", "IATA", "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", "DST", "Tz", "Type", "Source")
colnames(airlines) <- c("Name", "Airline_code")

# Combine datasets
all_data0 <- flights %>%
  dplyr::select(Airline_code, origin, destination) %>%
  dplyr::left_join(airports[, c("City", "Country", "IATA", "Latitude", "Longitude")], by = c("origin" = "IATA")) %>%
  dplyr::left_join(airports[, c("City", "Country", "IATA", "Latitude", "Longitude")], by = c("destination" = "IATA")) %>%
  dplyr::left_join(airlines, by = "Airline_code") %>%
  mutate(city = paste(City.x, ", ", Country.x, sep = ""))

colnames(all_data0) <- c("Airline_code", "origin", "destination", "City_origin", "Country_origin", "Latitude_origin", "Longitude_origin", "City_dest", "Country_dest", "Latitude_dest", "Longitude_dest", "airline", "city")

all_data <- na.omit(all_data0)

# Adjust city names for encoding issues
all_data$city <- dplyr::recode(all_data$city,
                               "Ã-stersund, Sweden" = "Oestersund, Sweden",
                               "Ã"ngelholm, Sweden" = "Aengelholm, Sweden",
                               "Ã???orlu, Turkey" = "Corlu, Turkey")

# Shiny UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 60, right = 10,
                selectInput("origin", "Origin",
                            selected = "Brisbane, Australia",
                            choices = levels(factor(all_data$city))),
                style = "opacity: 0.65; z-index: 1000;")
)

# Shiny Server
server <- function(input, output, session) {
  Dataframe2 <- reactive({
    all_data %>% filter(city == input$origin)
  })
  
  output$map <- renderLeaflet({
    data <- Dataframe2()
    
    df2 <- gcIntermediate(as.matrix(data[,c("Longitude_origin", "Latitude_origin")]),
                          as.matrix(data[,c("Longitude_dest", "Latitude_dest")]),
                          n=100, addStartEnd=TRUE, sp=TRUE, breakAtDateLine=FALSE)
    df2 <- as(df2, "SpatialLinesDataFrame")
    df2.ff <- fortify(df2)
    
    data$id <- as.character(c(1:nrow(data))) 
    gcircles <- merge(df2.ff, data, all.x=T, by="id")
    
    center <- ifelse(data$Longitude_origin[1] > 0, data$Longitude_origin[1], data$Longitude_origin[1] + 360)
    
    gcircles$long.recenter <- ifelse(gcircles$long < center - 180, gcircles$long + 360, gcircles$long)
    data$long.ori.recenter <- ifelse(data$Longitude_origin < center - 180, data$Longitude_origin + 360, data$Longitude_origin)
    data$long.dest.recenter <- ifelse(data$Longitude_dest < center - 180, data$Longitude_dest + 360, data$Longitude_dest)
    
    test_line <- sf::st_as_sf(gcircles, coords = c("long.recenter", "lat")) %>%
      dplyr::group_by(id, piece) %>%
      dplyr::summarize(do_union=FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()
    
    test_line2 <- dplyr::left_join(test_line, data)
    
    labels <- sprintf("<strong>%s, </strong> %s", data$City_dest, data$Country_dest) %>% lapply(htmltools::HTML)
    
    leaflet(data = test_line2) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Carto DB Positron") %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Carto DB dark") %>%
      addPolylines(weight = 1, opacity = 0.5, color = "#820a0a", label = ~airline) %>%
      addCircleMarkers(data = data, lng = ~long.ori.recenter, lat = ~Latitude_origin, radius = 0.5, fillOpacity = 0.1, weight = 2, opacity = 0.1, color = "red") %>%
      addCircleMarkers(data = data, lng = ~long.dest.recenter, lat = ~Latitude_dest, radius = 1, label = labels) %>%
      addLayersControl(baseGroups = c("Carto DB Positron", "Carto DB dark"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

