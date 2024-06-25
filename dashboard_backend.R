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
  mutate(city = paste(City.x, ", ", Country.x, sep = "")) %>%
  distinct(origin, destination, .keep_all = TRUE)

colnames(all_data0) <- c("Airline_code", "origin", "destination", "City_origin", "Country_origin", "Latitude_origin", "Longitude_origin", "City_dest", "Country_dest", "Latitude_dest", "Longitude_dest", "airline", "city")

all_data_filtered <- all_data0 %>% filter(City_origin != "")

all_data <- na.omit(all_data_filtered)

# Adjust city names for encoding issues
all_data$city <- dplyr::recode(all_data$city,
                               "Ã-stersund, Sweden" = "Oestersund, Sweden",
                               "Ã"ngelholm, Sweden" = "Aengelholm, Sweden",
                               "Ã???orlu, Turkey" = "Corlu, Turkey")

to_radians <- function(degrees) {
  return(degrees * pi / 180)
}

haversine <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  lat1 <- to_radians(lat1)
  lon1 <- to_radians(lon1)
  lat2 <- to_radians(lat2)
  lon2 <- to_radians(lon2)
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  # Radius of the earth in kilometers is 6371
  distance <- 6371 * c
  
  return(distance)  # Distance in kilometers
}

# Shiny UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 60, right = 10,
                selectInput("country", "Country", 
                            choices = c("All countries", sort(unique(all_data$Country_origin))),
                            selected = "All countries"),
                selectizeInput("origin", "Origin",
                               selected = "Graz, Austria",
                               choices = sort(unique(all_data$city)),
                               options = list(maxOptions = 8000)),
                sliderInput("destinations", "Number of Destinations", min = 1, max = 100, value = 5),
                selectInput("destination", "Destination",
                            selected = NULL,
                            choices = NULL),
                radioButtons("flight_type", "Flight Type",
                             choices = list("National" = "national", "International" = "international", "Both" = "both"),
                             selected = "both"),
                textInput("distance_output", "Distance"),
                numericInput("max_distance", "Max Distance (km)", value = 20000, min = 0, step = 100),
                style = "opacity: 0.65; z-index: 1000;")
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive expression to filter airports based on selected country
  filtered_data <- reactive({
    if (input$country == "All countries") {
      all_data
    } else {
      all_data %>% filter(Country_origin == input$country)
    }
  })
  
  observeEvent(input$origin, {
    dest_choices <- all_data %>%
      filter(city == input$origin) %>%
      mutate(dest_city = paste(City_dest, ", ", Country_dest, sep = "")) %>%
      pull(dest_city)
    
    dest_choices <- c("All", dest_choices)
    updateSelectInput(session, "destination", choices = dest_choices, selected = dest_choices[1])
  })
  
  # observeEvent(input$destination, {
  #   if(input$destination != "All" && input$destination != ""){
  #     splitter1 <- strsplit(input$origin, ",")[[1]][1]
  #     splitter2 <- strsplit(input$destination, ",")[[1]][1]
  #     
  #     origin_coords <- all_data %>%
  #       filter(City_origin == input$origin) %>%
  #       select(Latitude_origin, Longitude_origin) %>%
  #       slice(1)  # Assuming there is only one origin match
  #     
  #     print(origin_coords)
  #     
  #     dest_coords <- all_data %>%
  #       filter(City_dest == input$destination) %>%
  #       select(Latitude_dest, Longitude_dest) %>%
  #       slice(1)  # Assuming there is only one destination match
  #     
  #     # print(dest_coords)
  #   
  #     if (nrow(origin_coords) > 0 & nrow(dest_coords) > 0) {
  #       # Calculate distance using haversine function
  #       distance <- haversine(origin_coords$Latitude_origin, 
  #                             origin_coords$Longitude_origin, 
  #                             dest_coords$Latitude_dest, 
  #                             dest_coords$Longitude_dest)
  #       
  #       print(paste("Distance from", splitter1, "to", splitter2, "is", distance, "km"))
  #     } else {
  #       print("Error: Could not find coordinates for origin or destination.")
  #     }
  #   }
  # })
  
  # Reactive expression to determine valid origin choices based on selected number of destinations
  valid_origins <- reactive({
    req(input$destinations)  # Ensure input$destinations is available
    
    filtered_data() %>%
      group_by(city) %>%
      filter(n_distinct(destination) >= input$destinations) %>%
      pull(city) %>%
      unique() %>%
      sort()
  })
  
  # Update origin choices based on valid_origins reactive expression
  observe({
    new_choices <- valid_origins()
    selected_origin <- input$origin
    
    if (!(selected_origin %in% new_choices)) {
      selected_origin <- new_choices[1]
    }
    
    isolate({
      updateSelectizeInput(session, "origin", 
                           choices = new_choices,
                           selected = selected_origin)
    })
  })
  
  # Reactive expression to filter data based on selected origin and slider value
  Dataframe2 <- reactive({
    req(input$origin)  # Ensure input$origin is available
    
    data <- filtered_data() %>% filter(city == input$origin)
    
    # Filter based on number of destinations
    if (!is.null(input$destinations)) {
      data <- data %>% group_by(city) %>% filter(n_distinct(destination) >= input$destinations)
    }
    
    if(input$destination != "All"){
      data <- data %>% filter(paste(City_dest, ", ", Country_dest, sep = "") == input$destination)
    }
    
    if (input$flight_type == "national") {
      data <- data %>% filter(Country_origin == Country_dest)
    } else if (input$flight_type == "international") {
      data <- data %>% filter(Country_origin != Country_dest)
    }
    
    return(data)
  })
  
  output$map <- renderLeaflet({
    data <- Dataframe2()
    
    if (nrow(data) == 0) {
      return(NULL)  # No data to plot, return NULL
    }
    
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
  
  # observeEvent(input$map_marker_click, {
  #   click <- input$map_marker_click
  #   print(click)
  #   if (!is.null(click)) {
  #     clicked_dest <- all_data %>%
  #       filter(Longitude_dest == click$lng, Latitude_dest == click$lat) %>%
  #       mutate(city = paste(City_dest, ", ", Country_dest, sep = "")) %>%
  #       pull(city) %>%
  #       unique()
  #     
  #     print(clicked_dest)
  #     if (length(clicked_dest) > 0) {
  #       updateSelectInput(session, "origin", selected = clicked_dest[1])
  #     }
  #   }
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
