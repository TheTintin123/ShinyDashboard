#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

starwars <- starwars

minimum_mass <- min(starwars$mass, na.rm = T)
maximum_mass <- max(starwars$mass, na.rm = T)
x_options = list("Height"='height', "Mass"='mass', "Birth Year" = 'birth_year')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("StarWars Character Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("max_mass",
                        "Choose Maximum Mass displayed",
                        min = minimum_mass,
                        max = maximum_mass,
                        value = maximum_mass),
            selectInput("x_axis", "Choose an X-Axis Variable",
                        choices = x_options)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        starwars %>%
          filter(mass <= input$max_mass) %>%
          ggplot(aes_string(x=input$x_axis,y='mass')) +
          geom_point()
        
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
         #    xlab = 'Waiting time to next eruption (in mins)',
          #   main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
