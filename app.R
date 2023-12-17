#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet.minicharts)
library('manipulateWidget')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Philly MSA Residential and Work Area Characteristics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "var",
            "Select a Column:",
            choices = var_list,
            selected = var_list[1]
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          combineWidgetsOutput("plot4")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  
    binpal_wac <- reactive({
      colorQuantile("Reds", msa_wac_sf[[paste0(input$var,'_wac')]], 5,
                   na.color = "transparent")
    })
    
    binpal_rac <- reactive({
      colorQuantile("Oranges", msa_rac_sf[[paste0(input$var,'_rac')]], 5,
                    na.color = "transparent")
    })

    observeEvent(input$var, {
      output$plot4 <- renderLeaflet({
        wac_leaf<-leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -75.161802, lat = 39.957673, zoom = 11) %>%
          addPolygons(
            data = msa_wac_sf,
            fillColor =  ~binpal_wac(),
            color = "lightgrey",
            weight = 1,
            fillOpacity = 0.8,
            highlight = highlightOptions(
              weight = 2,
              color = "black",
              fillOpacity = 0.8
            )) %>% 
          syncWith("basicmaps")
        
        rac_leaf <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -75.161802, lat = 39.957673, zoom = 11) %>%
          addPolygons(
            data = msa_rac_sf,
            fillColor =  ~binpal_rac(),
            color = "lightgrey",
            weight = 1,
            fillOpacity = 0.8,
            highlight = highlightOptions(
              weight = 2,
              color = "black",
              fillOpacity = 0.8
            )) %>% syncWith("basicmaps")
        combineWidgets( wac_leaf,  rac_leaf, ncol = 2)
      })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
