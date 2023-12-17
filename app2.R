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
      leafletOutput("plot4"),
      plotOutput("hist1")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cc <- msa_wac_sf %>% filter(GEOID == 42101000402)
  
  var_wac <- reactive({
    paste0(input$var,'_wac')
  })
  
  
  
 
  
  cc_val <-   cc %>% pull(var_wac)
  
  var_wac_col <- msa_wac_sf[[var_wac]] 
  
  binpal_wac <- reactive({
    colorFactor("Reds", cut(msa_wac_sf[[var_wac]], breaks = color_breaks, labels = label_interval(color_breaks)))
  })
  

  observeEvent(input$var, {
    output$plot4 <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -75.161802, lat = 39.957673, zoom = 11) %>%
        addPolygons(
          data = msa_wac_sf,
          fillColor =  ~colorQuantile("Reds", msa_wac_sf$total_jobs_wac, 5,
                                          na.color = "transparent"),
          color = "lightgrey",
          weight = 1,
          fillOpacity = 0.8,
          highlight = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.8
          )) 

    })
  })
  
  output$hist1 <- ggplot(msa_wac_sf %>% filter(GEOID != 42101000402))+
    geom_histogram(aes_string(
      x = var_wac,
      color = cut(var_wac_col[var_wac_col != cc_val], breaks = color_breaks ),
    ))+
    geom_jitter(aes_string(x = var_wac, y = -100, color = cut(var_wac_col[var_wac_col != cc_val], breaks = color_breaks)), height = 100)+
    scale_color_brewer(palette = 'Reds', na.value = 'black')+
    labs(x = paste0(var,' Job Locations per m^2'), color = 'breaks',
         subtitle = paste0('Center City ', var, ' workers per m^2: ', cc_val%>% round()))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
