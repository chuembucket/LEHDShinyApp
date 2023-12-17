#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
      #plotOutput("plot4", height = 800),
      plotOutput("histo")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot4 <- renderPlot({
    var <- input$var
    
    # vector with min and man from of selected var from MSA and PHL
    rac_scalerange <- c(min(msa_rac_sf %>% pull(paste0(var,'_rac')), na.rm = T),
                        max(msa_rac_sf %>% pull(paste0(var,'_rac')), na.rm = T))
    
    wac_scalerange <- c(min(msa_wac_sf %>% pull(paste0(var,'_wac')), na.rm = T),
                        max(msa_wac_sf %>% pull(paste0(var,'_wac')), na.rm = T))
    
    
    msa_wac_gg <- ggplot(msa_wac_sf)+
      geom_sf(aes_string(fill = paste0(var,'_wac')), color = NA)+
      labs(fill = 'People per m^2')+
      scale_fill_viridis(limits = wac_scalerange)+
      mapTheme()
    
    
    msa_rac_gg <- ggplot(msa_rac_sf)+
      geom_sf(aes_string(fill = paste0(var,'_rac')), color = NA)+
      labs(fill = 'People per m^2')+
      scale_fill_viridis(limits = rac_scalerange)+
      mapTheme()
    
    

    ## patchwork it all together
    rac_top <- (msa_rac_gg) + 
                  plot_layout(guides = 'collect') +
                  plot_annotation(paste('Philly MSA',var,'Residential and Work Area Characteristics'),
                                  subtitle = "Where workers live") 
    
    
    wac_bot <- (msa_wac_gg) +
                  plot_layout(guides = 'collect') + plot_annotation(subtitle = 'Where workers work') 
    
    
    wrap_elements(rac_top) / wrap_elements(wac_bot) 
  },
  
  output$histo <- renderPlot({
    var <- input$var
    
    h1<-ggplot(msa_wac_sf)+
      geom_histogram(aes_string(x = paste0(var,'_wac'),
                                fill = ifelse(paste0(var,'_wac') == 0,'y','n')))+
      labs(x = paste0(var,' Job Locations per m^2'))
    
    h2<-ggplot(msa_rac_sf)+
      geom_histogram(aes_string(x = paste0(var,'_rac'), fill = paste0(var,'_rac')))+
      labs(x = paste0(var,' Home Locations per m^2'))

    (h1/h2) +
      plot_annotation(paste(var,'per m^2'),
                      subtitle = "count of census tracts") 
  }), width = 'auto', height = 'auto')
}

# Run the application 
shinyApp(ui = ui, server = server)