########################################################################################################################
#   natural streets explorer vienna                                                                                      #
#   2017 by Niels Flohr                                                                                            #
########################################################################################################################


## Install / Load Packages
#-----------------------------------------------------------------------------------------------------------------------
### ipak function: install and load multiple R packages.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#### Load/install Packages

## use ipak function to load packages
packages <- c("leaflet", "shiny", "rgdal", "sp", "geojsonio","RColorBrewer")
ipak(packages)
#-----------------------------------------------------------------------------------------------------------------------


# load data from a file
#-----------------------------------------------------------------------------------------------------------------------
#file <- file.path(getwd(),"vienna.geojson")
#if(!file.exists(file)){
#  quit(save="ask")
#}
#natural_roads <- geojson_read(as.location(file), what="sp")
#-----------------------------------------------------------------------------------------------------------------------


# load data from my github
#-----------------------------------------------------------------------------------------------------------------------
natural_roads_link <- "https://github.com/niels-flohr/shiny/natural_streets_vienna/raw/master/vienna.geojson"
natural_roads <- geojson_read(natural_roads_link, what = "sp")
#-----------------------------------------------------------------------------------------------------------------------

#initial category for dataset
category <- "Connect"

#-----------------------------------------------------------------------------------------------------------------------
#define labels (not used <-- needs too much memory)
#labels <- sprintf(
#  "<strong>%s</strong><br/> Connectivity: %g",
#  natural_roads$Id, natural_roads$Connect
#) %>% lapply(htmltools::HTML)
#-----------------------------------------------------------------------------------------------------------------------


######## Define UI for Webmap ######## ui.r
#-----------------------------------------------------------------------------------------------------------------------
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                h3("Natural Streets Vienna"),
                tags$br(),
                selectInput("cat", "Category",
                            colnames(natural_roads@data)[order(colnames(natural_roads@data))] # sort
                            ),
                #sliderInput("rng", "Values", min(natural_roads[[category]]), max(natural_roads[[category]]),
                #            value = range(natural_roads[[category]]), step = 1
                sliderInput("rng", "Values", 0, 383,
                            value = c(0,150), step = 1
                ),
                tags$div(style={"background:rgba(230, 230, 230,0.4);border-radius:10px; padding: 10px; max-width:300px"},
                            h4(tags$b("Information")),
                            p("This is a Shiny-Application using a dataset produced out of OSM-data with the Natural Streets Algorithm from B. Jiang via", tags$a(href="http://giscience.hig.se/binjiang/Axwoman.htm", "Axwoman"), "."),
                            p("More information on the parameters and methodology at: ", tags$a(href="http://giscience.hig.se/binjiang/JAG-final.pdf", "[1]"), ", ", tags$a(href="https://en.wikipedia.org/wiki/Space_syntax", "[2]")),
                            p("Choose Category and use Slider to visualize subset of chosen Category."),
                            p(tags$b("Categories:")),
                            tags$li("Connectivity (Connect)"),
                            tags$li("Control"),
                            tags$li("Mean Depth"),
                            tags$li("Global Integration (GInt)"),
                            tags$li("Local Integration (LInt)"),
                            tags$li("Total Depth"),
                            tags$li("Local Depth")
                )
  )
)

#-----------------------------------------------------------------------------------------------------------------------


######## Define Server for Webmap ######## server.r
#-----------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
   # filter data to what the user selected (reactive expression)
    filteredData <- reactive({
      natural_roads[natural_roads[[input$cat]] >= input$rng[1] & natural_roads[[input$cat]] <= input$rng[2],]
    })
  
    # reactive expression representing changing palette
    colorpal <- reactive({
      filtered <- filteredData()
      colorBin("YlOrRd", domain = natural_roads[[input$cat]], bins = 4)
    })
    
#    # notifications (only used for testing)
#    observeEvent(input$cat, {
#      val <- range(natural_roads[[input$cat]])
#      showNotification(toString(val))
#      pal <- colorpal()

#    })
    
    # update slider input according to new values from chosen category
    observe({
    val <- input$cat
    updateSliderInput(session, "rng", min = min(natural_roads[[val]]), max = max(natural_roads[[val]]),
    value = c(mean(natural_roads[[val]]),max(natural_roads[[val]])))
    })
    
    output$mymap <- renderLeaflet({
    # renderLeaflet() is used at server side to render the leaflet map
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
      # define the leaflet map object
      m <- leaflet(natural_roads) %>%
        setView(16.3, 48.2, 11) %>%
        addProviderTiles(providers$CartoDB.Positron)
        
      #m %>% addPolylines(color = ~pal(Connect), weight = 2, highlight = highlightOptions(
      #  weight = 5,
      #  color = "#666",
      #  dashArray = "",
      #  fillOpacity = 0.7,
      #  bringToFront = TRUE), label = labels,
      #  labelOptions = labelOptions(
      #   style = list("font-weight" = "normal", padding = "3px 8px"),
      #    textsize = "15px",
      #    direction = "auto"))
        
    })
  
    # Incremental changes to the map should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    
    # observer: clear and add polylines according to filtered data   
    observe({
      pal <- colorpal()
      filtered <- filteredData()
      leafletProxy("mymap", data = filteredData()) %>%
      clearShapes() %>%
      #addPolylines(color = "grey27", weight = 2) %>%
      addPolylines(color = ~pal(filtered[[input$cat]]), weight = 2)
    })
  
    # observer: draw new legend based on chosen category
    observe({
      pal <- colorpal()
    
      proxy <- leafletProxy("mymap", data = filteredData())
    
      proxy %>% clearControls()
      proxy %>% addLegend(pal = pal, values = natural_roads[[input$cat]], opacity = 0.7, title = NULL, position = "bottomright")
      
    })
}
#-----------------------------------------------------------------------------------------------------------------------


# Execute Shiny-Map
#-----------------------------------------------------------------------------------------------------------------------
shinyApp(ui, server)
#-----------------------------------------------------------------------------------------------------------------------
