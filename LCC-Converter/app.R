
library(shiny)
library(sp)
library(rgdal)
library(leaflet)
library(rclipboard)

proj4_lcc <- CRS("+proj=lcc +ellps=plessis +lon_0=7.737229 +lat_0=49.5 +lat_1=47.7 +lat_2=52.3 +x_0=500000 + y_0=300000 +k_0=1")
# p4_fmt <- c("+proj=lcc +ellps=plessis +lon_0=%s +lat_0=%s +lat_1=%s +lat_2=%s +x_0=%s +y_0=%s +k_0=%s")          # for runtime proj4 argument entry
# proj4_lcc <- c("+proj=lcc + ellps=plessis +lon_0=%s")
proj4_ll <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
elim <- c(-100, 540)
nlim <- c(100, 540)

`%between%` <- function(x, range) x >= min(range) & x <= max(range)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    rclipboardSetup(), 
    
    tags$head(
        tags$style(HTML("#do{background-color:lightblue"))
    ), 

    # Application title
    titlePanel("AEF Coordinates Converter"), 

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( width=4, 
            div( class="row", 
                 div(tags$h4("Enter AEF coordinates (decimal km)."))
            ), 
            div( class="row", 
                 div(class="span2",style="display:inline-block",numericInput("E", "Easting:", 308.101, min=elim[1], max=elim[2], width="100px")), 
                 div(class="span2",style="display:inline-block",numericInput("N", "Northing:", 284.755, min=nlim[1], max=nlim[2], width="100px")), 
                 div(class="span2",style="display:inline-block",actionButton("do", "Map it", with="100px", icon=icon("map-marked-alt")) )
            ), 
            # div( class="row", div(tags$h4("Proj4 arguments:")) ),
            # div(class="row",
            #     div(class="span2", style="display:inline-block",numericInput("lon_0", "Central meridian:", 7.737229, min=-180, max=180)),
            #     div(class="span2", style="display:inline-block",numericInput("lat_0", "Latitude of origin:", 49.5, min=-90, max=90))
            # ),
            # div(class="row",
            #     div(class="span2", style="display:inline-block",numericInput("lat_1", "First standard parallel:", 47.7, min=-90, max=90)),
            #     div(class="span2", style="display:inline-block",numericInput("lat_2", "Second standard parallel:", 52.3, min=-90, max=90))
            # ),
            # div(class="row",
            #     div(class="span2", style="display:inline-block",numericInput("x_0", "False easting:", 500000, min=0)),
            #     div(class="span2", style="display:inline-block",numericInput("y_0", "False northing:", 300000, min=0))
            # ),
            # div(class="row", div(class="span2", style="display:inline-block",numericInput("k_0", "Scale factor:", 1))),
            div( class="row",
                 div(uiOutput("clip")),
                 div(htmlOutput("note"))
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map", width="600px", height="500px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    easting <- reactive({
        if(input$E < elim[1]){
            elim[1]
        } else if(input$E > elim[2]){
            elim[2]
        } else {
            input$E
        }
    })
    northing <- reactive({
        if(input$N < nlim[1]){
            nlim[1]
        } else if(input$N > nlim[2]){
            nlim[2]
        } else {
            input$N
        }
    })
    
    coords_out <- eventReactive( input$do, {
        # proj4_lcc <- isolate(CRS(sprintf(p4_fmt, input$lon_0, input$lat_0, input$lat_1, input$lat_2, input$x_0, input$y_0, input$k_0)))     # for runtime proj4 argument entry
        coord_in <- isolate(SpatialPoints( coords=matrix(c(E=easting()*1e3, N=northing()*1e3), 1, 2), proj4string=proj4_lcc ))
        spTransform(coord_in, proj4_ll)
    }, ignoreNULL=FALSE, ignoreInit=FALSE )
    
    coords_txt <- reactive({
        long <- coords_out()@coords[1]
        lat <- coords_out()@coords[2]
        sprintf("%f°, %f°", long, lat)
    })
    
    output$map <- renderLeaflet({
        leaflet(coords_out()) %>% 
            addProviderTiles(providers$GeoportailFrance.orthos) %>%
            addProviderTiles(providers$Stamen.TonerLines) %>% 
            addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, options=tileOptions(opacity=1)) %>% 
            setView( lng=coords_out()@coords[1], lat=coords_out()@coords[2], zoom=16 ) %>% 
            addPopups(popup=paste(coords_txt()))
    })
    
    output$clip <- renderUI({
        rclipButton("coord_copy", "Copy lat/long to clipboard", coords_txt(), icon=icon("copy"))
    })
    
    textnote <- reactive({
        if(!input$E %between% elim){
            sprintf("Easting must be between %s and %s.", elim[1], elim[2])
        } else if(!input$N %between% nlim){
            sprintf("Northing must be between %s and %s.", nlim[1], nlim[2])
        } else {
            NULL
        }
    })
    
    output$note <- renderText({
        HTML(textnote())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
