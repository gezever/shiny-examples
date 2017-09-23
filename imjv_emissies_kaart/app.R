library(shiny)
library(SPARQL)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
emissiepunten <-
  read.csv("data/full_table.csv", header = TRUE, encoding = "UTF-8")
load("data/BE_ADMIN_MUNTY.RData")

ui <- fluidPage(
  navbarPage(
    "Puntemissies in het Vlaams Gewest",
    id = "nav",
    
    tabPanel(
      "Interactive map",
      div(
        class = "outer",
        
        tags$head(# Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")),
        
        leafletOutput("map", width = "100%", height =
                        "100%"),
        
        # Shiny versions prior to 0.11 should use class="modal" instead.
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 60, 
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 330,
          height = "auto",
          
          h2("Verken de Vervuiling"),
          
          selectInput(inputId = "year", label = "jaar", unique(unlist(emissiepunten$jaar))),
          selectInput(inputId = "substance", label = "stof", unique(unlist(emissiepunten$stof))),
          #selectInput("size", "Size", unique(unlist(emissiepunten$stof)), selected = "adultpop"),
          conditionalPanel(
            "input.color == 'superzip' || input.size == 'superzip'",
            # Only prompt for threshold when coloring or sizing by superzip
            numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
          ),
          
          plotOutput("test1", height = 200),
          plotOutput("test2", height = 250)
        ),
        
        tags$div(
          id = "cite",
          'Integraal Milieu Jaarverslag ',
          tags$em('2004-2017'),
          '  by ',tags$em('Geert Van Haute.')
        )
      )
    ),
    
    
    
    conditionalPanel("false", icon("crosshair"))
  )
)

server <- function(input, output, session) {
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$test1 <- renderPlot({
    
    
    hist(emissiepunten$jaar,
         col = '#00DD00',
         border = 'white')
  })
  
  output$map <- renderLeaflet({
    emissiepergemeente <-
      aggregate(
        subset(
          subset(emissiepunten, jaar == input$year),
          stof == input$substance
        )$hoeveelheid,
        by = list(
          CD_MUNTY_REFNIS = subset(
            subset(emissiepunten, jaar == input$year),
            stof == input$substance
          )$CD_MUNTY_REFNIS
        ),
        FUN = sum
      )
    
    data(BE_ADMIN_MUNTY)
   
    str(BE_ADMIN_MUNTY@data)
    map <-
      merge(
        BE_ADMIN_MUNTY,
        emissiepergemeente,
        by = "CD_MUNTY_REFNIS",
        all.x = TRUE,
        all.y = FALSE
      )
    map <- subset(map, TX_RGN_DESCR_NL %in% "Vlaams Gewest")
    
    ## Visualise the data
    pal <- colorBin(
      palette = rev(heat.colors(13)),
      domain = map$x,
      bins = unique(c(0, round(
        quantile(
          map$x,
          na.rm = TRUE,
          probs = seq(0.1, 0.9, by = 0.1)
        ), 0
      ),+Inf), na.color = "#cecece")
    )
    m <- leaflet(map) %>%
      addTiles() %>%
      addLegend(
        title = sprintf(
          "Emissie van:  %s<br/>voor het jaar: %s<br/> per gemeente in het Vlaams Gewest ",
          input$substance,
          input$year
        ),
        pal = pal,
        values = ~ x, 
        position = "topleft"
      ) %>%
      addPolygons(
        color = ~ pal(x),
        stroke = FALSE,
        smoothFactor = 0.2,
        fillOpacity = 0.55,
        popup = sprintf("%s %s",
                        map$x, 'ton/jaar')
      )
    
    m <-
      addPolylines(
        m,
        data = subset(BE_ADMIN_MUNTY, TX_RGN_DESCR_NL %in% "Vlaams Gewest"),
        weight = 1.5,
        color = "black"
      )
    
    m
    
    
  })
}
shinyApp(ui = ui, server = server)