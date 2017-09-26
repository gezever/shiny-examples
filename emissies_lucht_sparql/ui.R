library(leaflet)

# Choices for drop-downs
vars <- c("Emissie hoeveelheid" = "hoeveelheid", "Jaarlijkse omzet van het bedrijf" = "income")
stof_jaar <- unique(alle_emissies[,2:3])
stoffen <- sort(unique(unlist(stof_jaar$stof)))
jaren <- sort(unique(unlist(stof_jaar$jaar)))

navbarPage("Emissie naar lucht", id="nav",

  tabPanel("Interactieve kaart",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Vervuilingsverkenner"),
        selectInput(inputId = "year", "Jaar", jaren),
        selectInput(inputId = "substance", "Stof", stoffen),
        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n jaar)", 5)
        ),

        plotOutput("histJaar", height = 200),
        plotOutput("scatterEmissiehoeveelheidIncome", height = 250)
      ),

      tags$div(id="cite",
        'Puntemissie naar lucht in het Vlaams Gewest ', tags$em('2012–2015'), ' Geert Van Haute'
      )
    )
  ),
# 
#   tabPanel("Data explorer",
#     fluidRow(
#       column(3,
#         selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
#       ),
#       column(3,
#         conditionalPanel("input.states",
#           selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
#         )
#       ),
#       column(3,
#         conditionalPanel("input.states",
#           selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
#         )
#       )
#     ),
#     fluidRow(
#       column(1,
#         numericInput("minScore", "Min score", min=0, max=100, value=0)
#       ),
#       column(1,
#         numericInput("maxScore", "Max score", min=0, max=100, value=100)
#       )
#     ),
#     hr(),
#     DT::dataTableOutput("ziptable")
#   ),

  conditionalPanel("false", icon("crosshair"))
)
