library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
#zipdata <- alle_emissies[sample.int(nrow(alle_emissies), 10000),]
zipdata <- alle_emissies#[sample.int(nrow(alle_emissies), 9000),]
# By ordering by hoeveelheid, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(-zipdata$hoeveelheid),]
#emissies <- read.csv(file = "data/filename.csv")
#saveRDS(emissies, file = "data/emissies.rds")
#write.csv(zipdata, file = "/tmp/emissies.csv")

# sparql endpoint
#endpoint <- "https://id-ontwikkel.milieuinfo.be/imjv/sparql"


function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 4.4, lat = 51, zoom = 8)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
      
  })

  # Precalculate the breaks we'll need for the two histograms
  jaarBreaks <- hist(plot = FALSE, alle_emissies$jaar, breaks = 20)$breaks

  output$histJaar <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$jaar,
      breaks = jaarBreaks,
      main = "Rapportering",
      xlab = "Jaar",
      xlim = range(alle_emissies$jaar),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterEmissiehoeveelheidIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(income ~ hoeveelheid, data = zipsInBounds(), xlim = range(alle_emissies$hoeveelheid), ylim = range(alle_emissies$income)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    stofBy <- input$substance
    zipdata <- subset(subset(zipdata, stof == input$substance), jaar == input$year)
    #if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
     # colorData <- ifelse(zipdata$jaar >= (100 - input$threshold), "yes", "no")
      #pal <- colorFactor("viridis", colorData)
    #} else {
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    #}

   # if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      #radius <- ifelse(zipdata$jaar >= (100 - input$threshold), 30000, 3000)
    #} else {
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    #}

    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, 
                 #radius=log(radius +1) * 1000, 
                 radius=radius,
                 layerId=~zipcode,
                stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    emissiepuntlabeltabel <- showEmissiepuntLabel(selectedZip$refgebied)
    selectedZip <- alle_emissies[alle_emissies$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Rapporteringsjaar:", as.integer(selectedZip$jaar)),
      tags$strong(HTML(sprintf("%s, %s %s",
        selectedZip$stof, selectedZip$hoeveelheid, selectedZip$eenheid
      ))), tags$br(),
      tags$a(selectedZip$refgebied),
      tags$h4("Naam:", emissiepuntlabeltabel$label)
      
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
 
  # get the label of  an emission point
  showEmissiepuntLabel <- function(gebied) {
    query <- "
      SELECT ?label
      WHERE {
        <http://id.milieuinfo.be/imjv/emissiepunt/2625#id> <http://www.w3.org/2000/01/rdf-schema#label> ?label .
      }
    "
    qd <- SPARQL(endpoint,query)
    df <- qd$results
    emissiepuntlabeltabel<-rbind(df)?
    write.csv(emissiepuntlabeltabel, file = "data/emissiepuntlabeltabel.csv")
    return(emissiepuntlabeltabel)
  }
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- alle_emissies[alle_emissies$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Rapporteringsjaar:", as.integer(selectedZip$jaar)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$stof, selectedZip$hoeveelheid, selectedZip$eenheid
      ))), tags$br(),
      tags$a(selectedZip$refgebied)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  

  # When map is clicked, show a popup with stof info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })



  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | Stof %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
