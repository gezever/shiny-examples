library(shiny)
library(SPARQL)
library(BelgiumMaps.StatBel)
ui <- fluidPage( 
  sliderInput(inputId = "year",
              label = "jaar",
              value = 25, min = 2000, max = 2020),
  plotOutput("hist")
                  )
server <- function(input, output) {
  
  output$hist <- renderPlot({
    
    # Step 1 - Set up preliminaries and define query
    # Define the data.gov endpoint
    endpoint <- "https://id-ontwikkel.milieuinfo.be/imjv/sparql"
    
    # create query statement
    query <- "
PREFIX milieu: <http://id.milieuinfo.be/def#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX http: <http://www.w3.org/2011/http#>
    PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/attribute#>
    PREFIX locn: <http://www.w3.org/ns/locn#>
    PREFIX dbo: <http://dbpedia.org/ontology/>
    
    SELECT ?jaar ?stof ?hoeveelheid ?eenheid ?latitude ?longitude ?refgebied ?CD_MUNTY_REFNIS
    WHERE {
    ?subject <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/linked-data/cube#Observation> ;
    milieu:referentiegebied ?refgebied ;    
    milieu:hoeveelheid ?hoeveelheid ;
    sdmx:unitMeasure ?unit ;
    milieu:tijdsperiode ?jaar ;
    
    milieu:substantie ?substantie .
    FILTER ( ?hoeveelheid > 0 )
    ?substantie  skos:prefLabel ?stof.
    ?refgebied <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://id.milieuinfo.be/def#Emissiepunt> ;
    milieu:exploitatie ?xtie ;
    geo:lat ?latitude ;
    geo:long ?longitude .
    ?unit skos:altLabel ?eenheid .
    SERVICE <http://lodcbbomv-on-1.vm.cumuli.be:8080/lodomv/repositories/cbb> {
    ?xtie locn:address ?adres .
    ?adres dbo:nisCode ?nis .
    }
    ?nis a skos:Concept;
    skos:inScheme <http://id.fedstats.be/conceptscheme/nis#id>;
    skos:notation ?CD_MUNTY_REFNIS .
    }
"

    
    # Step 2 - Use SPARQL package to submit query and save results to a data frame
    qd <- SPARQL(endpoint,query)
    df <- qd$results
    #print(df, quote = TRUE, row.names = FALSE)
    full_table<-rbind(df)?
    write.csv(full_table, file = "full_table_3.csv")
  })
}
shinyApp(ui = ui, server = server)