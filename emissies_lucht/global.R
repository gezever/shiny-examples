library(dplyr)

alle_emissies <- readRDS("data/emissies.rds")
alle_emissies$latitude <- jitter(alle_emissies$latitude)
alle_emissies$longitude <- jitter(alle_emissies$longitude)

cleantable <- alle_emissies %>%
  select(
    
    Income = income,
    Lat = latitude,
    Long = longitude,
    Stof = stof,
    Emissiehoeveelheid = hoeveelheid,
    Jaar = jaar
  )
