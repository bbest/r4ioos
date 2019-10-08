# load libraries
library(here)
library(readr)
library(dplyr)
library(leaflet)
library(rerddap)
library(glue)
library(purrr)
library(stringr)
library(tidyr)
library(xts)
library(dygraphs)
addLegend = leaflet::addLegend
here      = here::here

map_sst <- function(sites, date = "2019-08-16"){
  date <- as.Date(date)
  
  leaflet(
    sites,
    # setup for geographic coordinates (EPSG:4326) b/c ERDDAP
    options = leafletOptions(
      crs = leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
    # basemap from GBIF in geographic coordinates (EPSG:4326)
    addTiles("//tile.gbif.org/4326/omt/{z}/{x}/{y}@1x.png?style=gbif-geyser") %>%
    # sea-surface temperature from ERDDAP
    addWMSTiles(
      baseUrl = 'https://coastwatch.pfeg.noaa.gov/erddap/wms/jplMURSST41mday/request?',
      layers = "jplMURSST41mday:sst",
      options = WMSTileOptions(
        version = "1.3.0", format = "image/png", transparent = T, opacity = 0.7,
        time = format(date,"%Y-%m-%dT00:00:00Z")))  %>%
    # addPolygons() %>% 
    # addMarkers(lng = ~lon, lat = ~lat, label = ~id, data=site) %>%
    addMarkers(lng = ~lon, lat = ~lat, label = ~id) %>%
    #addMouseCoordinates() %>%
    addLegend(
      position="bottomright",
      title = paste0("SST (°C)<br>", format(date,"%Y-%m-%d")),
      colorNumeric("Spectral", c(0,32), reverse=T), seq(0,32))
}

get_dates <- function(info){
  info$alldata$time %>%
    filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT")
}

get_timeseries <- function(info, lon, lat, csv, field="sst"){
  
  dates  <- get_dates(info)
  
  if (file.exists(csv)){
    d_prev <- read_csv(csv) %>%
      arrange(date)
    start_date <- read_csv(csv) %>%
      tail(1) %>%
      pull(date) %>%
      as.POSIXct()
  } else {
    start_date <- dates[1]
  }
  
  v <- griddap(
    info,
    longitude = c(lon, lon), latitude = c(lat, lat),
    time = c(start_date, dates[2]), fields = field)
  
  d_now <- v$data %>%
    as_tibble() %>%
    mutate(
      date = lubridate::as_date(time, "%Y-%m-%dT00:00:00Z")) %>%
    select(date, field) %>%
    arrange(date)
  
  if (file.exists(csv)){
    d <- bind_rows(d_prev, d_now) %>%
      filter(!duplicated(date))
  } else {
    d <- d_now
  }
  
  d %>%
    write_csv(csv)
  d
}

get_sst <- function(sites, csv_pfx = "data/sst_"){
  sites %>%
    mutate(
      csv  = glue("{csv_pfx}{id}.csv"),
      data = pmap(
        list(lon, lat, csv), 
        function(lon, lat, csv){
          get_timeseries(sst, lon, lat, csv)})) %>% 
    unnest(data) %>% 
    select(date, id, sst) %>% 
    spread(id, sst)
}

plot_sst <- function(d, title="SST", ...){
  xts(select(d, -date), order.by=d$date) %>%
    dygraph(main=title, ...) %>%
    dyRangeSelector()
}
