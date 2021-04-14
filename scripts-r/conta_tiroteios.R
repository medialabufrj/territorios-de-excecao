library(sf)
# Este arquivo foi feito para estudos preliminares e precisa ser atualizado.

link = 'DEPRECATED'

df = read.csv(link)

fc = st_as_sf(df, coords = c("longitude_ocorrencia","latitude_ocorrencia"), crs=4326)

rio = geojsonsf::geojson_sf("rio.geojson")

rio$fc = lengths(st_intersects(rio, fc))

library(tidyverse)

rio %>% select(BAIRRO,fc) %>% arrange(-fc)

library(leaflet)

cores = colorBin(palette = "PuBu",domain = rio$fc,bins=3,na.color = 'gray')

plot(rio)

leaflet(data=rio) %>%  addPolygons()
