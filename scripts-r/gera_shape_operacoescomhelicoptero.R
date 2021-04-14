# ATENÇÃO: ESTE ARQUIVO FOI FEITO PARA ESTUDOS PRELIMINARES E PRECISA SER UTILIZADO

#install.packages("sf")
library(tidyverse)
library(lubridate)
library(sf)

# Importa dados e levantamento de disparos
shape = st_read("geodados/rio.geojson")

link = "DEPRECATED"
dados = read.csv(link)
dados$DATA <- as.POSIXct(dados$DATA, na.rm = TRUE, format = "%d/%m/%Y")

# Filtra anos de interesse
com_disparos = dados %>% filter(DATA > "2018-01-01") %>%  filter(DATA < "2020-01-01" & COD_BAIRRO > 0 & Cidade == 'Rio de Janeiro' & DISPARO == '1')
operacoes_gerais = dados %>% filter(DATA > "2018-01-01") %>%  filter(DATA < "2020-01-01" & COD_BAIRRO > 0 & Cidade == 'Rio de Janeiro')

#rio = dados %>% filter(DATA > "2018-01-01") %>%  filter(DATA < "2020-01-01") %>% filter(Cidade == 'Rio de Janeiro')
rank = com_disparos %>% group_by(BAIRRO,COD_BAIRRO) %>% dplyr::summarise(freq =n()) %>% arrange(-freq)
oprank = operacoes_gerais %>% group_by(BAIRRO,COD_BAIRRO) %>% dplyr::summarise(freq =n()) %>% arrange(-freq)

sum(rank$freq)

# Exporta
#rgdal::writeOGR(geodados, "dados/operacoes_com_disparo.geojson", layer="geodados", driver="GeoJSON")
st_write(merge(shape,rank),dsn="dados/op_helicopteros_disparos.geojson")
st_write(merge(shape,oprank),dsn="dados/op_helicopteros_gerais.geojson")

oprank
