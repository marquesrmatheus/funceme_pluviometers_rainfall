#' ---
#' title: calculate rainfall factor from Universal Soil Loss Equation (USLE) with pluviometer's FUNCEME.
#' author: José Matheus da Rocha Marques (@marquesrmatheus)
#' date: 2020-05-31
#' ---

rm(list=ls())

# preparate r -------------------------------------------------------------
# read packages
#install.packages("raster")
#install.packages("intensity.analysis")


library(data.table)
library(writexl)
library(magrittr)
library(tidyverse) # Para processamento e plotagem de dados
library(gstat) # Para interpolar os dados
library(geobr) # Para ler os limites espaciais
library(readxl)# Para ler os dados do excel
library(sf) # Para manipular dados espaciais vetoriais
library(ggspatial) # Fornece ferramentas para os mapas
library(gplots) # Para acessar paletas de cores
library(cowplot) # Fornece ferramentas para gr?ficos
library(tmap) #plot maps
library(sp) #Spatial point
library(rgdal) #Projections and coordinates systems
library(raster) #rasters
library(intensity.analysis)

setwd("C:/scriptsr/R_postos_pluviometricos_interp_FUNCEME/")

#'read file
#'Methodology: Were summarize all months over 30 years for each pluviometer;
#'Example: All january's month from pluviometer, all february's month from pluviometer, all march's month from pluviometer... divided by 30 (over 30 years);
#'all january/30, all february/30, all march/30... all december/30
#'With it, was calculated getting all averages over 30 years for each month by pluviometer;
#' The column named "TOTAL" is the sum of these averages by months for each pluviometer (jan+feb+marapr...) = "rowSums(pluviometros[1,7:18])"
#'To get the rainfall factor, applyed the defalut formula (Rfactor = 67.355*((pluviometros[1:nrow(pluviometros),7]^2)/pluviometros[1:nrow(pluviometros),19])^0.85)

pluviometros <- read.csv("postos_fatorR.csv",
                 header = T,
                 sep = ",",
                 stringsAsFactors = FALSE)

View(pluviometros)
#Method 1: create columns for rainfall months. Method 1: apply mutate for each column
pluviometros <- mutate(.data=pluviometros,R.JAN=67.355*((pluviometros[1:nrow(pluviometros),7]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.FEV=67.355*((pluviometros[1:nrow(pluviometros),8]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.MAR=67.355*((pluviometros[1:nrow(pluviometros),9]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.APR=67.355*((pluviometros[1:nrow(pluviometros),10]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.MAY=67.355*((pluviometros[1:nrow(pluviometros),11]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.JUN=67.355*((pluviometros[1:nrow(pluviometros),12]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.JUL=67.355*((pluviometros[1:nrow(pluviometros),13]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.AGO=67.355*((pluviometros[1:nrow(pluviometros),14]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.SEP=67.355*((pluviometros[1:nrow(pluviometros),15]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.OCT=67.355*((pluviometros[1:nrow(pluviometros),16]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.NOV=67.355*((pluviometros[1:nrow(pluviometros),17]^2)/pluviometros[1:nrow(pluviometros),19])^0.85,
                       R.DEC=67.355*((pluviometros[1:nrow(pluviometros),18]^2)/pluviometros[1:nrow(pluviometros),19])^0.85
                       )

View(pluviometros) 
glimpse(pluviometros)

#Method 2: using R base 
#result <- 67.355 *(pluviometros[,7:18]^2 / pluviometros[,"TOTAL_pa"])^0.85 
#names(result) <- paste("R", names(pluviometros)[7:18], sep=".")
#pluviometros <- cbind(pluviometros, result)

# Method 3: Fill columns using dplyr package
pluviometros <- pluviometros %>%
  mutate_at(vars(JAN:DEC),
            .funs = list(R = ~ 67.355 * ((.)^2 / TOTAL_pa)^0.85)) %>% 
  rename_at(vars(JAN_R:DEC_R),
            .funs = list(~ paste0("R.",str_remove(.,"_R"))))


#create column from sum rainfalls
pluviometros <- pluviometros %>%  mutate(R.sum=apply(pluviometros[1:nrow(pluviometros), 20:31], 1, sum))
View(pluviometros)

#Coordinates
coord.df <- SpatialPoints(cbind(pluviometros$LONGITUDE,pluviometros$LATITUDE), proj4string=CRS("+init=epsg:4674"))



#Procedimento do ArcGIS para adicionar xydata por planilha do excel
dados.sf <- st_as_sf(pluviometros,                            # Convetendo os dados para sf
                     coords=c('LATITUDE','LONGITUDE'),    # Colunas usadas como coordenadas
                     crs=4674)                          # Definindo sistema de refer?ncia

dados.sf <- st_set_crs(dados.sf, "+proj=utm +zone=24 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") #mudando o datum
View(dados.sf)
#pontos.pluvio <- st_cast(dados.sf, "POINT",warn = TRUE)  #Convertendo linhas do mapa para pontos         

##  O argumento cellsize = c(.1,.1) define a resolu??o da grade        ###
##                                                                     ###
## Valores elevados de cellsize resultam em um mapa com BAIXA RESOLU??O ##
## Valores Menores de  cellsize resultam em um mapa com ALTA RESOLU??O  ##
## CUIDADO, valores baixos de cellsize exige mais esfor?o computacional,##
## consequentemente demanda mais tempo de processamento                ###

grade.pluvio <- st_make_grid(dados.sf,cellsize = c(30,30)) %>%      #Criando uma grade
  st_as_sf() %>%                            #Convertendo para sf
  filter(st_contains(dados.sf,., sparse = FALSE)) #Eliminando os pontos que intersectam as linhas
plot(grade.pluvio)

interp_points_idw <- gstat(formula = temp ~ 1,  # F?rmula para o inverso da dist?ncia
                           data = as(dados.sf, "Spatial"),  # Converte dados para Spatial
                           set = list(idp = 2))             # Potência para o inverso da dist?ncia
plot(interp_points_idw) #Vai dar error, pois n?o tem uma grade de coordenadas ditas

inter.sf <- predict(interp_points_idw,as(grade.pluvio,"Spatial")) %>% # Calculando valores para as grades
  st_as_sf()                              # Convertendo para sf
plot(inter.sf)


#UNIVARIADA
qqnorm(pluviometros$TOTAL)
mean(pluviometros$TOTAL)
median(pluviometros$TOTAL)
summary(pluviometros$TOTAL)
percentis=seq(0.01,0.99,.01) #percentis
quantile(pluviometros$TOTAL,percentis) #percentis
amplitude <- max(pluviometros$TOTAL) - min(pluviometros$TOTAL)
amplitude/5 #class intervals
var(pluviometros$TOTAL)
desviopadrao <- sd(pluviometros$TOTAL) #standard deviation 
cv <- 100*desviopadrao/mean(pluviometros$TOTAL) #em %
cv

#BIVARIADA
cor.pearson <- cor(pluviometros$TOTAL,pluviometros$R) #correlação
ggplot(pluviometros,aes(TOTAL,R))+
  geom_point()
cov(pluviometros$TOTAL,pluviometros$R)

plot(pnorm,2,4)

