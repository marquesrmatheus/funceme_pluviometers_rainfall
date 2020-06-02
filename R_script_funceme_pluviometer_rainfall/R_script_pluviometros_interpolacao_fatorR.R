#' ---
#' title: calculate rainfall factor from Universal Soil Loss Equation (USLE) with pluviometer's FUNCEME.
#' author: José Matheus da Rocha Marques (@marquesrmatheus)
#' date: 2020-06-02
#' ---

rm(list=ls())

# preparate r -------------------------------------------------------------
# read packages
library(data.table)
library(writexl)
library(tidyverse) # Para processamento e plotagem de dados
library(gstat) # Para interpolar os dados
library(geobr) # Para ler os limites espaciais
library(sf) # Para manipular dados espaciais vetoriais
library(ggspatial) # Fornece ferramentas para os mapas
library(gplots) # Para acessar paletas de cores
library(cowplot) # Fornece ferramentas para gr?ficos
library(tmap) #plot maps
library(sp) #Spatial point
library(rgdal) #Projections and coordinates systems
library(raster) #rasters

setwd("C:/scriptsr/R_postos_pluviometricos_interp_FUNCEME/")

#'read file
#'Sheet methodology: Were summarize all months over 30 years for each pluviometer;
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
glimpse(pluviometros)

# Calculate rainfall factor and create columns using dplyr package
pluviometros <- pluviometros %>%
  mutate_at(vars(JAN:DEC),
            .funs = list(R = ~ 67.355 * ((.)^2 / TOTAL_pa)^0.85)) %>% 
  rename_at(vars(JAN_R:DEC_R),
            .funs = list(~ paste0("R.",str_remove(.,"_R"))))

#create column from sum rainfalls: Rainfall column
pluviometros <- pluviometros %>%  mutate(R.sum=apply(pluviometros[1:nrow(pluviometros), 20:31], 1, sum))
View(pluviometros)
------------------------------------------------------

#Spatial interpolation
