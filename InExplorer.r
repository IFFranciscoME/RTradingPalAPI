
# -- ------------------------------------------------------------------------------ -- #
# -- Initial Developer: FranciscoME ----------------------------------------------- -- #
# -- GitHub Repossitory: http://bit.ly/GitHubRTradingPalAPI ----------------------- -- #
# -- License: GNU General Public License ------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

# -- ------------------------------------------------------------------------------ -- #
# -- -------------------------------------------------- Funciones de Inicializacion -- #
# -- ------------------------------------------------------------------------------ -- #

rm(list=ls()) # Remover objetos del Environment
cat("\014")   # Limpiar consola

pkg <- c("base","ggplot2","zoo","RCurl","tm","ROAuth","RJSONIO") 

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

setwd("~/Documents/TradingPal/InExplorer")

# -- ------------------------------------------------------------------------------ -- #
# -- ---------------------------------------------------- Cargar Scripts Auxiliares -- #
# -- ------------------------------------------------------------------------------ -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"

RTradingPalAPI <- paste(RawGitHub,"RTradingPalAPI/master/RTradingPalAPI.R",sep = "")
downloader::source_url(RTradingPalAPI,prompt=FALSE,quiet=TRUE)

DV <- paste(RawGitHub,"DataVisualization/master/DataVisualization.R",sep = "")
downloader::source_url(DV,prompt=FALSE,quiet=TRUE)

# -- ------------------------------------------------------------------------------ -- #
# -- ----------------------------------- Peticion y Ajuste Inicial Datos de Entrada -- #
# -- ------------------------------------------------------------------------------ -- #

# -- ------------------------------------------------------------------------------ -- #
# -- -------------------------------------------------------- Funciones Principales -- #
# -- ------------------------------------------------------------------------------ -- #

# -- Datos Maestros

# Instrumento
# Periodo de Analisis
# Periodicidad del precio

# -- ------------------------------------------------ Buscador de Canales Laterales -- #

# -- Buscador No-Supervisado

# Criterios para "Definir" un canal
# -- Se respetan consecutivamente el mismo soporte y resistencia durante el canal
# -- La cantidad de pips en cada movimiento direccional es relativamente constante
# -- Hay horarios bursátiles que favorecen la generación de este fenómeno

# -- ------------------------------------------ Buscador de Periodos de Volatilidad -- #

# -- Periodos de mayor volatilidad
# -- Periodos de menor volatilidad

# -- ----------------------------- Buscador de Reacciones a Indiciadores Economicos -- #
