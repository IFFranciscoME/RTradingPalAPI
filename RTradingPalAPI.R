
# -- ------------------------------------------------------------------------------ -- #
# -- Initial Developer: FranciscoME ----------------------------------------------- -- #
# -- GitHub Repossitory: http://bit.ly/GitHubRTradingPalAPI ----------------------- -- #
# -- License: GNU General Public License ------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

rm(list=ls()) # Remover objetos del Environment
cat("\014")   # Limpiar consola

pkg <- c("base","zoo","RCurl","jsonlite")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Lista de funciones contenidas en este codigo para API Trading Pal ------------ -- #

# -- Obtener Token de validacion ------------------------------------------------ ---- #
# -- --------------------------------------------------- GET /token?token=[token] --1- #
# -- ---------------------------------------------------------------------------- ---- #

GetToken <- function(Email, Pass){

http  <- "www.tradingpal.com/api/auth?email="
http1 <- paste(http,Email,sep="")
http2 <- paste("&password=",Pass,sep="")
httpF <- paste(http1,http2,sep="")

Query   <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
RetJson <- fromJSON(Query, simplifyDataFrame = TRUE)

UserId <- RetJson$uid
Token  <- RetJson$token
TokenE <- as.POSIXct(RetJson$auth$token$exp, origin ="1970-01-01")

DataM  <- data.frame(RetJson$uid,RetJson$token,
                    as.POSIXct(RetJson$auth$token$auth_time, origin = "1970-01-01"),
                    as.POSIXct(RetJson$expires, origin = "1970-01-01"))
colnames(DataM) <- c("UserId","Token","AuthTime","AuthExp")
return(DataM)
}

# -- Obtener Informacion/Precio actual de Instrumento --------------------------- ---- #
# -- -------------------------------------------------------------- GET /[symbol] --2- #
# -- ---------------------------------------------------------------------------- ---- #

GetSymbol <- function(){}

# -- Obtener Precios Historicos de Instrumento ---------------------------------- ---- #
# -- ---------------- GET /[symbol]/chart?period=[period]&from=[from]&till=[till] --3- #
# -- ---------------------------------------------------------------------------- ---- #

GetSymbolH <- function(){}

# -- Obtener Operaciones Activas en cuenta -------------------------------------- ---- #
# -- ------------------------------- GET /active Description Return active trades --4- #
# -- ---------------------------------------------------------------------------- ---- #

GetTrades <- function(){}

# -- Obtener Informacion de una Operacion Particular ---------------------------- ---- #
# -- -- GET /[tradeID] || [ticket] Description Returns trade by tradeID or Ticket --5- #
# -- ---------------------------------------------------------------------------- ---- #

GetTradeID <- function(){}

# -- Modificar TakeProfit y StopLoss de una Operacion --------------------------- ---- #
# -- -------------------- PUT /[tradeID]?sl=[sl]&tp=[tp] Description Update trade --6- #
# -- ---------------------------------------------------------------------------- ---- #

PutDesTrd1 <- function(){}

# -- Abrir una Operacion -------------------------------------------------------- ---- #
# -- --------------------------- POST /?token=[token] Descriptions Open new trade --7- #
# -- ---------------------------------------------------------------------------- ---- #

PutDesTrd2 <- function(){}

# -- Cerrar una Operacion ------------------------------------------------------- ---- #
# -- -------------------- DELETE /[tradeID]?token=[token] Description Close trade --8- #
# -- ---------------------------------------------------------------------------- ---- #

DelDesTrd1 <- function(){}

# -- Obtener Muro-Feed de un instrumento ---------------------------------------- ---- #
# -- ------------------------------------------- GET /[symbol]/feed?token=[token] --9- #
# -- ---------------------------------------------------------------------------- ---- #

GetSymbolF <- function(){}
