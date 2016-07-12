
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
DataM  <- data.frame(RetJson$uid,RetJson$token,
                    RetJson$auth$token$auth_time,
                    RetJson$expires)
colnames(DataM) <- c("UserId","Token","AuthTime","AuthExp")
return(DataM)
}

# -- Obtener Informacion/Precio actual de Instrumento --------------------------- ---- #
# -- -------------------------------------------------------------- GET /[symbol] --2- #
# -- ---------------------------------------------------------------------------- ---- #

GetSymbol <- function(Instrument){
  Tiemp <- Sys.time()
  http  <- "www.tradingpal.com/api/instruments/"
  httpF <- paste(http,Instrument,sep="")
  Query <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson <- fromJSON(Query, simplifyDataFrame = TRUE)
  DataM <- data.frame(Instrument,Tiemp,RJson$data$bid,RJson$data$ask)
  colnames(DataM) <- c("Instrumento","TimeStamp","Bid","Ask")
  return(DataM)
}

# -- Obtener Precios Historicos de Instrumento ---------------------------------- ---- #
# -- ---------------- GET /[symbol]/chart?period=[period]&from=[from]&till=[till] --3- #
# -- ---------------------------------------------------------------------------- ---- #

# www.tradingpal.com/api/instruments/eurusd/chart?period=m15&from=2016-07-04&till=2016-07-06

GetSymbolH <- function(){}

# -- Obtener Operaciones Activas en cuenta -------------------------------------- ---- #
# -- ------------------------------- GET /active Description Return active trades --4- #
# -- ---------------------------------------------------------------------------- ---- #

GetTrades <- function(UserID){
  http1  <- paste("http://www.tradingpal.com/api/users/",UserID,sep="")
  httpF <- paste(http1,"/trades",sep="")
  Query <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson <- fromJSON(Query, simplifyDataFrame = TRUE)
  RJson
  return(RJson)
}

# -- Obtener Historico de Operaciones Por Trader -------------------------------- ---- #
# --  GET /users/[uID]/trades/closed Description Returns close trades by traderID --5- #
# -- ---------------------------------------------------------------------------- ---- #

GetTradersHist <- function(UserID){
  http1  <- paste("http://www.tradingpal.com/api/users/",UserID,sep="")
  httpF  <- paste(http1,"/trades/closed",sep="")
  Query  <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson  <- fromJSON(Query, simplifyDataFrame = TRUE)
  
  DataM <- data.frame(RJson$id,RJson$op_type,RJson$symbol,
                      as.POSIXct(as.numeric(RJson$open$time)/1000, origin = "1970-01-01"),
                      RJson$open$price,
                      as.POSIXct(as.numeric(RJson$close$time)/1000, origin = "1970-01-01"),
                      RJson$close$price,
                      round(RJson$result$period/1000,1),
                      RJson$result$pnl_currency,RJson$result$pnl_pips,
                      RJson$lots,RJson$margin,RJson$free_margin,
                      RJson$sl,RJson$tp,
                      RJson$open$amount_risk_per_point, RJson$open$risk_in_pips,
                      RJson$user, RJson$from, RJson$copy_to, RJson$joint, RJson$joint_trade)
  colnames(DataM) <- c("OrderID","OrderType","Symbol",
                       "Open.TimeStamp", "Open.Price",
                       "Close.TimeStamp", "Close.Price",
                       "OrderDurationInSecs",
                       "PL.Currency", "PL.Pips",
                       "Lots", "OrderMargin", "AccountFreeMargin",
                       "StopLoss", "TakeProfit",
                       "RisPerPoint", "RiskInPips",
                       "UserID", "CopyFrom", "CopyTo", "Joint", "JointOrder")
  return(DataM)
}

# -- Obtener Informacion de una Operacion Particular ---------------------------- ---- #
# -- -- GET /[tradeID] || [ticket] Description Returns trade by tradeID or Ticket --6- #
# -- ---------------------------------------------------------------------------- ---- #

GetTradeID <- function(){}

# -- Modificar TakeProfit y StopLoss de una Operacion --------------------------- ---- #
# -- -------------------- PUT /[tradeID]?sl=[sl]&tp=[tp] Description Update trade --7- #
# -- ---------------------------------------------------------------------------- ---- #

PutDesTrd1 <- function(){}

# -- Abrir una Operacion -------------------------------------------------------- ---- #
# -- --------------------------- POST /?token=[token] Descriptions Open new trade --8- #
# -- ---------------------------------------------------------------------------- ---- #

PutDesTrd2 <- function(){}

# -- Cerrar una Operacion ------------------------------------------------------- ---- #
# -- -------------------- DELETE /[tradeID]?token=[token] Description Close trade --9- #
# -- ---------------------------------------------------------------------------- ---- #

DelDesTrd1 <- function(){}

# -- Obtener Muro-Feed de un instrumento ---------------------------------------- ---- #
# -- ------------------------------------------ GET /[symbol]/feed?token=[token] --10- #
# -- ---------------------------------------------------------------------------- ---- #

GetSymbolF <- function(){}
