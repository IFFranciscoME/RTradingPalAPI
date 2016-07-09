
# -- ------------------------------------------------------------------------------ -- #
# -- Initial Developer: FranciscoME ----------------------------------------------- -- #
# -- GitHub Repossitory: http://bit.ly/GitHubRTradingPalAPI ----------------------- -- #
# -- License: GNU General Public License ------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

# -- Lista de funciones contenidas en este codigo para API Trading Pal ------------ -- #

# -- Obtener Token de validacion ------------------------------------------------ ---- #
# -- --------------------------------------------------- GET /token?token=[token] --1- #
# -- ---------------------------------------------------------------------------- ---- #

GetToken <- function(){}

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

# -- ------------------------------------------------------------------------------ -- #
# -- ------------------------------------------------------------------- Function 1 -- #
# -- ------------------------------------------------------------------------------ -- #

GetHistPrices <- function(AccountType,Token,AccountID) {
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth       <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/instruments?accountId=",sep="")
  QueryInst  <- paste(Queryhttp,AccountID,sep="")
  QueryInst1 <- getURL(QueryInst,cainfo=system.file("CurlSSL","cacert.pem",
                package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  return(InstJson)
}