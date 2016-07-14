
# -- ------------------------------------------------------------------------------ -- #
# -- Initial Developer: FranciscoME ----------------------------------------------- -- #
# -- GitHub Repossitory: http://bit.ly/GitHubRTradingPalAPI ----------------------- -- #
# -- License: GNU General Public License ------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

# -- Lista de funciones contenidas en este codigo para API Trading Pal ------------ -- #

# -- Obtener Token de validacion ------------------------------------------------ ---- #
# -- --------------------------------------------------- GET /token?token=[token] --1- #
# -- ---------------------------------------------------------------------------- ---- #

GetToken <- function(Email, Pass){

  http  <- "www.tradingpal.com/api/auth?email="
  http1 <- paste(http,Email,sep="")
  http2 <- paste("&password=",Pass,sep="")
  httpF <- paste(http1,http2,sep="")

  Query   <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem",package="RCurl"))
  RetJson <- fromJSON(Query, simplifyDataFrame = TRUE)

  DataM  <- data.frame(RetJson$uid,RetJson$token,
                        as.POSIXct(RetJson$auth$token$auth_time,origin="1970-01-01"),
                        as.POSIXct(RetJson$expires,origin="1970-01-01"))
  colnames(DataM) <- c("UserId","Token","AuthTime","AuthExp")

return(DataM) }

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

return(DataM)}

# -- Obtener Precios Historicos de Instrumento ---------------------------------- ---- #
# -- ---------------- GET /[symbol]/chart?period=[period]&from=[from]&till=[till] --3- #
# -- ---------------------------------------------------------------------------- ---- #

GetSymbolH  <- function(P0_Symbol,P1_Period,P2_From,P3_To) {
  
  http  <- "www.tradingpal.com/api/instruments/"
  http1 <- paste(http,P0_Symbol,sep="")
  http2 <- paste(http1,"chart?period=",sep="/" )
  http3 <- paste(http2,P1_Period,sep="")
  http4 <- paste(http3,"from=",sep="&")
  http5 <- paste(http4,P2_From,sep="")
  http6 <- paste(http5,"till=",sep="&")
  http7 <- paste(http6,P3_To,sep="")

  Query <- getURL(http7,cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson <- fromJSON(Query, simplifyDataFrame = TRUE)
  Precios <- data.frame(as.POSIXct((RJson[,1]/1000), origin ="1970-01-01"),RJson[,2])
  colnames(Precios) <- c("TimeStamp","Precio")
  
return(Query) }

# -- Obtener Operaciones Activas en cuenta -------------------------------------- ---- #
# -- ------------------------------- GET /active Description Return active trades --4- #
# -- ---------------------------------------------------------------------------- ---- #

GetTrades <- function(UserID){

  http1  <- paste("http://www.tradingpal.com/api/users/",UserID,sep="")
  httpF <- paste(http1,"/trades",sep="")
  Query <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson <- fromJSON(Query, simplifyDataFrame = TRUE)

return(RJson) }

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
                      RJson$user,
                      ifelse(is.null(RJson$from),0,RJson$from),
                      ifelse(is.null(RJson$copy_to),0,RJson$copy_to),
                      ifelse(is.null(RJson$joint),0,RJson$joint),
                      ifelse(is.null(RJson$joint_trade),0,RJson$joint_trade))
  colnames(DataM) <- c("OrderID","OrderType","Symbol",
                       "Open.TimeStamp", "Open.Price",
                       "Close.TimeStamp", "Close.Price",
                       "OrderDurationInSecs",
                       "PL.Currency", "PL.Pips",
                       "Lots", "OrderMargin", "AccountFreeMargin",
                       "StopLoss", "TakeProfit",
                       "RisPerPoint", "RiskInPips",
                       "UserID", "CopyFrom", "CopyTo", "Joint", "JointOrder")

return(DataM) }

# -- Obtener Informacion de una Operacion Particular ---------------------------- ---- #
# -- -- GET /[tradeID] || [ticket] Description Returns trade by tradeID or Ticket --6- #
# -- ---------------------------------------------------------------------------- ---- #

GetTradeInfo <- function(P0_Token,P1_tradeID,P2_userID){

  http  <- "www.tradingpal.com/api/trades/"
  http2 <- paste(http,TP_Trades$id,sep="")
  http3 <- paste(http2,"?token=",sep="")
  httpf <- paste(http3,TP_Tk$Token,sep="")
  Param <- c(id = P1_tradeID, user = P2_userID)
  PF <- httpGET(httpf, style="POST", .params=Param,.opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)

return(RetJson) }

# -- Modificar TakeProfit y StopLoss de una Operacion --------------------------- ---- #
# -- -------------------- PUT /[tradeID]?sl=[sl]&tp=[tp] Description Update trade --7- #
# -- ---------------------------------------------------------------------------- ---- #

ModifyTrade <- function(P0_Token,P1_tradeID,P2_SL,P3_TP){
  
  http  <- "www.tradingpal.com/api/trades/"
  http1 <- paste(http,P1_tradeID,sep="")
  http2 <- paste(http1,P2_SL,sep="?sl=")
  http3 <- paste(http2,P3_TP,sep="&tp=")
  
return(http3) }

# -- Abrir una Operacion -------------------------------------------------------- ---- #
# -- --------------------------- POST /?token=[token] Descriptions Open new trade --8- #
# -- ---------------------------------------------------------------------------- ---- #

OpenTrade <- function(P0_Token,P1_symbol, P2_sl, P3_tp, P4_lots, P5_op_type){
  
  http  <- "www.tradingpal.com/api/trades/?token="
  http2 <- paste(http,P0_Token,sep="")
  Param <- c(symbol=P1_symbol,sl=P2_sl,tp=P3_tp,lots=P4_lots,op_type=P5_op_type)
  PF <- postForm(http2, style="POST", .params=Param,
    .opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
return(RetJson) }

# -- Cerrar una Operacion ------------------------------------------------------- ---- #
# -- -------------------- DELETE /[tradeID]?token=[token] Description Close trade --9- #
# -- ---------------------------------------------------------------------------- ---- #

CloseTrade <- function(P0_Token,P1_tradeID,P2_userID){
  
  http  <- "www.tradingpal.com/api/trades/"
  http2 <- paste(http,P1_tradeID, sep = "")
  http3 <- paste(http2,"?token=", sep = "")
  httpf <- paste(http3,P0_Token,sep="")
  Param <- c(id = P1_tradeID, user = P2_userID)
  PF <- DELETE(url = httpf)
  
return(PF) }

# -- Obtener Muro-Feed de un instrumento ---------------------------------------- ---- #
# -- ------------------------------------------ GET /[symbol]/feed?token=[token] --10- #
# -- ---------------------------------------------------------------------------- ---- #

GetSymbolF <- function(){}
