
# -- ------------------------------------------------------------------------------- -- #
# -- Initial Developer: FranciscoME ------------------------------------------------ -- #
# -- GitHub Repossitory: http://bit.ly/GitHubRTradingPalAPI ------------------------ -- #
# -- License: GNU General Public License ------------------------------------------- -- #
# -- ------------------------------------------------------------------------------- -- #

# -- Lista de funciones contenidas en este codigo para API Trading Pal ------------- -- #

# -- Obtener Token de validacion ---------------------------------------------- ------- #
# -- ------------------------------------------------- GET /token?token=[token] -- 1 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_GetToken <- function(Email, Pass) {

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

# -- Obtener Informacion/Precio actual de Instrumento ------------------------- ------- #
# -- ------------------------------------------------------------ GET /[symbol] -- 2 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_GetSymbol <- function(Instrument) {

  Tiemp <- Sys.time()
  http  <- "www.tradingpal.com/api/instruments/"
  httpF <- paste(http,Instrument,sep="")
  Query <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson <- fromJSON(Query, simplifyDataFrame = TRUE)
  DataM <- data.frame(Instrument,Tiemp,RJson$data$bid,RJson$data$ask)
  colnames(DataM) <- c("Instrumento","TimeStamp","Bid","Ask")

return(DataM) }

# -- Obtener Precios Historicos de Instrumento -------------------------------- ------- #
# -- -------------- GET /[symbol]/chart?period=[period]&from=[from]&till=[till] -- 3 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_GetSymbolH  <- function(P0_Symbol,P1_Period,P2_From,P3_To) {
  
  http  <- "www.tradingpal.com/api/instruments/"
  http1 <- paste(http,P0_Symbol,sep="")
  http2 <- paste(http1,"chart?period=",sep="/" )
  http3 <- paste(http2,P1_Period,sep="")
  http4 <- paste(http3,"from=",sep="&")
  http5 <- paste(http4,P2_From,sep="")
  http6 <- paste(http5,"till=",sep="&")
  http7 <- paste(http6,P3_To,sep="")

  Query   <- getURL(http7,cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson   <- fromJSON(Query, simplifyDataFrame = TRUE)

  Precios <- data.frame(as.POSIXct((RJson$time), origin ="1970-01-01", tz = "UTC"),
                        RJson$open, RJson$high, RJson$low, RJson$close)
  colnames(Precios) <- c("TimeStamp","Open","High","Low","Close")

return(Precios) }

# -- Obtener Operaciones Activas en cuenta ------------------------------------ ------- #
# -- ----------------------------- GET /active Description Return active trades -- 4 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_GetTrades <- function(UserID) {
 
  http1  <- paste("http://www.tradingpal.com/api/users/",UserID,sep="")
  httpF  <- paste(http1,"/trades",sep="")
  Query  <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))

  if(nchar(as.character(Query)) == 0){
    RJson <- data.frame(matrix(nrow=1,ncol=12,data=0))
    colnames(RJson) <- c("free_margin","id","isSelf","lots","margin","op_type",
                         "open","sl","symbol","tp","user","joint")
    
    } else RJson <- fromJSON(Query, simplifyDataFrame = TRUE)

return(RJson) }

# -- Obtener Historico de Operaciones Por Trader ------------------------------ ------- #
# --  GET /users/[uID]/trades/closed Returns close trades by traderID --------- -- 5 -- #
# -- -------------------------------------------------------------------------- ------- #

  TP_GetTradersHist <- function(UserID) {

  http1  <- paste("http://www.tradingpal.com/api/users/",UserID,sep="")
  httpF  <- paste(http1,"/trades/closed",sep="")
  Query  <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson  <- fromJSON(Query, simplifyDataFrame = TRUE)
  
  if(class(RJson) == "data.frame") {
         
  Bandera <- as.numeric(length(RJson[1,]))
  
  if(Bandera == 14) { # si la cuenta fue DEMO
  
  DataM <- data.frame(RJson$id,RJson$op_type,RJson$symbol,
                      as.POSIXct(as.numeric(RJson$open$time)/1000, origin="1970-01-01"),
                      RJson$open$price,
                      as.POSIXct(as.numeric(RJson$close$time)/1000, origin="1970-01-01"),
                      RJson$close$price,
                      round(RJson$result$period/1000,1),
                      RJson$result$pnl_currency,RJson$result$pnl_pips,
                      RJson$lots,RJson$margin,RJson$free_margin,
                      RJson$sl,RJson$tp,
                      RJson$user,
                      ifelse(is.null(RJson$from),0,RJson$from),
                      ifelse(is.null(RJson$copy_to),0,RJson$copy_to),
                      ifelse(is.null(RJson$joint),0,RJson$joint),
                      ifelse(is.null(RJson$joint_trade),0,RJson$joint_trade))
  
  colnames(DataM) <- c("ID","Type","Symbol",
                       "Open.T", "Open.P",
                       "Close.T", "Close.P",
                       "Duration.S",
                       "PnL1", "PnL2",
                       "Lots", "Margin", "AFM",
                       "SL", "TP",
                       "UserID", "CopyFrom", "CopyTo", "Joint", "JointOrder")
  } else {

    if(Bandera == 11) {
       
           DataM <- data.frame(RJson$open$time, RJson$op_type, RJson$symbol, RJson$ticket,
                          RJson$sl, RJson$tp, RJson$close$time, RJson$close$price, 
                          RJson$commission, RJson$result$pnl_currency,
                          RJson$result$pnl_pips)
           colnames(DataM) <- c("Open.Time","Op.Type","Symbol","Ticket","Sl","Tp",
                                "Close.Time", "Close.Price","Commission","PnL.Currency",
                                "PnL.Pips")
           
    } else DataM <- data.frame(matrix(nrow=1,ncol=14,data=0)) # Si no hay historico
    }
  } else 
    
    DataM <- data.frame(matrix(nrow=1,ncol=14,data=0)) # Si no hay historico

return(DataM) }

# -- Obtener Historico de Operaciones Por Usuario ----------------------------- ------- #
# --  GET /users/[uID]/trades/closed Returns close trades by traderID --------- -- 6 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_GetUsersTradesHist <- function(UserID) {
  
  http1  <- paste("http://www.tradingpal.com/api/users/",UserID,sep="")
  httpF  <- paste(http1,"/trades/closed",sep="")
  Query  <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem", package="RCurl"))
  RJson  <- fromJSON(Query, simplifyDataFrame = TRUE)
  
  if(is.numeric(RJson$open$time[1])) {
  
     RJson$open$time <- as.POSIXct(RJson$open$time/1000,
                                   origin=as.POSIXct("1970-01-01", tz="America/Monterrey"),
                                   tz = "America/Monterrey")
     
     RJson$close$time <- as.POSIXct(RJson$close$time/1000,
                                    origin=as.POSIXct("1970-01-01", tz="America/Monterrey"),
                                    tz = "America/Monterrey")
  } else {
  
  RJson$open$time <- as.POSIXct(gsub("Z", "",gsub("T"," ",RJson$open$time)),
                                origin=as.POSIXct("1970-01-01", tz="America/Monterrey"),
                                tz = "America/Monterrey")
  
  RJson$close$time <- as.POSIXct(gsub("Z", "",gsub("T"," ",RJson$close$time)),
                                origin=as.POSIXct("1970-01-01", tz="America/Monterrey"),
                                tz = "America/Monterrey")
  }
  
return(RJson) }

# -- Obtener Informacion de una Operacion Particular -------------------------- ------- #
# -- -- GET /[tradeID] || [ticket] Returns trade by tradeID or Ticket --------- -- 7 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_GetTradeInfo <- function(P0_Token,P1_tradeID,P2_userID) {

  http  <- "www.tradingpal.com/api/trades/"
  http2 <- paste(http,P1_tradeID,sep="")
  http3 <- paste(http2,"?token=",sep="")
  httpf <- paste(http3,P0_Token,sep="")
  Param <- c(token=P0_Token, id = P1_tradeID, user = P2_userID)
  PF <- httpGET(httpf, style="POST", .params=Param,.opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)

return(RetJson) }

# -- Modificar TakeProfit y StopLoss de una Operacion ------------------------- ------- #
# -- ------------------ PUT /[tradeID]?sl=[sl]&tp=[tp] Description Update trade -- 8 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_ModifyTrade <- function(P0_Token,P1_tradeID,P2_SL,P3_TP) {
  
  http  <- "www.tradingpal.com/api/trades/"
  http1 <- paste(http,P1_tradeID,sep="")
  http2 <- paste(http1,P2_SL,sep="?sl=")
  http3 <- paste(http2,P3_TP,sep="&tp=")
  
return(http3) }

# -- Abrir una Operacion ------------------------------------------------------ ------- #
# -- ------------------------- POST /?token=[token] Descriptions Open new trade -- 9 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_OpenTrade <- function(P0_Token,P1_symbol, P2_sl, P3_tp, P4_lots, P5_op_type) {
  
  if(P2_sl == 0){
         Param <- c(symbol=P1_symbol,tp=P3_tp,lots=P4_lots,op_type=P5_op_type)
    } else Param <- c(symbol=P1_symbol,sl=P2_sl,tp=P3_tp,lots=P4_lots,op_type=P5_op_type)
  
  http  <- "www.tradingpal.com/api/trades/?token="
  http2 <- paste(http,P0_Token,sep="")
  PF <- postForm(http2, style="POST", .params=Param, .opts=list(ssl.verifypeer = TRUE))
  
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
return(RetJson) }

# -- Cerrar una Operacion ----------------------------------------------------- ------- #
# -- ----------------- DELETE /[tradeID]?token=[token] Description Close trade -- 10 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_CloseTrade <- function(P0_Token,P1_tradeID,P2_userID){

  http  <- "www.tradingpal.com/api/trades/"
  http2 <- paste(http,P1_tradeID, sep = "")
  http3 <- paste(http2,"?token=", sep = "")
  httpf <- paste(http3,P0_Token,sep="")
  Param <- c(id = P1_tradeID, user = P2_userID)
  PF <- DELETE(url = httpf)

return(PF) }

# -- Obtener Usuarios en TradingPal ------------------------------------------ -------- #
# -- --------------------------- GET /api/users/[user-id]/relations/jointed_by -- 11 -- #
# -- ------------------------------------------------------------------------- -------- #

TP_GetAutoCopyUsers <- function(P1_userID)  {

  http  <- "www.tradingpal.com/api/users/"
  http2 <- paste(http,P1_userID,sep="")
  httpf <- paste(http2,"/relations/jointed_by",sep="")
  PF <- httpGET(httpf, style="POST", .opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  DF <- data.frame(do.call(rbind.data.frame, RetJson))
  Final <- data.frame(row.names(DF),DF[1])
  row.names(Final) <- NULL
  colnames(Final) <- c("UID","Riesgo")

return(Final) }

# -- Obtener Info General de la Cuenta --------------------------------------- -------- #
# -- ---------------------------------------- GET /api/users/[user-id]/account -- 12 -- #
# -- ------------------------------------------------------------------------- -------- #

TP_GetAccountInfo <- function(P0_Token,P1_userID) {
  
  http  <- "www.tradingpal.com/api/users/"
  http2 <- paste(http,P1_userID,sep="")
  http3 <- paste(http2,"/account?token=",sep="")
  httpf <- paste(http3,P0_Token,sep="")
  PF <- httpGET(httpf, style="POST", .opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)

return(RetJson) }

# -- Obtener Balance de la Cuenta -------------------------------------------- -------- #
# -- ---------------------------------------- GET /api/users/[user-id]/account -- 13 -- #
# -- ------------------------------------------------------------------------- -------- #

TP_GetAccountBalance <- function(P0_Token,P1_userID) {

  http  <- "www.tradingpal.com/api/users/"
  http2 <- paste(http,P1_userID,sep="")
  http3 <- paste(http2,"/account/balance?token=",sep="")
  httpf <- paste(http3,P0_Token,sep="")
  PF <- httpGET(httpf, style="POST", .opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)

return(RetJson) }

# -- Publicar en Muro de Usuario --------------------------------------------- -------- #
# -- ----------------------------------------  -- 14 -- #
# -- ------------------------------------------------------------------------- -------- #

TP_PostUserWall <- function(P0_Token, P1_Texto, P2_HashTags, P3_PeopleTags){
  
  Rep1 <- length(P2_HashTags)
  
  for(i in 1:Rep1) P1_Texto <- paste(P1_Texto, paste("#",P2_HashTags[i], sep=""), sep=" ")
  
  http  <- "www.tradingpal.com/api/posts/?token="
  http2 <- paste(http, P0_Token, sep="")
  Param <- list(content = P1_Texto, tags = P2_HashTags)
  PF <- postForm(http2, style="POST", .params=Param, .opts=list(ssl.verifypeer = TRUE))

}


TP_PostUserWall(P0_Token = A01_PELHAM_BJ$Token$Token,
                P2_HashTags = c("AlgoTrading","Forex"),
                P1_Texto = "Texto de contenido a las 18.55")

# Pendiente

# -- Publicar en Muro de Instrumento ----------------------------------------- -------- #
# -- ----------------------------------------  -- 15 -- #
# -- ------------------------------------------------------------------------- -------- #

# Pendiente

# -- -------------------------------------------------------------------------- ------- #
# --  Funciones que utilizan MASTER TOKEN --------------------------------------------- #
# -- -------------------------------------------------------------------------- ------- #

# -- Tipo de Usuario. Live o Demo --------------------------------------------- ------- #
# -- -------------------------------------------------------------------------- -- 1 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_Master_GetUserType <- function(P0_MasterToken,P1_UserId) {
  
  http  <- "http://www.tradingpal.com/api/users/"
  http1 <- paste(http,P1_UserId,sep="")
  http2 <- paste(http1,"/thumbnail?token=",sep="")
  http3 <- paste(http2,P0_MasterToken,sep="")
  http4 <- paste(http3, "&user=",sep="")
  httpF <- paste(http4, P1_UserId, sep="")
  
  Query   <- getURL(httpF, cainfo = system.file("CurlSSL","cacert.pem",package="RCurl"))
  RetJson <- fromJSON(Query, simplifyDataFrame = TRUE) 
  
return(RetJson) }

# -- Informacion de Operacion ------------------------------------------------- ------- #
# -- -------------------------------------------------------------------------- -- 2 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_Master_GetTradeInfo <- function(P0_MasterToken, P1_tradeID, P2_userID){
  
  http  <- "www.tradingpal.com/api/trades/"
  http2 <- paste(http,P1_tradeID,sep="")
  http3 <- paste(http2,"?token=",sep="")
  http4 <- paste(http3,P0_MasterToken,sep="")
  http5 <- paste(http4,"&user=",sep="")
  httpf <- paste(http5,P2_userID,sep="")
  
  Param <- c(token=P0_MasterToken, id = P1_tradeID, user = P2_userID)
  PF <- httpGET(httpf, style="POST", .params=Param,.opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
return(RetJson) }

# -- Balance de Cuenta -------------------------------------------------------- ------- #
# -- -------------------------------------------------------------------------- -- 3 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_Master_GetAccountBalance <- function(P0_MasterToken, P1_UserId) {
  
  http  <- "www.tradingpal.com/api/users/"
  http2 <- paste(http,P1_UserId,sep="")
  http3 <- paste(http2,"/account/balance?token=",sep="")
  http4 <- paste(http3,P0_MasterToken,sep="")
  http5 <- paste(http4,"&user=",sep="")
  httpf <- paste(http5,P1_UserId,sep="")
  
  PF <- httpGET(httpf, style="POST", .opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
return(RetJson) }

# -- Información de Cuenta ---------------------------------------------------- ------- #
# -- -------------------------------------------------------------------------- -- 4 -- #
# -- -------------------------------------------------------------------------- ------- #

TP_Master_GetAccountInfo <- function(P0_MasterToken,P1_userID) {

  http  <- "www.tradingpal.com/api/users/"
  http2 <- paste(http,P1_userID,sep="")
  http3 <- paste(http2,"/account?token=",sep="")
  http4 <- paste(http3,P0_MasterToken,sep="")
  http5 <- paste(http4,"&user=",sep="")
  httpf <- paste(http5,P1_userID,sep="")
  
  PF <- httpGET(httpf, style="POST", .opts=list(ssl.verifypeer = TRUE))
  RetJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
return(RetJson) }

# -- Obtener Historial Conjunto de la Cuenta --------------------------------- -------- #
# -- ----------------------------------------- GET /api/users/[user-id]/account -- 5 -- #
# -- ------------------------------------------------------------------------- -------- #

TP_Master_GetJointHist <- function(P0_MasterToken, P1_TraderID1, P2_UserID2) {
  
  http1 <- "http://www.tradingpal.com/api/users/"
  http2 <- paste(http1,P2_UserID2,sep="")
  http3 <- paste(http2,"/relations/joint/",sep="")
  http4 <- paste(http3,P1_TraderID1,sep="")
  http5 <- paste(http4,"/history?",sep="")
  http6 <- paste(http5,"token=",sep="")
  http7 <- paste(http6,P0_MasterToken,sep="")
  http8 <- paste(http7,"&user=",sep="")
  httpF <- paste(http8,P1_TraderID1,sep="")
  Query <- httpGET(httpF, style="POST", .opts=list(ssl.verifypeer = TRUE))
  Datos <- fromJSON(Query, simplifyDataFrame = TRUE)
  
return(Datos) }
