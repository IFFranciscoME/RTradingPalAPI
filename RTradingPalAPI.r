
# -- ------------------------------------------------------------------------------ -- #
# -- Initial Developer: FranciscoME ----------------------------------------------- -- #
# -- GitHub Repossitory: http://bit.ly/GitHubRTradingPalAPI ----------------------- -- #
# -- License: GNU General Public License ------------------------------------------ -- #
# -- ------------------------------------------------------------------------------ -- #

# -- ------------------------------------------------------------------------------ -- #

rm(list=ls()) # Remover objetos del Environment
cat("\014")   # Limpiar consola

pkg <- c("base","ggplot2","zoo","RCurl","tm","ROAuth","RJSONIO") 

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)


B00 <- "http://134.213.70.134:81/API/Accounts/GetChart?guid="
UID <- "d5ebbd1c-b36e-40b2-b562-48b96cebfed6"
SID <- 2
Ins <- "EURUSD"
Per <- 1
Fin <- 1445562000
Ffi <- 1445569200

# -- ------------------------------------------------------------------------------ -- #

TKGen <- "http://134.213.70.134:81/API/Accounts/Login?login=1000082333&password=tSDorsPVWoT"

WB1   <- "http://134.213.70.134:81/API/Accounts/GetChart?guid=d5ebbd1c-b36e-40b2-b562-48b96cebfed6&serverID=2&symbol=EURUSD&period=1&from=1445562000&till=1445569200"

QueryInst1 <- getURL(WB1,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))
InstJson   <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
InstJson

URL <- "http://134.213.70.134:81/API/Accounts/Login?login=1000082333&password=tSDorsPVWoT"

QueryInst1 <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))
InstJson   <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
InstJson

FxStreetW   <- "http://api.fxstreet.com/news/"
PublicKey   <- "16E6A3B4C7484DEEA210"
PrivateKey  <- "698E63074CB8406ABA4F"
SecurityKey <- "7E28621724BE4E6784F7"

