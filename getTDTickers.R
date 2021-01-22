require("jsonlite");require("data.table");require("pbapply")
api_key = "********************"
# A-Z letters
alpha = letters[1:26]
# ****************************************************
#         get Tickers Function
# ****************************************************
# function to get all tickers
getTickers = function(letter){
  # URL
  url =paste0("https://api.tdameritrade.com/v1/instruments?apikey=",
              api_key,"&symbol=",letter,".*&projection=symbol-regex")
  # read URL
  tmp = read_json(url, simplifyVector = TRUE)
  # rbind list
  tmp = rbindlist(tmp,use.names = TRUE,fill = TRUE)
  tmp 
}
# passing all letters to get all 
# Tickers that start with that letter
ALL = pblapply(as.list(alpha), getTickers)
# rbind list
ALL = rbindlist(ALL, use.names = TRUE)
# Save list of tickers
saveRDS(ALL,"tdTickers.rds")
#ALL <- readRDS("tdTickers.rds")
# ****************************************************
# unique asset types
unique(ALL$assetType)
# subsets all Equities
EQT = subset(ALL, ALL$assetType == "EQUITY")
# subsets all ETFs
ETF = subset(ALL, ALL$assetType == "ETF")
# ****************************************************
#         Fundamentals Function
# ****************************************************
# getting fundamental data
getFundamentals = function(ticker)
{
  # URL to get Fundamentals
  url =paste0("https://api.tdameritrade.com/v1/instruments?apikey=",
              api_key,"&symbol=",ticker,"&projection=fundamental")
  # read from URL
  tmp = read_json(url, simplifyVector = TRUE)
  # rbindlist
  tmp1 = rbindlist(lapply(tmp,"[[",1), 
                   fill = TRUE, use.names = TRUE)
  # adding additional fields
  info = as.data.frame(cbind(as.character(Sys.Date()),
                             tmp[[1]]$cusip,
                             tmp[[1]]$description,
                             tmp[[1]]$exchange,
                             tmp[[1]]$assetType))
  # format column names
  colnames(info) = c("getDate","cusip","description",
                     "exchange","assetType")
  # return DF
  cbind(tmp1,info)
}
# ****************************************************
#                 EQT Fundamentals
# ****************************************************
# get Fundamental Data for EQT
eqtDF = pblapply(as.list(1:nrow(EQT)), function(x){
  # sleep 1 minute : rate limit 120/minute
  if((x %% 120) == 0){Sys.sleep(60)} 
  # get the ticker
  ticker = EQT$symbol[x]
  # get Fundamental data
  tmp = try(getFundamentals(ticker),silent = TRUE)
  # will avoid breaking if error
  if(!inherits(tmp,'try-error')) tmp
})
# complete cases
eqtDF = eqtDF[lapply(eqtDF,length)>0] 
# rbind all
eqtDF = rbindlist(eqtDF,use.names = TRUE) 
# save DF
saveRDS(eqtDF,"eqFundamentalsTD.rds")    
#eqtDF = readRDS("eqFundamentalsTD.rds")
# ****************************************************
#                 ETF Fundamentals
# ****************************************************
# get Fundamental Data for EQT
etfDF = pblapply(as.list(1:nrow(ETF)), function(x){
  # sleep 1 minute : rate limit 120/minute
  if((x %% 120) == 0){Sys.sleep(60)}
  # get the ticker
  ticker = ETF$symbol[x]
  # get Fundamental data
  tmp = try(getFundamentals(ticker),silent=TRUE)
  # will avoid breaking if error
  if(!inherits(tmp,'try-error')) tmp
})
# complete cases
etfDF = etfDF[lapply(etfDF,length)>0]
# rbind all
etfDF = rbindlist(etfDF,use.names = TRUE)
# save DF
saveRDS(etfDF,"etfFundamentalsTD.rds")
#etfDF = readRDS("etfFundamentalsTD.rds")