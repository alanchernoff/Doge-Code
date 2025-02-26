setwd("C:/Users/acher/crypto/")
sd1="2022-01-01"
sd2="2023-12-31"

library(ggplot2)
library(dplyr)
library(stargazer)

coiner<-function(coin,start_date,stop_date){
  df=read.csv(paste(coin,'.csv',sep=''))
  names(df)[names(df) == "time"] <- "date"
  start=which(df$date==start_date)-1
  stop=which(df$date==stop_date)
  df=df[start:stop,]
  
  df$ActCurr = df$SplyAct1d/df$SplyCur
  df$SplyAct30dratio = df$SplyAct30/df$SplyCur
  df$SplyAdrBalUSDless10K = df$SplyAdrBalUSD10 - df$SplyAdrBalUSD10K
  
  df$Top1Mcnt <- df$AdrBalUSD1MCnt
  df$Top100Kcnt <- df$AdrBalUSD100KCnt - df$AdrBalUSD1MCnt
  df$Top10Kcnt <- df$SplyAdrBalUSD10K - df$AdrBalUSD100KCnt
  df$Less10Kcnt = df$AdrBalUSD10Cnt - df$AdrBalUSD10KCnt 
  
  df$Top1Mbal <- df$SplyAdrBalUSD1M
  df$Top100Kbal <- df$SplyAdrBalUSD100K - df$SplyAdrBalUSD1M
  df$Top10Kbal <- df$SplyAdrBalUSD10K - df$SplyAdrBalUSD100K
  df$Less10Kbal <- df$SplyAdrBalUSD10 - df$SplyAdrBalUSD10K 
  
  df$Ret <- c(NA, diff(log(df$PriceUSD)))
  df = df[-c(1),]
  df
}
jump_days <- function(dfraw){
  dfmat=data.matrix(dfraw, rownames.force = NA)
  dfmat=t(dfmat)
  dfmat=log(dfmat)
  dfdiff=diff(dfmat)
  delta=1/nrow(dfmat)
  alpha=matrix(0, (nrow(dfmat)-1), ncol(dfmat))
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){if (abs(dfdiff[i,j]) <= sqrt(delta)){alpha[i,j]=abs((dfdiff[i,j]))^2} else {alpha[i,j]=0}}} 
  alph_fin=5*sqrt(colSums(alpha)) 
  omega=0.47 
  BPD = colSums((abs(dfdiff))^4) 
  kfreq = (nrow(dfmat)+1)/2 
  data_10 = matrix(0, kfreq, ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:kfreq){data_10[i,j] = dfmat[(i-1)*2+1,j]}} 
  BPK = colSums((abs(diff(data_10)))^4) 
  SPK = BPK/BPD 
  trun_4 = matrix(0,nrow(dfmat)-1,ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:(nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_4[i,j] = abs((dfdiff[i,j]))^4}
    else {trun_4[i,j] = 0}}} 
  mp = pi^(-0.5)*4*gamma(5/2) 
  AP = (delta^(-1)/mp)*colSums(trun_4) 
  trun_8 = matrix(0, (nrow(dfmat)-1), ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_8[i,j] = abs((dfdiff[i,j]))^8} 
    else {trun_8[i,j] = 0}}} 
  mp_8 = pi^(-0.5)*16*gamma(9/2) 
  AP_8 = (delta^(-3)/mp_8)*colSums(trun_8) 
  Var = (delta* AP_8*160)/(3*AP^2) 
  ASJ = (2 - SPK)/sqrt(Var) 
  normASJ=pnorm(ASJ)
  jumpday = ifelse(normASJ > 0.95, 1, 0)
  jumpday[is.na(jumpday)] <- 0
  jday =data.frame(jumpday)
  jday
}
Vol_calc <- function (raw){
  mat=data.matrix(raw, rownames.force = NA) 
  mat=t(mat)
  #mat=log(mat)
  #calculate RV
  dif=diff(mat)
  RV=colSums((dif)^2)
  
  vol_df=data.frame(RV)
  vol_df
} 

dg = coiner('doge',sd1,sd2)
btc = coiner('btc',sd1,sd2)
eth = coiner('eth',sd1,sd2)

interval = 5

BTCraw=read.csv(file=paste0("raw/Coinbase_BTCfull_Minute.csv"),header=TRUE)
BTCraw <- BTCraw[, -c(1)]
BTCraw5 = BTCraw[, seq(1, ncol(BTCraw), interval)]

ETHraw=read.csv(file=paste0("raw/Coinbase_ETHfull_Minute.csv"),header=TRUE)
ETHraw <- ETHraw[, -c(1)]
ETHraw5 = ETHraw[, seq(1, ncol(ETHraw), interval)]

JBTC = jump_days(BTCraw5)
JETH = jump_days(ETHraw5)
VBTC= Vol_calc(BTCraw5)
VETH= Vol_calc(ETHraw5)

new_df <- data.frame(DgRet = dg$Ret, DgVol = dg$VtyDayRet30d, DgHash=dg$HashRate,
                     BTCRV = VBTC$RV, ETHRV = VETH$RV, 
                     BTCJD = JBTC$jumpday, ETHJD = JETH$jumpday, 
                     BTCret=btc$Ret, ETHret=eth$Ret,
                     BTCVol = btc$VtyDayRet30d, ETHVol = eth$VtyDayRet30d)

new_df2 <- data.frame(DG1m = dg$Top1Mcnt, DG100K = dg$Top100Kcnt, DG10K = dg$Top10Kcnt, DG10KL = dg$Less10Kcnt,
                      BTC1m = btc$Top1Mcnt, BTC100K = btc$Top100Kcnt, BTC10K = btc$Top10Kcnt, BTC10KL = btc$Less10Kcnt,
                      ETH1m = eth$Top1Mcnt, ETH100K = eth$Top100Kcnt, ETH10K = eth$Top10Kcnt, ETH10KL = eth$Less10Kcnt)



cor(new_df,use = "complete.obs")
cor(new_df2,use = "complete.obs")


modelbtc1 = lm(DgRet ~ BTCRV+BTCJD+DgHash+BTCret,new_df)
summary(modelbtc1)

modelbtc2 = lm(DgVol ~ BTCRV+BTCJD+BTCVol,new_df)
summary(modelbtc2)

modeleth1 = lm(DgRet ~ ETHRV+ETHJD+DgHash+ETHret,new_df)
summary(modeleth1)

modeleth2 = lm(DgVol ~ ETHRV+ETHJD+ETHVol,new_df)
summary(modeleth2)

model1m = lm(DG1m ~ BTC1m+ETH1m,new_df2)
summary(model)

# Fit models
modelbtc1 <- lm(DgRet ~ BTCRV + BTCJD + BTCret, new_df)
modeleth1 <- lm(DgRet ~ ETHRV + ETHJD + ETHret, new_df)
modelfull <- lm(DgRet ~ BTCRV + BTCJD + ETHRV + ETHJD + ETHret + BTCret, new_df)

modeleth2 <- lm(DgVol ~ ETHRV + ETHJD + ETHVol, new_df)
modelbtc2 <- lm(DgVol ~ BTCRV + BTCJD + BTCVol, new_df)
modelful2 <- lm(DgVol ~ BTCRV + BTCJD + BTCVol + ETHRV + ETHJD + ETHVol, new_df)

model1m <- lm(DG1m ~ BTC1m + ETH1m, new_df2)

summary(modelful2)



# Generate LaTeX table
stargazer(modelbtc1, modeleth1, modelfull,
          title = "Regression Results: Doge Returns", 
          label = "tab:regression_results1",
          dep.var.labels = c("Dogecoin Returns"),
          column.labels = c("BTC Model", "ETH Model", "Full Model"),
          covariate.labels = c("BTCRV", "BTCJD", "BTCret", 
                               "ETHRV", "ETHJD", "ETHret"),
          type = "latex", 
          digits = 3,
          scientific = TRUE)

stargazer(modelbtc2, modeleth2, modelful2,
          title = "Regression Results: Doge Volatility", 
          label = "tab:regression_results2",
          dep.var.labels = c("Dogecoin 30-day Return Volatility"),
          column.labels = c("BTC Model 2", "ETH Model 2"),
          covariate.labels = c("BTCRV", "BTCJD", "BTCVol", 
                               "ETHRV", "ETHJD", "ETHVol"),
          type = "latex",
          digits = 3,
          scientific = TRUE)

DogeHist=read.csv("dogehist.csv",header=TRUE)
# Function to convert Vol column to numeric
convert_volume <- function(vol) {
  vol <- gsub("B", "e9", vol) # Replace 'B' with 'e9'
  vol <- gsub("M", "e6", vol) # Replace 'M' with 'e6'
  as.numeric(vol) # Convert to numeric
}

# Apply the function to the Vol column
DogeHist <- DogeHist %>%
  mutate(Vol = convert_volume(Vol))

# Convert Date to proper Date format
DogeHist$Date <- as.Date(DogeHist$Date, format = "%m/%d/%Y")

# Create the plot
ggplot(DogeHist, aes(x = Date)) + 
  geom_line(aes(y = Price, color = "Price", group = 1), size = 1) +  # Add group = 1
  geom_bar(aes(y = Vol / max(Vol, na.rm = TRUE) * max(Price, na.rm = TRUE), fill = "Volume"), 
           stat = "identity", alpha = 0.4) +
  scale_y_continuous(
    name = "Price", 
    sec.axis = sec_axis(~ . * max(DogeHist$Vol, na.rm = TRUE) / max(DogeHist$Price, na.rm = TRUE), name = "Volume")
  ) +
  labs(title = "Dogecoin Price and Volume", x = NULL) +  # Remove x-axis label
  scale_color_manual(values = c("Price" = "blue")) +
  scale_fill_manual(values = c("Volume" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend

