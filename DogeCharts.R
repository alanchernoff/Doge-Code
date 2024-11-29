library(dplyr)
library(TTR)
library(tidyverse)
library(matrixStats)
library(Cairo)
library(ggplot2)

#Data Parameters
exchange_list = c("Binance","Bitfinex","Bitstamp","Coinbase","Exmo","Gemini","HitBTC","Kraken","Okcoin") 
dates = paste0("dates", yr,".csv")
interval_list = c("1min","2.5min","5min")
datefile=read.csv(dates, header=FALSE, sep=",")
datefile = datefile[,1]

#Volatility Calculation Functions
rv_calc<- function (raw){
  mat=data.matrix(raw, rownames.force = NA)  
  mat=t(mat)
  #mat=log(mat)
  #calculate RV
  dif=diff(mat)
  RV=colSums((dif)^2)
  RV
}
ex_read <- function(exchange,interval){
  raw=read.csv(file=paste0("raw/",exchange,"_ETH_Minute.csv"),header=TRUE)
  raw <- raw[, -c(1)]
  df = raw[, seq(1, ncol(raw), interval)]
  df
}

#Choose Colors for Charts
c2 <- "#000000"
c1 <- "#5D8AA8" #69b3a2 #rgb(0.2, 0.6, 0.9, 1)
time1 <- 2
coeff_list = c(0.000006,0.000015,0.00005,0.00005,
               0.0000045,0.000015,0.00005,0.00003,
               0.000006,0.000020,0.000005,0.000011)

#Generate Charts
for(j in 1:length(interval_list)){
  for(i in 1:length(comp_list)){
    comp = comp_list[i]
    coeff = coeff_list[((j-1)*4+i)]
    raw1=read.csv(file=paste0(comp,interval_list[j],yr,"e.csv"), header=FALSE, sep=",")
    rv=rv_calc(log(raw1))
    meanvol=Vol_calc(log(raw1))
    close=read.csv(file=paste0(comp,"close",yr,"e.csv"),header=FALSE)
    close = close[,1]
    
    merged1 = data.frame(datefile,close,rv)
    merged1$dates = as.Date(merged1$datefile, "%m/%d/%Y")
    
    merged2 = data.frame(datefile,close,meanvol)
    merged2$dates = as.Date(merged2$datefile, "%m/%d/%Y")
    
    fig1 = ggplot(merged1, aes(x=dates)) +
      geom_bar( aes(y=rv/coeff), stat="identity", size=.1, fill=c1, color=c1, alpha=.1) + 
      geom_line( aes(y=close),size=.1, color=c2 ) +
      scale_y_continuous(
        # Features of the first axis
        name = "Closing Price",
        # Add a second axis and specify its features
        sec.axis = sec_axis( trans=~.*coeff, name="Volatility"),
        expand = c(0.005,0)
      ) + 
      scale_x_date(date_breaks="1 year",date_labels="%Y", expand = c(0.005,0)) +
      theme_classic()+
      theme(
        aspect.ratio = 1,
        axis.title.y = element_text(color = c2, size=13),
        axis.title.y.right = element_text(color = c1, size=13),
        axis.title.x=element_blank(),
        axis.line.y=element_blank(),
        axis.line.y.right=element_blank(),
        axis.line.x=element_blank(),
        panel.border=element_rect(color="black",fill=NA,size=0.4)
      )
    CairoPNG(file=paste0(comp,interval_list[j],"rv.png"),width = 3.5,height = 2.8,units="in",dpi=96) 
    print(fig1)
    Sys.sleep(time1)
    print(fig1)
    dev.off()
    
    fig2 = ggplot(merged2, aes(x=dates)) +
      geom_bar( aes(y=meanvol/coeff), stat="identity", size=.1, fill=c1, color=c1, alpha=.1) + 
      geom_line( aes(y=close),size=.1, color=c2 ) +
      scale_y_continuous(
        # Features of the first axis
        name = "Closing Price",
        # Add a second axis and specify its features
        sec.axis = sec_axis( trans=~.*coeff, name="Volatility"),
        expand = c(0.005,0)
      ) + 
      scale_x_date(date_breaks="1 year",date_labels="%Y", expand = c(0.005,0)) +
      theme_classic()+
      theme(
        aspect.ratio = 1,
        axis.title.y = element_text(color = c2, size=13),
        axis.title.y.right = element_text(color = c1, size=13),
        axis.title.x=element_blank(),
        axis.line.y=element_blank(),
        axis.line.y.right=element_blank(),
        axis.line.x=element_blank(),
        panel.border=element_rect(color="black",fill=NA,size=0.4)
      )
    CairoPNG(file=paste0(comp,interval_list[j],"meanvol.png"),width = 3.5,height = 2.8,units="in",dpi=96) 
    print(fig2)
    Sys.sleep(time1)
    print(fig2)
    dev.off()
    
  }
}

#Generating a single chart for testing purposes
j=3
i=1

comp = comp_list[i]
coeff = coeff_list[((j-1)*4+i)]
raw1=read.csv(file=paste0(comp,interval_list[j],yr,"e.csv"), header=FALSE, sep=",")
rv=rv_calc(log(raw1))
#meanvol=Vol_calc(log(raw1))
close=read.csv(file=paste0(comp,"close",yr,"e.csv"),header=FALSE)
close = close[,1]

merged1 = data.frame(datefile,close,rv)
merged1$dates = as.Date(merged1$datefile, "%m/%d/%Y")

#merged2 = data.frame(datefile,close,meanvol)
#merged2$dates = as.Date(merged2$datefile, "%m/%d/%Y")

fig1 = ggplot(merged1, aes(x=dates)) +
  geom_bar( aes(y=rv/coeff), stat="identity", size=.1, fill=c1, color=c1, alpha=.1) + 
  geom_line( aes(y=close),size=.1, color=c2 ) +
  scale_y_continuous(
    # Features of the first axis
    name = "Closing Price",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*coeff, name="Volatility"),
    expand = c(0.005,0)
  ) + 
  scale_x_date(date_breaks="1 year",date_labels="%Y", expand = c(0.005,0)) +
  theme_classic()+
  theme(
    title = element_blank(),
    aspect.ratio =1,
    axis.title.y = element_text(color = c2, size=13),
    axis.title.y.right = element_text(color = c1, size=13),
    axis.title.x=element_blank(),
    axis.line.y=element_blank(),
    axis.line.y.right=element_blank(),
    axis.line.x=element_blank(),
    panel.border=element_rect(color="black",fill=NA,size=0.4)
  )
CairoPNG(file=paste0(comp,interval_list[j],"rv.png"),width = 3.5,height = 2.8,units="in",dpi=96) 
print(fig1)
fig1
#Sys.sleep(time1)
#print(fig1)
dev.off()