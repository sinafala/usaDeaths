# Sam Clark
# 2021-01
# 2021-09

# library(readr)
library(sqldf)
library(lubridate)
library(grDevices)


## clear
rm(list=ls())

## Working directory
setwd("./cdc-cv19-data")


## Data: download from CDC

# import all deaths data from https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD - https://bit.ly/3bQCeD8
# see https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6 - https://bit.ly/3inT1P7
url <- "https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD"
saveFile <- "./data/deaths.csv"
download.file(url, saveFile)
deaths <- read.csv(saveFile)
# View(deaths)
str(deaths)

# import excess deaths data from https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target= - https://bit.ly/3nSKMM7
# see https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm (Excess deaths with and without COVID-19) - https://bit.ly/35TtQiC
url <- "https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target="
saveFile <- "./data/excessDeaths.csv"
download.file(url, saveFile)
excessDeaths <- read.csv(saveFile)
# View(excessDeaths)
str(excessDeaths)

# import cv19 deaths data from https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD&bom=true&format=true - https://bit.ly/2XOpJQh
# see https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab - https://bit.ly/3bWKIsi
url <- "https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
saveFile <- "./data/cv19Deaths.csv"
download.file(url, saveFile)
cv19Deaths <- read.csv(saveFile,stringsAsFactors=FALSE)
# View(cv19Deaths)
str(cv19Deaths)
# the dates is not in standard yyy-mm-dd format: convert
cv19Deaths$Start.week <- as.character(mdy(cv19Deaths$Start.Date))
cv19Deaths$End.Week <- as.character(mdy(cv19Deaths$End.Date))
# convert character-encoded number columns to numeric data type! *** amateurs created this file!
cols <- 10:16
cv19Deaths[,cols] <- apply(cv19Deaths[,cols],2,function(x){as.numeric(gsub(",", "",x))})
# View(cv19Deaths)
str(cv19Deaths)


## merge and select

# sql: join three datasets, restrict, and select columns
sql <- 'select 
          d.`Jurisdiction.of.Occurrence` as geography
          ,d.`Week.Ending.Date` as weekEnding
          ,e.`Average.Expected.Count` as expectedDeaths
          ,e.`Observed.Number` as allDeaths
          ,(e.`Observed.Number` - e.`Average.Expected.Count`) as excessDeaths
          ,e.`Excess.Lower.Estimate` as excessDeathsLo
          ,e.`Excess.Higher.Estimate` as excessDeathsHi
          ,c.`COVID.19.Deaths` as cv19Deaths
        from 
          (deaths d inner join excessDeaths e
            on d.`Week.Ending.Date` = e.`Week.Ending.Date`
              and d.`Jurisdiction.of.Occurrence`= e.`State`) 
          inner join cv19Deaths c
            on d.`Week.Ending.Date` = c.`End.Week`
              and d.`Jurisdiction.of.Occurrence`= c.`State`
        where
          substr(d.`Week.Ending.Date`,1,4) = "2020"
            and d.`Jurisdiction.of.Occurrence` = "United States"
            and trim(e.`Type`)  = "Predicted (weighted)"
            and trim(e.`Outcome`)  = "All causes"
            and trim(c.`Group`) = "By Week"
        order by
          d.`Week.Ending.Date`'
# sql      
deathsData2020 <- sqldf(sql)
# check that we have 52 weeks and 1 record for each week
s <- 'select weekEnding from deathsData2020 group by weekEnding'
sqldf(s)
# or, check that no weekEnding is repeated, e.g. that we have the weekly and not a mix of weekly and other counts
s <- 'select weekEnding, count(*) as count from deathsData2020 group by weekEnding having count>1'
sqldf(s)
# check the final data file
View(deathsData2020)
str(deathsData2020)
write.csv(file="./data/deathsData2020.csv",deathsData2020)

# convert the week ending date to a date format
deathsData2020$weekEnding <- ymd(deathsData2020$weekEnding)
str(deathsData2020)


## plot

# excess deaths
expectedPlusExcess <- deathsData2020$expectedDeaths + deathsData2020$excessDeaths
expectedPlusExcess
expectedPlusExcessLo <- deathsData2020$expectedDeaths + deathsData2020$excessDeathsLo
expectedPlusExcessLo
expectedPlusExcessHi <- deathsData2020$expectedDeaths + deathsData2020$excessDeathsHi
expectedPlusExcessHi
# CDC uses the 'Hi' values - that is observed deaths - average number of predicted (expected) deaths
#   NOT observed deaths - 95% quantile of predicted deaths

# observed deaths
observed <- deathsData2020$allDeaths

# expected deaths
expected <- deathsData2020$expectedDeaths

# cv19
str(deathsData2020)
expectedPlusCV19 <- deathsData2020$expectedDeaths + deathsData2020$cv19Deaths

# total excess
excessTotal <- round(sum(deathsData2020$excessDeaths),-3)
excessTotal
# excessTotalText <- as.character(format(excessTotal,big.mark=",",scientific=FALSE)) # format with comma group separator
excessTotalText <- as.character(format(excessTotal,scientific=FALSE))
excessTotalText
writeLines(excessTotalText,"../latex/excessDeaths.txt")

# total CV19
CV19Total <- round(sum(deathsData2020$cv19Deaths),-3)
CV19Total
# cv19DeathsText <- as.character(format(CV19Total,big.mark=",",scientific=FALSE)) # format with comma group separator
cv19DeathsText <- as.character(format(CV19Total,scientific=FALSE))
cv19DeathsText
writeLines(cv19DeathsText,"../latex/cv19Deaths.txt")

# finally the plot
plotDeaths <- function(toPDF=FALSE,cx=0.7,transparent=0.8,cexLeg=0.75,lw=2) {
  
  if (toPDF) {
    pdf(file="./pdf/USA-2020-Deaths.pdf",h=4)
  }
  
  par.default <- par()
  par(mgp=c(3.5,0.75,0),oma=c(0,0,0,0),mar=c(4,3,3,0.25))
  
  y.max <- max(observed,expected,expectedPlusCV19,na.rm=TRUE)
  y.min <- min(observed,expected,expectedPlusCV19,na.rm=TRUE)
  scale <- 1000
  y.max <- ceiling(y.max/scale/5)*5
  y.min <- trunc(y.min/scale/5)*5
  plot(
    expected/scale
    ,type="l"
    ,col="blue"
    ,lwd=lw
    ,ylim=c(y.min,y.max)
    ,xaxt="n"
    ,yaxt="n"
    ,xlab=""
    ,ylab=""
    ,main="Deaths during 2020 in the United States of America"
  )
  ticks.x <- 1:52
  axis(
    1
    ,at=ticks.x
    ,labels=substr(deathsData2020$weekEnding,6,10)
    ,las=2
    ,cex.axis=cx
  )
  title(xlab="Week Ending (mm-dd)",line=3)
  for (j in c(1,ticks.x)) {
    abline(
      v=j
      ,lwd=.1
    )
  }
  
  ticks.y <- seq(y.min,y.max,5)
  axis(
    2
    ,at=ticks.y
    ,cex.axis=cx
    ,las=2
  )
  title(ylab="Deaths (1000s)",line=2)
  for (j in ticks.y) {
    abline(
      h=j
      ,lwd=.1
    )
  }
  
  points(
    c(rep(NA,10),expectedPlusCV19[11:52])/scale
    ,type="l"
    ,col="red"
    ,lwd=lw
  )

    points(
    observed/scale
    ,type="l"
    ,col="black"
    ,lw=lw
  )
    
  points(
    expected/scale
    ,type="l"
    ,col="blue"
    ,lw=lw
  )
  
  points(
    c(expectedPlusCV19[1:11],rep(NA,42))/scale
    ,type="l"
    ,col="red"
    ,lwd=lw*0.5
  )
  
  legend(
    x=22
    ,y=y.max
    # "bottomleft"
    ,legend=c(
      "Expected"
      ,"Expected + CV19"
      ,"Observed"    
      ,paste("Total excess >",format(excessTotal,big.mark=",",digits=1,scientific=FALSE))
      ,paste("Total CV19 >",format(CV19Total,big.mark=",",digits=1,scientific=FALSE))
    )
    ,col=c("blue","red","black","white","white","white")
    ,lty=c(1,1,1,1,1,1)
    ,lwd=c(lw,lw,lw,0,0,0)
    ,bg="white"
    ,cex=cexLeg
  )
  
  suppressWarnings(par(par.default))
  
  if (toPDF) {
    dev.off()
  }
  
}

plotDeaths(toPDF=FALSE,cx=0.75,transparent=0.9,lw=3)  
plotDeaths(toPDF=TRUE,cx=0.75,transparent=0.9,lw=3)  
  
  
  
# # finally the plot
# plotDeaths <- function(toPDF=FALSE,cx=0.7,transparent=0.8,cexLeg=0.75,lw=2) {
#   
#   if (toPDF) {
#     pdf(file="./pdf/USA-2020-Deaths.pdf",h=4)
#   }
#   
#   par.default <- par()
#   par(mgp=c(3.5,0.75,0),oma=c(0,0,0,0),mar=c(4,3,3,0.25))
#   
#   y.max <- max(observed,expected,expectedPlusCV19,na.rm=TRUE)
#   y.min <- min(observed,expected,expectedPlusCV19,na.rm=TRUE)
#   scale <- 1000
#   y.max <- ceiling(y.max/scale/5)*5
#   y.min <- trunc(y.min/scale/5)*5
#   plot(
#     expected/scale
#     ,type="l"
#     ,col="blue"
#     ,lwd=lw
#     ,ylim=c(y.min,y.max)
#     ,xaxt="n"
#     ,yaxt="n"
#     ,xlab=""
#     ,ylab=""
#     ,main="Deaths during 2020 in the United States of America"
#   )
#   ticks.x <- 1:52
#   axis(
#     1
#     ,at=ticks.x
#     ,labels=substr(deathsData2020$weekEnding,6,10)
#     ,las=2
#     ,cex.axis=cx
#   )
#   title(xlab="Week Ending (mm-dd)",line=3)
#   for (j in c(1,ticks.x)) {
#     abline(
#       v=j
#       ,lwd=.1
#     )
#   }
#   
#   ticks.y <- seq(y.min,y.max,5)
#   axis(
#     2
#     ,at=ticks.y
#     ,cex.axis=cx
#     ,las=2
#   )
#   title(ylab="Deaths (1000s)",line=2)
#   for (j in ticks.y) {
#     abline(
#       h=j
#       ,lwd=.1
#     )
#   }
#   
#   points(
#     c(rep(NA,10),expectedPlusCV19[11:52])/scale
#     ,type="l"
#     ,col="red"
#     ,lwd=lw
#   )
#   
#   points(
#     observed/scale
#     ,type="l"
#     ,col="black"
#     ,lw=lw
#   )
#   
#   shadeCol=adjustcolor("gray",alpha.f=transparent)
#   polygon(
#     c(50.5,52.5,52.5,50.5)
#     ,c(y.min,y.min,y.max,y.max)
#     ,col=shadeCol
#     ,border=shadeCol
#     ,lwd=0.5
#   )
#   
#   points(
#     expected/scale
#     ,type="l"
#     ,col="blue"
#     ,lw=lw
#   )
#   
#   points(
#     c(expectedPlusCV19[1:11],rep(NA,42))/scale
#     ,type="l"
#     ,col="red"
#     ,lwd=lw*0.5
#   )
#   
#   legend(
#     x=22
#     ,y=y.max
#     # "bottomleft"
#     ,legend=c(
#       "Expected"
#       ,"Expected + CV19"
#       ,"Observed"    
#       ,"Shaded may change"
#       ,paste("Total excess >",format(excessTotal,big.mark=",",digits=1,scientific=FALSE))
#       ,paste("Total CV19 >",format(CV19Total,big.mark=",",digits=1,scientific=FALSE))
#     )
#     ,col=c("blue","red","black",shadeCol,"white","white","white")
#     ,lty=c(1,1,1,1,1,1,1)
#     ,lwd=c(lw,lw,lw,10,0,0,0)
#     ,bg="white"
#     ,cex=cexLeg
#   )
#   
#   suppressWarnings(par(par.default))
#   
#   if (toPDF) {
#     dev.off()
#   }
#   
# }  
