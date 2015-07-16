###################
#### libaries #####
###################
library(compiler) # fct: cmpfun(...)
library(XLConnect) # allows to export excel files,e.g. writeWorksheetToFile("results/2014.12.3.xls",data=SWLFitRound,sheet="SWLFit")
library(ggplot2) # ggplots
library(grid) # several ggplots on 1 page
library(plyr) # # function ddply

##############################
#### database connection #####
##############################

library(RPostgreSQL)

drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv,dbname="hyalesthes",host="localhost",port=5432,user="pillo",password="oenothera")
#summary(con)
#dbListTables(con)

#dbDisconnect(con)
#dbUnloadDriver(drv)



# ... <-dbGetQuery(con,"select * FROM .... ORDER BY id")




########################
#### date and time #####
########################

# Date format - forms part of names of created files or graphs
today<-format(Sys.time(), "%Y.%m.%d")

# The IDate class is a simple wrapper around the Date 
# class that tries to keep an integer storage format. The ITime class, the 
# time of day, is stored as the number of seconds in a day.
#source("C:/Programme/R/R-2.11.1/library/IDateTime.R")



# total run time
# Usage:
# before run enter: start.time<-Sys.time()
# after run enter: end.time<-Sys.time();run.time(end.time)


run.time<-function(end.time){
run.sec<-trunc(difftime(end.time,start.time,unit="sec"))[[1]]
run.min<-trunc(difftime(end.time,start.time,unit="mins"))[[1]]
run.hours<-trunc(difftime(end.time,start.time,unit="hours"))[[1]]

if(run.hours==0){
m<-run.min*60
run.sec2<-run.sec-m
print(paste("Total run time: ",run.min," minutes and ",run.sec2," seconds",sep=""))}

if(run.hours>0){
h<-run.hours*60
run.min2<-run.min-h
m<-run.min*60
run.sec2<-run.sec-m
print(paste("Total run time: ",run.hours," hours and ", run.min2," minutes and ",run.sec2," seconds",sep=""))}
}





#******************************  Data exploration ***************************#


# correct formatting for units
unit <- list(
mgl = expression(paste("(",mg~l^{-1},")")),
mgkg = expression(paste("(",mg~kg^{-1},")")),
mugl = expression(paste("(",mu,g~l^{-1},")")),
mugkg = expression(paste("(",mu,g~kg^{-1},")")),
mum = expression(paste("(",mu,m,")")),
temp = expression(paste("T (",degree,"C)")),
phos = expression(paste(PO[4]^{3-phantom(0)},"(",mu,g~l^{-1},")"))
)



# Standard error of a mean
se<-function(x) sqrt(var(x)/length(x))


# Error bars for boxplots
#Usage error.bar(means, standard errors and bar labels)
error.bars<-function(yv,z,nn,ylab,ylim,main,cex.names){
xv<-barplot(yv,ylim=ylim,names=nn,ylab=ylab,main=main,cex.names=cex.names)
g=(max(xv)-min(xv))/50
for (i in 1:length(xv)) {
lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i], yv[i]+z[i]))
lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i], yv[i]-z[i]))
}}



# rounding up: 
# k=2, 321-> 400, 3221->3300
# k=3  321 -> 1000, 3221 -> 4000
roundout <- function(x,k){(floor( abs(x/10^k) +0.499999999999 ) +1)*10^k}


roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
     if(length(x) != 1) stop("'x' must be of length 1")
     10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# 3 significant digits
NumToString <- function(nval, sigdig=3, xpcut=6) {   # Filter out zero as a special case; calculate digits after the decimal
# from number of significant digits 
   if (nval == 0) digs = sigdig - 1 else digs <- sigdig - 1 - floor(log10(abs(nval))) 
   # Can't be negative 
   if (digs < 0) digs <- 0 
   # Switch to scientific notation if digits after the decimal > xp   
if (digs > xpcut) { fchar <- "e"; digs <- sigdig - 1 } else fchar <- "f" 
sprintf(paste("%.", digs, fchar, sep=""), nval) }



