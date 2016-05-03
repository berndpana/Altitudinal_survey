
#******************************  Data exploration ***************************#


# USEFUL DEFINITIONS
unit <- list(
  mgl = expression(paste("(",mg~l^{-1},")")),
  mgkg = expression(paste("(",mg~kg^{-1},")")),
  mugl = expression(paste("(",mu,g~l^{-1},")")),
  mugkg = expression(paste("(",mu,g~kg^{-1},")")),
  mum = expression(paste("(",mu,m,")")),
  temp = expression(paste("T (",degree,"C)")),
  oxy = expression(paste(O[2]," (",mg~l^{-1},")")),
  phyto= expression(paste("Phytoplankton (",mg~l^{-1},")")),
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



# 3 significant digits
NumToString <- function(nval, sigdig=3, xpcut=4) {   # Filter out zero as a special case; calculate digits after the decimal
  # from number of significant digits 
  if (nval == 0) digs = sigdig - 1 else digs <- sigdig - 1 - floor(log10(abs(nval))) 
  # Can't be negative 
  if (digs < 0) digs <- 0 
  # Switch to scientific notation if digits after the decimal > xp   
  if (digs > xpcut) { fchar <- "e"; digs <- sigdig - 1 } else fchar <- "f" 
  sprintf(paste("%.", digs, fchar, sep=""), nval) }
#library(broman) # rounding




##########################
# consolidatePhytoplasma #
##########################

# function calculates: phytoplasma occurrence per site

# function needs as import a data.frame with following structure:
# > head(qpcr[,c(2,8,9)])
# site type_a type_b
# 1    2      0      0
# 2    4      0      0
# 3    4      0      0
# 4    4      0      0
# 5    4      0      0
# 6    4      0      0

consolidatePhytoplasma<-function(df){
  uniqueSite<-unique(df$site)
  df.new<-data.frame(matrix(0,nrow=length(uniqueSite),ncol=6))
  colnames(df.new) <- c("site","type_a","type_b","phytoplasma","Ntotal","Ninfected")
  
  for(i in 1:length(uniqueSite)){
     dfSite<-df[df$site==uniqueSite[i],]
      df.new[i,1] <- uniqueSite[i]
      df.new[i,2] <- sum(dfSite[,2]) # type a
      df.new[i,3] <- sum(dfSite[,3]) # type b
      if (df.new[i,2]>0 | df.new[i,3] > 0){df.new[i,4] <-1} # infected or not?
      df.new[i,5] <- nrow(dfSite) # total analyzed insects
      df.new[i,6] <- df.new[i,2]+df.new[i,3]

  }
  return(df.new)
}




untangleSites <- function(data){
  # http://www.endmemo.com/program/R/grepl.php
  
  rownumbersCombinedTrees = which(data[,4]>1)
  
  for (i in 1:length(rownumbersCombinedTrees)){ # loop over rows with characters
    j <- rownumbersCombinedTrees[i]
    rowCombinedTrees <- as.vector(t(as.matrix(data[j,]))) # get the entire row where trees combined
    combinedTrees <- as.character(data[j,1])              # get id's of combined trees
    
    print(c(i,j,combinedTrees)) 
    
    
    newData <- t(replicate(data[j,4], rowCombinedTrees)) # duplicate rows based on number of trees
    newData[,1] <- seq(firstTree,lastTree) # # replace e.g. 1-10,1-10,1-10,1-10,1-10,1-10,1-10,1-10,1-10,1-10 with  1 2 3 4 5 6 7 8 9 10
    colnames(newData) <- colnames(data)
    # append newData 
    data <- data.frame(rbind(data,newData),row.names = NULL)
  }
}






###################
#### graphics #####
###################

# get equally spaced hues around the color wheel, starting from 15
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# example:
# n = 4
# cols = gg_color_hue(4)
# 
# dev.new(width=4, height=4)
# plot(1:n, pch=16, cex=2, col=cols)
# functions extracts reegression statistics used for ggplot



# Linear regression
lm_eqn_1p = function(df,x,y){
  m = lm(y ~ x, df);
  if (coef(m)[2] >= 0)  { # coefficient with postive sign or 0
    eq <- substitute(italic(y) == a + b~italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = NumToString(coef(m)[1]), 
                          b = NumToString(coef(m)[2]),
                          r2 = NumToString(summary(m)$r.squared,1))) # 1 significant digit
  } else { # coefficient with negative sign
    eq <- substitute(italic(y) == a - b~italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = NumToString(coef(m)[1]), 
                          b = NumToString(abs(coef(m)[2])),
                          r2 = NumToString(summary(m)$r.squared,1))) # 1 significant digit
  }
  as.character(as.expression(eq));                 
}


lm_eqn_1p_sq = function(df,x,y){
  m = lm(y ~ x + I(x^2), df);
    eq <- substitute(italic(y) == a + b~italic(x)+ c~italic(x^2)*","~~italic(r)^2~"="~r2, 
                     list(a = NumToString(coef(m)[1]), 
                          b = NumToString(coef(m)[2]),
                          c = NumToString(coef(m)[3]),
                          r2 = NumToString(summary(m)$r.squared,1))) # 1 significant digit

  as.character(as.expression(eq));                 
}



lm_eqn_2p = function(df,x1,x2,y){
  m = lm(y ~ x1+x2, df);
    eq <- substitute(italic(y) == a + b~italic(x1)+ c~italic(x2)*","~~italic(r)^2~"="~r2, 
                     list(a = NumToString(coef(m)[1]), 
                          b = NumToString(coef(m)[2]),
                          c = NumToString(coef(m)[3]),
                          r2 = NumToString(summary(m)$r.squared,1))) # 1 significant digit
  as.character(as.expression(eq));                 
}


lm_eqn = function(df,x,y){
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

d2 <- function(model) { NumToString(1-(model$deviance/model$null.deviance),3) }

# function to obtain bootstrapped preditced probabilities from the data 
bootPredict <- function(formula,data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- glm(formula,data=d,family="binomial")
  return(predict(fit,type="response"))
}

doUnivariateTest <- function(Site='', dfSim,dfObs,fun, plot = T){
  simulatedValues <- apply(dfSim, 2, fun)  # e.g. length= 20.000; length of combined chains, function fun applied to each iteration 
  observedValue <- fun(dfObs)               # length = 1; function fun applied to all observations
  
  low <- sum(simulatedValues < observedValue) / length(simulatedValues)
  up <- sum(simulatedValues <= observedValue) / length(simulatedValues)
  
  pv = mean(c(low, up))
  
  #pv = lowerP
  #else if (upperP > 0.5) pv = upperP
  #else if (lowerP < 0.5 & upperP > 0.5) pv = 0.5  
  
  if (plot == T){
    hist(simulatedValues, main = paste("model fit = ",NumToString(pv,2)))
    abline(v=observedValue, col = "red", lwd = 3)      
  }
  return(pv)
}
