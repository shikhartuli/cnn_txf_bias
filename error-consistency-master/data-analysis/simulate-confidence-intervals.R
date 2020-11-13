###################################################################
#   Numerical simulation of confidence intervals for 
#   cobs and kappa
#   -------------------------------------------------------
#   Based on: R version 3.5.1
###################################################################

library('binom')
rm(list=ls())

# Below we show the settings used in the paper.
# For a quick overview we recommend to reduce the gridSize
# considerably (e.g. gridSize = 200). 

###################################################################
#    SET PARAMETER AND PATHS
###################################################################

nTrialExp      = 160; # 160 or 1280 trials in experiment
alphalevel     = 0.05 # Set alpha level
gridSize       = 4200 # Gridsize of p_1 x p_2
repetitions    = 5;   # How many times do we repeat every gridpoint in later simulations
nBinsCI        = 100  # Number of bins for CI histogram binning for cobs and kappa

savePathFigure = '../figures/DiagnosticPlotsKappa/' # Path for Figure
savePathData   = paste('./confidence-intervals/',nTrialExp,'_trials.csv',sep='') # Path to store data

###################################################################
#    PREDEFINE ARRAYS
###################################################################
p1 = seq(0.0,0.15,length.out = gridSize/3)
p1 = c(p1,seq(0.15,0.85,length.out = gridSize/3))
p1 = c(p1,seq(0.85,1,length.out = gridSize/3))
p2 = p1

cexp = matrix(NA, nrow = repetitions*(gridSize^2), ncol = 1)
cobs = matrix(NA, nrow = repetitions*(gridSize^2), ncol = 1)
countIndex = 1 

###################################################################
#    SIMULATE COBS, CEXP AND KAPPA
###################################################################

for(p1points in 1:gridSize){
    for(p2points in 1:gridSize){
       # generate sequences
      seq1 =matrix(rbinom(nTrialExp*repetitions,1,p1[p1points]),nTrialExp,repetitions)
      seq2 =matrix(rbinom(nTrialExp*repetitions,1,p2[p2points]),nTrialExp,repetitions)
      # estimate accuracy
      p1Hat = colMeans(seq1);
      p2Hat = colMeans(seq2);
      # calculate cexp and cobs
      cexpCol= p1Hat*p2Hat + (1-p1Hat)*(1-p2Hat)
      cobsCol= colMeans(seq1 == seq2)
      cexp[countIndex:(countIndex+repetitions-1)] = cexpCol
      cobs[countIndex:(countIndex+repetitions-1)] = cobsCol
      countIndex = countIndex+repetitions
      }
}
# calculate kappa 
kappa = (cobs - cexp) /(1-cexp) 

###################################################################
# CALULATE CONFIDENCE INTERVALS FOR COBS AND KAPPA
###################################################################

ciBins = seq(0,1+(1/(nBinsCI-1)),length.out = nBinsCI+1)
cObsQuantiles = matrix(NA,nrow=nBinsCI,ncol = 3)
kappaQuantiles = matrix(NA,nrow=nBinsCI,ncol = 3)

for (i in 1:nBinsCI){
   # lower and upper part of bin
   cexpBinLower = ciBins[i]
   cexpBinUpper = ciBins[i+1]
   
   # get mean position of bin
   cObsQuantiles[i,1] = mean(cexp[(cexp>=cexpBinLower) & (cexp < cexpBinUpper)],na.rm = TRUE)
   kappaQuantiles[i,1] = cObsQuantiles[i,1] 
   
   # cobs quantile
   cobBinned = cobs[(cexp>=cexpBinLower) & (cexp < cexpBinUpper)]
   cObsQuantiles[i,2:3] = quantile(cobBinned,probs = c(alphalevel/2,1-(alphalevel/2)),na.rm = TRUE)
   
   # kappa quantile
   kappaBinnded =  kappa[(cexp>=cexpBinLower) & (cexp < cexpBinUpper)]
   kappaQuantiles[i,2:3] = quantile(kappaBinnded,probs = c(alphalevel/2,1-(alphalevel/2)),na.rm = TRUE)
}

#Export quantiles for plotting
CIExport = cbind(cexp=cObsQuantiles[,1], cobs_CIlow=cObsQuantiles[,2], cobs_CIHigh=cObsQuantiles[,3],kappa_CIlow=kappaQuantiles[,2],kappa_CIHigh=kappaQuantiles[,3])
write.csv(CIExport,savePathData)



###################################################################
# PLOTING
###################################################################

# analytical bound of obsand kappa for cexp < 0.5
cexpLower = seq(0.0001,0.49999, length.out = 1000)
cobsAnalytical1BoundUP =  1- sqrt(1 - 2 * cexpLower)
cobsAnalytical1BoundLow = rep(0,length(cexpLower))
kappaAnalytical1BoundUP = (cobsAnalytical1BoundUP - cexpLower)/(1-cexpLower)
kappaAnalytical1BoundLow = (cobsAnalytical1BoundLow - cexpLower)/(1-cexpLower)

# analytical bound of cobs and kappa for cexp > 0.5
cexpUpper = seq(0.50001,0.9999, length.out = 1000)
cobsAnalytical2BoundUP =  rep(1,length(cexpUpper))
cobsAnalytical2BoundLow = sqrt(2 * cexpUpper - 1)
kappaAnalytical2BoundUP = (cobsAnalytical2BoundUP - cexpUpper)/(1-cexpUpper)
kappaAnalytical2BoundLow = (cobsAnalytical2BoundLow - cexpUpper)/(1-cexpUpper)

# calculte binomial CU for cobs and erroneous CI for kappa
xvalBinomCI  = seq(0,1,length.out = 500)
binomialCI = binom.confint(x=nTrialExp*xvalBinomCI,n=nTrialExp,methods = 'exact')
wrongKappaCIUpper = qnorm(1-(alphalevel/2)) * sqrt(xvalBinomCI/(nTrialExp* (1-xvalBinomCI)))
wrongKappaCIHigh = -qnorm(1-(alphalevel/2)) * sqrt(xvalBinomCI/(nTrialExp* (1-xvalBinomCI)))

# plot options 
lineSize = 3
savePathFigurePDF = paste(savePathFigure,'DiagnosticPlotSimulateCobsKappa_',nTrialExp,'.pdf',sep='')
pdf(savePathFigurePDF) 
par(mfrow=c(1,2))
par(pty = 's')
plotIndizes = sample.int(length(cexp),100000,replace = FALSE) # randomly sample values to plot.

# start plotting cobs vs. cexp
plot(cexp[plotIndizes],cobs[plotIndizes],xlim = c(0,1),pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1),ylim = c(0,1),cex = 0.5,axes = FALSE,ylab=  expression("Observed  error overlap (c"[obs]*")"),xlab = expression("Error overlap expected by chance (c"[exp]*")"))
lines(cObsQuantiles[,1],cObsQuantiles[,2],col='#BB5566',lwd=lineSize)
lines(cObsQuantiles[,1],cObsQuantiles[,3],col='#BB5566',lwd=lineSize)
lines(cexpLower,cobsAnalytical1BoundLow,col='#004488',lwd=lineSize)
lines(cexpLower,cobsAnalytical1BoundUP,col='#004488',lwd=lineSize)
lines(cexpUpper,cobsAnalytical2BoundLow,col='#004488',lwd=lineSize)
lines(cexpUpper,cobsAnalytical2BoundUP,col='#004488',lwd=lineSize)
lines(xvalBinomCI,binomialCI$upper,col = '#DDAA33',lty = 2,lwd=lineSize)
lines(xvalBinomCI,binomialCI$lower,col = '#DDAA33' ,lty = 2,lwd=lineSize)
axis(side=2, at=seq(0,1,0.2))
axis(side=1, at=seq(0,1,0.2))

# start plotting kappa vs. cexp
par(pty = 's')
plot(cexp[plotIndizes],kappa[plotIndizes],xlim = c(0,1),pch = 19, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1),ylim = c(-1,1),cex = 0.5,axes = FALSE,ylab=expression(paste("Error consistency (", kappa, ")", sep="")),xlab = expression("Error overlap expected by chance (c"[exp]*")"))
lines(kappaQuantiles[,1],kappaQuantiles[,2],xlim = c(0,1),ylim = c(-1,1),col='#BB5566',lwd=lineSize)
lines(kappaQuantiles[,1],kappaQuantiles[,3],xlim = c(0,1),ylim = c(-1,1),col='#BB5566',lwd=lineSize)
lines(cexpLower,kappaAnalytical1BoundLow,col='#004488',lwd=lineSize)
lines(cexpLower,kappaAnalytical1BoundUP,col='#004488',lwd=lineSize)
lines(cexpUpper,kappaAnalytical2BoundLow,col='#004488',lwd=lineSize)
lines(cexpUpper,kappaAnalytical2BoundUP,col='#004488',lwd=lineSize)
lines(xvalBinomCI,wrongKappaCIUpper,col = '#DDAA33' ,lty = 2,lwd=lineSize,ylim=c(-1-1))
lines(xvalBinomCI,wrongKappaCIHigh,col = '#DDAA33',lty = 2 ,lwd=lineSize,ylim=c(-1-1))
axis(side=2, at=seq(-1,1,0.5))
axis(side=1, at=seq(0,1,0.2))
dev.off()

# crop figures and convert to png
system(paste('pdfcrop',savePathFigurePDF,savePathFigurePDF))
savePathFigurePNG = paste(substr(savePathFigurePDF,1,nchar(savePathFigurePDF)-3),'png',sep = '')
convertCommand = paste('convert -density 300',savePathFigurePDF,savePathFigurePNG)
system(convertCommand)
system(paste('rm',savePathFigurePDF))


# save legends for plotting in latex
savePathFigureLegendHorizontalPDF = paste(savePathFigure, 'Legend_horizontal.pdf',sep = '')
pdf(savePathFigureLegendHorizontalPDF)
par()
plot.new()
legend('center',ncol = 2,legend=c("Simulated data","95% Percentile","Analytical bounds","Erroneous confidence interval"),pch=c(19,NA,NA,NA), lty=c(0,1,1,2),lwd=2, col = c('black','#BB5566','#004488','#DDAA33'), cex=0.8, text.font=4, bg=rgb(0,0,0,alpha=0), box.lty=0)
dev.off()
system(paste('pdfcrop',savePathFigureLegendHorizontalPDF,savePathFigureLegendHorizontalPDF))
savePathFigureLegendHorizontalPNG = paste(savePathFigure, 'Legend_horizontal.png',sep = '')
convertCommand = paste('convert -density 300',savePathFigureLegendHorizontalPDF,savePathFigureLegendHorizontalPNG)
system(convertCommand)
system(paste('rm',savePathFigureLegendHorizontalPDF))

savePathFigureLegendVerticalPDF = paste(savePathFigure, 'Legend_vertical.pdf',sep = '')
pdf(savePathFigureLegendVerticalPDF)
par()
plot.new()
legend('center',ncol = 1,legend=c("Simulated data","95% Percentile","Analytical bounds","Erroneous confidence interval"),pch=c(19,NA,NA,NA), lty=c(0,1,1,2),lwd=2, col = c('black','#BB5566','#004488','#DDAA33'), cex=0.8, text.font=4, bg=rgb(0,0,0,alpha=0), box.lty=0)
dev.off()
system(paste('pdfcrop',savePathFigureLegendVerticalPDF,savePathFigureLegendVerticalPDF))
savePathFigureLegendVerticalPNG = paste(savePathFigure, 'Legend_vertical.png',sep = '')
convertCommand = paste('convert -density 300',savePathFigureLegendVerticalPDF,savePathFigureLegendVerticalPNG)
system(convertCommand)
system(paste('rm',savePathFigureLegendVerticalPDF))




