###################################################################
#   Data analysis script for error consistency analysis
#   -------------------------------------------------------
#   Based on: R version 3.5.1
###################################################################

source("data-analysis-helper.R")

FIGUREPATH = "../figures/"
DATAPATH = "../raw-data/"

# confidence intervals are simulated beforehand
# and stored in the directory below. If you would like to 
# simulate new confidence intervals (e.g. for a different number of trials),
# please execute "SimulateConfIntervalls.R" for this purpose.
CONFIDENCE.INTERVAL.DIR = "./confidence-intervals/"

###################################################################
#    LOADING EXPERIMENTAL DATA
###################################################################

# texture-shape experiments
sildat = get.expt.data("texture-shape_silhouettes")
edgedat = get.expt.data("texture-shape_edges")
cueconfdat = get.expt.data("texture-shape_cue-conflict", is.style.transfer = TRUE)

# noise-generalisation experiments
uniformnoisedat = get.expt.data("noise-generalisation_uniform-noise")
contrastpngdat = get.expt.data("noise-generalisation_contrast-png")
phasescramblingdat = get.expt.data("noise-generalisation_phase-scrambling")
highpassdat = get.expt.data("noise-generalisation_high-pass")
lowpassdat = get.expt.data("noise-generalisation_low-pass")
eidolondat = get.expt.data("noise-generalisation_eidolon")
e0dat = get.eidolon.dat.preprocessed(eidolondat, 0)
e3dat = get.eidolon.dat.preprocessed(eidolondat, 3)
e10dat = get.eidolon.dat.preprocessed(eidolondat, 10)

# Brain-Score data
BrainScore = get.brainscore.data(paste(DATAPATH, "brain-score_metrics/", "brainscore_metrics.csv", sep=""))

###################################################################
#    PRINT NUMBERS REPORTED IN PAPER
###################################################################

# ignore warning messages:
# "no non-missing arguments", "argument is not numeric or logical"
# -> they are expected here since only one subject is analysed
PLOT.SUBJECTS = list(resnet)
plot.consistency(cueconfdat, method="kappa", verbose=TRUE)
#[1] "avg human-human consistency: 0.331"
#[1] "avg CNN-CNN consistency: NA"
#[1] "avg CNN-human consistency: 0.068"

PLOT.SUBJECTS = list(cornet.S)
plot.consistency(cueconfdat, method="kappa", verbose=TRUE)
#[1] "avg human-human consistency: 0.331"
#[1] "avg CNN-CNN consistency: NA"
#[1] "avg CNN-human consistency: 0.066"

PLOT.SUBJECTS = list(alexnet)
plot.consistency(cueconfdat, method="kappa", verbose=TRUE)
#[1] "avg human-human consistency: 0.331"
#[1] "avg CNN-CNN consistency: NA"
#[1] "avg CNN-human consistency: 0.08"

PLOT.SUBJECTS = list(resnet, cornet.S)
plot.consistency(cueconfdat, method="kappa", verbose=TRUE)
#[1] "avg human-human consistency: 0.331"
#[1] "avg CNN-CNN consistency: 0.711" 
#[1] "avg CNN-human consistency: 0.067"

PLOT.SUBJECTS = subject.factory(edgedat, PYTORCH.MODELS, distinguish.model.families = TRUE)
plot.consistency.vs.acc(edgedat, "top.5", verbose=TRUE)
# [1] "Maximum observed CNN-to-CNN consistency: 0.793 between the following two models: resnet18 densenet121"

# correlation of Brain-Score behaviour and ImageNet top-1 accuracy
dat = BrainScore[complete.cases(BrainScore$ImageNet, BrainScore$behaviour), ] # remove model 'vggface' without ImageNet accuracy
round(cor(dat$ImageNet, dat$behaviour), 3)
# [1] 0.834

#    LINEAR MODEL FIT FOR CONSISTENCY VS. ACCURACY
PLOT.SUBJECTS = subject.factory(cueconfdat, PYTORCH.MODELS, distinguish.model.families = TRUE)
plot.consistency.vs.acc(cueconfdat, x.value="top.5")
# Multiple R-squared:  0.0005459,	Adjusted R-squared:  -0.00578
# F-statistic: 0.08631 on 1 and 158 DF,  p-value: 0.7693

plot.consistency.vs.acc(edgedat, x.value="top.5")
# Multiple R-squared:  0.003013,	Adjusted R-squared:  -0.003297 
# F-statistic: 0.4775 on 1 and 158 DF,  p-value: 0.4906

plot.consistency.vs.acc(sildat, x.value="top.5")
# Multiple R-squared:  0.253,	Adjusted R-squared:  0.2483 
# F-statistic: 53.53 on 1 and 158 DF,  p-value: 1.21e-11

###################################################################
#    PRINT ACCURACIES TO TABLE
###################################################################

t = data.frame(observer=character(0), cue.conflict=numeric(0),
               edge=numeric(0), silhouette=numeric(0))
for(s in c(as.character(unique(cueconfdat[cueconfdat$is.human==TRUE, ]$subj)), PYTORCH.MODELS, "cornet-s")) {
  print(s)
  t = rbind(t, data.frame(observer=s,
                          cue.conflict=round(get.single.accuracy(cueconfdat[cueconfdat$subj==s, ]), 2),
                          edge=round(get.single.accuracy(edgedat[edgedat$subj==s, ]), 2),
                          silhouette=round(get.single.accuracy(sildat[sildat$subj==s, ]), 2)))
}
print(xtable(t, type = "latex"), file = "../documentation/observer_accuracies.tex")

###################################################################
#    PLOT DENSITY OF C_EXP FOR P_I AND P_J (FIGURE SF.2)
###################################################################

gridsize = 300 
p_i = seq(0.0, 1, length.out = gridsize)
p_j = p_i
cexp = outer(p_i,p_j,'*') +  outer((1-p_i),(1-p_j),'*')
savePathFigure =(paste(FIGUREPATH, '/DiagnosticPlotsKappa/CexpDensity.png', sep=''))
png(savePathFigure)
filled.contour(cexp,color.palette=viridis,nlevels = 40,asp =1,xlim = c(0,1),ylim=c(0,1),key.title = title(expression('c'[exp])),
               xlab =expression('p'[i]),
               key.axes = axis(4, seq(0, 1, by = 0.25),line = 1),
               plot.axes = { axis(1, seq(0,1,length.out = 6), at = seq(0,1,length.out = 6), line=1);
                 contour(cexp, levels = c(0.5,0.3,0.7), labels = c("0.5","0.3","0.7"),labcex=1,lwd = 2, col = ("white"), add = TRUE);
                 axis(2, seq(0,1,length.out = 6), line=1)},
               frame.plot = F,
               xaxp  = c(0, 2, 5)
)
title(ylab=expression("p"[j]), line=2)
dev.off()


###################################################################
#    CORNet-S: SHAPE BIAS
###################################################################

pdf(file=paste(FIGUREPATH, "texture-shape_cue-conflict/texture-shape_cue-conflict_shape-bias_all_pytorch_models.pdf", sep=""), 
    width=6.5, 
    height=6.5)
par(mfrow=c(1,1))
PLOT.SUBJECTS = subject.factory(cueconfdat, PYTORCH.MODELS, distinguish.model.families = TRUE)
plot.shape.bias(cueconfdat)
dev.off()

pdf(file=paste(FIGUREPATH, "texture-shape_cue-conflict/texture-shape_cue-conflict_shape-bias_cornet_S.pdf", sep=""), 
    width=6.5, 
    height=6.5)
par(mfrow=c(1,1))
PLOT.SUBJECTS = list(resnet, cornet.S)
plot.shape.bias(cueconfdat, plot.legend = TRUE, legend.position="top")
dev.off()

###################################################################
#    CORNet-S: NOISE GENERALISATION
###################################################################

# custom par settings for all of these plots
custom.mar = c(5.1, 5.1, 4.1, 2.1)
plot.observer.range = TRUE
width=6.0
height=5.0

# condition 'Inf' is excluded here
pdf(file=paste(FIGUREPATH,"noise-generalisation_high-pass/noise-generalisation_high-pass_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(highpassdat[highpassdat$condition!="Inf", ],
                 plot.observer.range = plot.observer.range,
                 plot.legend = F,
                 logarithmic.scale = T,
                 log.base = 2,
                 main="",
                 xlab=expression("Log"[2]*" of filter standard deviation"),
                 ylab="",
                 inverse.x.axis = TRUE)
dev.off()

# condition '0' is excluded here
pdf(file=paste(FIGUREPATH,"noise-generalisation_low-pass/noise-generalisation_low-pass_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(lowpassdat[lowpassdat$condition!="0", ],
                 plot.legend = F,
                 logarithmic.scale = T,
                 log.base=2,
                 plot.observer.range = plot.observer.range,
                 main="",
                 xlab=expression("Log"[2]*" of filter standard deviation"),
                 ylab="")
dev.off()

## uniform noise
pdf(file=paste(FIGUREPATH,"noise-generalisation_uniform-noise/noise-generalisation_uniform-noise_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(uniformnoisedat,
                 legend.position = "topright",
                 cex.legend=1.7,
                 plot.observer.range = plot.observer.range,
                 main="",
                 xlab="Uniform noise width",
                 x.range=c(0,0.9),
                 ticks=c(0.0, 0.2, 0.4, 0.6, 0.9))
dev.off()

## contrast
pdf(file=paste(FIGUREPATH,"noise-generalisation_contrast-png/noise-generalisation_contrast-png_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(contrastpngdat,
                 plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 plot.legend = F,
                 log.base = 10,
                 main="",
                 xlab=expression("Log"[10]*" of contrast in percent"),
                 ylab="",
                 inverse.x.axis = TRUE)
dev.off()

## eidolon I (coh = 1.0)
pdf(file=paste(FIGUREPATH,"noise-generalisation_eidolon/noise-generalisation_eidolon_I_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e10dat, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"))
dev.off()

## eidolon II (coh = 0.3)
pdf(file=paste(FIGUREPATH,"noise-generalisation_eidolon/noise-generalisation_eidolon_II_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e3dat, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"),
                 ylab="")
dev.off()

## eidolon III (coh = 0.0)
pdf(file=paste(FIGUREPATH,"noise-generalisation_eidolon/noise-generalisation_eidolon_III_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(e0dat, plot.legend = F, plot.observer.range = plot.observer.range,
                 logarithmic.scale = T,
                 log.base = 2, main="", xlab=expression("Log"[2]*" of 'reach' parameter"),
                 ylab="")
dev.off()


# phase noise
pdf(file=paste(FIGUREPATH,"noise-generalisation_phase-scrambling/noise-generalisation_phase-scrambling_accuracy.pdf", sep=""), 
    width=width, 
    height=height)
par(mfrow=c(1,1), mar=custom.mar)
plot.performance(phasescramblingdat,
                 plot.observer.range = plot.observer.range,
                 plot.legend = F,
                 main="",
                 x.range=c(0, 180),
                 xlab="Phase noise width [°]",
                 ticks=c(0, 30, 60, 90, 120, 150, 180),
                 ylab="")
dev.off()

###################################################################
#    PLOT ERROR CONSISTENCY ANALYSES
###################################################################

DATASET.LIST = list(edgedat, sildat, cueconfdat)

figure.width = 5.0
figure.height = 5.0

for(dataset in DATASET.LIST) {
  dataset.name = as.character(unique(dataset$experiment.name))
  print(dataset.name)
  if(length(dataset.name)>1) {
    stop("dataset name mismatch")
  }
  dataset.path = paste(FIGUREPATH, dataset.name, sep="/")
  if(!dir.exists(dataset.path)) {
    dir.create(dataset.path)
    print(paste("creating directory ", dataset.path, sep=""))
  }
  for(method in c("consistency", "kappa")) {
    pdf(file=paste(dataset.path, "/", dataset.name, "_", method, "_four_networks.pdf", sep=""), 
        width=figure.width, 
        height=figure.height)
    PLOT.SUBJECTS = list(alexnet, vgg, googlenet, resnet)
    plot.consistency(dataset, method=method, legend.cex=0.9, distinguish.model.families = FALSE,
                     plot.bound=FALSE,
                     plot.legend=(method!="consistency"))
    dev.off()
    
    pdf(file=paste(dataset.path, "/", dataset.name, "_", method, "_all_pytorch_models.pdf", sep=""), 
        width=figure.width, 
        height=figure.height)
    PLOT.SUBJECTS = subject.factory(dataset, PYTORCH.MODELS, distinguish.model.families = TRUE)
    if(method=="consistency") {
      plot.consistency(dataset, method=method, plot.legend = FALSE, distinguish.model.families = TRUE)
    } else {
      plot.consistency(dataset, method=method, distinguish.model.families = TRUE)
    }
    dev.off()
    
    pdf(file=paste(dataset.path, "/", dataset.name, "_", method, "_cornet_S.pdf", sep=""), 
        width=figure.width, 
        height=figure.height)
    PLOT.SUBJECTS = list(resnet, cornet.S)
    plot.consistency(dataset, method=method, legend.cex=1.0, points.cex=1.2)
    dev.off()
  }
  for(top in c("top.1", "top.5")) {
    pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", top, "_accuracy.pdf", sep=""), 
        width=figure.width+1, 
        height=figure.height)
    PLOT.SUBJECTS = subject.factory(dataset, PYTORCH.MODELS, distinguish.model.families = TRUE)
    plot.consistency.vs.acc(dataset, x.value=top, plot.special.legend=dataset.name=="texture-shape_cue-conflict",
                            plot.names=FALSE, y.axis.max.value=0.69)
    dev.off()
    
    
    pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", top, "_accuracy_four_networks.pdf", sep=""), 
        width=figure.width, 
        height=figure.height)
    PLOT.SUBJECTS = list(alexnet, vgg, googlenet, resnet)
    plot.consistency.vs.acc(dataset, x.value=top,
                            plot.names=FALSE, plot.average.CI=FALSE)
    dev.off()
  }
  
  # Brain-Score plotting
  PLOT.SUBJECTS = subject.factory(dataset, BRAINSCORE.MODELS, distinguish.model.families = TRUE)
  pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_average.pdf", sep=""), 
      width=figure.width+1, 
      height=figure.height)
  plot.consistency.vs.acc(dataset, x.value="avg", plot.names=FALSE,
                          x.axis.dataset = BrainScore, xlab="Brain-Score: 'average' score",
                          x.range=c(0.28, 0.4), x.pos.text.label=0.385,
                          y.axis.max.value=0.69)
  dev.off()
  
  pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_behaviour.pdf", sep=""), 
      width=figure.width+1, 
      height=figure.height)
  plot.consistency.vs.acc(dataset, x.value="behaviour", plot.names=FALSE,
                          x.axis.dataset = BrainScore, x.range = c(0.25, 0.6),
                          xlab="Brain-Score: 'behaviour' score", x.pos.text.label=0.415,
                          y.axis.max.value=0.69)
  dev.off()
  
  pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_V1.pdf", sep=""), 
      width=figure.width+1, 
      height=figure.height)
  plot.consistency.vs.acc(dataset, x.value="V1", plot.names=FALSE,
                          x.axis.dataset = BrainScore, x.range = c(0.16, 0.25),
                          xlab="Brain-Score: 'V1' score", x.pos.text.label=0.233,
                          y.axis.max.value=0.69)
  dev.off()
  
  pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_V2.pdf", sep=""), 
      width=figure.width+1, 
      height=figure.height)
  plot.consistency.vs.acc(dataset, x.value="V2", plot.names=FALSE,
                          x.axis.dataset = BrainScore, x.range = c(0.22, 0.3),
                          xlab="Brain-Score: 'V2' score", x.pos.text.label=0.24,
                          y.axis.max.value=0.69)
  dev.off()
  
  pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_V4.pdf", sep=""), 
      width=figure.width+1, 
      height=figure.height)
  plot.consistency.vs.acc(dataset, x.value="V4", plot.names=FALSE,
                          x.axis.dataset = BrainScore, x.range = c(0.55, 0.65),
                          xlab="Brain-Score: 'V4' score", x.pos.text.label=0.64,
                          y.axis.max.value=0.69)
  dev.off()
  
  pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_IT.pdf", sep=""), 
      width=figure.width+1, 
      height=figure.height)
  plot.consistency.vs.acc(dataset, x.value="IT", plot.names=FALSE,
                          x.axis.dataset = BrainScore, x.range = c(0.45, 0.6),
                          xlab="Brain-Score: 'IT' score", x.pos.text.label=0.58,
                          y.axis.max.value=0.69)
  dev.off()
}



###################################################################
#    CROP PDFs TO MINIMUM SIZE (REMOVING WHITE SPACE AT BORDERS)  
###################################################################
crop.pdfs("../figures/")
