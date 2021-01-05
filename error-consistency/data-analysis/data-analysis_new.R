###################################################################
#   Data analysis script for error consistency analysis
#   -------------------------------------------------------
#   Based on: R version 3.5.1
###################################################################

source("data-analysis-helper_new.R")

FIGUREPATH = "../figures_new_ft/"
DATAPATH = "../raw-data_new/"

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
#    PLOT SHAPE BIAS
###################################################################


pdf(file=paste(FIGUREPATH, "texture-shape_cue-conflict/texture-shape_cue-conflict_shape-bias.pdf", sep=""), 
    width=6.5, 
    height=6.5)
par(mfrow=c(1,1))
# PLOT.SUBJECTS = list(resnet, alexnet, vgg, googlenet, ViT_B_32, ViT_L_16)
PLOT.SUBJECTS = list(resnet, resnet_ft, ViT_B_32, ViT_B_32_ft)
plot.shape.bias(cueconfdat, plot.legend = TRUE, legend.position="topleft")
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
    PLOT.SUBJECTS_CNN = list(alexnet, vgg, googlenet, resnet)
    PLOT.SUBJECTS = list(alexnet, vgg, googlenet, resnet, resnet_ft, ViT_B_32, ViT_B_32_ft)
    PLOT.SUBJECTS_ViT = list(ViT_B_16, ViT_B_32, ViT_L_16, ViT_L_32)
    plot.consistency(dataset, method=method, legend.cex=0.9, distinguish.model.families = FALSE,
                     plot.bound=FALSE,
                     plot.legend=(method!="consistency"))
    dev.off()
  
    # pdf(file=paste(dataset.path, "/", dataset.name, "_", method, "_all_pytorch_models.pdf", sep=""), 
    #     width=figure.width, 
    #     height=figure.height)
    # PLOT.SUBJECTS = subject.factory(dataset, PYTORCH.MODELS, distinguish.model.families = TRUE)
    # if(method=="consistency") {
    #   plot.consistency(dataset, method=method, plot.legend = FALSE, distinguish.model.families = TRUE)
    # } else {
    #   plot.consistency(dataset, method=method, distinguish.model.families = TRUE)
    # }
    # dev.off()
    
    # pdf(file=paste(dataset.path, "/", dataset.name, "_", method, "_cornet_S.pdf", sep=""), 
    #     width=figure.width, 
    #     height=figure.height)
    # PLOT.SUBJECTS = list(resnet, cornet.S)
    # plot.consistency(dataset, method=method, legend.cex=1.0, points.cex=1.2)
    # dev.off()
  }
  # for(top in c("top.1", "top.5")) {
  #   pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", top, "_accuracy.pdf", sep=""), 
  #       width=figure.width+1, 
  #       height=figure.height)
  #   PLOT.SUBJECTS = subject.factory(dataset, PYTORCH.MODELS, distinguish.model.families = TRUE)
  #   plot.consistency.vs.acc(dataset, x.value=top, plot.special.legend=dataset.name=="texture-shape_cue-conflict",
  #                           plot.names=FALSE, y.axis.max.value=0.69)
  #   dev.off()
    
    
  #   pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", top, "_accuracy_four_networks.pdf", sep=""), 
  #       width=figure.width, 
  #       height=figure.height)
  #   PLOT.SUBJECTS = list(alexnet, vgg, googlenet, resnet)
  #   plot.consistency.vs.acc(dataset, x.value=top,
  #                           plot.names=FALSE, plot.average.CI=FALSE)
  #   dev.off()
  # }
  
  # # Brain-Score plotting
  # PLOT.SUBJECTS = subject.factory(dataset, BRAINSCORE.MODELS, distinguish.model.families = TRUE)
  # pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_average.pdf", sep=""), 
  #     width=figure.width+1, 
  #     height=figure.height)
  # plot.consistency.vs.acc(dataset, x.value="avg", plot.names=FALSE,
  #                         x.axis.dataset = BrainScore, xlab="Brain-Score: 'average' score",
  #                         x.range=c(0.28, 0.4), x.pos.text.label=0.385,
  #                         y.axis.max.value=0.69)
  # dev.off()
  
  # pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_behaviour.pdf", sep=""), 
  #     width=figure.width+1, 
  #     height=figure.height)
  # plot.consistency.vs.acc(dataset, x.value="behaviour", plot.names=FALSE,
  #                         x.axis.dataset = BrainScore, x.range = c(0.25, 0.6),
  #                         xlab="Brain-Score: 'behaviour' score", x.pos.text.label=0.415,
  #                         y.axis.max.value=0.69)
  # dev.off()
  
  # pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_V1.pdf", sep=""), 
  #     width=figure.width+1, 
  #     height=figure.height)
  # plot.consistency.vs.acc(dataset, x.value="V1", plot.names=FALSE,
  #                         x.axis.dataset = BrainScore, x.range = c(0.16, 0.25),
  #                         xlab="Brain-Score: 'V1' score", x.pos.text.label=0.233,
  #                         y.axis.max.value=0.69)
  # dev.off()
  
  # pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_V2.pdf", sep=""), 
  #     width=figure.width+1, 
  #     height=figure.height)
  # plot.consistency.vs.acc(dataset, x.value="V2", plot.names=FALSE,
  #                         x.axis.dataset = BrainScore, x.range = c(0.22, 0.3),
  #                         xlab="Brain-Score: 'V2' score", x.pos.text.label=0.24,
  #                         y.axis.max.value=0.69)
  # dev.off()
  
  # pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_V4.pdf", sep=""), 
  #     width=figure.width+1, 
  #     height=figure.height)
  # plot.consistency.vs.acc(dataset, x.value="V4", plot.names=FALSE,
  #                         x.axis.dataset = BrainScore, x.range = c(0.55, 0.65),
  #                         xlab="Brain-Score: 'V4' score", x.pos.text.label=0.64,
  #                         y.axis.max.value=0.69)
  # dev.off()
  
  # pdf(file=paste(dataset.path, "/", dataset.name, "_kappa_vs_", "BrainScore_IT.pdf", sep=""), 
  #     width=figure.width+1, 
  #     height=figure.height)
  # plot.consistency.vs.acc(dataset, x.value="IT", plot.names=FALSE,
  #                         x.axis.dataset = BrainScore, x.range = c(0.45, 0.6),
  #                         xlab="Brain-Score: 'IT' score", x.pos.text.label=0.58,
  #                         y.axis.max.value=0.69)
  # dev.off()
}
