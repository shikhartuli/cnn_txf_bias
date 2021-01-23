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
  }

}

###################################################################
#    SAVE COHEN-KAPPA RESULTS
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
  for(method in c("kappa")) { 
    MODELS = list(ViT_B_32, ViT_B_32_ft, resnet, resnet_ft)

    y.value.name = "cohens.kappa"
    x.value.name = "expected.consistency"

    s_no <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    df <- data.frame(s_no)

    for(model in MODELS) {
    	kappas = c()
        for(subj1 in unique(dataset[dataset$is.human==TRUE, ]$subj)) {
        consistency = consistency.analysis(dataset, obs1=subj1, obs2 = model$data.name)
        kappas <- c(kappas, consistency[y.value.name][1,1])
        print(paste("Model: ", model$name, " ", "Cohen's kappa: ", consistency[y.value.name][1,1], sep=""))
        }
        df[model$data.name] <- kappas
    } 
    write.csv(df, paste(dataset.path, "/", dataset.name, "_", method, ".csv", sep=""), row.names=FALSE)
  }
  
}
