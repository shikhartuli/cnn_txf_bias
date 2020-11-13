###################################################################
#   Data analysis HELPER script for error consistency analysis
#   All important functions for data analysis should be collected
#   here (to be used for plotting, presentations, and in the
#   data-analysis.R script)
#   -------------------------------------------------------
#   Based on: R version 3.5.1
###################################################################

library(lattice)
library(jpeg)
library(R.matlab)
library(graphics)
library(pROC)
library(psych)
library(viridis)
library(grid)
library(gridExtra)
library(stats)
library(png)
library(pBrackets)
library(PET)
library(TeachingDemos)
library(binom)
library(RColorBrewer)
library(ggplot2)
library(scales) # load after library(psych) since it needs to mask function 'alpha'
library(xtable)


###################################################################
#               general settings
###################################################################

CATEGORIES = sort(c("airplane", "bear", "bicycle", "bird", "boat", "bottle", "car",
                    "cat", "chair", "clock", "dog", "elephant", "keyboard", "knife",
                    "oven", "truck"))   

PYTORCH.MODELS = c("alexnet", "vgg11-bn", "vgg13-bn", "vgg16-bn",
                   "vgg19-bn", "squeezenet1-0", "squeezenet1-1",
                   "densenet121", "densenet169", "densenet201",
                   "inception-v3", "resnet18", "resnet34",
                   "resnet50", "resnet101", "resnet152")

BRAINSCORE.MODELS = c("alexnet", "vgg16-bn",
                      "vgg19-bn", "squeezenet1-0", "squeezenet1-1",
                      "densenet121", "densenet169", "densenet201",
                      "inception-v3", "resnet18", "resnet34",
                      "resnet50", "resnet101", "resnet152")

IMAGENET.ACC = data.frame(model.name=c("alexnet", "vgg11-bn", "vgg13-bn", "vgg16-bn", "vgg19-bn", "squeezenet1-0", "squeezenet1-1", "densenet121", "densenet169", "densenet201", "inception-v3", "resnet18", "resnet34", "resnet50", "resnet101", "resnet152"),
                          top.1=c(56.522, 70.370, 71.586, 73.360, 74.218, 58.092, 58.178, 74.434, 75.600, 76.896, 69.538, 69.758, 73.314, 76.130, 77.374, 78.312),
                          top.5=c(79.066, 89.810, 90.374, 91.516, 91.842, 80.422, 80.624, 91.972, 92.806, 93.370, 88.654, 89.078, 91.420, 92.862, 93.546, 94.046))

###################################################################
#               define colours
###################################################################

human.100 = rgb(165, 30, 55, maxColorValue = 255)
human.80 = rgb(180, 77, 80, maxColorValue = 255)
human.70 = rgb(188, 98, 97, maxColorValue = 255)
human.60 = rgb(197, 121, 116, maxColorValue = 255)
human.40 = rgb(216, 166, 159, maxColorValue = 255)
human.20 = rgb(235, 210, 205, maxColorValue = 255)
human.10 = rgb(244, 232, 229, maxColorValue = 255)
HUMAN.COLS = c(human.100, human.80, human.60, human.40, human.20)

gold.10 = rgb(247, 244, 238, maxColorValue=255)
gold.100 = rgb(145, 105, 70, maxColorValue = 255)

vgg.100 = rgb(0, 105, 170, maxColorValue = 255)
alexnet.100 = rgb(65, 90, 140, maxColorValue = 255)
googlenet.100 = rgb(80, 170, 200, maxColorValue = 255)

###################################################################
#               define subjects
###################################################################

alexnet = list(name="AlexNet",
               color="#916946",
               pch=22,
               data.name="alexnet")
googlenet = list(name="GoogLeNet",
                 color="#B4A096",
                 pch=21,
                 data.name="inception-v3")
vgg = list(name="VGG-16",
           color="#238b45",
           pch=24,
           data.name="vgg16-bn")
resnet = list(name="ResNet-50",
              color="#4292c6",
              pch=21,
              data.name="resnet50")
cornet.S = list(name="CORnet-S",
                color="#D29600",
                pch=21,
                data.name="cornet-s")

###################################################################
#               some more general definitions
###################################################################

DNN.RANGE.LWD = 2
LINES.LWD = 2.5
POINTS.CEX.VAL = 2.5

HUMAN.PCH = 18
PARTICIPANTS = list()
for(i in 1:1000) {
  n = paste("subject-", ifelse(i<10, "0", ""), i, sep="")
  PARTICIPANTS[[i]] = list(name=n,
                           color=human.100,
                           pch=HUMAN.PCH,
                           data.name=n)
}
HUMAN.DATA.NAME = "participants (avg.)"
human.avg = list(name="participants (avg.)",
                 color=human.100,
                 pch=HUMAN.PCH, # 21Note: this is a filled circle, in contrast to the vision-model-DNN experiments.
                 data.name=HUMAN.DATA.NAME)

image.categories = list()
for(cat in CATEGORIES) {
    image.categories[[cat]] = readPNG(paste("category-images-for-plotting/",
                                          cat, ".png", sep=""))
}

get.all.subjects = function(dat, avg.human.data) {
  # Return all subjects, including networks.
  
  if(is.null(PLOT.SUBJECTS)) {
    stop("please define PLOT.SUBJECTS first!")
  }
  
  subjects = NULL
  z = 1
  subj.counter = 1
  # add subject only if contained in data
  for(s in PLOT.SUBJECTS) {
    if(nrow(dat[dat$subj==PLOT.SUBJECTS[[z]]$data.name, ]) > 0) {
      subjects[[subj.counter]] = PLOT.SUBJECTS[[z]]
      subj.counter = subj.counter + 1
    }
    z = z + 1
  }
  
  i = length(subjects) + 1
  
  if(avg.human.data) {
    subjects[[i]] = human.avg
  } else {
    counter = 1
    for(p in PARTICIPANTS) {
      if(p$data.name %in% unique(dat$subj)) {
        subjects[[i]] = p
        subjects[[i]]$color = HUMAN.COLS[i - length(subjects)]
        i = i+1
        counter = counter+1
      }
    }
  }
  return(subjects) 
}

###################################################################
#    DEFINE ANALYSIS & PLOTTING FUNCTIONS FOR ERROR CONSISTENCY
###################################################################

get.consistency.upper.bound = function(c.exp) {
  # Given an expected consistency, return  maximum
  # possible upper limit on consistency
  return(1-get.consistency.lower.bound(1-c.exp))
}


get.consistency.lower.bound = function(c.exp) {
  # Given an expected consistency, return minimum
  # possible lower limit on consistency
  return(sqrt(2.0*c.exp-1))
}


consistency.analysis = function(dat, obs1, obs2) {
  # Return data frame with consistency results for obs1 x obs2
  
  dat1 = dat[dat$subj==obs1, ]
  dat2 = dat[dat$subj==obs2, ]
  num.trials = nrow(dat1)
  if(nrow(dat1) != nrow(dat2)) {
    stop(paste("data mismatch: unequal # of rows for subjects ", obs1, " and ", obs2,
               ": ", as.character(nrow(dat1)), " != ", as.character(nrow(dat2)), sep=""))
  }
  if(nrow(dat1)==0 | nrow(dat2)==0) {
    stop("missing data")
  }
  
  # expected consistency under the independence assumption
  p1 = get.single.accuracy(dat1)
  p2 = get.single.accuracy(dat2)
  expected.consistency = p1*p2 + (1-p1)*(1-p2)
  
  # observed consistency
  dat1 = dat1[order(dat1$image.id), ]
  dat2 = dat2[order(dat2$image.id), ]
  
  if(any(dat1$image.id != dat2$image.id)){
    print(dat1$image.id)
    print(dat2$image.id)
    stop(paste("datasets cannot be compared: different images for subjects ", obs1, " and ", obs2, sep=""))
  }
  
  equal.responses = dat1$is.correct == dat2$is.correct
  num.equal.responses = sum(equal.responses) # gives number of 'TRUE' values
  observed.consistency = num.equal.responses / num.trials
  
  if(observed.consistency==1.0) {
    cohens.kappa = 1.0
  } else {
    estimate = cohen.kappa(x=cbind(dat1$is.correct, dat2$is.correct))
    cohens.kappa = estimate$kappa
  }
  
  return(data.frame(expected.consistency=expected.consistency,
                    observed.consistency=observed.consistency,
                    cohens.kappa=cohens.kappa))
}


get.mean.kappa.from.list = function(kappa, alpha=.05) {
  # Return sample mean & variance as well as CI for list of kappa values
  
  sample.mean.kappa = mean(kappa)
  sample.variance.kappa = var(kappa)
  sample.CI.width.one.side = qnorm(1-(alpha/2.0)) * sqrt(sample.variance.kappa/length(kappa))
  sample.CI.upper = sample.mean.kappa + sample.CI.width.one.side
  sample.CI.lower = sample.mean.kappa - sample.CI.width.one.side
  
  
  return(data.frame(
    sample.mean.kappa=sample.mean.kappa,
    sample.CI.upper=sample.CI.upper,
    sample.CI.lower=sample.CI.lower,
    sample.CI.width.one.side=sample.CI.width.one.side))
}


plot.consistency.vs.acc = function(dat, 
                                   x.value=c("top.5", "top.1", "avg"),
                                   plot.standard.legend=FALSE,
                                   plot.special.legend=FALSE,
                                   plot.names=TRUE,
                                   x.axis.dataset=IMAGENET.ACC,
                                   x.range=NULL,
                                   y.axis.max.value=0.0,
                                   legend.position="topright",
                                   ylab=expression(paste("Error consistency (", kappa, ")", sep="")),
                                   xlab="",
                                   x.pos.text.label=NULL,
                                   plot.average.CI=TRUE,
                                   verbose=FALSE,
                                   ...) {
  # Plot consistency as a function of a value (e.g. accuracy)
  
  if(is.null(x.range)) {
    if(x.value=="top.1") {
      x.range=c(55, 80)
    } else if(x.value=="top.5") {
      x.range=c(75, 95)
    } else {
      x.range = range(x.axis.dataset[, x.value])
    }
  }
  
  if(x.value=="top.5") {
    xlab="ImageNet validation accuracy (top-5)"
    x.pos.text.label = 84
  } else if(x.value=="top.1") {
    xlab="ImageNet validation accuracy (top-1)"
    x.pos.text.label = 63
  } else {
    if(is.null(x.pos.text.label)) {
      x.pos.text.label = mean(x.range)
    }
  }
  
  # human-vs-human consistency
  human.data = data.frame()
  i = 1
  for(subj1 in unique(dat[dat$is.human==TRUE, ]$subj)) {
    j = 1
    for(subj2 in unique(dat[dat$is.human==TRUE, ]$subj)) {
      if(i>j) {
        consistency = consistency.analysis(dat, obs1=subj1, obs2 = subj2)
        p = data.frame(cohens.kappa=consistency$cohens.kappa,
                       pch=20, col=human.100)
        human.data = rbind(human.data, p)
      }
      j = j+1
    }
    i = i+1
  }
  human.mean.kappa.stats = get.mean.kappa.from.list(human.data$cohens.kappa)
  human.avg.consistency = mean(human.mean.kappa.stats$sample.mean.kappa)
  
  # CNN-vs-CNN consistency
  max.consistency = -Inf
  max.consistent.models = NULL
  cnn.data = data.frame()
  i = 1
  for(cnn1 in PLOT.SUBJECTS) {
    j = 1
    for(cnn2 in PLOT.SUBJECTS) {
      if(i>j) {
        consistency = consistency.analysis(dat, obs1=cnn1, obs2 = cnn2)
        p = data.frame(cohens.kappa=consistency$cohens.kappa,
                       pch=20, col=gold.100)
        cnn.data = rbind(cnn.data, p)
        if(consistency$cohens.kappa > max.consistency) {
          max.consistency = consistency$cohens.kappa
          max.consistent.models = paste(cnn1$data.name, cnn2$data.name)
        }
      }
      j = j+1
    }
    i = i+1
  }
  cnn.mean.kappa.stats = get.mean.kappa.from.list(cnn.data$cohens.kappa)
  cnn.avg.consistency = mean(cnn.mean.kappa.stats$sample.mean.kappa)
  
  if(verbose) {
    print(paste("Maximum observed CNN-to-CNN consistency:", as.character(round(max.consistency, 3)),
                "between the following two models:", max.consistent.models))
  }
  
  # CNN-vs-human consistency
  cnn.human.data = data.frame()
  cnn.human.CI = data.frame()
  for(cnn in PLOT.SUBJECTS) {
    for(subj1 in unique(dat[dat$is.human==TRUE, ]$subj)) {
      if(!cnn$data.name %in% unique(x.axis.dataset$model.name)) {
        stop(paste("Unknown value for model ", cnn$data.name, sep=""))
      }
      x = x.axis.dataset[x.axis.dataset$model.name==cnn$data.name, x.value]
      consistency = consistency.analysis(dat, obs1=subj1, obs2 = cnn$data.name)
      p = data.frame(x=x,
                     cohens.kappa=consistency$cohens.kappa,
                     pch=20, col=cnn$color, data.name=cnn$data.name)
      cnn.human.data = rbind(cnn.human.data, p)
    }
    mean.results.kappa = get.mean.kappa.from.list(kappa=cnn.human.data[cnn.human.data$data.name==cnn$data.name, ]$cohens.kappa)
    cnn.human.CI = rbind(cnn.human.CI, data.frame(upper=mean.results.kappa$sample.CI.upper,
                                                  lower=mean.results.kappa$sample.CI.lower,
                                                  x=x,
                                                  data.name=cnn$data.name))
  }
  
  # plot lines
  min.value = min(cnn.human.data$cohens.kappa)
  max.value = max(cnn.human.data$cohens.kappa)
  y.range = c(min(0, min.value), max(max.value, human.avg.consistency+0.05, cnn.avg.consistency+0.05,
                                     cnn.mean.kappa.stats$mean.CI.upper, human.mean.kappa.stats$mean.CI.upper,
                                     y.axis.max.value))
  print(y.range)
  
  plot(x.range, y.range, type="n",
       xlab=xlab, yaxt="n",
       ylab=ylab, bty="n", ...)
  axis(side = 2, at = seq(from=round(y.range[1], 1), to=round(y.range[2], 1), by=0.1))
  
  reg1 = lm(cohens.kappa~x, data=cnn.human.data) 
  print(summary(reg1))
  
  if(plot.average.CI) {
    # human average CI
    rect(xleft=x.range[1]-0.75, xright=x.range[2]+10,
         ybottom = human.mean.kappa.stats$sample.CI.lower,
         ytop = human.mean.kappa.stats$sample.CI.upper,
         col = human.10, border = human.10)
    
    # CNN vs. CNN average CI
    rect(xleft=x.range[1]-0.75, xright=x.range[2]+10,
         ybottom = cnn.mean.kappa.stats$sample.CI.lower,
         ytop = cnn.mean.kappa.stats$sample.CI.upper,
         col = gold.10, border = gold.10)
  }
  
  # horizontal lines and regression line for humans vs. CNNs
  abline(reg1, lty=2, lwd=1.5)
  abline(h=human.avg.consistency, lty=2, col=human.100, lwd=3)
  text(x=mean(x.range), y=human.avg.consistency, col=human.100, pos=3,
       labels=c(paste("humans vs. humans: ", as.character(round(human.avg.consistency, 2)),
                      " \u00B1 ", as.character(round(human.mean.kappa.stats$sample.CI.width.one.side, 2)), sep="")))
  abline(h=cnn.avg.consistency, lty=2, col=gold.100, lwd=3)
  text(x=mean(x.range), y=cnn.avg.consistency, col=gold.100, pos=3,
       labels=c(paste("CNN vs. CNNs: ", as.character(round(cnn.avg.consistency, 2)),
                      " \u00B1 ", as.character(round(cnn.mean.kappa.stats$sample.CI.width.one.side, 2)), sep="")))
  
  # calculate angle as described here:
  # http://blog.pmean.com/rotating-text/
  delta.x = 1
  delta.y = reg1$coefficients["x"]
  px.per.xy <- par("cra") / par("cxy")
  hyp.angle <- atan2(delta.y*px.per.xy[2], delta.x*px.per.xy[1]) * 180 / pi
  text(x=x.pos.text.label, y=as.numeric(reg1$coefficients["(Intercept)"])+x.pos.text.label*reg1$coefficients["x"],
       col="black", pos=3,
       labels=c("CNN vs. humans"), srt=hyp.angle)
  
  
  # plot individual data points
  for(i in 1:nrow(cnn.human.data)) {
    points(x=cnn.human.data[i, ]$x, y=cnn.human.data[i, ]$cohens.kappa,
           pch=cnn.human.data[i, ]$pch,
           col=paste(as.character(cnn.human.data[i, ]$col), "3D", sep=""), # add transparency
           bg=as.character(cnn.human.data[i, ]$bg), cex=1.25)
  }
  
  # Confidence Intervals
  for(cnn in PLOT.SUBJECTS) {
    arrows(x0=cnn.human.CI[cnn.human.CI$data.name==cnn$data.name, ]$x,
           y0=cnn.human.CI[cnn.human.CI$data.name==cnn$data.name, ]$lower,
           y1=cnn.human.CI[cnn.human.CI$data.name==cnn$data.name, ]$upper,
           angle=90, code=3, length=0.05, col="gray28")
  }
  
  # plot average data points
  for(cnn in PLOT.SUBJECTS) {
    data.subset = cnn.human.data[cnn.human.data$data.name==cnn$data.name, ]
    points(x=data.subset[1, ]$x, y=mean(data.subset$cohens.kappa),
           pch=21, col="black",
           bg=as.character(data.subset[1, ]$col), cex=1.5)
  }
  
  # add text label
  if(plot.names) {
    name.list = c()
    for(cnn in PLOT.SUBJECTS) {
      if(!substr(cnn$data.name, 1, 3) %in% name.list) {
        name.list = c(name.list, substr(cnn$data.name, 1, 3))
        x = mean(cnn.human.data[cnn.human.data$data.name==cnn$data.name, ]$x)
        y = max(cnn.human.data[cnn.human.data$data.name==cnn$data.name, ]$cohens.kappa)
        text(x=x, y=y+0.01, labels = c(cnn$name), pos = 4, srt=55,
             col=cnn$color, offset=0)
      }
    }
  }
  
  name.values=c()
  pch.values=c()
  col.values=c()
  for(subj in PLOT.SUBJECTS) {
    if(plot.special.legend) {
      if(!subj$data.name %in% PYTORCH.MODELS) {
        stop("plot.special.legend only defined for PYTORCH.MODELS")
      }
    }
    name.values = c(name.values, paste(subj$name, "vs. humans"))
    pch.values = c(pch.values, subj$pch)
    col.values = c(col.values, subj$color)
  }
  if(plot.standard.legend) {
    legend(legend.position, legend=name.values,
           col = col.values, pch=pch.values,
           pt.bg = col.values, cex=0.75, bty="n")
  }
  
  if(plot.special.legend) {
    x = x.range-0.5
    y = y.range-0.10
    legend.cex=0.7
    # first three model families
    indices.1 = c(NA_integer_,6,NA_integer_)
    legend(x=x, y=y,
           col=col.values[indices.1], pch=pch.values[indices.1], pt.bg=col.values[indices.1],
           legend=c("", "", ""), bty="n", cex=legend.cex, pt.cex=1.3)
    indices.2 = c(1, 7, 11)
    legend(x=x+0.5, y=y,
           col=col.values[indices.2], pch=pch.values[indices.2], pt.bg=col.values[indices.2],
           legend=c("alexnet vs. humans", "sqeezenet(1-0|1-1) vs. humans", "inception-v3 vs. humans"), bty="n",
           cex=legend.cex, pt.cex=1.3, text.col=col.values[c(1,7,11)])
    # last three model families
    indices.1 = c(NA_integer_,NA_integer_,12)
    legend(x=x+9+0.5*0.0, y=y,
           col=col.values[indices.1], pch=pch.values[indices.1], pt.bg=col.values[indices.1],
           legend=c("", "", ""), bty="n", cex=legend.cex, pt.cex=1.3)
    indices.2 = c(NA_integer_,2,13)
    legend(x=x+9+0.5*1.0, y=y,
           col=col.values[indices.2], pch=pch.values[indices.2], pt.bg=col.values[indices.2],
           legend=c("", "", ""), bty="n", cex=legend.cex, pt.cex=1.3)
    indices.3 = c(8,3,14)
    legend(x=x+9+0.5*2.0, y=y,
           col=col.values[indices.3], pch=pch.values[indices.3], pt.bg=col.values[indices.3],
           legend=c("", "", ""), bty="n", cex=legend.cex, pt.cex=1.3)
    indices.4 = c(9,4,15)
    legend(x=x+9+0.5*3.0, y=y,
           col=col.values[indices.4], pch=pch.values[indices.4], pt.bg=col.values[indices.4],
           legend=c("", "", ""), bty="n", cex=legend.cex, pt.cex=1.3)
    indices.5 = c(10,5,16)
    legend(x=x+9+0.5*4.0, y=y,
           col=col.values[indices.5], pch=pch.values[indices.5], pt.bg=col.values[indices.5],
           legend=c("densenet-(121|169|201) vs. humans", "vgg-(11|13|16|19)-bn vs. humans", 
                    "resnet-(18|34|50|101|152) vs. humans"),
           bty="n", cex=legend.cex, pt.cex=1.3, text.col = col.values[c(10,5,16)])
    
  }
}



plot.consistency = function(dat, method=c("consistency", "kappa"),
                            plot.legend=TRUE, verbose=FALSE,
                            legend.cex=0.62, points.cex=1.0,
                            distinguish.model.families=TRUE,
                            plot.bound=FALSE, plot.CI=TRUE) {
  # Plot observed consistency as a function of expected consistency
  
  if(method=="consistency") {
    ylab=expression("Observed error overlap (c"[obs]*")")
    plot.diagonal = TRUE
    legend.position="bottomright"
    y.value.name = "observed.consistency"
    x.value.name = "expected.consistency"
  } else if(method=="kappa") {
    ylab=expression(paste("Error consistency (", kappa, ")", sep=""))
    plot.diagonal = FALSE
    legend.position="topleft"
    y.value.name = "cohens.kappa"
    x.value.name = "expected.consistency"
  } else {
    stop("method not implemented")
  }
  
  human.data = data.frame()
  # human-vs-human consistency
  i = 1
  for(subj1 in unique(dat[dat$is.human==TRUE, ]$subj)) {
    j = 1
    for(subj2 in unique(dat[dat$is.human==TRUE, ]$subj)) {
      if(i>j) {
        consistency = consistency.analysis(dat, obs1=subj1, obs2 = subj2)
        p = data.frame(x=consistency[x.value.name][1,1],
                       y=consistency[y.value.name][1,1],
                       pch=HUMAN.PCH, col=human.100, bg=human.100,
                       cex=points.cex)
        human.data = rbind(human.data, p)
      }
      j = j+1
    }
    i = i+1
  }
  
  # CNN-vs-human consistency
  cnn.human.data = data.frame()
  for(subj1 in unique(dat[dat$is.human==TRUE, ]$subj)) {
    cnn.subjects = get.all.subjects(dat, avg.human.data = TRUE)
    for(cnn in PLOT.SUBJECTS) {
      consistency = consistency.analysis(dat, obs1=subj1, obs2 = cnn$data.name)
      p = data.frame(x=consistency[x.value.name][1,1],
                     y=consistency[y.value.name][1,1],
                     pch=cnn$pch, col=cnn$color, bg=cnn$color,
                     cex=points.cex)
      cnn.human.data = rbind(cnn.human.data, p)
    }
  }
  
  # CNN-vs-CNN consistency
  cnn.data = data.frame()
  max.consistency = -Inf
  max.consistent.models = NULL
  if(length(PLOT.SUBJECTS) > 3) {
    cnn.to.cnn.cex = points.cex-0.4 # smaller symbols for more models
  } else {
    cnn.to.cnn.cex = points.cex
  }
  i = 1
  for(subj1 in PLOT.SUBJECTS) {
    j = 1
    for(subj2 in PLOT.SUBJECTS) {
      if(i>j) {
        if(grepl("resnet", subj1$data.name) & grepl("resnet", subj2$data.name) |
           grepl("densenet", subj1$data.name) & grepl("densenet", subj2$data.name) |
           grepl("squeezenet", subj1$data.name) & grepl("squeezenet", subj2$data.name) |
           grepl("vgg", subj1$data.name) & grepl("vgg", subj2$data.name)) {
          # same model family
          pch = 3
          col = "black"
        } else {
          pch = 4
          col = gold.100
        }
        consistency = consistency.analysis(dat, obs1=subj1$data.name, obs2 = subj2$data.name)
        p = data.frame(x=consistency[x.value.name][1,1],
                       y=consistency[y.value.name][1,1],
                       pch=pch, col=col, bg=gold.100,
                       cex=cnn.to.cnn.cex)
        cnn.data = rbind(cnn.data, p)
        if(consistency[y.value.name][1,1] > max.consistency) {
          max.consistency = consistency[y.value.name][1,1]
          max.consistent.models = paste(subj1$data.name, subj2$data.name)
        }
      }
      j = j+1
    }
    i = i+1
  }
  
  min.value = min(min(human.data$y), min(cnn.data$y), min(cnn.human.data$y))
  max.value = max(max(human.data$y), max(cnn.data$y), max(cnn.human.data$y))
  y.range = c(min(0, min.value), max(1, max.value))
  
  plot(c(0, 1), y.range, type="n",
       xlab=expression("Error overlap expected by chance (c"[exp]*")"),
       ylab=ylab, bty = "n")
  
  if(plot.CI) {
    # plot confidence intervals
    num.trials = nrow(dat[dat$subj==PLOT.SUBJECTS[[1]]$data.name, ])
    x.values = c()
    lower.CI = c()
    upper.CI = c()
    
    ci.dat = get.confidence.interval.from.file(num.trials = num.trials)
    NUM.CI.POINTS = nrow(ci.dat)
    for(i in 1:NUM.CI.POINTS) {
      if(method=="consistency") {
        x.values = c(x.values, ci.dat[i, ]$c.exp)
        lower.CI = c(lower.CI, ci.dat[i, ]$c.obs.CI.lower)
        upper.CI = c(upper.CI, ci.dat[i, ]$c.obs.CI.upper)
      } else if(method=="kappa") {
        x.values = c(x.values, ci.dat[i, ]$c.exp)
        lower.CI = c(lower.CI, ci.dat[i, ]$kappa.CI.lower)
        upper.CI = c(upper.CI, ci.dat[i, ]$kappa.CI.upper)
      }
    }
    
    polygon(x = c(x.values, rev(x.values)),
            y = c(lower.CI, rev(upper.CI)),
            col = "grey92", border = "grey80")
  }
  if(plot.diagonal) {
    segments(0, 0, 1, 1, lty=2) # dashed diagonal
  }
  
  if(plot.bound) {
    length.out = 500000
    x = seq(from=0, to=1, length.out=length.out)
    y1 = seq(from=0, to=0, length.out=length.out)
    y2 = seq(from=1, to=1, length.out=length.out)
    
    bound.col = "gray60"
    points(x[x>0.5], get.consistency.lower.bound(x[x>0.5]), type = "l", col=bound.col)
    points(x[x<0.5], get.consistency.upper.bound(x[x<0.5]), type = "l", col=bound.col)
    points(x[x<0.5], y1[x<0.5], type="l", col=bound.col)
    points(x[x>0.5], y2[x>0.5], type="l", col=bound.col)
  }
  
  combined.data = rbind(human.data, cnn.data, cnn.human.data)
  combined.data$col = as.character(combined.data$col)
  combined.data$bg = as.character(combined.data$bg)
  for(i in 1:nrow(combined.data)) {
    points(x=combined.data[i, ]$x, y=combined.data[i, ]$y,
           pch=combined.data[i, ]$pch, col=combined.data[i, ]$col, bg=combined.data[i, ]$bg,
           cex=combined.data[i, ]$cex)
  }
  
  if(verbose) {
    print(paste("avg human-human consistency:", as.character(round(mean(human.data$y), 3))))
    print(paste("avg CNN-CNN consistency:", as.character(round(mean(cnn.data$y), 3))))
    print(paste("avg CNN-human consistency:", as.character(round(mean(cnn.human.data$y), 3))))
    print(paste("Maximum observed CNN-to-CNN consistency:", as.character(round(max.consistency, 3)),
                "between the following two models:", max.consistent.models))
  }
  
  
  if(plot.legend) {
    name.values = c("humans vs. humans")
    pch.values = c(HUMAN.PCH)
    col.values = c(human.100)
    
    if(length(PLOT.SUBJECTS)==2) {
      name.values = c(name.values, paste(PLOT.SUBJECTS[[1]]$name,
                                         PLOT.SUBJECTS[[2]]$name,
                                         sep=" vs. "))
      pch.values = c(pch.values, 4)
      col.values = c(col.values, gold.100)
      
    } else {
      if(distinguish.model.families) {
        name.values = c(name.values, "CNNs vs. CNNs (same model family)", "CNNs vs. CNNs (different model family)")
        pch.values = c(pch.values, 3, 4)
        col.values = c(col.values, "black", gold.100)
      } else {
        name.values = c(name.values, "CNNs vs. CNNs")
        pch.values = c(pch.values, 4)
        col.values = c(col.values, gold.100)
      }
    }
    for(subj in PLOT.SUBJECTS) {
      name.values = c(name.values, paste(subj$name, "vs. humans"))
      pch.values = c(pch.values, subj$pch)
      col.values = c(col.values, subj$color)
    }
    
    legend(legend.position, legend=name.values,
           col = col.values, pch=pch.values,
           pt.bg = col.values, cex=legend.cex,
           bty="n")
  }
}


get.distinct.colors = function() {
  # Return list of distinct colors
  # code source:
  # https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
  qual.col.pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col.vector = unlist(mapply(brewer.pal, qual.col.pals$maxcolors, rownames(qual.col.pals)))
  return(col.vector)  
}


get.model.family.colors = function(subj) {
  # Return a year-coded colour for networks
  if(subj=="resnet18") {
    return("#9ecae1")
  } else if(subj=="resnet34") {
    return("#6baed6")
  } else if(subj=="resnet50") {
    return("#4292c6")
  } else if(subj=="resnet101") {
    return("#2171b5")
  } else if(subj=="resnet152") {
    return("#08519c")
  } else if(subj=="vgg11-bn") {
    return("#74c476")
  } else if(subj=="vgg13-bn") {
    return("#41ab5d")
  } else if(subj=="vgg16-bn") {
    return("#238b45")
  } else if(subj=="vgg19-bn") {
    return("#006d2c")
  } else if(subj=="densenet121") {
    return("#fdd0a2")
  } else if(subj=="densenet169") {
    return("#fdae6b")
  } else if(subj=="densenet201") {
    return("#fd8d3c")
  } else if(subj=="squeezenet1-0") {
    return("#dd3497")
  } else if(subj=="squeezenet1-1") {
    return("#7a0177")
  } else if(subj=="inception-v3") {
    return("#B4A096")
  } else if(subj=="alexnet") {
    return("#916946")
  } else {
    stop("subj not found")
  }
}


subject.factory = function(dat, models,
                           distinguish.model.families=FALSE) {
  # Return list of CNN subjects for dat with random colors
  subj.list = list()
  counter = 1
  
  col.vector = get.distinct.colors()
  
  for(m in models) {
    if(!m %in% unique(dat$subj)) {
      stop(paste("model not found in data: ", m))
    }
    if(distinguish.model.families) {
      col = get.model.family.colors(m)
    } else {
      col = col.vector[counter]
    }
    subj.list[[counter]] = list(name=m, color=col, pch=20, data.name=m)
    counter = counter + 1
  }
  
  return(subj.list)
}


###################################################################
#    GENERAL PLOTTING FUNCTIONALITY (INDEPENDENT OF ERROR ANALYSIS)
###################################################################

plot.performance = function(dat, logarithmic.scale=FALSE, 
                            line.for.chance=TRUE,
                            h.of.line.for.chance = 1/16.0,
                            avg.human.data=TRUE,
                            normalize=FALSE,
                            legend.position=NULL,
                            plot.legend=TRUE,
                            top.n=0,
                            entropy=FALSE,
                            plot.dnn.range=!normalize & (top.n==0) & !isTRUE(entropy),
                            plot.observer.range=avg.human.data & !normalize & (top.n==0) & !isTRUE(entropy),
                            main=NULL,
                            ylab=NULL,
                            log.base=10,
                            inverse.x.axis=unique(dat$experiment.name) %in% c("contrast-experiment", "contrast-png-experiment", "highpass-experiment"),
                            x.range=range(as.numeric(dat$condition)),
                            ticks=NULL,
                            cex.lab=2.0,
                            cex.axis=1.5,
                            cex.legend=1.3,
                            ...) {
  #Plot performance for an experiment, split by subject
  
  # ASSERT INPUT IS CORRECT, ASSIGN DEFAULT VALUES
  if(plot.observer.range & ! avg.human.data) {
    stop("plot.observer.range and avg.human.data cannot be both TRUE -> overcrowded plot.")
  }
  if(isTRUE(entropy) & (top.n!=0)) {
    stop("entropy == TRUE and top.n != 0 cannot both be the case -> use either of them!")
  }
  
  if(logarithmic.scale) {
    x.range = log(x.range, base = log.base)
  }
  if(isTRUE(entropy)) {
    y.range = range(0, log(1.0/h.of.line.for.chance, base=2))
  } else {
    y.range = range(0, 1)
  }
  x.lim = x.range
  if(inverse.x.axis) {
    x.lim = rev(x.range)
  }
  
  if(is.null(main)) {
    expt.name = levels(dat$experiment.name)[1]
    main = ifelse(normalize, paste("Normalized accuracy for", expt.name),
                  paste("Accuracy for", expt.name))
  }
  if(is.null(ylab)) {
    if(top.n == 0 & !isTRUE(entropy)) {
      ylab = ifelse(normalize, "Normalized classification accuracy",
                    "Classification accuracy")
    } else if(isTRUE(entropy)) {
      ylab = "Entropy [bits]"
    } else {
      ylab = ifelse(normalize, paste("Normalized fraction of top-", top.n, " decisions", sep=""),
                    paste("Fraction of top-", top.n, " decisions", sep=""))
    }
    
  }
  
  # PLOTTING
  plot(x.range, y.range, type="n", bty="n", main=main, ylab=ylab, xlim=x.lim, 
       xaxt="n", cex.lab=cex.lab, cex.axis=cex.axis, ...)
  axis(side=1, at=ticks, cex.axis=cex.axis)
  
  subjects = get.all.subjects(dat, avg.human.data)

  for(s in subjects) {
    
    if((!(s$data.name == HUMAN.DATA.NAME)) | (!avg.human.data)) {
      acc = get.accuracy(dat[dat$subj==s$data.name, ], top.n, entropy)
    } else {
      if(top.n == 0) {
        acc = get.accuracy(dat[dat$is.human==TRUE, ], top.n, entropy)
      } else {
        # Plot average of single observers' fraction of responses.
        # This is different from the combined human observers' average!
        # If every observer chose 3 categories, but different ones, 
        # this wouldn't show in the combined data, only in the overall
        # average.
        acc = get.accuracy(dat[dat$is.human==TRUE, ], top.n, entropy)
        
        acc$y = rep(0.0, times=length(unique(dat$condition)))
        
        for(z in unique(dat[dat$is.human==TRUE, ]$subj)) {
          acc.for.subj = get.accuracy(dat[dat$subj==z, ], top.n, entropy)
          acc$y = acc$y + acc.for.subj$y
        }
        acc$y = acc$y / length(unique(dat[dat$is.human==TRUE, ]$subj))
      }
    }
    
    x = acc$x
    if(logarithmic.scale) {
      x = log(acc$x, base=log.base)
    }
    y = acc$y / 100
    if(normalize) {
      y = get.normalized.value(y)
    }
    
    # PLOT MAIN DATA
    lines(x, y, type="b", lwd=LINES.LWD,
          lty=1, col=s$color, pch=s$pch, cex=POINTS.CEX.VAL, bg=s$color)
    
    # PLOT RANGE
    if(((s$data.name %in% PLOT.SUBJECTS) & plot.dnn.range) | 
       (plot.observer.range & !(s$data.name %in% PLOT.SUBJECTS))) {
      # plot range of results - for DNNs the range of all sessions,
      # for human observers the range of all observers.
      
      if(s$data.name %in% PLOT.SUBJECTS) {
        acc.all.sessions = get.acc.for.range(dat[dat$subj==s$data.name, ])
      } else {
        acc.all.sessions = get.acc.for.range(dat[dat$is.human==TRUE, ], is.human = TRUE)
      }
      
      counter = 1
      for(c in sort(as.numeric(unique(dat$condition)))) {
        range = get.min.and.max(acc.all.sessions, counter)
        
        if(logarithmic.scale) {
          c = log(c, base=log.base)
        }
        if(normalize) {
          range$min = get.normalized.value(range$min, min(acc$y/100), max(acc$y/100))
          range$max = get.normalized.value(range$max, min(acc$y/100), max(acc$y/100))
        }
        
        if(range$min != range$max) {
          arrows(c, range$min, c, range$max,
                 length=0.05, angle=90, code=3, col=s$color, lwd = DNN.RANGE.LWD)
        }
        
        counter = counter+1
      }
    }
  }
  
  if(line.for.chance) {
    if(entropy) {
      abline(h=log(1.0/h.of.line.for.chance, base=2), lty=5) 
    } else {
      abline(h=h.of.line.for.chance, lty=5)
    }
  }
  if(plot.legend) {
    add.legend(subjects, legend.position,
               cex=cex.legend)
  }
}

plot.shape.bias = function(dat, x.is.content=TRUE,
                           plot.average.line=TRUE,
                           plot.frac.other.decisions=TRUE,
                           categories=NULL,
                           plot.legend=FALSE,
                           legend.position=0.66,
                           avg.human.data=TRUE,
                           custom.subjects=NULL,
                           ...) {
  # plot results for cue conflict stimuli (generated by style transfer)
  
  orig.xpd = par()$xpd
  # hard-coded values for category images
  xright = 0.02
  img.height = 0.85
  img.width = 0.06
  unwanted.y.offset = 0.5
  barplot.max.width = 0.1
  barplot.dist.from.plot = 0.05
  xlab="Fraction of 'shape' decisions"
  
  if(is.null(categories)) {
    categories = get.category.order(dat, x.is.content = x.is.content)
  }
  
  if(x.is.content) {
    obj = list(ylab = "Shape categories", mtext.side=2,
               categories.xleft=0.0-xright-img.width,
               categories.xright=0.0-xright,
               x.values.plotted.from=1.0,
               inversion.sign=-1.0,
               table.col.name="frac.content.decisions")
  } else {
    obj = list(ylab = "Texture categories", mtext.side=4,
               categories.xleft=1+xright,
               categories.xright=1+xright+img.width,
               x.values.plotted.from=0.0,
               inversion.sign=1.0,
               table.col.name="frac.texture.decisions")
  }
  
  # create empty plot
  left.xlim = 0.0 - obj$x.values.plotted.from*(xright+img.width) - (1-obj$x.values.plotted.from)*(xright+barplot.dist.from.plot+barplot.max.width)
  right.xlim = 1.0 + (1-obj$x.values.plotted.from)*(xright+img.width) + (obj$x.values.plotted.from)*(xright+barplot.dist.from.plot+barplot.max.width)
  
  plot.xlim = c(left.xlim, right.xlim)
  plot(plot.xlim, xlim=plot.xlim,
       ylim = c(unwanted.y.offset, length(categories)-unwanted.y.offset-(1-0.85)),
       type="n", xaxt="n", yaxt="n", bty="n", ylab="", xlab="")
  
  # add axes
  x.axis.seq = seq(from=0.0, to=1.0, by=0.1)
  axis(side=1, at=x.axis.seq, tick = T,
       labels=rev(rev(x.axis.seq)))
  axis(side=3, at=x.axis.seq, tick = T,
       labels=rev(x.axis.seq))
  lines(c(0.0, 0.0), c(0.0, length(categories)), lty=1)
  lines(c(1.0, 1.0), c(0.0, length(categories)), lty=1)
  
  # axes label text
  par(xpd=NA)
  mtext("Fraction of 'texture' decisions", side=1, line=3)
  arrow.offset = 1.75
  arrows(x0=1, y0=length(categories)+arrow.offset, x1=0, y1=length(categories)+arrow.offset, length = 0.15, angle = 30)
  mtext("Fraction of 'shape' decisions", side=3, line=3)
  arrows(x0=0, y0=-arrow.offset, x1=1, y1=-arrow.offset, length = 0.15, angle = 30)
  mtext(obj$ylab, side=obj$mtext.side, line=0)
  par(xpd=orig.xpd)
  
  # dotted horizontal lines to separate categories
  for(i in 1:(length(categories)-1)) {
    lines(x=c(0, 1),
          y=rep((i - (1-img.height)/2.0), times=2), lty=3,
          col=alpha("black", 0.2))
  }
  
  counter = 0
  for(category in categories) {
    rasterImage(image.categories[[category]],
                xleft = obj$categories.xleft,
                ybottom = counter,
                xright = obj$categories.xright, 
                ytop = counter+img.height)
    counter = counter + 1
  }
  
  # get data in correct format
  table = get.fractions.of.responses(dat, x.is.content=x.is.content,
                                     avg.human.data = avg.human.data,
                                     custom.subjects = custom.subjects)
  table$x.pos = obj$x.values.plotted.from + obj$inversion.sign * 
    (table[[obj$table.col.name]] / (1.0 - table$frac.other.decisions))
  
  # plot average lines
  if(is.null(custom.subjects)) {
    subjects = get.all.subjects(dat, avg.human.data = avg.human.data)
  } else {
    subjects = custom.subjects
  }
  if(plot.average.line) {
    for(subj in subjects) {
      subj.data = table[table$subj==subj$data.name, ]
      mean.value = obj$x.values.plotted.from + obj$inversion.sign *
        (mean(subj.data[[obj$table.col.name]] /
                (mean(subj.data$frac.content.decisions + subj.data$frac.texture.decisions))))
      lines(c(mean.value, mean.value), c(0.0, length(categories)), lty=1,
            col=alpha(subj$color, 0.3), lwd=2.0)
      
    }
  }
  
  # plot data points
  subj.counter = 0
  for(subj in rev(subjects)) {
    counter = 0
    for(category in categories) {
      x.pos = table[table$subj==subj$data.name & table$category==category, ]$x.pos
      pch = subj$pch
      if(subj$name == HUMAN.DATA.NAME) {
        pch = subj$pch # set to 21 for filled circles
      }
      points(x.pos, counter+img.height/2.0, pch=pch, col=subj$color, 
             bg=subj$color, cex=POINTS.CEX.VAL)
      if(plot.frac.other.decisions) { # add horizontal barplots
        # if the '1.0' in the following line is deleted, wrong decisions are plotted - if it is added, correct decisions are plotted.
        bar.width = barplot.max.width * (1.0 - table[table$subj==subj$data.name & table$category==category, ]$frac.other.decisions)
        bar.block.distance = 0.1 # distance between groups of bars for different categories
        bar.height = (1.0 - 2*bar.block.distance) / length(subjects)
        
        bar.xleft = obj$x.values.plotted.from - obj$inversion.sign*barplot.dist.from.plot
        bar.xright = bar.xleft - obj$inversion.sign*bar.width
        subj.offset = subj.counter * bar.height
        ybottom = counter + subj.offset
        # grey
        rect(xleft=bar.xleft, xright=bar.xleft - obj$inversion.sign*barplot.max.width,
             ybottom=ybottom,
             ytop=ybottom + bar.height,
             col = alpha("black", 0.0), border = "grey")
        # colour
        rect(xleft=bar.xleft, xright=bar.xright,
             ybottom=ybottom,
             ytop=ybottom + bar.height,
             col = subj$col, border = T)
      }
      counter = counter + 1
    }
    subj.counter = subj.counter + 1
  }
  
  if(plot.legend) {
    legend.y = length(categories)-0.5
    add.legend(subjects, legend.position = legend.position,
               cex=0.9, pt.cex=2.0, y=legend.y)
  }
}


###################################################################
#               Helper functions
###################################################################

get.fractions.of.responses = function(dat, x.is.content=TRUE,
                                      avg.human.data=TRUE,
                                      custom.subjects=NULL) {
  # Return data frame that splits responses into content, style etc.
  # Utility function for style transfer results plotting.
  # The following convention is followed:
  # Trials in which shape category = texture category are excluded.
  # This enables one to have the following partition of the 
  # remaining data:
  # - texture decisions
  # - shape decisions
  # - 'other' decisions (!= shape and != texture)
  # ...those three cases then sum to 1.0, which is necessary.
  
  res = data.frame(subj=character(),
                   category=character(),
                   frac.content.decisions=numeric(),
                   frac.texture.decisions=numeric(),
                   frac.other.decisions=numeric())
  if(is.null(custom.subjects)) {
    subjects = get.all.subjects(dat, avg.human.data = avg.human.data)
  } else {
    subjects = custom.subjects
  }
  for(s in subjects) {
    subj = s$data.name
    
    if(nrow(dat[dat$subj==subj, ]) <1 & subj != HUMAN.DATA.NAME) {
      stop(paste("no data for subject", subj, "found! Perhaps check data.name"))
    }
    
    for(category in CATEGORIES) {
      if(x.is.content) {
        if(avg.human.data & subj==HUMAN.DATA.NAME) {
          a = dat[dat$is.human==TRUE & dat$category==category, ]
        } else {
          a = dat[dat$subj==subj & dat$category==category, ]
        }
      } else {
        if(avg.human.data & subj==HUMAN.DATA.NAME) {
          a = dat[dat$is.human==TRUE & dat$texture==category, ]
        } else {
          a = dat[dat$subj==subj & dat$texture==category, ]
        }
      }
      
      # exclude texture = shape case
      b = a[a$texture != a$category, ]
      num.images = nrow(b)
      
      # decision = category (= content, = shape)
      frac.content.decisions = nrow(b[as.character(b$object_response)==as.character(b$category), ]) / num.images
      
      # decision = texture
      frac.texture.decisions = nrow(b[as.character(b$object_response)==as.character(b$texture), ]) / num.images
      
      # decision = neither texture nor category/content/shape
      frac.other.decisions = nrow(b[as.character(b$object_response)!=as.character(b$texture) & as.character(b$object_response)!=as.character(b$category), ]) / num.images
      
      if(! all.equal(frac.other.decisions + frac.texture.decisions + frac.content.decisions, 1.0)) {
        print("the problematic values are:")
        print(frac.texture.decisions)
        print(frac.content.decisions)
        print(frac.other.decisions)
        warning("values do not sum to 1.0 as they should!")
      }
      
      res = rbind(res, data.frame(subj=subj, category=category,
                                  frac.content.decisions=frac.content.decisions,
                                  frac.texture.decisions=frac.texture.decisions,
                                  frac.other.decisions=frac.other.decisions))
    }
  }
  return(res)
}


get.accuracies.for.experiments.helper = function(dat.list) {
  # return list of accuracies for different observers and experiments
  # note: all data need to have the same subjects!
  
  accuracies.here = list()
  counter = 1
  for(s in get.all.subjects(dat.list[[1]], avg.human.data = TRUE)) {
    new.subject = list(acc=numeric(length(dat.list)), col=s$color,
                       subj=character(length(dat.list)))
    for(i in 1:length(dat.list)) {
      acc.fun = get.single.accuracy
      if(s$data.name %in% PLOT.SUBJECTS) {
        new.subject$acc[i] = acc.fun(dat.here[[i]][dat.here[[i]]$subj==s$data.name, ])
        new.subject$subj[i] = s$name
      } else {
        new.subject$acc[i] = acc.fun(dat.here[[i]][dat.here[[i]]$is.human==TRUE, ])
        new.subject$subj[i] = "Humans"
      }
      
    }
    accuracies.here[[counter]] = new.subject
    counter = counter + 1
  }
  return(accuracies.here)
}



get.confidence.interval.from.file = function(num.trials, confidence.interval.dir=CONFIDENCE.INTERVAL.DIR) {
  # read previously computed confidence interval
  
  filepath = paste(confidence.interval.dir, num.trials, "_trials.csv", sep="")
  if(!file.exists(filepath)) {
    stop(paste("file does not exist:", filepath))
  }
  ci.dat = read.csv(filepath)
  if(ncol(ci.dat)!=6) {
    stop(paste(ncol(ci.dat), "number of columns found instead of 6"))
  }
  colnames(ci.dat) = c("index", "c.exp", "c.obs.CI.lower", "c.obs.CI.upper", "kappa.CI.lower", "kappa.CI.upper")
  
  # rows with 'NA' indicate that confidence interval is e.g. diverging
  ci.dat = na.omit(ci.dat)
  return(ci.dat)
}


get.eidolon.dat.preprocessed = function(dat, separating.condition) {
  # Eidolon data is a special case because condition is 3-dimensional
  # (compared to other 1-dimensional experiments). Therefore this function
  # can be used to extract the whole data for the middle condition.
  # Parameter separating.condition is one of 0, ..., 10 .
  
  dat.new = dat[grepl(paste("-", as.character(separating.condition), "-", sep=""), dat$condition), ]
  dat.new$condition = as.character(dat.new$condition)
  dat.new$condition = lapply(dat.new$condition, function(y){strsplit(y, "-")[[1]][1]})
  dat.new$condition = as.numeric(dat.new$condition)
  return(dat.new)
}


get.normalized.value = function(old.val, old.min=min(old.val), old.max=max(old.val)) {
  #Return vector of elements squeezed between old.min and 1.
  
  if(any(old.val < old.min)) {
    #stop("old.val needs to be >= old.min")
  }
  new.val = ((old.val-old.min)/(old.max-old.min))*(1-old.min)+old.min
  return(new.val)
}


get.min.and.max = function(acc.all.sessions, index) {
  # Return min and max y-value of acc.all.sessions
  
  min = Inf
  max = -Inf
  
  for(a in acc.all.sessions) {
    if(index > length(a$y)) {
      stop("index needs to be <= length")
    }
    
    if(a$y[index] < min)
      min = a$y[index]
    if(a$y[index] > max)
      max = a$y[index]
  }
  
  return(list(min=min, max=max))
}

get.accuracy = function(dat, top.n=0, entropy=FALSE) {
  # Return data.frame with x and y for condition and accuracy.
  # If top.n != 0, return fraction of top-n responses instead
  # - that is, the fraction of responses that fall onto the
  # top n most frequent response categories.
  
  tab = table(dat$is.correct, by=dat$condition)
  false.index = 1
  true.index = 2
  acc = tab[true.index, ] / (tab[false.index, ]+tab[true.index, ])
  d = as.data.frame(acc)
  
  if(length(colnames(tab)) != length(unique(dat$condition))) {
    stop("Error in get.accuracy: length mismatch.")
  }
  
  if((top.n != 0) | isTRUE(entropy)) {
    counter = 1
    for(c in colnames(tab)) {
      if(! c %in% unique(dat$condition)) {
        warning("Warning for get.accuracy: condition mismatch.")
      }
      if(isTRUE(entropy)) {
        resp = get.entropy(dat[dat$condition==c, ])
      } else {
        resp = get.top.n.frac.of.responses(dat[dat$condition==c, ], top.n)
      }
      d[counter, ] = resp
      counter = counter + 1
    }
  }
  
  #enforce numeric ordering instead of alphabetic (otherwise problem: 100 before 20)
  if(!is.factor(dat$condition)) {
    #condition is numeric
    
    d$order = row.names(d)
    d$order = as.numeric(d$order)
    d = d[with(d, order(d$order)), ]
    d$order = NULL
    e = data.frame(x = as.numeric(row.names(d)), y=100*d[ , ])
  } else {
    #condition is non-numeric
    e = data.frame(x = row.names(d), y=100*d[ , ])
  }
  return(e)
}

get.category.order = function(dat, x.is.content) {
  table = get.fractions.of.responses(dat[dat$is.human==TRUE, ], x.is.content=x.is.content, avg.human.data = TRUE)
  table$frac.shape.decisions.amongst.correct.decisions = table$frac.content.decisions / (table$frac.content.decisions+table$frac.texture.decisions)
  c = table[order(table$frac.shape.decisions.amongst.correct.decisions), ]$category
  return(rev(as.character(c)))
}


get.texture = function(imagename) {
  # Helper function: return 'style' category as character from imagename.
  # This is hard-coded to the imagename formatting.
  
  # a = truck3-bird2.png
  a = tail(strsplit(imagename, "_")[[1]], 1)
  # b = bird2.png
  b = tail(strsplit(a, "-")[[1]], 1)
  # c = bird2
  c = strsplit(b, ".png")[[1]]
  # category = bird
  category = gsub('[[:digit:]]+', '', c)
  return(category)
}


get.brainscore.data = function(csv.path) {
  # Given csv.path, load Brain-Score metrics and model results
  
  BrainScore = read.csv(csv.path)
  
  names(BrainScore) = c("model.name", "avg", "V1", "V2", "V4", "IT", "ITtemp", "behaviour", "ImageNet")
  BrainScore$model.name = as.character(BrainScore$model.name)
  BrainScore[BrainScore$model.name=="vgg-16", ]$model.name = "vgg16-bn"
  BrainScore[BrainScore$model.name=="vgg-19", ]$model.name = "vgg19-bn"
  BrainScore[BrainScore$model.name=="squeezenet1_0", ]$model.name = "squeezenet1-0"
  BrainScore[BrainScore$model.name=="squeezenet1_1", ]$model.name = "squeezenet1-1"
  BrainScore[BrainScore$model.name=="densenet-121", ]$model.name = "densenet121"
  BrainScore[BrainScore$model.name=="densenet-169", ]$model.name = "densenet169"
  BrainScore[BrainScore$model.name=="densenet-201", ]$model.name = "densenet201"
  BrainScore[BrainScore$model.name=="inception_v3", ]$model.name = "inception-v3"
  BrainScore[BrainScore$model.name=="resnet-18", ]$model.name = "resnet18"
  BrainScore[BrainScore$model.name=="resnet-34", ]$model.name = "resnet34"
  BrainScore[BrainScore$model.name=="resnet-50_v1", ]$model.name = "resnet50"
  BrainScore[BrainScore$model.name=="resnet-101_v1", ]$model.name = "resnet101"
  BrainScore[BrainScore$model.name=="resnet-152_v1", ]$model.name = "resnet152"
  
  # differences to Brain-Score:
  # see https://github.com/brain-score/candidate_models/blob/745f9d1bc747e936cbdef1e7fc599b16cf9dc677/candidate_models/base_models/__init__.py#L350
  # vgg uses Keras and batch norm
  # squeezenet: same (PyTorch)
  # Densenet: Keras instead
  # inception_v3: TFSlimModel instead
  # resnet-18, 14: same (PyTorch)
  # resnet-50_v1, -101_v1, -152_v1: TFSlimModel
  
  for(m in BRAINSCORE.MODELS) { # sanity check
    if(nrow(BrainScore[BrainScore$model.name==m, ])!=1) {
      stop(paste("model name mismatch for model", m))
    }
  }
  
  return(BrainScore)
}


get.expt.data = function(expt.name,
                         is.style.transfer=FALSE) {
  # Read data and return in the correct format
  
  if(!exists("DATAPATH")) {
    stop("you need to define the DATAPATH variable")
  }
  
  dat = NULL
  expt.path = paste(DATAPATH, expt.name, sep="")
  files = list.files(expt.path)
  
  if(length(files) < 1) {
    warning(paste("No data for expt", expt.name, "found! Check DATAPATH."))
  }
  
  for (i in 1:length(files)) {
    if(!endsWith(files[i], ".csv")) {
      warning("File without .csv ending found (and ignored)!")
    } else {
      dat = rbind(dat, read.csv(paste(expt.path, files[i], sep="/")))
    }
  }
  
  dat$imagename = as.character(dat$imagename)
  dat$is.correct = as.character(dat$object_response) == as.character(dat$category)
  dat$is.human = ifelse(grepl("subject", dat$subj), TRUE, FALSE)
  dat$image.id = sapply(dat$imagename, get.short.imagename)
  if(is.style.transfer) { # make extra column with texture = style category
    dat$texture = lapply(dat$imagename, get.texture)
    dat$texture = as.factor(as.character(dat$texture))
  }
  
  if(grepl("contrast", expt.name)) {
    dat$condition = as.character(dat$condition)
    dat$condition = lapply(dat$condition, function(y){substring(y, 2)})
    dat$condition = as.character(as.numeric(dat$condition)) # '05' -> '5' etc
  }
  
  return(data.frame(experiment.name = expt.name, dat))
}


drop.resp.levels = function(dat) {
  # Return data without "na" as a response
  
  dat = dat[dat$object_response!="na", ]
  dat$object_response = droplevels(dat$object_response)
  return(dat)
}

get.acc.for.range= function(dat, is.human=FALSE) {
  # Return list of accuracies for range, to be used for computing range.
  # if is.human: for each human observer, if not:
  # for each session of a netowrk.
  # Each entry contains the accuracies for all conditions.
  
  acc.all.sessions = list()
  counter = 1
  if(is.human) {
    subjects = get.all.subjects(dat, avg.human.data = FALSE)
    for(s in subjects) {
      if(! s$data.name %in% PLOT.SUBJECTS) {
        acc.all.sessions[[counter]] = get.accuracy(dat[dat$subj==s$data.name, ])
        acc.all.sessions[[counter]]$y = acc.all.sessions[[counter]]$y / 100
        counter = counter + 1
      }
      
    }
  } else {
    for(session.num in unique(dat$session)) {
      acc.all.sessions[[counter]] = get.accuracy(dat[dat$session==session.num, ])
      acc.all.sessions[[counter]]$y = acc.all.sessions[[counter]]$y / 100
      counter = counter + 1
    }
  }
  
  
  return(acc.all.sessions)
}


get.single.accuracy = function(dat) {
  # Return accuracy for whatever data as a single value.
  if(is.na(length(dat$is.correct)) | length(dat$is.correct)==0) {
    return(NaN)
  } else {
    return(sum(dat$is.correct) / length(dat$is.correct))
  }
}


endsWith = function(argument, match, ignore.case = TRUE) {
  # Return: does 'argument' end with 'match'?
  # Code adapted from:
  # http://stackoverflow.com/questions/31467732/does-r-have-function-startswith-or-endswith-like-python
  
  if(ignore.case) {
    argument = tolower(argument)
    match = tolower(match)
  }
  n = nchar(match)
  
  length = nchar(argument)
  
  return(substr(argument, pmax(1, length - n + 1), length) == match)
}

get.short.imagename = function(full.name) {
  # Get image-specific suffix of full.name, excluding possible
  # experiment-specific prefix (e.g. 0001_... for trial #1)
  
  split = strsplit(full.name, "_")[[1]]
  if(length(split) > 1) {
    name = tail(split, 2)
    if(startsWith(name[1], "n0")) { # ImageNet image: keep n0...prefix
      name = paste(name[1], name[2], sep="_")
    } else {
      name = name[2]
    }
  } else {
    name = split
  }
  return(name)
}

add.legend = function(subjects, legend.position=NULL, cex=1.25, 
                      y=NULL, pt.cex=POINTS.CEX.VAL, ...) {
  # Add a legend to a plot 
  # -> will be usually called from within the plotting function's code
  # This function will not care for the actual
  # subject numbers but plots them as 1,2,...,n without gaps!
  
  name.values = c()
  color.values = c()
  pch.values = c()
  
  counter = 1
  for(s in subjects) {
    
    is.found = FALSE
    for(p in PARTICIPANTS) {
      if(p$data.name == s$data.name) {
        name.values = c(name.values, paste("subject-0", as.character(counter),
                                           sep=""))
        counter = counter + 1
        is.found = TRUE
        break()
      }
    }
    if(!is.found) {
      name.values = c(name.values, s$name)
    }
    color.values = c(color.values, s$color)
    pch.values = c(pch.values, s$pch)
  }
  if(is.null(legend.position)) {
    legend.position="topleft"
  }
  legend(legend.position, y=y, legend=name.values,
         cex=cex, col=color.values,
         pt.bg = color.values, pt.cex = pt.cex,
         pch=pch.values, lty=1, ...)
  
}



crop.pdfs = function(dir.path) {
  # crop all PDFs in a directory to remove all white margins.
  
  files = list.files(path=dir.path, pattern="*.pdf", full.names=TRUE, recursive=TRUE,
                     include.dirs = TRUE)
  print(files)
  for(f in files) {
    if(! endsWith(f, "crop.pdf")) { # if not already cropped
      system(paste("pdfcrop", f))
    }
  }
}
