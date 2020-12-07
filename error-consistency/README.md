# Data and analysis code from "Beyond accuracy: quantifying trial-by-trial behaviour of CNNs and humans by measuring error consistency"

Error consistency is a quantitative analysis for measuring whether two decision making systems systematically make errors on the same inputs. The paper is available on [arXiv](https://arxiv.org/abs/2006.16736).

## dependencies
The R analysis scripts have the following dependencies which can be installed via ``install.packages("package-name")``. Data analysis was performed using R version `3.5.1`.

	library(lattice)
	library(jpeg)
	library(R.matlab)
	library(graphics)
	library(pROC)
	library(psych)
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
	library(scales)
	library(xtable)
	library(viridis)
	library(binom)

## raw-data
Experiments with the prefix `noise-generalisation` are from this [paper](http://papers.nips.cc/paper/7982-generalisation-in-humans-and-deep-neural-networks.pdf); the raw data is copied from the corresponding [github repository](https://github.com/rgeirhos/generalisation-humans-DNNs). 
Experiments with the prefix `texture-shape` are from this [paper](https://openreview.net/forum?id=Bygh9j09KX); the raw data is copied from the corresponding [github repository](https://github.com/rgeirhos/texture-vs-shape).



