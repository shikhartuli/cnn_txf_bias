# Cohen's κ for error consistency

Error consistency is a quantitative analysis for measuring whether two decision making systems systematically make errors on the same inputs. The paper is available on [arXiv](https://arxiv.org/abs/2006.16736). The code is from [wichmann-lab/error-consistency](https://github.com/wichmann-lab/error-consistency) repository.

## Dependencies

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

## Raw-data

Experiments with the prefix `texture-shape`, that were used in this paper, are from [here](https://openreview.net/forum?id=Bygh9j09KX); the raw data is copied from the corresponding repository: [rgeirhos/texture-vs-shape](https://github.com/rgeirhos/texture-vs-shape).

## Results

Results after running error consistency experiments for pre-trained and fine-tuned CNNs and ViTs are available at [results/figures_new_ft](https://github.com/shikhartuli/cnn_txf_bias/tree/main/error-consistency/figures_new_ft).
