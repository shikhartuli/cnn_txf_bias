# Data, code and materials from <br>"ImageNet-trained CNNs are biased towards texture; increasing shape bias improves accuracy and robustness"

This repository contains information, data and materials from the paper [ImageNet-trained CNNs are biased towards texture; increasing shape bias improves accuracy and robustness](https://openreview.net/forum?id=Bygh9j09KX) by Robert Geirhos, Patricia Rubisch, Claudio Michaelis, Matthias Bethge, Felix A. Wichmann, and Wieland Brendel. We hope that you may find this repository a useful resource for your own research.

The core idea is explained in the Figure below: If a Convolutional Neural Network sees a cat with elephant texture, it thinks it's an elephant even though the shape is still clearly a cat. We found this "texture bias" to be common for ImageNet-trained CNNs, which is in contrast to the widely held belief that CNNs mostly learn to recognise objects by detecting their shapes.
![intro_figure](./paper-figures/introduction/intro_figure.png) 

Please don't hesitate to contact me at robert.geirhos@bethgelab.org or open an issue in case there is any question! Reproducibility & Open Science are important to me, and I appreciate feedback on what could be improved.

This README is structured according to the repo's structure: one section per subdirectory (alphabetically).

##### Related repositories:
Note that Stylized-ImageNet, an important dataset used in this paper, has its own repository at [rgeirhos:Stylized-ImageNet](https://github.com/rgeirhos/Stylized-ImageNet).

Some aspects of this repository are borrowed from our earlier work, "Generalisation in humans and deep neural networks" (published at NeurIPS 2018). The corresponding code, data and materials can be obtained from [rgeirhos:generalisation-humans-DNNs](https://github.com/rgeirhos/generalisation-humans-DNNs). For convencience, some human data from this repo (which are used in the texture-vs-shape work for comparison) are included here directly (under ``raw-data/raw-data-from-generalisation-paper/``).


## code
The ``code/`` directory contains mapping functionality that can be used to determine the corresponding entry-level class (out of 16, e.g. "dog") from a vector of length 1,000 (softmax output of a typical ImageNet classifier). In order to use this, follow the steps below:

```python
    # get softmax output
    softmax_output = SomeCNN(input_image) # replace with your favourite CNN

    # convert to numpy
    softmax_output_numpy = SomeConversionToNumpy(softmax_output) # replace with conversion

    # create mapping
    mapping = probabilities_to_decision.ImageNetProbabilitiesTo16ClassesMapping()
    
    # obtain decision 
    decision_from_16_classes = mapping.probabilities_to_decision(softmax_output_numpy)
```

## data-analysis
The ``data-analysis/`` directory contains the main analysis script ``data-analysis.R`` and some helper functionality. All created plots will then be stored in the ``paper-figures/`` directory.

Please note: For AlexNet, VGG-16 and GoogLeNet we used the caffe implementation; for ResNet-50 the torchvision implementation. When computing the shape bias for AlexNet and VGG-16 using torchvision, the values differ (as pointed out in issue #7). The shape bias of AlexNet is 25.3%, for VGG-16 it is 9.2%. Both shape bias values are lower than the ones reported in the paper that were obtained with caffe. This means that using the torchvision implementation, we obtain even more extreme texture bias for these two models. Generally we recommend using the torchvision implementation (more commonly used & up-to-date framework).

## lab-experiment
Everything necessary to run an experiment in the lab with human participants. This is based on MATLAB.

##### experimental-code
Contains the main MATLAB experiment, `shape_texture_experiment.m`, as well as a `.yaml` file where the specific parameter values used in an experiment are specified (such as the stimulus presentation duration). Some functions depend on our in-house iShow library which can be obtained from [here](http://dx.doi.org/10.5281/zenodo.34217).

##### helper-functions
Some of the helper functions are based on other people's code, please check out the corresponding files for the copyright notices.


## models
The file ``load_pretrained_models.py`` will load the following models that are trained on Stylized-ImageNet:

```python
    model_A = "resnet50_trained_on_SIN"
    model_B = "resnet50_trained_on_SIN_and_IN"
    model_C = "resnet50_trained_on_SIN_and_IN_then_finetuned_on_IN"
```
These correspond to the models reported in Table 2 of the paper (method details in Section A.5 of the Appendix). Additionally, AlexNet and VGG-16 trained on SIN are provided. Please note that the overall performance of those two models is not great since the hyperparameters used during training were likely suboptimal. The top1/top5 performance of VGG-16 trained on SIN and evaluated on ImageNet are: Prec@1 52.260 Prec@5 76.390 (evaluated on SIN: Prec@1 48.958 Prec@5 73.092).

We used the [PyTorch ImageNet training script](https://github.com/pytorch/examples/tree/master/imagenet)  to train the models. These are the training hyperparameters:

- batch size: 256
- optimizer: SGD (`torch.optim.SGD`)
- momentum: 0.9
- weight decay: 1e-4
- number of epochs: 60 (`model_A`) respectively 45 (`model_B`). However, these 45 epochs for `model_B` correspond to 90 epochs of normal ImageNet training since the dataset used to train `model_B` is twice as large (combined ImageNet and Stylized-ImageNet), thus in every epoch the classifier sees twice as many images as in a standard epoch.
- learning rate: 0.1 multiplied by 0.1 after every 20 epochs (`model_A`) respectively after every 15 epochs (`model_B`).
- pretrained on ImageNet: True (for `model_A` and for `model_B`), i.e. using `torchvision.models.resnet50(pretrained=True)`. Initialising models weights with the standard weights from ImageNet training proved beneficial for overall accuracy.

`model_C` was initialised with the weights of `model_B`. Fine-tuning on ImageNet was then performed for 60 epochs using a learning rate of 0.01 multiplied by 0.1 after 30 epochs. The other hyperparameters (batch size, optimizer, momentum & weight decay) were identical to the ones used for training `model_A` and `model_B`.

For dataset preprocessing, we used the standard ImageNet normalization for both IN and SIN (as e.g. used in the [PyTorch ImageNet training script](https://github.com/pytorch/examples/tree/master/imagenet)), with the following mean and standard deviation:

- mean = [0.485, 0.456, 0.406]
- std = [0.229, 0.224, 0.225]

These were the training transformations:

```python
    train_transforms = transforms.Compose([
                                  transforms.RandomResizedCrop(224),
                                  transforms.RandomHorizontalFlip(),
                                  transforms.ToTensor()])
```

and those the validation transformations:
```python                                  
    val_transforms = transforms.Compose([
                                      transforms.Resize(256),
                                      transforms.CenterCrop(224),
                                      transforms.ToTensor()])                 
```
Input format: RGB.

#### Shape bias and IN accuracies of different SIN-trained models

These are the shape bias values of the four models mentioned above. As a rough guideline, the more epochs a model was trained on ImageNet the lower its shape bias; the more epochs a model was trained on Stylized-ImageNet the higher its shape bias. Fine-tuning on ImageNet (as for model_C) leads to improved ImageNet performance, even better than a standard ResNet-50, but it also means that the model "forgets" the shape bias it had before finetuning.

| model | shape bias | top-1 IN acc | top-5 IN acc |
|---|---|---|---|
| standard ResNet-50 | 21.39% | 76.13 | 92.86 |
| model_A            | 81.37% | 60.18 | 82.62 |
| model_B            | 34.65% | 74.59 | 92.14 |
| model_C            | 20.54% | 76.72 | 93.28 |

Note that these values are computed using a slightly different probability aggregation method as reported in the paper. We here used the average: ImageNet class probabilities were mapped to the corresponding 16-class-ImageNet category using the average of all corresponding fine-grained category probabilities. We recommend using this approach instead of other aggregation methods (summation, max, ...).

## paper-figures
Contains all figures of the paper. All figures reporting results can be generated by the scripts in `data-analysis/`.


## raw-data
Here, a ``.csv`` file for each observer and network experiment contains the raw data, including a total number of 48,560 human psychophysical trials across 97 participants in a controlled lab setting.


## stimuli
These are the raw stimuli used in our experiments. Each directory contains stimuli images split into 16 subdirectories (one per category).


# FAQ

#### Code to run style transfer:
I used [Leon Gatys' code](https://github.com/leongatys/PytorchNeuralStyleTransfer) to run style transfer with default settings and hyperparameters as specified in the code. The final content and style loss depend on the image.

#### Can you share Stylized-ImageNet directly?
Unfortunately, due to copyright restrictions I am not allowed to share this version of ImageNet directly, since not all of the original ImageNet images are permitted for using / sharing / modification.

#### In addition to the cue conflict stimuli, can you share the stimuli for the texture experiment / the 'original' experiment / ...?
Unfortunately, the image permissions do not allow me to share or distribute these stimuli.

#### How do I compute the shape bias of a model?
1. Evaluate your models on all 1,280 images here (https://github.com/rgeirhos/texture-vs-shape/tree/master/stimuli/style-transfer-preprocessed-512).
2. Map model decisions to 16 classes using the code provided above (https://github.com/rgeirhos/texture-vs-shape#code).
3. Exclude images without a cue conflict (e.g. texture=cat, shape=cat).
4. Take the subset of "correctly" classified images (either shape or texture category correctly predicted).
5. Compute "shape bias" as the following fraction: (correct shape decisions) / (correct shape decisions + correct texture decisions).
