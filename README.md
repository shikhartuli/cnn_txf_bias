# Are Convolutional Neural Networks or Transformers more like human vision?

![Python Version](https://img.shields.io/badge/python-v3.6%20%7C%20v3.7%20%7C%20v3.8-blue)
![Tensorflow](https://img.shields.io/badge/tensorflow-v2.4-orange)
![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fshikhartuli%2Fcnn_txf_bias&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=hits&edge_flat=false)

This repository contains the code and fine-tuned models of popular Convolutional Neural Networks (CNNs) and the recently proposed Vision Transformer (ViT) on the augmented Imagenet dataset and the shape/texture bias tests run on the Stylized Imagenet dataset.

This work compares CNNs and the ViT against humans in terms of error consistency beyond traditional metrics. Through these tests, we were able to show that recently proposed self-attention based Transformer models have more human-like errors that traditional CNNs.

![Illustration](https://github.com/shikhartuli/cnn_txf_bias/blob/main/CNN_vs_Human.png?raw=true)

## Colab

You can directly run tests on the results using a Google Colaboratory without needing to install anything on your local machine. Click "Open in Colab" below:

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/shikhartuli/cnn_txf_bias/blob/main/tests/confusion_matrices.ipynb)

## Developer

[Shikhar Tuli](https://github.com/shikhartuli). For any questions, comments or suggestions, please reach me at [stuli@princeton.edu](mailto:stuli@princeton.edu).

## Cite this work

If you use our experimental results or fine-tuned models, please cite:
```
@article{tuli2021cogsci,
      title={Are Convolutional Neural Networks or Transformers more like human vision?}, 
      author={Shikhar Tuli and Ishita Dasgupta and Erin Grant and Thomas L. Griffiths},
      year={2021},
      eprint={2105.07197},
      archivePrefix={arXiv},
      primaryClass={cs.CV}
}
```
