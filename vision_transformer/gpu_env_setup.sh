#!/bin/sh

# Script to install required packages in conda for GPU setup
# Author : Shikhar Tuli

module load anaconda3
conda create --name cnn_txf_bias python=3.6 anaconda

conda activate cnn_txf_bias

pip install -qr ./vit_jax/requirements_new.txt
pip install tfa-nightly
pip install tensorflow_io
pip install --upgrade jax jaxlib==0.1.57+cuda110 -f https://storage.googleapis.com/jax-releases/jax_releases.html
pip install opencv-python