#!/bin/sh

# Script to install required packages in conda for GPU setup
# Author : Shikhar Tuli

module load anaconda3
conda env create --name cnn_txf_bias tensorflow-gpu

conda activate cnn_txf_bias
conda install --channel conda-forge opencv

pip install -qr ./vit_jax/requirements_new_gpu.txt
pip install tfa-nightly
pip install tensorflow_io

pip install --upgrade pip
pip install --upgrade jax jaxlib==0.1.57+cuda110 -f https://storage.googleapis.com/jax-releases/jax_releases.html