#!/bin/sh

# Script to install required packages in conda for GPU setup
# Author : Shikhar Tuli

module load anaconda3
conda create --name cnn_txf_bias

conda activate cnn_txf_bias
conda install ipykernel
conda install --channel anaconda pip
conda install --channel conda-forge opencv

pip install --upgrade pip
pip install -qr ./vit_jax/requirements_new.txt --usefeature=2020-resolver
pip install tfa-nightly
pip install tensorflow_io
pip install --upgrade jax jaxlib==0.1.57+cuda110 -f https://storage.googleapis.com/jax-releases/jax_releases.html