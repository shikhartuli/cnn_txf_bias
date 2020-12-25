#!/bin/sh

# Script to install required packages in conda for GPU setup
# Author : Shikhar Tuli

module load anaconda3
conda create --name ctb_test python=3.6.9 anaconda

conda activate ctb_test

pip install -qr ./vit_jax/requirements_new.txt
pip install tfa-nightly
pip install tensorflow_io
pip install --upgrade jax jaxlib==0.1.57+cuda101 -f https://storage.googleapis.com/jax-releases/jax_releases.html
pip install opencv-python