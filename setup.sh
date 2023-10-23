#! /usr/bin/env sh

# Base environment
conda_path=$(dirname $CONDA_EXE)
echo "Using conda in $conda_path"

if conda info --envs | grep -q dismod_mr; then
    echo "dismod_mr env exists! Skip creating.";
else
    conda create --name=dismod_mr python=3.6 pymc==2.3.8;
fi

# Dependences
source $conda_path/activate dismod_mr
pip install --upgrade pip
pip install dismod_mr

# Jupyter notebook
pip install ipykernel
python -m ipykernel install --name=Python3.6-PyMC2.3.8 --user
