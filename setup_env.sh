#! /usr/bin/env bash

set -e

python_version=3.11.7
python_dist_path=$HOME/.local/python-${python_version}
python_virtualenv_name=python-${python_version}

# Install a base Python if not available
if [[ ! -f $python_dist_path/bin/python3 ]]; then

    if [[ ! -d $python_dist_path ]]; then
       mkdir -p $python_dist_path
    fi

    # Download Python Standalone Builds from
    # https://github.com/indygreg/python-build-standalone

    cd $python_dist_path
    wget -nc  https://raw.githubusercontent.com/indygreg/python-build-standalone/latest-release/latest-release.json
    tag=$(grep -oP '"tag": "\K[^"]+' latest-release.json)
    asset_url_prefix=$(grep -oP '"asset_url_prefix": "\K[^"]+' latest-release.json)
    python_pkg=cpython-${python_version}+${tag}-x86_64-unknown-linux-gnu-pgo+lto-full.tar.zst

    wget -nc ${asset_url_prefix}/${python_pkg}

    # Only unzip the python directory and extract to current dist path
    tar -I zstd -axvf $python_pkg python/install --strip-components=2
    cd -
fi

# Install and upgrade pip and virtualenv to base environment
export PATH=${python_dist_path}/bin:$PATH
pip install pip virtualenv --upgrade --break-system-packages

# Setup virtual environment for current project
virtualenv ~/.virtualenvs/${python_virtualenv_name} --python ${python_dist_path}/bin/python3
echo ${python_virtualenv_name} > .venv
chmod 600 .venv

# Message
echo -e "
Python environment setup completely. Use the follow command to activate
       source ~/.virtualenvs/${python_virtualenv_name}/bin/activate
"

# Install other dependences
# source $conda_path/activate dismod_mr
# pip install --upgrade pip
# pip install dismod_mr

# Jupyter notebook
# pip install ipykernel
# python -m ipykernel install --name=Python3.6-PyMC2.3.8 --user
