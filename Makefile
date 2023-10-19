model_env:
	if conda info --envs | grep -q dismod_mr; then echo "dismod_mr env exists!"; else conda create --name=dismod_mr python=3.6 pymc==2.3.8; fi
	conda activate dismod_mr
	pip install --upgrade pip
	pip install dismod_mr jupyter
