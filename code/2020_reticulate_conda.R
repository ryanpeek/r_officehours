# using conda/reticulate in R

# if already installed, don't install again! 

library(reticulate)

# see envs
conda_list()
conda_version()

# see versions
py_discover_config()

# use global version
#use_python("/Users/ryanpeek/.pyenv/shims/python")
#Sys.setenv(RETICULATE_PYTHON = "/Users/ryanpeek/.pyenv/shims/python")

# create an env:
#conda_create(envname = "play")

# save all info required to create an environment file using:
# `conda env export > environment.yml`
# and recreate with:
# `conda env create -f environment.yml`

# remove/delete an env
#conda_remove("play")

# set env
use_condaenv("play")

# check python version
conda_python(envname = "play")

# use python default
use_python("/Users/ryanpeek/miniconda3/envs/play/bin/python")
repl_python() # get 2.7.16 as default

# specify diff python version
Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3")
repl_python() # 3.7.8
