# This file provides the full overview of all files and commands necessary to reproduce 
# the analyses and results figures (minus minor graphical adjustments) presented in 
# "Mapping microbial abundance and prevalence to changing oxygen concentration in deep-sea 
# sediments using Support Vector Machines and differential expression".

# The script assumes that all files necessary are in the data folder.
# Packages listed in the load_dependencies.R script may need to be installed before running the script.

# The analyses were all tested using R version 3.6.3 on a 32 gb RAM, 8 core Linux (ubuntu 18.04) computer.
# There is no guarantee that it will work on different versions of R.

## IMPORTANT! IMPORTANT!
# Although possible, running this script directly is strongly discouraged.
# Instead, enter the R scripts in the order they appear in the list below and run them individually 
# in order to maintain an overview - and control - over what's happening.
# This is very important since some of the files in this script will take hours and 
# maybe even days to run, depending on your setup.
## IMPORTANT! IMPORTANT!

## Stages of analysis
rm(list = ls()) # clear environment
setwd(".") # sets working directory to current path.

# The script is only set up to work in the arrangement presented below.

# ====
# Load required packages and auxillary functions
# ====
# The auxillary functions are written partly to accommodate processing for this specificy project,
# but in a matter generalisible also to other projects.
# Additional helper functions will appear in various scripts.
source("load_dependencies.R") 

# ====
# Import and process all necessary 16S and context data
# ====
# Includes mapping file and O2 data. NO3 and TOC are loaded later.
# See auxillary_functions/o2_concentration_helpers.R for relevant helper functions.
# Note: The mapping file is not necessarily complete at this stage. Some variables
# may be added during subsequent processing.
source("import_clean_postprocess_data.R")

# ====
# Subset the full OTU table to three anoxic samples per core
# ====
source("subset_anoxic_samples.R")

# ====
# Perform SVM analyses
# ====

# IMPORTANT: The following script may take upto several days to run depending on your computer setup.
# The script features progress tags which are not shown when simply sourcing them, so
# entering the scripts and running them from within is strongly encouraged in order to maintain
# overview.

# Perform SVM classification.
source("svm_classification.R")

# Results from running SVM with lowered cutoff value 
# IMPORTANT: This file assumes that a number of files above have been run.
source("svm_low_o2_cutoff.R")

# ====
# Perform differential expression analyses
# ====
# See auxillary_functions/differential_expression_helpers.R for relevant helper functions.
source("differential_expression.R")

# ====
# Plot main manuscript figures
# ====

# Instead of plotting figures directly, this script returns ggpplot objects that may be called
# from the console.
source("create_manuscript_figures.R")

# ====
# Plot supplementary figures
# ====

# Here, figures are plotted directly. Note that scripts are sourced from within.
source("create_supplementary_figures.R")

# ====
# Perform statistical tests
# ====

# This script shows the analyses underlying all statistics presented in the manuscript with 
# reference to specific lines where possible.

# All other relevant statistical tests and results
source("statistical_analysis.R")