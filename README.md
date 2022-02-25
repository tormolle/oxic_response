# oxic_response
R code that reproduces all analyses, results and figures in the manuscript 
Møller, TE, et al. (2022) *Mapping microbial abundance and prevalence to changing oxygen concentration in 
deep-sea sediments using machine learning and differential abundance.*
In Frontiers in Microbiology. DOI:to come.

All code was run and tested using R version 3.6.3 on a computer with the ubuntu linux 18.04 OS.

The files in the script must be run in the order specified within the `README_FIRST_processing_script.R` file in order to properly work. 
Additionally, the data required to run the scripts may be found as supplements to the referenced article. 
These must be stored within the R working directory in a separate folder named `data` in order to run as intended.

Furthermore, the files `cmultRepl_inhouse.R`, `differential_expression_helpers.R`, `get_unique_taxa.R`, `ggbarplot_gen.R`,
`multiplot.R`, `o2_concentration_helpers.R`, `pca2ggplot.R`, `process_tax_file.R`, `remove_empty_columns.R` and `svm_output_helpers.R` 
should be stored in a subdirectory named `auxillary_functions`.
