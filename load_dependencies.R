# Load dependencies and data
.packs <- c("gbm", 
            "plyr", 
            "caret", 
            "vegan", 
            "ggplot2", 
            "propr",
            "knitr",
            "tidyverse",
            "reshape2",
            "compositions",
            "party",
            "egg",
            "decontam",
            "GauPro",
            "readxl",
            "readODS")
sapply(.packs, require, character.only = T)

# Source auxillary functions
lapply(list.files(path = "auxillary_functions", full.names = T, recursive = T), source) 