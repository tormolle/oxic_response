# write new mapping file with 3µM as cutoff for anoxic samples

lowmap <- det$rmap # copy mapping file, necessary for downstream SVM classification

# Import and interpolate O2 profiles for all cores 
lowo2 <- .assign_o2_cons_id(.read_excel_allsheets("data/oxygen_profiles.xlsx"), 
                             micro_map = lowmap,
                             zero_threshold = 0.003,
                             add_long = T)

# Add oxy_cat and o2 to mapping directly if rownames are identical
if (identical(rownames(lowo2$long), lowmap$rowname)) {
  lowmap$oxy_cons <- lowo2$long$o2
  lowmap$oxy_cat <- lowo2$long$oxy_cat
}

identical(rownames(lowmap), rownames(det$rOTU))

## Set up all oxy cat configs

# split at 1-5 vs 6-7 for balancing
lowmap$oxy_bin <- "high"
lowmap$oxy_bin[lowmap$oxy_cat %in% c(6, 7)] <- "loan"

# split tert at 1-5 v 6 v 7
lowmap$oxy_tert <- lowmap$oxy_cat
lowmap$oxy_tert[lowmap$oxy_tert %in% 1:5] <- "high"
lowmap$oxy_tert[lowmap$oxy_tert == 6] <- "low"
lowmap$oxy_tert[lowmap$oxy_tert == 7] <- "anox"

# split at 1-4 v 5 v 6 v 7 for balancing
lowmap$oxy_quart <- lowmap$oxy_cat
lowmap$oxy_quart[lowmap$oxy_quart %in% 1:4] <- "high"
lowmap$oxy_quart[lowmap$oxy_quart == 5] <- "mid"
lowmap$oxy_quart[lowmap$oxy_quart == 6] <- "low"
lowmap$oxy_quart[lowmap$oxy_quart == 7] <- "anox"

# Analyse
svmclass$rclass_quart_low <- .svmclass_rotu(level = "class", mode = "quart", 
                                           incr = 20, perms = svmclass$perms[, 1:16],
                                           map = lowmap)

# Plotting is done in create_supplementary_figures.R (figure S8)
