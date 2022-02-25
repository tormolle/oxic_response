# Import, clean and post-process AMOR
# ====
# Import and do basic processing
# ====

cmp <- list(
  otus = read.csv("data/all.otutab.nonchimeras.csv", row.names = 1),
  tax = process_tax_file("data/otus.csv"),
  contaminant_genera = read_ods("data/contaminant_genera.ods")
)

# Fix kingdom and superkingdom names
if(any(is.na(cmp$tax$kingdom))) {
  cmp$tax$kingdom[is.na(cmp$tax$kingdom)] <- "No hits"
  cmp$tax$superkingdom[which(stringr::str_count(cmp$tax$superkingdom, "size") == 1)] <- "No hits"
}

# Merge OTUs and taxonomy
cmp$all_otu_tax <- transform(merge(x = cmp$tax, 
                                   y = cmp$otus, 
                                   by = "row.names"),
                             row.names = Row.names, Row.names = NULL)

# Transpose so that samples are rows
cmp$otus <- as.data.frame(t(cmp$otus))

# ====
# Post-processing: 
# Remove single occurrences, decontaminate and filter known contaminants
# ====

# 1. Remove singletons
cmp$otus <- cmp$otus[, which(colSums(cmp$otus) > 1)]

# 2. Decontaminate AMOR samples
cmp$decont_batch = c(1, # CB06 + CB03/06
                     rep(2, 3), # CB08 + CB05
                     3, # CB10
                     rep(4, 2), # CB11
                     rep(5, 3), # CB14
                     rep(6, 5), # CB15
                     rep(1, length(which(grepl("GS10_PC12", rownames(cmp$otus)) == 1))),
                     rep(1, length(which(grepl("GS13_CC02", rownames(cmp$otus)) == 1))),
                     rep(1, length(which(grepl("GS14_GC02", rownames(cmp$otus)) == 1))),
                     rep(2, length(which(grepl("GS14_GC04", rownames(cmp$otus)) == 1))),
                     rep(2, length(which(grepl("GS14_GC08", rownames(cmp$otus)) == 1))),
                     rep(2, length(which(grepl("GS14_GC09", rownames(cmp$otus)) == 1))),
                     rep(2, length(which(grepl("GS14_GC12", rownames(cmp$otus)) == 1))),
                     rep(1, length(which(grepl("GS14_GC14", rownames(cmp$otus)) == 1))),
                     rep(2, length(which(grepl("GS15_GC01", rownames(cmp$otus)) == 1))),
                     rep(3, length(which(grepl("GS16_GC04", rownames(cmp$otus)) == 1))),
                     rep(4, length(which(grepl("GS16_GC05", rownames(cmp$otus)) == 1))),
                     rep(4, length(which(grepl("GS16_GC06", rownames(cmp$otus)) == 1))),
                     rep(4, length(which(grepl("GS16_GC07", rownames(cmp$otus)) == 1))),
                     rep(5, length(which(grepl("GS17_GC02", rownames(cmp$otus)) == 1))),
                     rep(6, length(which(grepl("GS17_GC04", rownames(cmp$otus)) == 1))),
                     rep(5, length(which(grepl("GS17_GC05", rownames(cmp$otus)) == 1)))
)
cmp$decont_status = c(rep(TRUE, 15), rep(FALSE, 418))

# Identify contaminants
cmp$isContaminant <- cmp$otus[c(1:15, which(grepl("GS1", rownames(cmp$otus)))), ] %>% 
  remove_empty_columns %>% 
  as.matrix %>%
  isContaminant(neg = cmp$decont_status, batch = cmp$decont_batch)

# Isolate contaminants
cmp$contaminants <- cmp$otus[c(1:15, which(grepl("GS1", rownames(cmp$otus)))), ] %>% 
  remove_empty_columns %>% 
  .[, cmp$isContaminant$contaminant]

# Remove contaminants from AMOR OTU table before addressing cross-talk
cmp$amor_decontaminated <- cmp$otus[c(1:15, which(grepl("GS1", rownames(cmp$otus)))), ] %>% 
  remove_empty_columns %>% 
  .[, !cmp$isContaminant$contaminant]

# Remove contaminants from full OTU table
cmp$otus <- cmp$amor_decontaminated

# Remove blank samples
cmp$otus <- cmp$otus[!grepl("blnk", rownames(cmp$otus)), ]

# 3. Check remaining non-contaminants against contaminant_genera.
# Conservative approach selected herein: Remove identified OTUs regardless.
cmp$otus <- cmp$otus[, !names(cmp$otus) %in% 
                       rownames(cmp$otu_tax[which(cmp$otu_tax$genus %in% cmp$contaminant_genera$genus), ])]

# Update OTU and taxonomy table
cmp$postprocessed_otu_tax <- transform(merge(x = cmp$tax, 
                                             y = as.data.frame(t(cmp$otus)), 
                                             by = "row.names"),
                                       row.names = Row.names, Row.names = NULL)

# ====
# Remove shallow libraries, GS14_GC14 and rare OTUs
# ====

# 1. Remove samples with fewer than 1000 reads.
cmp$otus <- cmp$otus[rowSums(cmp$otus) > 1000, ] %>% remove_empty_columns

# 2. Remove GS14_GC14 from the table
cmp$otus <- cmp$otus[-which(grepl("GS14_GC14", rownames(cmp$otus))) ,]

# 3. Remove all OTUs with fewer than 100 reads.
cmp$otus <- cmp$otus[, colSums(cmp$otus) >= 100]

cmp$otu_tax <- transform(merge(x = cmp$tax, 
                               y = as.data.frame(t(cmp$otus)), 
                               by = "row.names"),
                         row.names = Row.names, Row.names = NULL)

# ====
# Import (merge) and trim mapping files according to above sample removal
# ====

# 1. Import files
cmp$map <- read_ods(path = "data/mapping.ods") %>%
  as.data.frame

# 2. Adjust mapping files according to OTU table and vice versa

# Remove GC14 from mapping and adjust core_id accordingly
cmp$map <- cmp$map[-which(cmp$map$core == "GC14"), ] 
cmp$map$core_id[which(cmp$map$core_id > 8)] <- cmp$map$core_id[which(cmp$map$core_id > 8)] - 1

# add rownames column to map to avoid confusion between depth and id columns downstream
cmp$map$rowname <- paste(cmp$map$cruise, cmp$map$core, cmp$map$depth, sep = "_")
rownames(cmp$map) <- cmp$map$rowname

# Import and interpolate O2 profiles for all cores 
# (function see: auxillary_functions/o2_concentration_helpers.R)
cmp$o2 <- .assign_o2_cons_id(.read_excel_allsheets("data/oxygen_profiles.xlsx"), 
                             micro_map = cmp$map,
                             zero_threshold = 0.005,
                             add_long = T)

# Add oxy_cat and o2 to mapping directly if rownames are identical
if (identical(rownames(cmp$o2$long), cmp$map$rowname)) {
  cmp$map$oxy_cons <- cmp$o2$long$o2
  cmp$map$oxy_cat <- cmp$o2$long$oxy_cat
}

# 2.5 Change OTU table rownames according to mapping files
rn <- rownames(cmp$otus)
rownames(cmp$otus) <- seq(nrow(cmp$otus))
rntmp <- c()
for (i in seq(rownames(cmp$otus))) {
  rownames(cmp$otus)[i] <-
    cmp$map$rowname[which(paste(cmp$map$cruise, cmp$map$core, cmp$map$id, sep = "_") == rn[i])]
}
rm(rn)

# reorder GS17_GC02 samples after increasing depth (done manually):
cmp$otus <- cmp$otus[c(1:216, 267:277, 217:266, 278:387), ]
tmp <- cmp$otus

# 3. Concatenate and trim mapping files
cmp$map <- cmp$map[which(cmp$map$rowname %in% rownames(cmp$otus)), ]

# 4. Update full OTU table with new names
cmp$otu_tax <- transform(merge(x = cmp$tax, 
                               y = as.data.frame(t(cmp$otus)), 
                               by = "row.names"),
                         row.names = Row.names, Row.names = NULL)
