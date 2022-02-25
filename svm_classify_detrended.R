# Classify detrended OTU data based according to quaternary oxic state

set.seed(123)

# Set up data
det$dOTU_clr <- sense$four$dOTU_clr
det$dmap <- cmp$map[which(rownames(cmp$map) %in% rownames(det$dOTU_clr)), ]
det$dOTU <- cmp$otus[which(rownames(cmp$otus) %in% rownames(det$dOTU_clr)), 
                     which(names(cmp$otus) %in% names(det$dOTU_clr))]
det$dotu_tax <- transform(merge(x = cmp$otu_tax[, 1:9], 
                                y = as.data.frame(t(det$dOTU)), 
                                by = "row.names"),
                          row.names = Row.names, Row.names = NULL)

# split at 1-4 v 5 v 6 v 7 for balancing
det$dmap$oxy_quart <- det$dmap$oxy_cat
det$dmap$oxy_quart[det$dmap$oxy_quart %in% 1:4] <- "high"
det$dmap$oxy_quart[det$dmap$oxy_quart == 5] <- "mid"
det$dmap$oxy_quart[det$dmap$oxy_quart == 6] <- "low"
det$dmap$oxy_quart[det$dmap$oxy_quart == 7] <- "anox"

# Analyse
class_quart_detrended <- .svmclass_rotu(level = "class", mode = "quart", incr = 20, pp = 128,
                                           otus = det$dOTU_clr, otu_tax = det$dotu_tax, map = det$dmap)
