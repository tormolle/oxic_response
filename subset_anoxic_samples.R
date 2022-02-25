# Sensitivity analysis for differing numbers of anoxic samples below the oxic zone.
# For each config, detrend using first differences and remove the last (deepest) sample. 

# ====
# Define functions
# ====
# select data subset
.cmp_selfun_flexible <- function(trim = F, below = 3) {
  OTUout <- list() # create output OTU list
  mapout <- list() # create output map list
  for(i in unique(cmp$map$core_id)) {
    n <- cmp$otus[cmp$map$core_id == i, ] # select relevant OTU table rows
    nr <- nrow(n) # get number of rows of selection
    m <- cmp$map[cmp$map$core_id == i, ] # get corresponding map table
    if(any(m$oxy_cat <= 6)) { # are any of the samples oxic?
      wo <- which(m$oxy_cat <= 6) # which are oxic?
      no <- length(wo) # how many?
      if((nr - no) < 3) { # if less than 3, get them all and add out elements
        # get all samples
        OTUout[[i]] <- n
        mapout[[i]] <- m
      }
      else { # else, select the next three samples after the lowest oxic one and add out elements
        OTUout[[i]] <- n[seq(max(wo) + below), ]
        mapout[[i]] <- m[seq(max(wo) + below), ]
      }
    }
    else { # append no output for wholly anoxic cores
      OTUout[[i]] <- NULL
      mapout[[i]] <- NULL
    }
  }
  # append output into full OTU and map tables
  out <- list()
  # merge all tables
  out[["OTU"]] <- Reduce(rbind, OTUout) 
  # add rownames to reduced list as these are stripped away during merging
  rownames(out$OTU) <- unlist(lapply(OTUout, rownames))
  out[["map"]] <- Reduce(rbind, mapout) 
  
  if(trim) {
    # remove all samples with < 1000 reads
    s <- which(rowSums(out$OTU) < 1000)
    out$OTU <- out$OTU[-s, ]
    out$map <- out$map[-s, ]
    
    # remove all OTUs with < 100 reads
    o <- which(colSums(out$OTU) < 100)
    out$OTU <- out$OTU[, -o]
  }
  
  # report output
  out
}

# Detrend on first differences for each core
.detrend_firstdiff_corewise <- function(df, mapping) {
  cid <- unique(mapping$core_id)
  out <- data.frame()
  for(id in cid) {
    core <- df[which(mapping$core_id == id), ]
    coreout <- core[-(nrow(core)-1), ]
    for (j in seq(ncol(core))) {
      coreout[, j] <- diff(core[, j])
    }
    if (nrow(out) == 0) out <- coreout
    else out <- rbind(out, coreout)
  }
  names(out) <- names(df)
  out
}

# ====
# Create data subsets
# ====
sense <- list()
sense$two <- .cmp_selfun_flexible(below = 2) 
sense$two$OTU <- remove_empty_columns(sense$two$OTU)
sense$two$OTU_cmR <- cmultRepl_inhouse(sense$two$OTU)
sense$two$OTU_clr <- clr(sense$two$OTU_cmR)

sense$three <- .cmp_selfun_flexible(below = 3) 
sense$three$OTU <- remove_empty_columns(sense$three$OTU)
sense$three$OTU_cmR <- cmultRepl_inhouse(sense$three$OTU)
sense$three$OTU_clr <- clr(sense$three$OTU_cmR)

sense$four <- .cmp_selfun_flexible(below = 4) 
sense$four$OTU <- sense$four$OTU[-unique(which(is.na(sense$four$OTU), arr.ind = T)[, 1]), ]
sense$four$map <- sense$four$map[-unique(which(is.na(sense$four$map), arr.ind = T)[, 1]), ]
sense$four$OTU <- remove_empty_columns(sense$four$OTU)
sense$four$OTU_cmR <- cmultRepl_inhouse(sense$four$OTU)
sense$four$OTU_clr <- clr(sense$four$OTU_cmR)

sense$five <- .cmp_selfun_flexible(below = 5) 
sense$five$OTU <- sense$five$OTU[-unique(which(is.na(sense$five$OTU), arr.ind = T)[, 1]), ]
sense$five$map <- sense$five$map[-unique(which(is.na(sense$five$map), arr.ind = T)[, 1]), ]
sense$five$OTU <- remove_empty_columns(sense$five$OTU)
sense$five$OTU_cmR <- cmultRepl_inhouse(sense$five$OTU)
sense$five$OTU_clr <- clr(sense$five$OTU_cmR)

sense$six <- .cmp_selfun_flexible(below = 6) 
sense$six$OTU <- sense$six$OTU[-unique(which(is.na(sense$six$OTU), arr.ind = T)[, 1]), ]
sense$six$map <- sense$six$map[-unique(which(is.na(sense$six$map), arr.ind = T)[, 1]), ]
sense$six$OTU <- remove_empty_columns(sense$six$OTU)
sense$six$OTU_cmR <- cmultRepl_inhouse(sense$six$OTU)
sense$six$OTU_clr <- clr(sense$six$OTU_cmR)

# ====
# Detrend data corewise on first differences, removing last sample
# ====
sense$two$dOTU <- .detrend_firstdiff_corewise(sense$two$OTU, sense$two$map)
names(sense$two$dOTU) <- names(sense$two$OTU)
sense$two$dOTU <- apply(sense$two$dOTU, 1, function(x) scale(x)) %>% t() %>% as.data.frame
sense$two$dOTU_clr <- clr(sense$two$dOTU) %>% as.data.frame

sense$three$dOTU <- .detrend_firstdiff_corewise(sense$three$OTU, sense$three$map)
names(sense$three$dOTU) <- names(sense$three$OTU)
sense$three$dOTU <- apply(sense$three$dOTU, 1, function(x) scale(x)) %>% t() %>% as.data.frame
sense$three$dOTU_clr <- clr(sense$three$dOTU) %>% as.data.frame

sense$four$dOTU <- .detrend_firstdiff_corewise(sense$four$OTU, sense$four$map)
names(sense$four$dOTU) <- names(sense$four$OTU)
sense$four$dOTU <- apply(sense$four$dOTU, 1, function(x) scale(x)) %>% t() %>% as.data.frame
sense$four$dOTU_clr <- clr(sense$four$dOTU) %>% as.data.frame

sense$five$dOTU <- .detrend_firstdiff_corewise(sense$five$OTU, sense$five$map)
names(sense$five$dOTU) <- names(sense$five$OTU)
sense$five$dOTU <- apply(sense$five$dOTU, 1, function(x) scale(x)) %>% t() %>% as.data.frame
sense$five$dOTU_clr <- clr(sense$five$dOTU) %>% as.data.frame

sense$six$dOTU <- .detrend_firstdiff_corewise(sense$six$OTU, sense$six$map)
names(sense$six$dOTU) <- names(sense$six$OTU)
sense$six$dOTU <- apply(sense$six$dOTU, 1, function(x) scale(x)) %>% t() %>% as.data.frame
sense$six$dOTU_clr <- clr(sense$six$dOTU) %>% as.data.frame

# ====
# Create zero-replaced OTU tables as input to propd.
# ====

det <- list()

# Use the data table with three anoxic samples
det$rOTU <- cmp$otus[which(rownames(cmp$otus) %in% rownames(sense$three$OTU)), ] %>% 
  remove_empty_columns
det$rOTU_cmR <- cmultRepl_inhouse(det$rOTU)
det$rOTU_clr <- clr(det$rOTU_cmR) %>% as.data.frame
det$rmap <- cmp$map[which(rownames(cmp$map) %in% rownames(sense$three$map)), ]
det$rotu_tax <- transform(merge(x = cmp$otu_tax[, 1:9], 
                                y = as.data.frame(t(det$rOTU)), 
                                by = "row.names"),
                          row.names = Row.names, Row.names = NULL)

# Aggregate to higher taxonomic levels
det$family <- get_unique_taxa(det$rotu_tax, "family", "GS") %>% as.data.frame
det$order <- get_unique_taxa(det$rotu_tax, "order", "GS") %>% as.data.frame
det$class <- get_unique_taxa(det$rotu_tax, "class", "GS") %>% as.data.frame
det$phylum <- get_unique_taxa(det$rotu_tax, "phylum", "GS") %>% as.data.frame

# Get rank order of abundance
det$phylumorder <- order(colSums(det$phylum), decreasing = T)
det$classorder <- order(colSums(det$class), decreasing = T)
det$orderorder <- order(colSums(det$order), decreasing = T)
det$familyorder <- order(colSums(det$family), decreasing = T)
