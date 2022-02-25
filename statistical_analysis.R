# Each of the below statements correspond verbatim to text in the manuscript
# so that they can be located by text search instead of reference to line number.

# Statements are indicated by double ## and other comments by single #

## Among the 36 phyla, 194 classes, 194 orders and 347 families identified...
dim(det$phylum)
dim(det$class)
dim(det$order)
dim(det$family)

## the top ten abundant classes along the Arctic Mid-Ocean Ridge (AMOR) are...
names(det$class)[order(colSums(det$class/rowSums(det$class)), decreasing = T)[c(1:5, 7:11)]] # names

sum(det$class$Alphaproteobacteria/rowSums(det$class))*(100/184)
sum(det$class$Nitrososphaeria/rowSums(det$class))*(100/184)
sum(det$class$S085/rowSums(det$class))*(100/184)
sum(det$class$Planctomycetacia/rowSums(det$class))*(100/184)
sum(det$class$Gammaproteobacteria/rowSums(det$class))*(100/184)
sum(det$class$Phycisphaerae/rowSums(det$class))*(100/184)
sum(det$class$Deltaproteobacteria/rowSums(det$class))*(100/184)
sum(det$class$`MD2896-B214`/rowSums(det$class))*(100/184)
sum(det$class$`Chloroflexi Subdivision 5 (SAR202)`/rowSums(det$class))*(100/184)
sum(det$class$Pacearchaeota/rowSums(det$class))*(100/184)

## ...all achieve median calssification rates (the percentage of correctly assigned 
# samples) above 76%
tmp <- rbind(cbind(svmclass$rclass_quart[[1]]$tperm[, ncol(svmclass$rclass_quart[[1]]$tperm)], variable = names(svmclass$rclass_quart)[1], perm = "Non-permuted"),
             cbind(svmclass$rclass_quart[[1]]$pperm[, ncol(svmclass$rclass_quart[[1]]$pperm)], variable = names(svmclass$rclass_quart)[1], perm = "Permuted"))
# 2
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[2]]$tperm[, ncol(svmclass$rclass_quart[[2]]$tperm)], variable = names(svmclass$rclass_quart)[2], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[2]]$pperm[, ncol(svmclass$rclass_quart[[2]]$pperm)], variable = names(svmclass$rclass_quart)[2], perm = "Permuted"))
# 3
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[3]]$tperm[, ncol(svmclass$rclass_quart[[3]]$tperm)], variable = names(svmclass$rclass_quart)[3], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[3]]$pperm[, ncol(svmclass$rclass_quart[[3]]$pperm)], variable = names(svmclass$rclass_quart)[3], perm = "Permuted"))
# 4
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[4]]$tperm[, ncol(svmclass$rclass_quart[[4]]$tperm)], variable = names(svmclass$rclass_quart)[4], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[4]]$pperm[, ncol(svmclass$rclass_quart[[4]]$pperm)], variable = names(svmclass$rclass_quart)[4], perm = "Permuted"))
# 5
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[5]]$tperm[, ncol(svmclass$rclass_quart[[5]]$tperm)], variable = names(svmclass$rclass_quart)[5], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[5]]$pperm[, ncol(svmclass$rclass_quart[[5]]$pperm)], variable = names(svmclass$rclass_quart)[5], perm = "Permuted"))
# 6
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[6]]$tperm[, ncol(svmclass$rclass_quart[[6]]$tperm)], variable = names(svmclass$rclass_quart)[6], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[6]]$pperm[, ncol(svmclass$rclass_quart[[6]]$pperm)], variable = names(svmclass$rclass_quart)[6], perm = "Permuted"))
# 7
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[7]]$tperm[, ncol(svmclass$rclass_quart[[7]]$tperm)], variable = names(svmclass$rclass_quart)[7], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[7]]$pperm[, ncol(svmclass$rclass_quart[[7]]$pperm)], variable = names(svmclass$rclass_quart)[7], perm = "Permuted"))
# 8
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[8]]$tperm[, ncol(svmclass$rclass_quart[[8]]$tperm)], variable = names(svmclass$rclass_quart)[8], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[8]]$pperm[, ncol(svmclass$rclass_quart[[8]]$pperm)], variable = names(svmclass$rclass_quart)[8], perm = "Permuted"))
# 9
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[9]]$tperm[, ncol(svmclass$rclass_quart[[9]]$tperm)], variable = names(svmclass$rclass_quart)[9], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[9]]$pperm[, ncol(svmclass$rclass_quart[[9]]$pperm)], variable = names(svmclass$rclass_quart)[9], perm = "Permuted"))
# 10 
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[10]]$tperm[, ncol(svmclass$rclass_quart[[10]]$tperm)], variable = names(svmclass$rclass_quart)[10], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[10]]$pperm[, ncol(svmclass$rclass_quart[[10]]$pperm)], variable = names(svmclass$rclass_quart)[10], perm = "Permuted"))

tmp <- as.data.frame(tmp)
names(tmp) <- c("value", "variable", "Permuted")
tmp$value <- as.numeric(as.character(tmp$value))
tmp$value <- tmp$value / 46
tmp %>% 
  filter(Permuted == "Non-permuted") %>%
  group_by(variable) %>%
  summarise(median = median(value), lq = quantile(value, 0.025), hq = quantile(value, 0.975)) 

# higher than by random chance
median(tmp$value[tmp$Permuted == "Permuted"])

## Between-core differences in oxygen penetration depth (10-116 cmbsf) 
# as well as the much stronger correlation between per-sample 
# classification accuracy and oxygen category...
tmp <- rbind(btmp$rank_abundance,
             ttmp$rank_abundance,
             qtmp$rank_abundance
) %>% 
  select(-answer) %>% 
  group_by(sample) %>%
  summarise_at(vars(fraction), mean) %>% 
  as.data.frame 
tmp$sample <- as.character(tmp$sample)
tmp$core <- NA
tmp$oxy_cat <- NA
tmp$oxy_cons <- NA
tmp$depth <- NA
for (i in seq(nrow(tmp))) {
  tmp$core[i] <- paste(strsplit(tmp$sample[i], split = "_")[[1]][1],
                       strsplit(tmp$sample[i], split = "_")[[1]][2], sep = "_")
  tmp$oxy_cat[i] <- det$rmap$oxy_cat[which(det$rmap$rowname == tmp$sample[i])]
  tmp$oxy_cons[i] <- det$rmap$oxy_cons[which(det$rmap$rowname == tmp$sample[i])]
  tmp$depth[i] <- det$rmap$depth[which(det$rmap$rowname == tmp$sample[i])]
}
tmp <- tmp[order(tmp$fraction, decreasing = T), ]

cor.test(tmp$fraction, tmp$oxy_cat, method = "spearman") # with oxygen category
cor.test(tmp$fraction, as.numeric(tmp$depth), method = "spearman") # with sediment depth

## Classification rates tend to plateau using only a fraction of the class’s OTUs...
.extract_svm_opt_acc <- function(svmobj, n = 11) {
  if (n > 11) stop("n must be <= 11!")
  
  out <- as.data.frame(matrix(NA, ncol = n, nrow = 2 * nrow(svmobj[[1]]$tperm)))
  for (i in seq(n)) {
    tmp <- svmobj[[i]]
    out[, i] <- rbind(tmp$tperm[tmp$opt_otus], tmp$pperm[tmp$opt_otus])
  }
  names(out) <- names(svmobj)[1:n]
  out$Permuted <- c(rep("Base", nrow(svmobj[[1]]$tperm)),
                    rep("Permuted", nrow(svmobj[[1]]$pperm)))
  out
}

svmclass$rclass_quart_opt_acc <- .extract_svm_opt_acc(svmclass$rclass_quart, n = 10)
tmp <- svmclass$rclass_quart_opt_acc[1:128, 1:10]
tmp <- apply(tmp, 2, as.numeric)
tmp <- tmp / 46

# tmpcls will be used further for subsequent tests, do not delete.
tmpcls <- data.frame(taxon = dimnames(tmp)[[2]], 
                     median = apply(tmp, 2, median))

e <- c(unlist(lapply(svmclass$rclass_quart, function(x) x$opt_otus)))
tmpcls$opt_OTUs <- e*20 - 10
# test
cor.test(tmpcls$opt_OTUs, tmpcls$median, method = "spearman") 

## Median classification rate at the plateau is not significantly 
# correlated with the relative abundance
tmpcls$csums <- colSums(((det$class/rowSums(det$class))*(1/1.83))[, det$classorder[c(1:5, 7:11)]])
cor.test(tmpcls$csums, tmpcls$median, method = "spearman") # test

## Moreover, classification rate is not connected to the 
# fraction of OTUs needed to reach the plateau
cor.test(tmpcls$opt_OTUs, tmpcls$median, method = "spearman")

## Classification accuracy (how often a specific sediment sample is assigned 
# correctly) is strongly correlated with oxygen category
# see comment to line 224 above.

## mean accuracy decreases significantly from categories 1-4 to 5 and 6 and then to 7...
tmp <- rbind(btmp$rank_abundance,
             ttmp$rank_abundance,
             qtmp$rank_abundance
) %>% 
  select(-answer) %>% 
  group_by(sample) %>%
  summarise_at(vars(fraction), mean) %>% 
  as.data.frame 
tmp$sample <- as.character(tmp$sample)
tmp$core <- NA
tmp$oxy_cat <- NA
tmp$oxy_cons <- NA
tmp$depth <- NA
for (i in seq(nrow(tmp))) {
  tmp$core[i] <- paste(strsplit(tmp$sample[i], split = "_")[[1]][1],
                       strsplit(tmp$sample[i], split = "_")[[1]][2], sep = "_")
  tmp$oxy_cat[i] <- det$rmap$oxy_cat[which(det$rmap$rowname == tmp$sample[i])]
  tmp$oxy_cons[i] <- det$rmap$oxy_cons[which(det$rmap$rowname == tmp$sample[i])]
  tmp$depth[i] <- det$rmap$depth[which(det$rmap$rowname == tmp$sample[i])]
}
tmp <- tmp[order(tmp$fraction, decreasing = T), ]

wilcox.test(tmp$fraction[tmp$oxy_cat %in% 1:4], 
            tmp$fraction[tmp$oxy_cat %in% 5:6], alternative = "greater") # mean accuracy in 1-4 > than in 5-6
wilcox.test(tmp$fraction[tmp$oxy_cat %in% 5:6], 
            tmp$fraction[tmp$oxy_cat == 7], alternative = "greater") # 5-6 > 7

## While approximately 63% of the samples in categories 1-4 (>25μM) 
# are correctly classified at least 80% of the time...
sum(table(tmp$oxy_cat[tmp$fraction > 0.9])[1:4]) / sum(table(tmp$oxy_cat)[1:4]) 
mean(tmp$fraction[tmp$oxy_cat == 7])

## only in three of 11 cores did organic carbon content correlate significantly with depth...
TOC_tmp <- .read_excel_allsheets("data/AMOR_CN_profiles.xlsx")
cor.test(TOC_tmp[["GS14_GC02"]]$depth_CN, TOC_tmp[["GS14_GC02"]]$organic_carbon, method = "spearman") # neg insignificant
cor.test(TOC_tmp[["GS14_GC08"]]$depth_CN, TOC_tmp[["GS14_GC08"]]$organic_carbon, method = "spearman") # pos insignificant
cor.test(TOC_tmp[["GS14_GC09"]]$depth_CN, TOC_tmp[["GS14_GC09"]]$organic_carbon, method = "spearman") # pos insignificant
cor.test(TOC_tmp[["GS14_GC12"]]$depth_CN, TOC_tmp[["GS14_GC12"]]$organic_carbon, method = "spearman") # neg significant (0.03)
cor.test(TOC_tmp[["GS15_GC01"]]$depth_CN, TOC_tmp[["GS15_GC01"]]$organic_carbon, method = "spearman") # pos insignificant
cor.test(TOC_tmp[["GS16_GC04"]]$depth_CN, TOC_tmp[["GS16_GC04"]]$organic_carbon, method = "spearman") # neg insignificant
cor.test(TOC_tmp[["GS16_GC05"]]$depth_CN, TOC_tmp[["GS16_GC05"]]$organic_carbon, method = "spearman") # neg significant (0.03)
cor.test(TOC_tmp[["GS16_GC06"]]$depth_CN, TOC_tmp[["GS16_GC06"]]$organic_carbon, method = "spearman") # neg significant (0.03)
cor.test(TOC_tmp[["GS17_GC02"]]$depth_CN, TOC_tmp[["GS17_GC02"]]$organic_carbon, method = "spearman") # no data
cor.test(TOC_tmp[["GS17_GC04"]]$depth_CN, TOC_tmp[["GS17_GC04"]]$organic_carbon, method = "spearman") # no data
cor.test(TOC_tmp[["GS17_GC05"]]$depth_CN, TOC_tmp[["GS17_GC05"]]$organic_carbon, method = "spearman") # no data

## 167 OTUs (3.8% of all) are never differentially expressed across either the 1-6 or 1-7 category pairs...
(ncol(det$rOTU) -
    length(unique(c(det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Pair"], 
                    det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Partner"])))) / 
  ncol(det$rOTU)

## These OTUs are highly abundant and diverse, accounting for 41% of all reads and spanning
# 18 phyla and 44 classes.
# 1 - sum of all differentially expressed OTUs
1 - sum(det$rOTU[, unique(c(det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Pair"], 
                            det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Partner"]))]) / sum(det$rOTU)

# taxonomic representation
# class
length(unique(det$rotu_tax[which(!rownames(det$rotu_tax) %in% 
                                   unique(c(det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Pair"], 
                                            det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Partner"]))), "class"]))
# phylum
length(unique(det$rotu_tax[which(!rownames(det$rotu_tax) %in% 
                                   unique(c(det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Pair"], 
                                            det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Partner"]))), "phylum"]))


## 71.4% of all reads, and 24 of 169 OTUs, assigned to Nitrosopumilales were not differentially expressed...
rownames(det$rotu_tax)[det$rotu_tax$order == "Ca. Nitrosopumilales"] # 169 OTUs

# 24 OTUs
table(det$rotu_tax[which(!rownames(det$rotu_tax) %in% 
                           unique(c(det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Pair"], 
                                    det$prpd_oxy_cat$long[det$prpd_oxy_cat$long$name %in% c("6_1", "7_1"), "Partner"]))), "class"])

## all 24 OTUs not differentially expressed belong to the Nitrosopumilaceae family...
# see data/arb-silva.de_align_resultlist_948641_nitrosopumilus.csv

## We find that instead the vast majority of reads ascribed to Nitrosopumilales (95.2%) 
# are differentially expressed along the length of the nitrate gradient...
NO3 <- list()
NO3$cmp <- .assign_gc_cons_id(.read_excel_allsheets("data/no3.xlsx", add_long = F), 
                              species_name = "NO3", species_depth = "depth",
                              micro_map = cmp$map, zero_threshold = 0.0001, add_long = T)

# change names of long table as well as category vaules
NO3$cmp$long$NO3_cat <- 0
names(NO3$cmp$long)[2:3] <- c("NO3", "NO3_cat")
for (i in seq(nrow(NO3$cmp$long))) {
  if (is.na(NO3$cmp$long$NO3[i])) NO3$cmp$long$NO3[i] = 8
  if (NO3$cmp$long$NO3[i] > 0.03) NO3$cmp$long$NO3_cat[i] = 1
  else if (NO3$cmp$long$NO3[i] > 0.025) NO3$cmp$long$NO3_cat[i] = 2
  else if (NO3$cmp$long$NO3[i] > 0.02) NO3$cmp$long$NO3_cat[i] = 3
  else if (NO3$cmp$long$NO3[i] > 0.015) NO3$cmp$long$NO3_cat[i] = 4
  else if (NO3$cmp$long$NO3[i] > 0.01) NO3$cmp$long$NO3_cat[i] = 5
  else if (NO3$cmp$long$NO3[i] > 0.005) NO3$cmp$long$NO3_cat[i] = 6
  else NO3$cmp$long$NO3_cat[i] = 7
}

# perform propd analysis
NO3$det <- NO3$cmp$long[which(rownames(NO3$cmp$long) %in% rownames(det$rOTU)), ]

# Run propd analysis on cmp set
NO3$prpd <- pairwise_propd(det$rOTU_cmR,
                           conditions = num2char(NO3$det$NO3_cat), 
                           propd_thetaval = 0.50,
                           mine_prp = T)
NO3$prpd <- .prpd_name_to_column(NO3$prpd)
NO3$prpd <- .prpd_reduce_union(NO3$prpd)
NO3$prpd$long <- NO3$prpd$long[which(!is.na(NO3$prpd$long$name)), ]
NO3$prpd$long$name <- unwind_tuple(NO3$prpd$long$name, NO3$cmp$long$NO3_cat)
NO3$prpd$long <- top_n_propd_pairs_batch(NO3$prpd$long, n = .25)

# check percentages
tmp <- det$rotu_tax[which(!rownames(det$rotu_tax) %in% 
                           unique(c(NO3$prpd$long[NO3$prpd$long$name %in% c("1_6", "1_7"), "Pair"], 
                                    NO3$prpd$long[NO3$prpd$long$name %in% c("1_6", "1_7"), "Partner"]))),  1:9]
sum(det$rOTU[, c(rownames(tmp)[tmp$class == "Nitrososphaeria"])]) / sum(det$class$Nitrososphaeria) # 24.7%
