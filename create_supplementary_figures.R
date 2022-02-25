# All supplementary figures

# NOTE! NOTE!
# This script sources two more scripts in order to create figures S9 and S13, respectively.
# These scripts may take hourse to run. Do not source the present script unless intended.
# NOTE! NOTE!

# ====
# S1: Diversity measurements prior to removal of shallow libraries
# ====

tmp <- as.data.frame(t(cmp$postprocessed_otu_tax[, 10:ncol(cmp$postprocessed_otu_tax)]))
data.frame(shannon = diversity(tmp, index = "shannon"),
           simpson = diversity(tmp, index = "simpson"),
           rowsum = rowSums(tmp)) %>% 
  melt(id.vars = "rowsum") %>%
  ggplot(aes(x = log10(rowsum), y = value)) +
  geom_point() +
  facet_wrap(. ~ variable, scales = "free_y") +
  geom_vline(xintercept = log10(1000), colour = "red") +
  labs(x = "Log(10) library size", y = "Diversity value") # 6x4

# ====
# S2:Sensitivity analysis scatter plots
# ====

multiplot(
  # two
  sense$two$OTU_clr %>%
    pca2ggplot() %>%
    ggplot(aes(PC1, PC2, colour = as.factor(ifelse(sense$two$map$oxy_cat == 7, "anoxic", "oxic")))) +
    geom_point(size = 2, show.legend = F) +
    coord_fixed() +
    ggtitle("2 anoxic samples") +
    guides(colour = guide_legend(title = "Oxic")),
  sense$three$OTU_clr %>%
    pca2ggplot() %>%
    ggplot(aes(PC1, PC2, colour = as.factor(ifelse(sense$three$map$oxy_cat == 7, "anoxic", "oxic")))) +
    geom_point(size = 2, show.legend = F) +
    coord_fixed() +
    ggtitle("3 anoxic samples") +
    guides(colour = guide_legend(title = "Oxic")),
  sense$four$OTU_clr %>%
    pca2ggplot() %>%
    ggplot(aes(PC1, PC2, colour = as.factor(ifelse(sense$four$map$oxy_cat == 7, "anoxic", "oxic")))) +
    geom_point(size = 2, show.legend = F) +
    coord_fixed() +
    ggtitle("4 anoxic samples") +
    guides(colour = guide_legend(title = "Oxic")),
  sense$five$OTU_clr %>%
    pca2ggplot() %>%
    ggplot(aes(PC1, PC2, colour = as.factor(ifelse(sense$five$map$oxy_cat == 7, "anoxic", "oxic")))) +
    geom_point(size = 2, show.legend = F) +
    coord_fixed() +
    ggtitle("5 anoxic samples") +
    guides(colour = guide_legend(title = "Oxic")),
  sense$six$OTU_clr %>%
    pca2ggplot() %>%
    ggplot(aes(PC1, PC2, colour = as.factor(ifelse(sense$six$map$oxy_cat == 7, "anoxic", "oxic")))) +
    geom_point(size = 2) +
    coord_fixed() +
    ggtitle("6 anoxic samples") +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(title = "Oxic")),
  cols = 2
) # 8x10 in

# ====
# S3: O2 profiles
# ====

o2_tmp <- .read_excel_allsheets("data/oxygen_profiles.xlsx")
multiplot(
  .interpolate_plot_o2(o2_tmp[[3]]$O2, o2_tmp[[3]]$depth_O2, cmp$map$depth[cmp$map$core_id == 3]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS14_GC02"),
  .interpolate_plot_o2(o2_tmp[[5]]$O2, o2_tmp[[5]]$depth_O2, cmp$map$depth[cmp$map$core_id == 5]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS14_GC08"),
  .interpolate_plot_o2(o2_tmp[[6]]$O2, o2_tmp[[6]]$depth_O2, cmp$map$depth[cmp$map$core_id == 6]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS14_GC09"),
  .interpolate_plot_o2(o2_tmp[[7]]$O2, o2_tmp[[7]]$depth_O2, cmp$map$depth[cmp$map$core_id == 7]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS14_GC12"),
  .interpolate_plot_o2(o2_tmp[[8]]$O2, o2_tmp[[8]]$depth_O2, cmp$map$depth[cmp$map$core_id == 8]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS15_GC01"),
  .interpolate_plot_o2(o2_tmp[[9]]$O2, o2_tmp[[9]]$depth_O2, cmp$map$depth[cmp$map$core_id == 9]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS16_GC04"),
  .interpolate_plot_o2(o2_tmp[[10]]$O2, o2_tmp[[10]]$depth_O2, cmp$map$depth[cmp$map$core_id == 10]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS16_GC05"),
  .interpolate_plot_o2(o2_tmp[[11]]$O2, o2_tmp[[11]]$depth_O2, cmp$map$depth[cmp$map$core_id == 11]) + 
    labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS16_GC06"),
  .interpolate_plot_o2(o2_tmp[[13]]$O2, o2_tmp[[13]]$depth_O2, cmp$map$depth[cmp$map$core_id == 13]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS17_GC02"),
  .interpolate_plot_o2(o2_tmp[[14]]$O2, o2_tmp[[14]]$depth_O2, cmp$map$depth[cmp$map$core_id == 14]) + 
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS17_GC04"),
  .interpolate_plot_o2(o2_tmp[[15]]$O2, o2_tmp[[15]]$depth_O2, cmp$map$depth[cmp$map$core_id == 15]) +
    guides(colour = FALSE) + labs(y = "O2 [mM]", x = "Depth [cm]", title = "GS17_GC05"),
  cols = 3
) # 15x12 in

# ====
# S4-6: Barplots phyla, classes and orders
# ====

# See auxillary_functions folder for function definition
ggbarplot_gen(det$phylum, title = "All phyla with more than 1% relative abundance") # 12x20 in
ggbarplot_gen(det$class, title = "All classes with more than 1% relative abundance") # 12x20 in
ggbarplot_gen(det$order, title = "All orders with more than 1% relative abundance") # 12x20 in

# ====
# S7: Converging predictions
# ====

multiplot(
  # Alphaproteobacteria
  rbind(cbind(svmclass$rclass_quart$Alphaproteobacteria$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$Alphaproteobacteria$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Alphaproteobacteria", x = NULL, y = "Fraction of correct classifications"),
  # Nitrososphaeria
  rbind(cbind(svmclass$rclass_quart$Nitrososphaeria$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$Nitrososphaeria$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Nitrososphaeria", x = NULL, y = "Fraction of correct classifications"),
  # S085
  rbind(cbind(svmclass$rclass_quart$S085$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$S085$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "S085", x = NULL, y = "Fraction of correct classifications"),
  # Planctomycetacia
  rbind(cbind(svmclass$rclass_quart$Planctomycetacia$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$Planctomycetacia$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Planctomycetacia", x = NULL, y = "Fraction of correct classifications"),
  # Gammaproteobacteria
  rbind(cbind(svmclass$rclass_quart$Gammaproteobacteria$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$Gammaproteobacteria$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Gammaproteobacteria", x = NULL, y = "Fraction of correct classifications"),
  # Phycisphaerae
  rbind(cbind(svmclass$rclass_quart$Phycisphaerae$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$Phycisphaerae$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Phycisphaerae", x = NULL, y = "Fraction of correct classifications"),
  # Deltaproteobacteria
  rbind(cbind(svmclass$rclass_quart$Deltaproteobacteria$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$Deltaproteobacteria$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Deltaproteobacteria", x = NULL, y = "Fraction of correct classifications"),
  # MD2896−B214
  rbind(cbind(svmclass$rclass_quart$`MD2896-B214`$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$`MD2896-B214`$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "MD2896−B214", x = NULL, y = "Fraction of correct classifications"),
  # Chloroflexi Subdivision 5 (SAR202)
  rbind(cbind(svmclass$rclass_quart$`Chloroflexi Subdivision 5 (SAR202)`$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$`Chloroflexi Subdivision 5 (SAR202)`$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Chloroflexi Subdivision 5 (SAR202)", x = NULL, y = "Fraction of correct classifications"),
  # Pacearchaeota
  rbind(cbind(svmclass$rclass_quart$Pacearchaeota$tperm, State = "Non-permuted"),
        cbind(svmclass$rclass_quart$Pacearchaeota$pperm, State = "Permuted")) %>% 
    melt() %>% 
    mutate(value = as.numeric(as.character(value))) %>%
    mutate(value = value / 46) %>%
    ggplot(aes(x = variable, y = value, colour = State)) +
    geom_boxplot(show.legend = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = "Pacearchaeota", x = NULL, y = "Fraction of correct classifications"),
  cols = 2) # 15x15 in

# ====
# S8: Lowered threshold for anoxia
# ====
tes <- svmclass$rclass_quart_low
tes <- .merge_all_preds_max(tes, svmclass$perms[, 1:16], lowmap, "oxy_quart",  n = 10)

names(tes$summary) <- c("answer", "<5 µM", ">25 µM", "10-5 µM", "25-10 µM", "sample", "core", "oxy_cat")
tes$summary$answer <- as.character(tes$summary$answer)
tes$summary$answer[tes$summary$answer == "high"] <- ">25 µM"
tes$summary$answer[tes$summary$answer == "mid"] <- "25-10 µM"
tes$summary$answer[tes$summary$answer == "low"] <- "10-5 µM"
tes$summary$answer[tes$summary$answer == "anox"] <- "<5 µM"

tes$summary %>%
  select(-core, -oxy_cat) %>% 
  mutate(answer = factor(answer, levels = c(">25 µM", "25-10 µM", "10-5 µM", "<5 µM"))) %>%
  melt %>%
  mutate(variable = factor(variable, levels = c("<5 µM", "10-5 µM", "25-10 µM", ">25 µM"))) %>%
  ggplot(aes(x = sample, y = value, group = variable, colour = variable)) +
  geom_boxplot() +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = .5),
        panel.background = element_blank(),
        panel.grid.major = element_blank()) +
  facet_grid(. ~ answer) +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  labs(y = "Prediction", colour = "Quartic") # 6x4 in

# ====
# S9: Classification performance of dominant OTUs
# ====

# Run analyses required to create plot.
source("svm_classify_dominant_otus.R")

multiplot(
  data.frame(cs = sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83)), rank = 1:ncol(det$rOTU)) %>%
    ggplot() +
    geom_point(aes(x = rank, y = log10(cs))) +
    geom_abline(slope = 0.0006007, intercept = -3.9803957, colour = "red") +
    geom_vline(xintercept = c(ncol(det$rOTU) - length(which(log10(sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83), decreasing = T)) > -1)),
                              ncol(det$rOTU) - length(which(log10(sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83), decreasing = T)) > -0.5)),
                              ncol(det$rOTU) - length(which(log10(sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83), decreasing = T)) > 0))),
               linetype = "dashed", colour = "blue") +
    theme_light() +
    labs(x = "Cumulative rank abundance", y = "log10 relative \n abundance", tag = "A"),
  
  data.frame(
    Alphaproteobacteria = c(svmclass$rclass_quart$Alphaproteobacteria$tperm[, 16], svmclass$rclass_quart$Alphaproteobacteria$pperm[, 16]),
    OnePercent = c(hdom$ronep$tperm$prediction, hdom$ronep$pperm$prediction),
    PthreePercent = c(hdom$rzthreep$tperm$prediction, hdom$rzthreep$pperm$prediction),
    PonePercent = c(hdom$rponep$tperm$prediction, hdom$rponep$pperm$prediction),
    Permuted = c(rep("Non-permuted", 128), rep("Permuted", 128))
  ) %>% 
    rename(">1% relative \n abundance" = "OnePercent") %>%
    rename(">0.31% relative \n abundance" = "PthreePercent") %>%
    rename(">0.1% relative \n abundance" = "PonePercent") %>%
    melt %>% 
    mutate(value = as.numeric(value)/46) %>% 
    ggplot(aes(x = variable, y = value, colour = Permuted)) +
    geom_boxplot() +
    theme_light() +
    labs(y = "Fraction of correct \n classifications", x = "", tag = "B"),
  cols = 1) # 9x6 in

# ====
# S10: TOC profiles
# ====
TOC_tmp <- .read_excel_allsheets("data/toc_profiles.xlsx")
multiplot(
  ggplot(data = TOC_tmp[["GS14_GC02"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS14_GC02") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS14_GC08"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS14_GC08") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS14_GC09"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS14_GC09") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS14_GC12"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS14_GC12") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS15_GC01"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS15_GC01") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS16_GC04"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS16_GC04") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS16_GC05"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS16_GC05") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS16_GC06"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS16_GC06") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS17_GC02"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS17_GC02") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS17_GC04"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS17_GC04") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  ggplot(data = TOC_tmp[["GS17_GC05"]], aes(x = depth, y = organic_carbon)) +
    geom_line(group = 1) +
    labs(y = "% OC", x = "Depth [cm]", title = "GS17_GC05") +
    scale_x_reverse() +
    coord_flip() +
    theme_article(),
  cols = 3) # 12x12 in

# ====
# S11: Propd boxplot classes
# ====
.give.n <- function(x){
  return(c(y = median(x)*1.15, label = length(x))) 
}

ggplot(det$prpd_oxy_cat$long[-which(is.na(det$prpd_oxy_cat$long$name)), ], aes(name, theta)) +
  geom_boxplot() + 
  stat_summary(fun.data = .give.n, geom = "text", fun = median) +
  theme_article() +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank()) +
  labs(y = "Disjointed proportionality") # 6x4 in

# ====
# S12: Unaffected taxa order and family levels
# ====
# Note that this is a different version of the .unaffected_taxa_det function
# tailored to create the below plot specifically.
.unaffected_taxa_det <- function(tax_lvl, grps, min_seq = 1, sd_line = 1, taxa = NULL) {
  tmp <- lapply(grps, function(x) .det_fpairs(grp = x, negate = T, transform = F) %>% names)
  tmp <- Reduce(intersect, tmp)
  tmp <- det$rotu_tax[tmp, ] %>%
    get_unique_taxa(tax_lvl, "GS")
  if (!is.null(taxa)) tmp <- tmp[, which(names(tmp) %in% taxa)]
  tmp <- tmp[, which(colSums(tmp) > min_seq)] %>% colSums
  tmpp <- get_unique_taxa(det$rotu_tax, tax_lvl, "GS")
  tmpp <- colSums(tmpp[, which(names(tmpp) %in% names(tmp))]) 
  tmp <- tmp[names(tmpp)]
  
  df <- data.frame(Taxon = c(names(tmp), names(tmpp)),
                   Abundance = c(tmp, tmpp),
                   Group = c(rep("Deselected", length(tmp)), rep("Reference", length(tmpp))))
  df$Ratio <- c(df$Abundance[1:length(tmp)] / df$Abundance[(length(tmp) + 1):(2 * length(tmp))], 
                df$Abundance[(length(tmp) + 1):(2 * length(tmp))] / df$Abundance[1:length(tmp)])
  
  ggplot(df[1:length(tmp), ], aes(fill = Group, y = Ratio, x = factor(Taxon, levels = taxa))) +
    geom_bar(stat = "identity", colour = "black") +
    geom_text(aes(label = round(Ratio, digits = 3), y = Ratio - 0.06)) 
}

multiplot(
  # Order
  .unaffected_taxa_det("order", c("6_1", "7_1"), min_seq = 1000, sd_line = 0,
                       taxa = names(det$order)[det$orderorder[c(1:5, 7:11)]]) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2), limits = c(0, 1), expand = c(0, 0)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    coord_flip() +
    labs(tag = "A", title = "Orders"),
  # Family
  .unaffected_taxa_det("family", c("6_1", "7_1"), min_seq = 1000, sd_line = 0,
                       taxa = names(det$family)[det$familyorder[c(1:5, 7:11)]]) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2), limits = c(0, 1), expand = c(0, 0)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    coord_flip() +
    labs(tag = "B", title = "Families"),
  cols = 2)

# ====
# S13: Compare detrended and non-detrended data
# ====
# analyse detrended data (note: this script may take hours to complete)
source("svm_classify_detrended.R")

# The figure depicting results from non-detrended data is taken directly from 
# create_manuscript_figures.R (fig3)

# B: Detrended data
tmp <- rbind(cbind(class_quart_detrended[[1]]$tperm[, ncol(class_quart_detrended[[1]]$tperm)], variable = names(class_quart_detrended)[1], perm = "Non-permuted"),
             cbind(class_quart_detrended[[1]]$pperm[, ncol(class_quart_detrended[[1]]$pperm)], variable = names(class_quart_detrended)[1], perm = "Permuted"))
# 2
tmp <- rbind(tmp, cbind(class_quart_detrended[[2]]$tperm[, ncol(class_quart_detrended[[2]]$tperm)], variable = names(class_quart_detrended)[2], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[2]]$pperm[, ncol(class_quart_detrended[[2]]$pperm)], variable = names(class_quart_detrended)[2], perm = "Permuted"))
# 3
tmp <- rbind(tmp, cbind(class_quart_detrended[[3]]$tperm[, ncol(class_quart_detrended[[3]]$tperm)], variable = names(class_quart_detrended)[3], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[3]]$pperm[, ncol(class_quart_detrended[[3]]$pperm)], variable = names(class_quart_detrended)[3], perm = "Permuted"))
# 4
tmp <- rbind(tmp, cbind(class_quart_detrended[[4]]$tperm[, ncol(class_quart_detrended[[4]]$tperm)], variable = names(class_quart_detrended)[4], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[4]]$pperm[, ncol(class_quart_detrended[[4]]$pperm)], variable = names(class_quart_detrended)[4], perm = "Permuted"))
# 5
tmp <- rbind(tmp, cbind(class_quart_detrended[[5]]$tperm[, ncol(class_quart_detrended[[5]]$tperm)], variable = names(class_quart_detrended)[5], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[5]]$pperm[, ncol(class_quart_detrended[[5]]$pperm)], variable = names(class_quart_detrended)[5], perm = "Permuted"))
# 6
tmp <- rbind(tmp, cbind(class_quart_detrended[[6]]$tperm[, ncol(class_quart_detrended[[6]]$tperm)], variable = names(class_quart_detrended)[6], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[6]]$pperm[, ncol(class_quart_detrended[[6]]$pperm)], variable = names(class_quart_detrended)[6], perm = "Permuted"))
# 7
tmp <- rbind(tmp, cbind(class_quart_detrended[[7]]$tperm[, ncol(class_quart_detrended[[7]]$tperm)], variable = names(class_quart_detrended)[7], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[7]]$pperm[, ncol(class_quart_detrended[[7]]$pperm)], variable = names(class_quart_detrended)[7], perm = "Permuted"))
# 8
tmp <- rbind(tmp, cbind(class_quart_detrended[[8]]$tperm[, ncol(class_quart_detrended[[8]]$tperm)], variable = names(class_quart_detrended)[8], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[8]]$pperm[, ncol(class_quart_detrended[[8]]$pperm)], variable = names(class_quart_detrended)[8], perm = "Permuted"))
# 9
tmp <- rbind(tmp, cbind(class_quart_detrended[[9]]$tperm[, ncol(class_quart_detrended[[9]]$tperm)], variable = names(class_quart_detrended)[9], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[9]]$pperm[, ncol(class_quart_detrended[[9]]$pperm)], variable = names(class_quart_detrended)[9], perm = "Permuted"))
# 10 
tmp <- rbind(tmp, cbind(class_quart_detrended[[10]]$tperm[, ncol(class_quart_detrended[[10]]$tperm)], variable = names(class_quart_detrended)[10], perm = "Non-permuted"))
tmp <- rbind(tmp, cbind(class_quart_detrended[[10]]$pperm[, ncol(class_quart_detrended[[10]]$pperm)], variable = names(class_quart_detrended)[10], perm = "Permuted"))

tmp <- as.data.frame(tmp)
names(tmp) <- c("value", "variable", "Permuted")
tmp$value <- as.numeric(as.character(tmp$value))
tmp$value <- tmp$value / 46
tmp$variable <- factor(tmp$variable, levels = names(det$class)[det$classorder[c(1:5, 7:11)]])
figs$quart_detrended <- 
  tmp %>% 
  ggplot(aes(x = variable, y = as.numeric(value))) +
  geom_boxplot(aes(colour = Permuted), show.legend = F) +
  coord_flip() +
  theme_light() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(0.1, 0.75, 0.1), limits = c(0.05, 0.75), expand = c(0, 0)) +
  labs(y = "Fraction of correct predictions", tag = "B", title = "Detrended data")

egg::ggarrange(figs$rsvm_fullpred + labs(tag = "A"), 
               figs$quart_detrended, nrow = 1) # 10x4 in

# ====
# S14: NO3 profiles
# ====
# Function to interpolate and plot NO3 concentrations
.interpolate_plot_NO3 <- function(NO3, dNO3, d16s, threshold = 0.001) {
  
  # Find indices to use in interpolation
  nonzero <- c(which(NO3 > 0), max(which(NO3 > 0) + 1))
  
  micro_NO3 <- c(rep(0, length(d16s))) # placeholder vector for microbial NO3 concs
  d16s <- as.numeric(d16s)
  
  # produce nls model and coeff estimates
  mod <- lm(NO3[nonzero] ~ dNO3[nonzero])
  icpt = coef(mod)[1]
  slope = coef(mod)[2]
  
  # calculate NO3 concentrations at corresponding microbial sampling horizons
  for (i in seq(micro_NO3)) {
    micro_NO3[i] <- icpt + slope * d16s[i]
  }
  
  # nullify any measurement below threshold (including negative values)
  micro_NO3[which(micro_NO3 < threshold)] <- 0
  
  NO3_out <- data.frame(depth = d16s, NO3 = micro_NO3)
  NO3_in <- data.frame(depth = dNO3, NO3 = NO3)
  
  require("ggplot2")
  ggplot() +
    # add NO3 measurement data
    geom_line(data = NO3_in, aes(x = depth, y = NO3, colour = "measured")) +
    # add interpolated 16s NO3 conc
    geom_line(data = NO3_out, aes(x = depth, y = NO3, colour = "interpolated")) +
    geom_hline(yintercept = 0.03) +
    geom_hline(yintercept = 0.025) +
    geom_hline(yintercept = 0.02) +
    geom_hline(yintercept = 0.015) +
    geom_hline(yintercept = 0.01) +
    geom_hline(yintercept = 0.005) +
    scale_x_reverse() +
    coord_flip()
  
}

NO3$raw <- .read_excel_allsheets("data/no3.xlsx")
multiplot(
  .interpolate_plot_NO3(NO3$raw[[1]]$NO3, NO3$raw[[1]]$depth, cmp$map$depth[cmp$map$core_id == 3]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS14_GC02"),
  .interpolate_plot_NO3(NO3$raw[[2]]$NO3, NO3$raw[[2]]$depth, cmp$map$depth[cmp$map$core_id == 5]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS14_GC08"),
  .interpolate_plot_NO3(NO3$raw[[3]]$NO3, NO3$raw[[3]]$depth, cmp$map$depth[cmp$map$core_id == 6]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS14_GC09"),
  .interpolate_plot_NO3(NO3$raw[[4]]$NO3, NO3$raw[[4]]$depth, cmp$map$depth[cmp$map$core_id == 7]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS14_GC12"),
  .interpolate_plot_NO3(NO3$raw[[5]]$NO3, NO3$raw[[5]]$depth, cmp$map$depth[cmp$map$core_id == 8]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS15_GC01"),
  .interpolate_plot_NO3(NO3$raw[[6]]$NO3, NO3$raw[[6]]$depth, cmp$map$depth[cmp$map$core_id == 9]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS16_GC04"),
  .interpolate_plot_NO3(NO3$raw[[7]]$NO3, NO3$raw[[7]]$depth, cmp$map$depth[cmp$map$core_id == 10]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS16_GC05"),
  .interpolate_plot_NO3(NO3$raw[[8]]$NO3, NO3$raw[[8]]$depth, cmp$map$depth[cmp$map$core_id == 11]) + 
    labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS16_GC06"),
  .interpolate_plot_NO3(NO3$raw[[9]]$NO3, NO3$raw[[9]]$depth, cmp$map$depth[cmp$map$core_id == 13]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS17_GC02"),
  .interpolate_plot_NO3(NO3$raw[[10]]$NO3, NO3$raw[[10]]$depth, cmp$map$depth[cmp$map$core_id == 14]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS17_GC04"),
  .interpolate_plot_NO3(NO3$raw[[11]]$NO3, NO3$raw[[11]]$depth, cmp$map$depth[cmp$map$core_id == 15]) + 
    guides(colour = FALSE) + labs(y = "NO3 [mM]", x = "Depth [cm]", title = "GS17_GC05"),
  cols = 3
)

