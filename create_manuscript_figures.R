# Create all figures to enter the manuscript

figs <- list()

# define colour palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Figures 1 and 2 are not made in R.

# ====
# 3: SVM results figure
# ====

tmp <- rbind(cbind(svmclass$rclass_quart[[1]]$tperm[, ncol(svmclass$rclass_quart[[1]]$tperm)], variable = names(svmclass$rclass_quart)[1], perm = "No"),
             cbind(svmclass$rclass_quart[[1]]$pperm[, ncol(svmclass$rclass_quart[[1]]$pperm)], variable = names(svmclass$rclass_quart)[1], perm = "Yes"))
# 2
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[2]]$tperm[, ncol(svmclass$rclass_quart[[2]]$tperm)], variable = names(svmclass$rclass_quart)[2], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[2]]$pperm[, ncol(svmclass$rclass_quart[[2]]$pperm)], variable = names(svmclass$rclass_quart)[2], perm = "Yes"))
# 3
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[3]]$tperm[, ncol(svmclass$rclass_quart[[3]]$tperm)], variable = names(svmclass$rclass_quart)[3], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[3]]$pperm[, ncol(svmclass$rclass_quart[[3]]$pperm)], variable = names(svmclass$rclass_quart)[3], perm = "Yes"))
# 4
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[4]]$tperm[, ncol(svmclass$rclass_quart[[4]]$tperm)], variable = names(svmclass$rclass_quart)[4], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[4]]$pperm[, ncol(svmclass$rclass_quart[[4]]$pperm)], variable = names(svmclass$rclass_quart)[4], perm = "Yes"))
# 5
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[5]]$tperm[, ncol(svmclass$rclass_quart[[5]]$tperm)], variable = names(svmclass$rclass_quart)[5], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[5]]$pperm[, ncol(svmclass$rclass_quart[[5]]$pperm)], variable = names(svmclass$rclass_quart)[5], perm = "Yes"))
# 6
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[6]]$tperm[, ncol(svmclass$rclass_quart[[6]]$tperm)], variable = names(svmclass$rclass_quart)[6], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[6]]$pperm[, ncol(svmclass$rclass_quart[[6]]$pperm)], variable = names(svmclass$rclass_quart)[6], perm = "Yes"))
# 7
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[7]]$tperm[, ncol(svmclass$rclass_quart[[7]]$tperm)], variable = names(svmclass$rclass_quart)[7], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[7]]$pperm[, ncol(svmclass$rclass_quart[[7]]$pperm)], variable = names(svmclass$rclass_quart)[7], perm = "Yes"))
# 8
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[8]]$tperm[, ncol(svmclass$rclass_quart[[8]]$tperm)], variable = names(svmclass$rclass_quart)[8], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[8]]$pperm[, ncol(svmclass$rclass_quart[[8]]$pperm)], variable = names(svmclass$rclass_quart)[8], perm = "Yes"))
# 9
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[9]]$tperm[, ncol(svmclass$rclass_quart[[9]]$tperm)], variable = names(svmclass$rclass_quart)[9], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[9]]$pperm[, ncol(svmclass$rclass_quart[[9]]$pperm)], variable = names(svmclass$rclass_quart)[9], perm = "Yes"))
# 10 
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[10]]$tperm[, ncol(svmclass$rclass_quart[[10]]$tperm)], variable = names(svmclass$rclass_quart)[10], perm = "No"))
tmp <- rbind(tmp, cbind(svmclass$rclass_quart[[10]]$pperm[, ncol(svmclass$rclass_quart[[10]]$pperm)], variable = names(svmclass$rclass_quart)[10], perm = "Yes"))

tmp <- as.data.frame(tmp)
names(tmp) <- c("value", "variable", "randomisation")
tmp$value <- as.numeric(as.character(tmp$value))
tmp$value <- tmp$value / 46
tmp$variable <- factor(tmp$variable, levels = names(det$class)[det$classorder[c(1:5, 7:11)]])
figs$rsvm_fullpred <- 
  tmp %>% 
  ggplot(aes(x = variable, y = as.numeric(value))) +
  geom_boxplot(aes(colour = randomisation)) +
  coord_flip() +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.1), limits = c(0.05, 0.9), expand = c(0, 0)) +
  labs(y = "Fraction of correct classifications", colour = "Oxic state\nrandomised")
figs$rsvm_fullpred # 6x4 in

# ====
# 4: SVM samplewise classification rate
# ====
# See helper functions in svm_output_helpers.R

## Set up figure input
qtmp <- svmclass$rclass_quart
qtmp <- .merge_all_preds_max(svmobj = qtmp, perms = svmclass$perms, 
                              map = det$rmap, mode = "oxy_quart", n = 10)
ttmp <-  svmclass$rclass_tert
ttmp <- .merge_all_preds_max(svmobj = ttmp, perms = svmclass$perms, 
                              map = det$rmap, mode = "oxy_tert", n = 10)
btmp <-  svmclass$rclass_bin
btmp <- .merge_all_preds_max(svmobj = btmp, perms = svmclass$perms, 
                             map = det$rmap, mode = "oxy_bin", n = 10)

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
for (i in seq(nrow(tmp))) {
  tmp$core[i] <- paste(strsplit(tmp$sample[i], split = "_")[[1]][1],
                       strsplit(tmp$sample[i], split = "_")[[1]][2], sep = "_")
  tmp$oxy_cat[i] <- det$rmap$oxy_cat[which(det$rmap$rowname == tmp$sample[i])]
}
tmp <- tmp[order(tmp$fraction, decreasing = T), ]
tmp$oxy_cat <- paste("Category", tmp$oxy_cat, sep = " ")
tmp$sample <- factor(tmp$sample, levels = tmp$sample)

figs$rankab_point <- 
  ggplot(tmp, aes(sample, fraction, colour = as.character(oxy_cat))) +
  geom_hline(yintercept = seq(0.9, 0.1, -0.1), alpha = 0.3) +
  geom_point(size = 3) +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10, limits = c(0.1, 1), expand = c(0, 0)) +
  theme(axis.text.x = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  labs(y = "Mean accuracy", x = "Sample", 
       tag = "A", colour = "Oxygen \n category") 
figs$rankab_dens <- 
  ggplot(tmp, aes(y = fraction, group = as.character(oxy_cat), 
                  colour = as.character(oxy_cat), fill = as.character(oxy_cat))) +
  geom_density(alpha = 0.3) +
  scale_color_manual(values = cbbPalette, 
                     labels = c("1 (>150 µM)", "2 (150-100 µM)", "3 (100-50 µM)", 
                                "4 (50-25 µM)", "5 (25-10 µM)", "6 (10-5 µM)", "7 (<5 µM)")) +
  scale_fill_manual(values = cbbPalette,
                    labels = c("1 (>150 µM)", "2 (150-100 µM)", "3 (100-50 µM)", 
                               "4 (50-25 µM)", "5 (25-10 µM)", "6 (10-5 µM)", "7 (<5 µM)")) +
  scale_y_continuous(n.breaks = 10, limits = c(0.1, 1), expand = c(0,0)) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) +
  labs(y = "Fraction of correct predictions", x = "", tag = "",
       fill = "Oxygen \n category", colour = "Oxygen \n category")

# Create density plot as box plot
figs$rankab_box <- ggplot(tmp, aes(x = oxy_cat, y = fraction, colour = oxy_cat)) +
  geom_boxplot(show.legend = F) +
  theme_bw() +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  labs(x = "", y = "Average accuracy", tag = "A")

# Multi-figure showing misclassifications for each config
names(btmp$summary) <- c("answer", ">10 µM", "<10 µM", "sample", "core", "oxy_cat")
btmp$summary$answer <- as.character(btmp$summary$answer)
btmp$summary$answer[which(btmp$summary$answer == "high")] <- ">10 µM"
btmp$summary$answer[which(btmp$summary$answer == "loan")] <- "<10 µM"

names(ttmp$summary) <- c("answer", "<5 µM", ">10 µM", "10-5 µM", "sample", "core", "oxy_cat")
ttmp$summary$answer <- as.character(ttmp$summary$answer)
ttmp$summary$answer[ttmp$summary$answer == "high"] <- ">10 µM"
ttmp$summary$answer[ttmp$summary$answer == "low"] <- "10-5 µM"
ttmp$summary$answer[ttmp$summary$answer == "anox"] <- "<5 µM"

names(qtmp$summary) <- c("answer", "<5 µM", ">25 µM", "10-5 µM", "25-10 µM", "sample", "core", "oxy_cat")
qtmp$summary$answer <- as.character(qtmp$summary$answer)
qtmp$summary$answer[qtmp$summary$answer == "high"] <- ">25 µM"
qtmp$summary$answer[qtmp$summary$answer == "mid"] <- "25-10 µM"
qtmp$summary$answer[qtmp$summary$answer == "low"] <- "10-5 µM"
qtmp$summary$answer[qtmp$summary$answer == "anox"] <- "<5 µM"

figs$svm_bin_smry <- 
  btmp$summary %>%
  select(-core, -oxy_cat) %>% 
  mutate(answer = factor(answer, levels = c(">10 µM", "<10 µM"))) %>%
  melt %>%
  mutate(variable = factor(variable, levels = c("<10 µM", ">10 µM"))) %>%
  ggplot(aes(x = sample, y = value, group = variable, colour = variable)) +
  geom_boxplot() +
  coord_flip() +
  theme_light() +
  theme(axis.text.y = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = .5),
        panel.background = element_blank(),
        panel.grid.major = element_blank()) +
  facet_grid(. ~ answer) +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  labs(tag = "B", y = "", x = "Prediction", colour = "Binary")
figs$svm_tert_smry <- 
  ttmp$summary %>%
  select(-core, -oxy_cat) %>% 
  mutate(answer = factor(answer, levels = c(">10 µM", "10-5 µM", "<5 µM"))) %>%
  melt %>%
  mutate(variable = factor(variable, levels = c("<5 µM", "10-5 µM", ">10 µM"))) %>%
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
  labs(tag = "C", y = "Sample-wise fraction of correct classifications", colour = "Ternary")
figs$svm_quart_smry <- 
  qtmp$summary %>%
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
  labs(tag = "D", y = "", colour = "Quaternary")

# plot the above and the pred figures
dev.off()
pushViewport(viewport(layout = grid.layout(2, 50)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(figs$rankab_point, vp = vplayout(1, 1:25))
print(figs$rankab_dens, vp = vplayout(1, 25:50))
print(figs$svm_bin_smry, vp = vplayout(2, 1:11))
print(figs$svm_tert_smry, vp = vplayout(2, 12:28))
print(figs$svm_quart_smry, vp = vplayout(2, 29:50)) # 9x6 in
# Incremental graphical changes, like colour changes, have been made to the printed figure

# Using the boxplot instead of the point/density plots
dev.off()
pushViewport(viewport(layout = grid.layout(2, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(figs$rankab_box, vp = vplayout(1, 1:3))
print(figs$svm_bin_smry, vp = vplayout(2, 1))
print(figs$svm_tert_smry, vp = vplayout(2, 2))
print(figs$svm_quart_smry, vp = vplayout(2, 3)) # 9x6 in

# ====
# 5: Classification rates versus differential abundance
# ====
# A
figs$OTU_6_box <- data.frame(OTU_6 = det$rOTU_clr$OTU_6,
                             O2 = det$rmap$oxy_cons,
                             oxy_cat = det$rmap$oxy_cat) %>% 
  ggplot(aes(x = oxy_cat, y = OTU_6, group = oxy_cat, colour = as.factor(oxy_cat))) +
  geom_boxplot(show.legend = F, outlier.shape = NA) +
  geom_jitter(show.legend = F) +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  theme_light() +
  scale_y_continuous(limits = c(-0.4, 11), breaks = seq(0, 12.5, 2.5)) +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  labs(y = "Relative abundance", x = NULL, 
       title = "OTU_6 (Never differentially abundant)", 
       subtitle = "Nitrososphaeria (High accuracy)")
# B
figs$OTU_821_box <- data.frame(OTU_821 = det$rOTU_clr$OTU_821,
                               O2 = det$rmap$oxy_cons,
                               oxy_cat = det$rmap$oxy_cat) %>% 
  ggplot(aes(x = oxy_cat, y = OTU_821, group = oxy_cat, colour = as.factor(oxy_cat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(show.legend = F) +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(-0.4, 7.5), breaks = seq(0, 12.5, 2.5)) +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  labs(y = NULL, x = "Oxygen category", colour = "Oxygen\n category",
       title = "OTU_821 (Often differentially abundant)", 
       subtitle = "SAR202 (High accuracy)")

# Use GS16_GC04 as sample core and plot O2 and NO3 vs the proportions
tmp <- data.frame(
  OTU_6 = det$rOTU_clr[grepl("GS16_GC04", rownames(det$rOTU_clr)), "OTU_6"],
  OTU_821 = det$rOTU_clr[grepl("GS16_GC04", rownames(det$rOTU_clr)), "OTU_821"],
  depth = as.numeric(det$rmap$depth[grepl("GS16_GC04", rownames(det$rOTU_clr))])
)
# C
# load necessary O2 and NO3 profiles
o2_tmp <- .read_excel_allsheets("data/oxygen_profiles.xlsx")
NO3 <- list()
NO3$raw <- .read_excel_allsheets("data/no3.xlsx")
# make subplot
figs$diffab_samplecore <- ggplot() +
  geom_line(data = o2_tmp$GS16_GC04, aes(x = depth_O2, y = 50*O2, colour = "50*O2")) +
  geom_line(data = NO3$raw$GS16_GC04, aes(x = depth, y = 50*NO3, colour = "50*NO3")) +
  geom_point(data = tmp, aes(x = depth, y = OTU_6, colour = "OTU_6")) +
  geom_line(data = tmp, aes(x = depth, y = OTU_6, colour = "OTU_6")) +
  geom_point(data = tmp, aes(x = depth, y = OTU_821, colour = "OTU_821")) +
  geom_line(data = tmp, aes(x = depth, y = OTU_821, colour = "OTU_821")) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_x_reverse(limits = c(200, 0)) +
  labs(x = "Sediment depth", y = "50*[NO3/O2] or relative abundance", shape = NULL, 
       colour = "Variable", title = "GS16_GC04 (sample core)", subtitle = "") +
  theme_light()

# Compile and plot figure
egg::ggarrange(plots = list(figs$OTU_6_box, figs$OTU_821_box, figs$diffab_samplecore), 
               ncol = 3, labels = c("A", "B", "C"), widths = c(0.4, 0.4, 0.2),
               label.args = list(gp = grid::gpar(fontface = "plain"))) # 12x5 in
# Figure modified in illustrator following print

# ====
# 6: Propd, unaffected classes
# ====
# See differential_expression_helpers.R for helper function.

figs$unaff_taxa <- 
  .unaffected_taxa_det("class", c("6_1", "7_1"), min_seq = 1000, sd_line = 0,
                                   taxa = names(det$class)[det$classorder[c(1:5, 7:11)]]) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2), limits = c(0, 1), expand = c(0, 0)) +
  theme(title = element_blank(),
        legend.position = "none") +
    coord_flip()

# Display plot
figs$unaff_taxa # 6x4 in
