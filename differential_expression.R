## perform propd analysis
# See differential_expression_helpers.R for description of helper functions.
det$prpd_oxy_cat <- pairwise_propd(det$rOTU_cmR, 
                                   conditions = num2char(char2num(det$rmap$oxy_cat)), 
                                   propd_thetaval = 0.50, 
                                   mine_prp = T)
det$prpd_oxy_cat <- .prpd_name_to_column(det$prpd_oxy_cat)
det$prpd_oxy_cat$long <- NULL
det$prpd_oxy_cat$long_orig <- NULL
det$prpd_oxy_cat <- .prpd_reduce_union(det$prpd_oxy_cat)
det$prpd_oxy_cat$long$name <- unwind_tuple(det$prpd_oxy_cat$long$name, det$rmap$oxy_cat)
det$prpd_oxy_cat$long_orig <- det$prpd_oxy_cat$long
det$prpd_oxy_cat$long <- top_n_propd_pairs_batch(det$prpd_oxy_cat$long, n = .25) 
