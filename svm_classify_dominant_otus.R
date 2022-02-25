# The purpose of this script is to investigate whether the hyperdominant OTUs in AMOR oxic sediments, 
# namely those whose rank-abundance is higher than log-linear, predict oxygen concentration 
# as well as expected by ecological theory, which states that these should be sensitive
# predictors of key environmental variables.

## First, define the function used to quantify classification rates.
# This function is slightly different from the one in svm_classification.R
# in that it doesn't allow for incremental OTU increases and so 
# requires fewer lines to run.
.svmclass_rotus <- function(otus, dep,
                     perms = NULL, nperm = 16, 
                     permute_dep = T) {
  # Report time
  cat("Function started", as.character(Sys.time())[1], "\n")
  
  # Set up permutations
  if (is.null(perms)) {
    perms <- matrix(ncol = nperm, nrow = round(0.25 * nrow(as.data.frame(dep))), NA) 
    for (k in seq(nperm)) {
      perms[, k] <- sample(nrow(as.data.frame(dep)), round(0.25 * nrow(as.data.frame(dep)))) 
    }
    cat("Generating", nperm, "random perms \n")
  } 
  
  # preallocate output
  preds <- list()
  
  tperm <- as.data.frame(matrix(NA, ncol = 2, nrow = ncol(perms)))
  names(tperm) <- c("prediction", "kappa"); rownames(tperm) <- paste("perm", seq(ncol(perms)))
  if (permute_dep) pperm <- tperm; rownames(pperm) <- paste0(rownames(tperm), "_p")
  
  # for each iteration
  for (p in seq(ncol(perms))) {
    cat("base permutation", p, "\n")
    
    # create data set to analyse
    train_data <- cbind(otus, dep = dep)
    # set up data subsets
    training <- train_data[-perms[, p], ]
    test <- train_data[perms[, p], ]
    
    preds[[p]] <- as.data.frame(matrix(NA, ncol = 2, nrow = nrow(test)))
    names(preds[[p]]) <- c("answer", "prediction")
    preds[[p]]$answer <- test$dep
    
    # Run analysis
    fit <- caret::train(
      dep ~ .,
      data = training,
      method = "svmRadial",
      trControl = trainControl(method = "cv", number = 10),
      tuneLength = 5, 
      scale = F)
    pred <- predict(fit, test)
    preds[[p]]$prediction <- pred
    tperm[p, 1] <- length(which(pred == test$dep))
    tperm[p, 2] <- fit$results$Kappa[as.numeric(rownames(fit$bestTune))]
    
    # if permute_dep
    if (permute_dep) {
      # create data set to analyse
      train_data$dep <- sample(train_data$dep)
      # set up data subsets
      training <- train_data[-perms[, p], ]
      test <- train_data[perms[, p], ]
      
      # Run analysis
      fit <- caret::train(
        dep ~ .,
        data = training,
        method = "svmRadial",
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 5, 
        scale = F)
      pred <- predict(fit, test)
      pperm[p, 1] <- length(which(pred == test$dep))
      pperm[p, 2] <- fit$results$Kappa[as.numeric(rownames(fit$bestTune))]
    }
    
  } # end perms
  
  out <- list(
    tperm = tperm,
    preds = preds,
    perms = perms
  )
  if (permute_dep) out[["pperm"]] <- pperm
  
  cat("Function ended", as.character(Sys.time()[1]), "\n")
  
  out
}

## Second, identify the subset of OTUs that are "off the chart" abundant.
# Determine the slope and intercept of the log-linear rank abundance regression curve
lm(log10(sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83))) ~ c(1:ncol(det$rOTU))) 

# Plot rank abundance and superposition log-linear regression curve
data.frame(cs = sort(colSums(det$OTU / rowSums(det$OTU)) * (1/1.83)), rank = 1:ncol(det$OTU)) %>%
  ggplot() +
  geom_point(aes(x = rank, y = log10(cs))) +
  geom_abline(slope = 0.0006152, intercept = -4.0511193, colour = "red")

## Select incrementally larger OTU subsets from those considerably above the regression line:

hdom <- list()
set.seed(123)
# At least 1% relative abundance
hdom$ronep <- .svmclass_rotus(otus = det$rOTU_clr[, log10(sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83), decreasing = T)) > 0],
                     dep = det$rmap[, "oxy_quart"],
                     perms = svmclass$perms)

# At least 0.316% relative abundance (10^-0.5)
hdom$rzthreep <- .svmclass_rotus(otus = det$rOTU_clr[, log10(sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83), decreasing = T)) > -0.5],
                     dep = det$rmap[, "oxy_quart"],
                     perms = svmclass$perms)

# At least 0.1% relative abundance
hdom$rponep <- .svmclass_rotus(otus = det$rOTU_clr[, log10(sort(colSums(det$rOTU / rowSums(det$rOTU)) * (1/1.83), decreasing = T)) > -1],
                     dep = det$rmap[, "oxy_quart"],
                     perms = svmclass$perms)

