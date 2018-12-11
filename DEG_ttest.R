DEG_ttest <- function(exp, samplesinfo, treatedGroup = "treated", controlGroup = "control", adjPcut = 0.05) {
  FCs <- data.frame(FC = rowMeans(exp[, which(as.character(samplesinfo$Class) == treatedGroup)]) /
                    rowMeans(exp[, which(as.character(samplesinfo$Class) == controlGroup)]))
  is.na(FCs) <- sapply(FCs, is.infinite)
  is.na(FCs) <- sapply(FCs, is.nan)
  exp <- exp[which(!is.na(FCs)),]

  Ps <- sapply(1:nrow(exp), function(x) {
  if(all(is.na(exp[x,]))) NULL
    else try(t.test(as.numeric(exp[x, which(as.character(samplesinfo$Class) == controlGroup)]),
                    as.numeric(exp[x, which(as.character(samplesinfo$Class) == treatedGroup)]))$p.value, silent = T) } )
  Ps <- unlist(Ps)
  AdjPs <- p.adjust(Ps, method = "BH", n = length(Ps))
  FCs <- data.frame(FC = rowMeans(exp[, which(as.character(samplesinfo$Class) == treatedGroup)]) /
                    rowMeans(exp[, which(as.character(samplesinfo$Class) == controlGroup)]))
  Log2FCs <- log2(FCs)
  result <- data.frame(Ps, AdjPs, FCs, Log2FCs)
  result <- result[result[, "AdjPs"] < adjPcut, ]
  result
}

DEG_ttest_random <- function(exp, samplesinfo, treatedGroup = "treated", controlGroup = "control", adjPcut = 0.05, interactions = 1000, debug = FALSE, numberOfHealthySamples, numberOfTreatedSamples) {
  write.table( x = paste( "Interaction", "#DEGs", "OutlierSamples", sep = "\t" ),
               file = paste0( "random_outlier_removing_DEGs_", treatedGroup,".txt" ),
               append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE )
  for(i in 1:interactions) {
    if(debug) print(paste("############ Interaction ", i, "############", sep = "" ))
    #### Random outlier removing####
    
    if(debug) print("Random outlier samples from healthy and treated conditions...")
    outlierHealthy <- as.vector(sample(x = samplesinfo[ which( samplesinfo$Class == controlGroup ), 1], numberOfHealthySamples, replace = TRUE))
    outlierTreated <- as.vector(sample(x = samplesinfo[ which( samplesinfo$Class == treatedGroup ), 1], numberOfTreatedSamples, replace = TRUE))
    outlierJoined <- c(outlierHealthy, outlierTreated)
    
    exp2 <- exp[,!(names(exp) %in% outlierJoined) ]
    samplesinfo2 <- samplesinfo[!(samplesinfo$Sample %in% outlierJoined),]
    
    if(debug) print("Doing the statistical analysis...")
    DEG_table2 <- DEG_ttest(exp2, samplesinfo2, treatedGroup, controlGroup, adjPcut)
    
    if(debug) print("Saving results in file...")
    write.table( x = paste( i, dim(DEG_table2)[1], paste( outlierJoined, collapse = "|" ), sep = "\t" ), file = paste0( "random_outlier_removing_DEGs_", treatedGroup,".txt" ), append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
    
    if(debug) print("############ Done ############")
    if(debug) print("##############################")
  }
  randomDEGs <- read.delim(paste0("random_outlier_removing_DEGs_", treatedGroup, ".txt"), header = T)
  
  data.frame(min = min(randomDEGs$X.DEGs), max = max(randomDEGs$X.DEGs), mean = mean(randomDEGs$X.DEGs))
}
