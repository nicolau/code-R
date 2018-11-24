DEG_limma <- function(exp, samplesinfo, treatedGroup = "treated", controlGroup = "control", adjPcut = 0.05) {
  design <- model.matrix(~0+samplesinfo$Class)
  colnames(design) <- gsub(".*Class", "", colnames(design))
  fit <- limma::lmFit(exp, design)
  cont <- limma::makeContrasts(paste0(treatedGroup, "-", controlGroup), levels = design)
  fit.cont <- limma::contrasts.fit(fit, cont)
  fit <- limma::eBayes(fit.cont)
  result <- limma::topTable(fit, n=Inf)
  result <- result[result[,"adj.P.Val"] < adjPcut,]
  result
}

DEG_limma_random <- function(exp, samplesinfo, treatedGroup = "treated", controlGroup = "control", adjPcut = 0.05, interactions = 1000, debug = FALSE, numberOfHealthySamples, numberOfTreatedSamples) {
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
    DEG_table2 <- DEG_limma(exp2, samplesinfo2, treatedGroup, controlGroup, adjPcut)
    
    if(debug) print("Saving results in file...")
    write.table( x = paste( i, dim(DEG_table2)[1], paste( outlierJoined, collapse = "|" ), sep = "\t" ), file = paste( "random_outlier_removing_DEGs_", treatedGroup,".txt", sep = "" ), append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
    
    if(debug) print("############ Done ############")
    if(debug) print("##############################")
  }
}
