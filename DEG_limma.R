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
