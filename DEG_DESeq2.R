DEG_DESeq2 <- function(exp, samplesinfo, treatedGroup, controlGroup) {
  contrasts <- c("Class", treatedGroup, controlGroup)
  dds <- DESeq2::DESeqDataSetFromMatrix(countData = exp, colData = samplesinfo, design = ~ Class)
  
  dds <- DESeq2::DESeq(dds)
  res <- DESeq2::results(dds, contrast = contrasts)
  resApeT <- DESeq2::lfcShrink(dds, coef = 2, type = "apeglm", lfcThreshold = 1)
  resApeT 
}
