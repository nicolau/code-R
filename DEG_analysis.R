DEG_analysis <- function(data, samplesinfo, nontreated = "CTRL", treated = "INF", disp, method = c("edgeR", "DESeq2"), shrinkageFC = F, verbose = F) {
  
  if(verbose) message("Filtering data expression")
  samplesinfo <- samplesinfo[which(samplesinfo$Class == nontreated | samplesinfo$Class == treated),]
  rnaseqMatrix <- data[ , c( as.character( samplesinfo$Sample ) ) ]
  rnaseqMatrix <- round(rnaseqMatrix)
  rnaseqMatrix <- rnaseqMatrix[rowSums(edgeR::cpm(rnaseqMatrix) > 1) >= 2,]
  repControl <- dim(samplesinfo[ which( samplesinfo$Class == nontreated ), ])[1]
  repInfected <- dim(samplesinfo[ which( samplesinfo$Class == treated ), ])[1]
  
  if(method == "edgeR") {
    suppressMessages(library(edgeR))
    
    if(verbose) message("Creating group factor")
    conditions = factor(c(rep(nontreated, repControl), rep(treated, repInfected)))
    
    if(verbose) message("Creating DGEList object and normalize expression")
    exp_study = edgeR::DGEList(counts=rnaseqMatrix, group=conditions)
    exp_study = edgeR::calcNormFactors(exp_study)
    
    if(verbose) message("Analyzing DEG with dispersion")
    if(disp == 0) {
      disp <- edgeR::estimateDisp(y = rnaseqMatrix)$common.dispersion
    }
    et = edgeR::exactTest(exp_study, pair=c(treated, nontreated), dispersion=disp)
    tTags = edgeR::topTags(et, n=NULL)
    result_table = tTags$table
    #result_table = data.frame(sampleA=treated, sampleB=nontreated, result_table)
    result_table$logFC = -1 * result_table$logFC
    
    return(result_table)
    # result_table %>% datatable()
  }
  else if(method == "DESeq2") {
    suppressMessages(library(DESeq2))
    suppressMessages(library(apeglm)) 
    
    colData <- samplesinfo[ which( colnames(rnaseqMatrix) == samplesinfo$Sample ), c(1, 2) ]
    colData$Class <- as.factor(colData$Class)
    
    contrasts <- c("Class", treated, nontreated)
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = rnaseqMatrix, colData = colData, design = ~Class)
    dds <- DESeq2::DESeq(dds)
    
    if(shrinkageFC) {
      res <- DESeq2::lfcShrink(dds, contrast = contrasts)
    } else {
      res <- DESeq2::results(dds, contrast = contrasts)
    }
    
    res$log2FC <- res$log2FoldChange
    res <- res[, c(1,7,2,3,4,5,6)]
    colnames(res)[3] <- ifelse(test = shrinkageFC, yes = "log2FCShrinkage", no = colnames(res)[3])
    #resApeT %>% as.data.frame() %>% datatable()
    res <- res[order(res$padj),]
    resCounts <- merge(res, counts(dds, normalized = TRUE), by = "row.names")
    return(resCounts)
  }
}
