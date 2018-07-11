run_fgsea <- function( pathwaysDatabase, ranksOfGenes, minSizeGroup = 15, maxSizeGroup = 500, npermGroup = 1000, filterPathways = FALSE, pFilter = 0.05 ) {
  ##### Enrichment analysis
  ### Read GMT file
  gmtFile <- gmtPathways(pathwaysDatabase)
  
  #exclude empty spaces in gmt
  for(i in 1:length(gmtFile)){
    exclude <- -which(gmtFile[[i]]=="")
    if(length(exclude > 0)){
      gmtFile[[i]] <- gmtFile[[i]][-exclude]
    }
  }
  fgseaRes <- fgsea(pathways = examplePathways, stats = exampleRanks, minSize = 15, maxSize = 500, nperm = 1000)
  if(filterPathways) {
    fgseaRes <- fgseaRes[which(fgseaRes$padj <= pFilter),]
  }
  return(fgseaRes)
}
