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
  ranks <- read.table(ranksOfGenes, header = TRUE, colClasses = c("character", "numeric"))
  ranks <- setNames(ranks[,2], ranks[,1])
  fgseaRes <- fgsea(pathways = gmtFile, stats = ranks, minSize = minSizeGroup, maxSize = maxSizeGroup, nperm = npermGroup)
  if(filterPathways) {
    fgseaRes <- fgseaRes[which(fgseaRes$padj <= pFilter),]
  }
  return(fgseaRes)
}

