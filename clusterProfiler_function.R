clusterprofiler_enrichment <- function(geneList, GMTFile, path = ".", padjCut = 0.05, qvalueCut = 0.25) {
  # libraries
  suppressMessages(suppressWarnings({
    library("clusterProfiler")
    library("GSA")
    library("reshape2")
    library("GSEABase")
    library("IRanges")
  }))
  
  # BiocManager::install("GSEABase")
  # BiocManager::install("IRanges")
  # BiocManager::install(c("IRanges", "AnnotationDbi"), version = "3.9")
  # BiocManager::install("AnnotationDbi", version = "3.8")
  # BiocManager::
  # remove.packages("IRanges")
  # running date
  
  date <- format(Sys.Date(), "%Y%m%d")
  # path <- "~/sandbox/databases/reactome"
  # GMTFile <- "ReactomePathwaysLevel3.gmt"
  gmtFile   <- file.path(path, GMTFile)
  geneSets   <- read.gmt(gmtFile)
  # message("AQUI")
  
  # geneList <- as.character(geneList[, "V1"])
  
  # run ORA
  df <- enricher(geneList,
                 minGSSize     = 10,
                 maxGSSize     = NA,
                 universe      = unique(unlist(geneSets$Symbol)),
                 pvalueCutoff  = padjCut,
                 qvalueCutoff = qvalueCut,
                 pAdjustMethod = "fdr",
                 TERM2GENE     = geneSets
  )
  
  #names(df@geneSets)
  df <- as.data.frame(df)
  
  
  # calculate overlap
  x <- colsplit(df[, 3], "\\/", c("a", "b"))
  y <- colsplit(df[, 4], "\\/", c("a", "b"))
  df <- as.data.frame(df)
  df[, "overlap"] <- round(100 * x[, 1]/y[, 1], 1)
  
  # reorder columns
  colsOrder <- c("ID", "Description", "GeneRatio", "BgRatio",
                 "overlap", "pvalue", "p.adjust", "qvalue",
                 "geneID", "Count")
  
  df <- df[, colsOrder]
  
  # write output
  # outname <- paste("clusterProfiler_testGMP_", paste(strsplit(f, "_")[[1]][1:6], collapse="_"),
  #                  "_padj_", padjCut, "_qvalue_", qvalueCut, "_", date, ".tsv", sep="")
  # dim(df)
  # write.table(df, outname, row.names=F, col.names=T, quote=F, sep="\t")
  
  
  # cat("Done\n")
  
  return(df)  
  
}
