getGeneSymbolFromTranscriptId <- function(values, type = c("transcript", "gene"), organism = c("mmusculus", "hsapiens"), onlyProteinCoding = FALSE, onlyGeneSymbol = FALSE) {
  require(biomaRt)
  att <- NULL
  symbol <- NULL
  ensembl <- NULL
  if(organism == "mmusculus") {
    symbol <- "mgi_symbol"
    ensembl<-  useMart("ensembl", dataset="mmusculus_gene_ensembl")
  }
  else if(organism == "hsapiens") {
    symbol <- "hgnc_symbol"
    ensembl<-  useMart("ensembl", dataset="hsapiens_gene_ensembl")
  }
  
  filterData <- NULL
  if(type == "transcript") {
    filterData <- "ensembl_transcript_id"
  }
  else if(type == "gene") {
    filterData <- "ensembl_gene_id"
  }
  results <- getBM(attributes=c(symbol, "transcript_biotype", filterData), filters = filterData, values = values, mart = ensembl)
  if(onlyProteinCoding) {
    if(onlyGeneSymbol) {
      return(results[results$transcript_biotype == "protein_coding",1])
    }
    else {
      return(results[results$transcript_biotype == "protein_coding",])
    }
  }
  else {
    if(onlyGeneSymbol) {
      return(results[,1])
    }
    else {
      return(results)
    }
  }
}

#result <- getGeneSymbolFromTranscriptId(values = c("ENSMUST00000093162", "ENSMUST00000139143"), type = "transcript", organism = "mmusculus")
#result <- getGeneSymbolFromTranscriptId(values = c("ENSMUST00000093162", "ENSMUST00000139143"), type = "transcript", organism = "mmusculus", onlyProteinCoding = T)
#result <- getGeneSymbolFromTranscriptId(values = c("ENSMUST00000093162", "ENSMUST00000139143"), type = "transcript", organism = "mmusculus", onlyProteinCoding = F, onlyGeneSymbol = T)
#result <- getGeneSymbolFromTranscriptId(values = c("ENSG00000204490"), type = "gene", organism = "hsapiens")
