getGeneSymbolFromTranscriptId <- function(values, type = c("transcriptId", "geneId", "geneSymbol"), organism = c("mmusculus", "hsapiens"), onlyProteinCoding = FALSE, onlyGeneSymbol = FALSE) {
  require(biomaRt)
  att <- NULL # Attributes to show as results
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
  if(type == "transcriptId") {
    filterData <- "ensembl_transcript_id"
    att <- c("ensembl_transcript_id") 
  }
  else if(type == "geneId") {
    filterData <- "ensembl_gene_id"
    att <- c("ensembl_gene_id", "transcript_start") 
  }
  else if(type == "geneSymbol") {
    filterData <- symbol
    att <- c("ensembl_transcript_id", "ensembl_gene_id", "transcript_start") 
  }
  results <- getBM(attributes=c(symbol, "transcript_biotype", att), filters = filterData, values = values, mart = ensembl)
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

#result <- getGeneSymbolFromTranscriptId(values = c("ENSMUST00000093162", "ENSMUST00000139143"), type = "transcriptId", organism = "mmusculus")
#result <- getGeneSymbolFromTranscriptId(values = c("ENSMUST00000093162", "ENSMUST00000139143"), type = "transcriptId", organism = "mmusculus", onlyProteinCoding = T)
#result <- getGeneSymbolFromTranscriptId(values = c("ENSMUST00000093162", "ENSMUST00000139143"), type = "transcriptId", organism = "mmusculus", onlyProteinCoding = F, onlyGeneSymbol = T)
#result <- getGeneSymbolFromTranscriptId(values = c("ENSG00000204490"), type = "geneId", organism = "hsapiens")
#result <- getGeneSymbolFromTranscriptId(values = c("Ago1"), type = "geneSymbol", organism = "mmusculus")
