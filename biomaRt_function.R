getGeneSymbolFromTranscriptId <- function(values, type = c("transcriptId", "geneId", "geneIdVersion", "geneSymbol", "entrezgene_id"), organism = c("mmusculus", "hsapiens"), onlyProteinCoding = FALSE, onlyGeneSymbol = FALSE, hostname = "https://uswest.ensembl.org") {
  require(biomaRt)
  att <- NULL # Attributes to show as results
  symbol <- NULL
  ensembl <- NULL
  if(organism == "mmusculus") { 
    symbol <- "mgi_symbol"
    ensembl<-  useMart("ensembl", dataset="mmusculus_gene_ensembl", host=hostname)
  }
  else if(organism == "hsapiens") {
    symbol <- "hgnc_symbol"
    ensembl<-  useMart("ensembl", dataset="hsapiens_gene_ensembl", host=hostname)
  }
  
  filterData <- NULL
  if(type == "transcriptId") {
    filterData <- "ensembl_transcript_id"
    att <- c("ensembl_transcript_id")
  }
  else if(type == "geneId") { 
    filterData <- "ensembl_gene_id"
    att <- c("ensembl_gene_id")   
  }
  else if(type == "geneIdVersion") { 
    filterData <- "ensembl_gene_id_version"
    att <- c("ensembl_gene_id_version")   
  }
  else if(type == "geneSymbol") {
    filterData <- symbol
    att <- c("ensembl_gene_id")
    #att <- c("ensembl_transcript_id", "ensembl_gene_id")
  }
  else if(type == "probeId") {
    filterData <- "affy_hg_u133a"
    att <- c("affy_hg_u133a")
  }
  else if(type == "entrezgene_id") {
    filterData <- "entrezgene_id"
    att <- c("entrezgene_id")
    #att <- c("ensembl_transcript_id", "ensembl_gene_id")
  }
  results <- getBM(attributes=c(symbol, "transcript_biotype", "description", "external_gene_name", att), filters = filterData, values = values, mart = ensembl)
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
