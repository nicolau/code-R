FC_filter <- function(exp, samplesinfo, controlGroup, treatedGroup) {
  FCs <- data.frame(FC = rowMeans(exp[, which(as.character(samplesinfo$Class) == treatedGroup)]) /
    rowMeans(exp[, which(as.character(samplesinfo$Class) == controlGroup)]))

  is.na(FCs) <- sapply(FCs, is.infinite)
  is.na(FCs) <- sapply(FCs, is.nan)
  exp <- exp[which(!is.na(FCs)),]
  exp
}
