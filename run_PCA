run_PCA <- function(data, phenodata, savebarplot = F, savescatterplot = F, directory = ".") {
  
  if(directory != "." & !dir.exists(directory)) {
    dir.create(path = directory, recursive = T)
  }
  
  # transpose the data to have variables (genes) as columns
  data_for_PCA <- t(data)

  if(savebarplot) {  
    ## calculate MDS (matrix of dissimilarities)
    mds <- cmdscale(dist(data_for_PCA), k=3, eig=TRUE)
    # k = the maximum dimension of the space which the data are to be represented in
    # eig = indicates whether eigenvalues should be returned  
    
    # transform the Eigen values into percentage
    eig_pc <- mds$eig * 100 / sum(mds$eig)
    
    suppressMessages(library(ggplot2))
    eig_pc <- eig_pc[1:10]
    ggplot(data = data.frame(eigen = eig_pc, PC = paste0("Dim ", 1:length(eig_pc))), aes(x = reorder(PC, -eigen), eigen)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(text = element_text(size = 8),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      xlab("Dimensions") +
      ylab("Explained var.")
    ggsave(filename = paste0(directory, "/MDS_explained_variance.pdf"), width = 2)
  }
  
  if(savescatterplot) {
    ## calculate MDS
    mds <- as.data.frame(cmdscale(dist(data_for_PCA), k=3)) # Performs MDS analysis
    colnames(mds) <- c("Dim1", "Dim2", "Dim3")
    
    typePlot     <- "jpg"
    mds$AgeGroup <- phenodata[, "AgeGroup"]
    mds$Class    <- phenodata[, "Class"]
    
    suppressMessages(library(ggalt))
    ggplot(mds, aes(Dim1, Dim2, color=AgeGroup, shape=Class)) +
      geom_point(size=3) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(text = element_text(size = 8)) +
      xlab(paste0("Dim 1 (", format(eig_pc[1], digits = 4), "% explained var.)")) +
      ylab(paste0("Dim 2 (", format(eig_pc[2], digits = 4), "% explained var.)")) +
      coord_cartesian(xlim = 1.2 * c(min(mds$Dim1), max(mds$Dim1)),
                      ylim = 1.2 * c(min(mds$Dim2), max(mds$Dim2))) +   # change axis limits
      coord_fixed() +
      xlim(c(ceiling(min(mds$Dim1))-1,ceiling(max(mds$Dim1))+1)) +
      ylim(c(ceiling(min(mds$Dim2))-1,ceiling(max(mds$Dim2))+1))
    ggsave(filename = paste0(directory, "/MDS_D1_vs_D2_.", typePlot), width = 4, height = 4)
    ggsave(filename = paste0(directory, "/MDS_D1_vs_D2_.pdf"), width = 4, height = 4)
  }
}
