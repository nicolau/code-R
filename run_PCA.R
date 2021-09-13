run_PCA <- function(data, pdata, directory = ".", show_barplot = FALSE, show_scatterplot = FALSE, show_labes = FALSE){
  
  data_for_PCA <- t(data)
  
  if(show_barplot & show_scatterplot) {
    stop("Error: Not use show_barplot and show_scatterplot T in the same time.")
  }
  
  ## calculate MDS (matrix of dissimilarities)
  mds <- cmdscale(dist(data_for_PCA), k=3, eig=TRUE)
  # k = the maximum dimension of the space which the data are to be represented in
  # eig = indicates whether eigenvalues should be returned  
  
  # transform the Eigen values into percentage
  eig_pc <- mds$eig * 100 / sum(mds$eig)
  
  suppressMessages(library(ggplot2))
  eig_pc <- eig_pc[1:10]
  
  if(show_barplot) {
    p <- ggplot(data = data.frame(eigen = eig_pc, PC = paste0("Dim ", 1:length(eig_pc))), aes(x = reorder(PC, -eigen), eigen)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(text = element_text(size = 8),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      xlab("Dimensions") +
      ylab("Explained var.")
    # ggsave(filename = paste0(directory, "/MDS_explained_variance.pdf"), width = 2)
    return(p)
  }
  
  if(show_scatterplot) {
    ## calculate MDS
    mds <- as.data.frame(cmdscale(dist(data_for_PCA), k = 3)) # Performs MDS analysis
    mds$Class    <- pdata$Class
    mds$Batch    <- pdata$Batch
    colnames(mds) <- c("Dim1", "Dim2", "Dim3", "Class", "Batch")
    
    
    suppressMessages(library(ggalt))
    p <- ggplot(mds, aes(Dim1, Dim2, color = Class, shape = Batch)) +#, shape = Batch)) +
      geom_point(size=3) +
      theme_bw()
      if(show_labels) {
        p <- p + geom_label_repel(aes(label = Sample, fill = factor(Class)), color = 'white', size = 3.5)
      }
      p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(text = element_text(size = 8), legend.position = "top") +
      xlab(paste0("Dim 1 (", format(eig_pc[1], digits = 4), "% explained var.)")) +
      ylab(paste0("Dim 2 (", format(eig_pc[2], digits = 4), "% explained var.)")) +
      coord_cartesian(xlim = 1.2 * c(min(mds$Dim1), max(mds$Dim1)),
                      ylim = 1.2 * c(min(mds$Dim2), max(mds$Dim2))) +   # change axis limits
      # coord_fixed() +
      xlim(c(ceiling(min(mds$Dim1))-1,ceiling(max(mds$Dim1))+1)) +
      ylim(c(ceiling(min(mds$Dim2))-1,ceiling(max(mds$Dim2))+1))
    # ggsave(filename = paste0(directory, "/MDS_D1_vs_D2_.pdf"), width = 4, height = 4)
    return(p)
  }
}
