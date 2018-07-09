library( ggplot2 ) # package version 1.0.1
library( gplots ) # package version 2.17.0
library( RColorBrewer ) # package version 1.1.2

allDataFC <- read.delim("heatmap/DEG_pvalue_0.05_unique_CD4_comparisons_FoldChange_unique_matrix_to_heatmap_subset_genes.txt", header = TRUE, row.names = 1)

head(allDataFC)
#       shScml2_vs_shCD4 shCbx4_vs_shCD4 shKdm4b_vs_shCD4 Memory_vs_Effector
#Zeb2          0.8909227       0.6276642        0.5780944          0.1738805
#Il2ra         1.3474699       2.0158051        1.6817345          0.9662038
#Runx1         0.9135298       0.7723135        0.6258999          0.7157964

comparisonTitle <- "subset_genes_memory_effector_Cbx4_and_Scml2_comparisons_FC"
comp <-allDataFC[,c(4,1:2)]

dir <- "heatmap"
comp <- comp[ apply( comp, 1, function( row ) all( row != 0 ) ), ]

#comp.zscore = t( scale( t( comp ), center = TRUE, scale = TRUE ) )
#comp.zscore <- na.omit( comp.zscore )
comp.zscore <- as.matrix(log(comp,2)) # For FoldChange

distCor <- function( comp.zscore ) as.dist( 1 - cor( t( comp.zscore ), method = "kendall" ) )
hclustAvg <- function( comp.zscore ) hclust( comp.zscore, method = "average" )

pdf(paste( dir, comparisonTitle,".pdf", sep = "" ), width = 6, height = 8 )
hmcol = rev( colorRampPalette(brewer.pal(9, "RdBu"))(100) )

heatmap.2( comp.zscore, Rowv = TRUE, Colv = TRUE,
           hclustfun = hclustAvg,
           col = hmcol,
           #col = greenred( 70 ),
           margins = c( 11, 10 ), # For Foldchange
           #margins = c( 7, 9 ), # For biologic replicates samples
           cexRow = 1, cexCol = 1, ylab = "Genes", xlab = "Samples",
           key.xlab = "Log2 FoldChange", # key.xlab = "Z-score line",
           key.ylab = "Density", keysize = 1, key.par = list(cex=0.8),
           key.title =NA, tracecol=NA, breaks = seq(-2.5, 2.5, length.out = 101) )
dev.off()
