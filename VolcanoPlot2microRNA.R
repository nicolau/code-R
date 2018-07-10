library(ggplot2)
library(dplyr)
library(ggrepel)
install.packages("ggrepel")

setwd("directory")

timepoint <- "2h"
microRNAs <- read.csv(file = paste( timepoint, "_control_vs_infected_all.csv", sep = "" ), header = T) # Data from DESeq2 results
microRNAs = mutate(microRNAs, sig = ifelse(pvalue < 0.05, "Pvalue<0.05", "Not Sig"))
microRNAs_sub <- microRNAs %>% filter(sig == "Pvalue<0.05")
pdf(paste("dirResults/volcanoPlot_microRNAs_", timepoint, ".pdf", sep = "" ), height = 5, width = 7)
ggplot(microRNAs, aes(log2FoldChange, -log10(pvalue))) +
  geom_hline(yintercept=-log10(0.05), linetype="dashed", color = "gray", size = 1)+
  geom_point(aes(colour=sig)) +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold")) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.position="top") +
  scale_color_manual(values=c("red", "black")) +
  geom_label_repel(aes(label = microRNAs), size = 2.5, data = microRNAs_sub) +
  ylim(0, 2) +
  xlim(-1, 1) +
  theme(legend.position = "None")
dev.off()
