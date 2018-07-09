library(VennDiagram)
plot.single.venn <- function(a1 = c( "a", "b", "c", "d", "e", "t", "g" ),
                             labels = c("Group1"),
                             file = "NULL",
                             saveGroupFile = FALSE,
                             graphicType = c("tiff", "png", "pdf")) {
  data <- NULL
  data$values <- unique(c(a1))
  
  data$a1 <- a1
  
  # Reference four-set diagram
  venn.plot <- draw.single.venn(
    area = length(data$a1),
    category = labels,
    fill = c("orange"),
    lty = "dashed",
    cex = 2,
    cat.cex = 2,
    cat.pos = c(0),
    cat.col = c("orange")
  )
  
  if(file != "NULL") {
    dev.off()
    if(graphicType == "tiff") {
      tiff(filename = file)
      grid.draw(venn.plot)
    }
    else if(graphicType == "pdf") {
      pdf(file)
      grid.draw(venn.plot)
    }
    else {
      png(filename = file)
      grid.draw(venn.plot)
    }
    dev.off()
  }
  else {
    dev.off()
    grid.draw(venn.plot)
  }
  
  if(saveGroupFile) {
    if(file != "NULL") {
      prefix <- sub("^([^.]*).*", "\\1", file)
    }
    else {
      prefix <- "groups"
    }
    write.table(data$a1, file = paste(prefix, "_G1.txt"))
  }
  
  return(data$a1)
}

plot.pairwise.venn <- function(a1 = c( "a", "b", "c", "d", "e", "t", "g" ),
                               a2 = c( "x", "b", "c", "d", "e", "f", "g" ),
                               labels = c("Group1", "Group2"),
                               file = "NULL",
                               saveGroupFile = FALSE,
                               graphicType = c("tiff", "png", "pdf")) {
  data <- NULL
  data$values <- unique(c(a1, a2))
  
  data$a1 <- a1
  data$a2 <- a2
  
  data$n12 <- intersect(a1, a2)
  
  # Reference four-set diagram
  venn.plot <- draw.pairwise.venn(
    area1 = length(data$a1),
    area2 = length(data$a2),
    length(data$n12),
    category = labels,
    fill = c("orange", "blue"),
    lty = "dashed",
    cex = 2,
    cat.cex = 2,
    cat.pos = c(0, 0),
    cat.col = c("orange", "blue")
  )
  
  if(file != "NULL") {
    dev.off()
    if(graphicType == "tiff") {
      tiff(filename = file)
      grid.draw(venn.plot)
    }
    else if(graphicType == "pdf") {
      pdf(file)
      grid.draw(venn.plot)
    }
    else {
      png(filename = file)
      grid.draw(venn.plot)
    }
    dev.off()
  }
  else {
    dev.off()
    grid.draw(venn.plot)
  }
  
  if(saveGroupFile) {
    if(file != "NULL") {
      prefix <- sub("^([^.]*).*", "\\1", file)
    }
    else {
      prefix <- "groups"
    }
    write.table(data$n12, file = paste(prefix, "_G1_vs_G2.txt"))
  }
  
  return(data$n12)
}

plot.triple.venn <- function(a1 = c( "a", "b", "c", "d", "e", "t", "g" ),
                             a2 = c( "x", "b", "c", "d", "e", "f", "g" ),
                             a3 = c( "y", "b", "c", "d", "x", "f", "g" ),
                             labels = c("Group1", "Group2", "Group3"),
                             file = "NULL",
                             saveGroupFile = FALSE,
                             graphicType = c("tiff", "png", "pdf")) {
  data <- NULL
  data$values <- unique(c(a1, a2, a3))
  
  data$a1 <- a1
  data$a2 <- a2
  data$a3 <- a3
  
  data$n12 <- intersect(a1, a2)
  data$n13 <- intersect(a1, a3)
  
  data$n23 <- intersect(a2, a3)
  
  data$n123 <- intersect(data$n12, a3)
  
  # Reference four-set diagram
  venn.plot <- draw.triple.venn(
    area1 = length(data$a1),
    area2 = length(data$a2),
    area3 = length(data$a3),
    n12 = length(data$n12),
    n13 = length(data$n13),
    n23 = length(data$n23),
    n123 = length(data$n123),
    category = labels,
    fill = c("green", "red", "blue"),
    lty = "dashed",
    cex = 2,
    cat.cex = 2,
    cat.col = c("green", "red", "blue")
  )
  
  if(file != "NULL") {
    dev.off()
    if(graphicType == "tiff") {
      tiff(filename = file)
      grid.draw(venn.plot)
    }
    else if(graphicType == "pdf") {
      pdf(file)
      grid.draw(venn.plot)
    }
    else {
      png(filename = file)
      grid.draw(venn.plot)
    }
    dev.off()
  }
  else {
    dev.off()
    grid.draw(venn.plot)
  }
  
  if(saveGroupFile) {
    if(file != "NULL") {
      prefix <- sub("^([^.]*).*", "\\1", file)
    }
    else {
      prefix <- "groups"
    }
    write.table(data$n12, file = paste(prefix, "_G1_vs_G2.txt"))
    write.table(data$n13, file = paste(prefix, "_G1_vs_G3.txt"))
    write.table(data$n23, file = paste(prefix, "_G2_vs_G3.txt"))
    write.table(data$n123, file = paste(prefix, "_G1_vs_G2_vs_G3.txt"))
  }
  
  return(data$n123)
}


plot.quad.venn <- function(a1 = c( "a", "b", "c", "d", "e", "t", "g" ),
                           a2 = c( "x", "b", "c", "d", "e", "f", "g" ),
                           a3 = c( "y", "b", "c", "d", "x", "f", "g" ),
                           a4 = c( "z", "b", "c", "d", "t", "f", "y" ),
                           labels = c("Group1", "Group2", "Group3", "Group4"),
                           file = "NULL",
                           saveGroupFile = FALSE,
                           graphicType = c("tiff", "png", "pdf")) {
  data <- NULL
  data$values <- unique(c(a1, a2, a3, a4))
  
  data$a1 <- a1
  data$a2 <- a2
  data$a3 <- a3
  data$a4 <- a4
  
  data$n12 <- intersect(a1, a2)
  data$n13 <- intersect(a1, a3)
  data$n14 <- intersect(a1, a4)
  
  data$n23 <- intersect(a2, a3)
  data$n24 <- intersect(a2, a4)
  
  data$n34 <- intersect(a3, a4)
  
  data$n123 <- intersect(data$n12, a3)
  data$n124 <- intersect(data$n12, a4)
  data$n134 <- intersect(data$n13, a4)
  
  data$n234 <- intersect(data$n23, a4)
  
  data$n1234 <- intersect(data$n123, a4)
  
  # Reference four-set diagram
  venn.plot <- draw.quad.venn(
    area1 = length(data$a1),
    area2 = length(data$a2),
    area3 = length(data$a3),
    area4 = length(data$a4),
    n12 = length(data$n12),
    n13 = length(data$n13),
    n14 = length(data$n14),
    n23 = length(data$n23),
    n24 = length(data$n24),
    n34 = length(data$n34),
    n123 = length(data$n123),
    n124 = length(data$n124),
    n134 = length(data$n134),
    n234 = length(data$n234),
    n1234 = length(data$n1234),
    category = labels,
    fill = c("orange", "red", "green", "blue"),
    lty = "dashed",
    cex = 2,
    cat.cex = 2,
    cat.col = c("orange", "red", "green", "blue")
  )
  
  if(file != "NULL") {
    dev.off()
    if(graphicType == "tiff") {
      tiff(filename = file)
      grid.draw(venn.plot)
    }
    else if(graphicType == "pdf") {
      pdf(file)
      grid.draw(venn.plot)
    }
    else {
      png(filename = file)
      grid.draw(venn.plot)
    }
    dev.off()
  }
  else {
    dev.off()
    grid.draw(venn.plot)
  }
  
  if(saveGroupFile) {
    if(file != "NULL") {
      prefix <- sub("^([^.]*).*", "\\1", file)
    }
    else {
      prefix <- "groups"
    }
    write.table(data$n12, file = paste(prefix, "_G1_vs_G2.txt"))
    write.table(data$n13, file = paste(prefix, "_G1_vs_G3.txt"))
    write.table(data$n14, file = paste(prefix, "_G1_vs_G4.txt"))
    write.table(data$n23, file = paste(prefix, "_G2_vs_G3.txt"))
    write.table(data$n24, file = paste(prefix, "_G2_vs_G4.txt"))
    write.table(data$n34, file = paste(prefix, "_G3_vs_G4.txt"))
    write.table(data$n123, file = paste(prefix, "_G1_vs_G2_vs_G3.txt"))
    write.table(data$n124, file = paste(prefix, "_G1_vs_G2_vs_G4.txt"))
    write.table(data$n134, file = paste(prefix, "_G1_vs_G3_vs_G4.txt"))
    write.table(data$n234, file = paste(prefix, "_G2_vs_G3_vs_G4.txt"))
    write.table(data$n1234, file = paste(prefix, "_G1_vs_G2_vs_G3_vs_G4.txt"))
  }
  
  return(data$n1234)
}
