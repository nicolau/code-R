addStudy <- function(listStudies, ID, expression, phenodata, outputdir, control) {
  if(is.null(listStudies)) {
    listStudies <- list()
  }
  listStudies[[ID]][["expression"]] <- expression
  listStudies[[ID]][["phenodata"]]  <- phenodata
  listStudies[[ID]][["control"]]    <- control
  listStudies[[ID]][["outdir"]]     <- outputdir
  return(listStudies)
}

runMDPListStudies <- function(studiesList) {
  for(id in names(studiesList)) {
    expressionFile <- studiesList[[id]][["expression"]]
    phenodataFile  <- studiesList[[id]][["phenodata"]]
    outputDir      <- studiesList[[id]][["outdir"]]
    controlLabel   <- studiesList[[id]][["control"]]

    exp       <- read.table(expressionFile, header = T, na.strings = "NA", sep = "\t")
    phenodata <- read.table(phenodataFile,  header = T, na.strings = "NA", sep = "\t")

    phenodata <- phenodata[phenodata$Sample %in% colnames(exp), ]
    exp       <- exp[,c("Symbol", as.character(phenodata$Sample))]
    phenodata <- phenodata[phenodata$Sample %in% colnames(exp), ]

    rownames(exp) <- exp[, 1]
    exp           <- exp[, 2:(ncol(exp))]

    MDP_dir <- outputDir
    if(!dir.exists(MDP_dir)) {
      dir.create(MDP_dir)
    }

    mdp.results <- mdp(data = exp, pdata = phenodata, control_lab = controlLabel,
                       file_name = paste(MDP_dir, "/MPD_", controlLabel, "_", sep=""))
  }
}

# Examples
# studies <- list()
# studies <- addStudy(listStudies = studies, ID = "GSE0001", expression = "GSE0001_expression.tsv", phenodata = "GSE0001_expression.tsv", outputdir = "GSE0001", control = "control")
# studies <- addStudy(listStudies = studies, ID = "GSE0002", expression = "GSE0002_expression.tsv", phenodata = "GSE0002_expression.tsv", outputdir = "GSE0002", control = "uninfected")
# studies <- addStudy(listStudies = studies, ID = "GSE0003", expression = "GSE0003_expression.tsv", phenodata = "GSE0003_expression.tsv", outputdir = "GSE0003", control = "healthy")
# studies <- addStudy(listStudies = studies, ID = "GSE0004", expression = "GSE0004_expression.tsv", phenodata = "GSE0004_expression.tsv", outputdir = "GSE0004", control = "HC")
# studies <- addStudy(listStudies = studies, ID = "GSE0005", expression = "GSE0005_expression.tsv", phenodata = "GSE0005_expression.tsv", outputdir = "GSE0005", control = "control")
# runMDPListStudies(studies)
