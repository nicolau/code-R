
removeLowGenesVariance <- function(exp, threshold=0) {
	exp_var <- matrixStats::rowVars(exp, na.rm=TRUE)
	exp <- exp[which(exp_var != threshold),]
	return(exp)
}
