
removeLowGenesExpression <- function(exp, minimumGeneExpression=0, percentageSamples=75, geneCol = "Symbol") {
	#Colapse symbols that are duplicated by taking the one with higest expression
	exp$meanG     <- apply( exp[ , 2:ncol( exp ) ], 1, mean )
	exp           <- exp[ order( exp[ , geneCol ], exp[ , 'meanG' ] ), ]
	exp           <- exp[ !duplicated( exp[, geneCol] ), ]
	# rownames(exp) <- exp$Symbol

	# Get all sample without mean column (last column)
	# Run only variable minimumGeneExpression was defined
	if(percentageSamples == 0) {
		exp <- exp[ , c(geneCol, 2:( ncol( exp ) - 1 ) ) ]
	} else if(percentageSamples > 0) {
		exp2 <- exp[ , c(geneCol, 2:( ncol( exp ) - 1 )) ]
		NumberOfSamplesMinimumValueAllowed <- round( (percentageSamples/100) * ncol( exp2 ) )
		exp2 <- exp2[rowSums( exp2 <= minimumGeneExpression ) <= NumberOfSamplesMinimumValueAllowed, ]
		exp <- exp2
	}
	return(exp)
}
