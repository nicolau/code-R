
removeLowGenesExpression <- function(exp, samplesinfo, minimumGeneExpression=0, percentageSamples=75) {

	exp         <- exp[ ,c( "Symbol", as.character( samplesinfo$Sample ) ) ]
	samplesinfo <- samplesinfo[ samplesinfo$Sample %in% colnames( exp ), ]

	#Colapse symbols that are duplicated by taking the one with higest expression
	exp$meanG     <- apply( exp[ , 2:ncol( exp ) ], 1, mean )
	exp           <- exp[ order( exp[ , 'Symbol' ], exp[ , 'meanG' ] ), ]
	exp           <- exp[ !duplicated( exp$Symbol ), ]
	rownames(exp) <- exp$Symbol

	# Get all sample without mean column (last column)
	# Run only variable minimumGeneExpression was defined
	if(percentageSamples == 0) {
		exp <- exp[ , 2:( ncol( exp ) - 1 ) ]
	} else if(percentageSamples > 0) {
		exp2 <- exp[ , 2:( ncol( exp ) - 1 ) ]
		NumberOfSamplesMinimumValueAllowed <- round( (percentageSamples/100) * ncol( exp2 ) )
		exp2 <- exp2[rowSums( exp2 <= minimumGeneExpression ) <= NumberOfSamplesMinimumValueAllowed, ]
		exp <- exp2
	}
	return(exp)
}
