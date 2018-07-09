library(reshape2)
library(ggplot2)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



plotBar <- function( DF, title_label = "Title Graph", xlabel = "Samples",
                     ylabel = "FPKM", pos_legend = "none", width_pdf = 12 ){
  DF1 <- melt( DF, id.var = "samples" )
  df2 <- data_summary( DF1, varname = "value", groupnames = c( "samples", "variable" ) )
  ggplot( df2, aes( x = samples, y = value, fill = samples ) ) + 
    geom_bar( stat = "identity", color = "black", position = position_dodge() ) +
    geom_errorbar( aes( ymin = value - sd, ymax = value + sd ), width = .2, position = position_dodge( .9 ) ) +
    labs( title = title_label, x = xlabel, y = ylabel ) +
    #theme_classic() +
    theme( axis.text = element_text( size = 14 ), axis.title = element_text( size = 18, face = "bold" ) ) +
    theme( legend.text = element_text( size = 16 ) ) +
    scale_fill_manual( values = c( '#999999', '#E69F00', '#999999', '#E69F00' ) )
}

# ALF1-2
DF <- data.frame( samples = c( "CW", "CW", "CWW", "CWW", "BFT", "BFT", "BFTW", "BFTW" ),
                  values = c( 432.93, 435.29, 568.58, 1336.18, 575.66, 361.53, 1018.72, 1033.33 ) )
plotBar(DF, title_label = "ALF-1")

#Lectin-2
DF <- data.frame( samples = c( "CW", "CW", "CWW", "CWW", "BFT", "BFT", "BFTW", "BFTW" ),
                  value = c(38.09, 68.79, 128.91, 230.03, 94.31, 101.39, 220.32, 251.76))
plotBar(DF, title_label = "Lectin-2")

#Actin
DF <- data.frame( samples = c( "CW", "CW", "CWW", "CWW", "BFT", "BFT", "BFTW", "BFTW" ),
                  value = c(59100.22, 44138.79, 39773.7, 46831.57, 50641.91, 53971.81, 48380.12, 36054.91))
plotBar(DF, title_label = "Actin")
