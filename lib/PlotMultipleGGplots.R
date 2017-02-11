#Plot Multiple Plots of "ggplot"
#Replace ... with ggplot variables to plot multiple plots.
#cols specifies how many columns are there for the plot
#For example:
#multiplot(p.1, p.2, p.3, p.4, cols = 2)
#will plot p.1, p.2, p.3, p.4 as a two by two graph.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    
    print(plots[[1]])
  } 
  else {
    
    grid.newpage()
    
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
