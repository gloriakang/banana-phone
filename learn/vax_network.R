library(ggplot2)
View(network_df2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

g = ggplot(data = network_df2, mapping = aes(y = avg.deg.cent, x = sentiment)) + geom_violin() + geom_text(aes(label=name)) + geom_jitter()
h = ggplot(data = network_df2, mapping = aes(y = avg.bet.cent, x = sentiment)) + geom_violin()
multiplot(g,h)


ggplot(data = network_df2, mapping = aes(y = avg.deg.cent, x = sentiment)) + geom_boxplot()

ggplot(data = network_df2, mapping = aes(y = avg.bet.cent, x = avg.deg.cent, color = sentiment)) +
  geom_point(alpha=0.5)


ggplot(data = network_df2, mapping = aes(x = sentiment, y = ))