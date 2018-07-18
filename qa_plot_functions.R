# Here, we give some functions to plot a given QC metric across subjects by site.


###
# SETUP
###

# Load some needed packages
library(grid)
library(ggplot2)
library(plyr)
library(RColorBrewer)

# CMI-Based Color Scheme
cmi_main_blue="#0071b2"
cmi_grey="#929d9e"
cmi_light_blue="#00c4d9"
cmi_pea_green="#b5bf00"

cmi_rich_green="#73933d"
cmi_rich_purple="#8e7fac"
cmi_rich_red="#d75920"
cmi_rich_blue="#4c87a1"
cmi_rich_aqua="#66c7c3"
cmi_rich_orange="#eebf42"

cmi_vibrant_yellow="#ffd457"
cmi_vibrant_orange="#f58025"
cmi_vibrant_green="#78a22f"
cmi_vibrant_garnet="#e6006f"
cmi_vibrant_purple="#9A4d9e"
cmi_vibrant_blue="#19398a"

cmi_site_colors = c(cmi_vibrant_blue,
                    cmi_rich_blue,
                    cmi_vibrant_purple,
                    cmi_vibrant_garnet,
                    cmi_rich_red,
                    cmi_vibrant_orange,
                    cmi_vibrant_yellow,
                    cmi_vibrant_green)
cmi_site_colors_ramp = colorRampPalette(cmi_site_colors)


###
# FUNCTIONS - TO FILTER DATA
###

remove_nas <- function(df, measure) {
    na_inds <- is.na(df[[measure]])
    cat(sprintf("...removing %i points with NA values\n", sum(na_inds)))
    df      <- df[!na_inds,]
    return(df)
}

get_outlier_inds <- function(dat, times.iqr=3) {
    # We figure out the lower and upper limit of acceptable data
    # similar to the approach taken with Tukey box plots
    upper.limit <- quantile(dat, 0.75) + times.iqr*IQR(dat)
    lower.limit <- quantile(dat, 0.25) - times.iqr*IQR(dat)
    # and remove the rows that are outside this bound
    inds    <- (dat > upper.limit) | (dat < lower.limit)
    return(inds)
}

# Sometimes extreme data-points can skew the plot 
# and make it difficult to see the spread of the data.
# If requested, we can remove these points
# Note: this only removes outliers for a given measure
remove_outliers <- function(df, measure, times.iqr=3) {
    dat     <- df[[measure]]    
    inds    <- get_outlier_inds(dat, times.iqr)
    df      <- df[!inds,]
    cat(sprintf("...removed %i outlier points\n", sum(inds)))
    
    return(df)
}


###
# FUNCTIONS - RELATED TO PERCENTILES
###


# In preperation for plotting the percentile lines
# we calculate the percentiles in advance
# and have some code to do the plotting for later
# We will be looking at 1%, 5%, 25%, 50%, 75%, 95%, & 99%
calc_percentiles <- function(df, measure) {
    # In our plots, we want to have percentile lines to indicate the 
    # distribution of each site relative to the whole sample
    qvals       <- c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
    qcat        <- c(1,5,25,50,25,5,1)
    qline       <- c(3, 2, 5, 1, 5, 2, 3)
    qsize       <- c(.4, .25, .3, .25, .3, .25, .4)
    qcols       <- c("grey10", "grey10", "grey10", "grey50", "grey10", "grey10", "grey10")
    
    # Get the percentiles
    percentiles <- quantile(df[[measure]], qvals, na.rm=T)
    
    # Merge with name (qcat), line type (qline), and line width (qsize)
    # There's a weird error if I include qcols so won't do that here
    percentiles_df          <- as.data.frame(cbind(percentiles, qcat, qline, qsize))
    percentiles_df$qline    <- as.factor(qline)
    percentiles_df$qcat     <- as.factor(qcat)
    attr(percentiles_df, "qcols") <- qcols
    
    return(percentiles_df)
}

# This function will add percentile lines in the background
# plot: ggplot object
# pdf: percentile data frame
compile_percentiles <- function(pdf, measure) {
  cols <- attr(pdf, "qcols")
  ret <- lapply(1:nrow(pdf), function(i) {
    p <- pdf[i,]
    if (!is.null(cols)) {
      plot <- geom_hline(aes(yintercept=percentiles), data=p, 
                         size=as.numeric(p$qsize), linetype=as.numeric(p$qline), 
                         color=cols[i])
      #as.character(p$qcolor[1])
    } else {
      plot <- geom_hline(aes(yintercept=percentiles), data=p, 
                         size=as.numeric(p$qsize[1]), linetype=as.numeric(p$qline[1]), 
                         color="grey50")
    }
    return(plot)
  })
  return(ret)
}


###
# FUNCTIONS - TO DO THE PLOTTING
###


# Now we finally have one function that does the plotting bit
# It will also call the percentile functions above
# Also assume a site column and a global (all site) column
plot_measure <- function(df, measure, desc, site.col="site.name", plot=TRUE, 
                         outfile=NULL, rm.outlier=FALSE) 
{
    cat("Plotting measure", measure, "-", desc, "\n")
    
    # 1. Remove any missing (NA) values
    df <- remove_nas(df, measure)
    
    # 2. Remove outliers > 3xIQR
    if (rm.outlier) df <- remove_outliers(df, measure)
    
    # Add global column if needed
    if (!("global" %in% colnames(df))) {
      df$global <- "All Sites"
    }
    
    # 3. Start plot
    pg1=ggplot(df, aes_string(x=site.col, y=measure))
    
    # 4. Add the percentile lines (1%, 5%, 25%, 50%, 75%, 95%, 99%)
    perc_df <- calc_percentiles(df, measure)
    pg2=pg1 + compile_percentiles(perc_df, measure)  
  
    # 5. Add main plot
    # - violin plot + boxplot for all the data
    # - jitter plot for each site (adjust the color)
    # - x and y labels
    # subset(df,site=="SI")
    nsites <- length(unique(df[[site.col]]))
    pg3=pg2 + 
      geom_violin(data=df,aes_string(x=site.col), color="gray50") + 
      geom_boxplot(data=df,aes_string(x=site.col), width=.2, fill="gray75", outlier.size=0) + 
      #geom_jitter(data=subset(df,site=="Rutgers"),aes_string(x=site.col,color=site.col), position = position_jitter(width = .1,height=0),size=2,alpha=.75) + 
      
      
      scale_color_manual(values=c(brewer.pal(4,"Dark2"), cmi_site_colors_ramp(nsites))) + 
	  ggtitle(desc) +
      ylab("") + 
      xlab("")
	  # scale_x_discrete(labels="") +
    # 6. Setup text, margins, etc
    pg4=pg3 +
      theme_bw() + 
      theme(panel.grid.major.x= element_blank()) + 
      theme(panel.grid.minor.x= element_blank()) + 
      theme(panel.grid.major.y= element_blank()) + 
      theme(panel.grid.minor.y= element_blank()) + 
	  theme(plot.title = element_text(lineheight=.8, family = "ArialMT",size=10, face="bold")) +
      theme(axis.title.x      = element_text(family = "ArialMT", face = "plain", 
                                  size=10)) +  
      theme(axis.title.y      = element_text(family = "ArialMT", face = "plain", 
                                  size=10, angle=90, vjust=0.75)) +  
      theme(axis.text.x       = element_text(family = "ArialMT", face = "plain", 
                                  size=8, vjust=0.95, hjust=1, angle=45)) + 
      theme(axis.text.y       = element_text(family = "ArialMT", face = "plain", 
                                  size=8, angle=0, hjust=0.5)) + 
      theme(axis.ticks.length = unit(.15, "lines")) + 
      theme(axis.ticks.margin = unit(.15,"lines")) + 
      theme(plot.margin       = unit(c(0, 0.25, 0, 0), "lines")) + 
      theme(legend.position   = "none")

    # End
    pg=pg4
    
    # 7. Plot
    if (plot) {
        cat("...plotting\n")
        print(pg)
    }
    
    # 8. Save
    if (!is.null(outfile)) {
        cat("...saving to", outfile, "\n")
        ggsave(outfile, pg, height=3, width=6, dpi=100)
    }
    
    return(pg)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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