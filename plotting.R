library(ggplot2)
library(RColorBrewer)
library(scales)

# load data
csv <- read.table(file = "C://Users/acormier/Desktop/evolution_techno_sequencage_ligth.csv", header = T, sep = ";")

# color function by parents
ColourPalleteMulti <- function(df, group, subgroup){
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 80)(nrow(categories))) # Set the top of the colour pallete
  category.end  <- (scales::hue_pal(l = 20)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  return(colours)
}

# select colors
csv$group <- paste0(csv$Platform, "-", csv$Label, sep = "")
color_final <- ColourPalleteMulti(csv, "Platform", "Label")

# plot
ggplot(data = csv, aes(x = csv$Read.length..mode..average.or.N50., y = csv$Bases.per.run..gigabases., color = csv$group)) +
  geom_line(lwd = 1.0) +
  geom_point(lwd = 2.8, pch = 16) +
  scale_y_log10(limits = c(0.00001,100000)) +
  scale_x_log10(limits = c(10,100000), labels=function(x) format(x, scientific = FALSE)) +
  #scale_colour_manual(values = color_final) +
  scale_colour_manual(values = color_final,labels=levels(csv$Label)) +
  ylab("Gigabase per run (log scale)") +
  xlab("Read length (log scale)") +
  theme_classic() + 
  theme(legend.title = element_blank()) +
  annotation_logticks()
