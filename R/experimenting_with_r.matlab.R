library(R.matlab)

data <- readMat("/Users/sk/Dropbox/Science/Projects/PhD/manuscripts/ms_D_nanosims_labeling/data/nanosims_data/2014_April/chain3_CH10_3_t1_2/cells.mat")

plot_roi_mask

library(plyr)

library(reshape2)
df <- melt(data)

library(ggplot2)
rois <- subset(
  mutate(df,
    x.px = Var2,
    y.px = max(Var1) - Var1,
    frame_size.px = max(x.px),
    frame_size.um = 10,
    x.um = x.px/frame_size.px * frame_size.um,
    y.um = y.px/frame_size.px * frame_size.um,
    roi = value),
  roi > 0, # remove background which is not a real roi
  select = c("x.px", "y.px", "frame_size.px", "x.um", "y.um", "frame_size.um", "roi")
)

units <- "um"
p <- ggplot(rois, aes_string(
    x = paste0("x.", units), 
    y = paste0("y.", units), 
    fill = "factor(roi)")) + 
  geom_raster() + 
  scale_y_continuous(units, lim = c(0, max(rois[[paste0("frame_size.", units)]])), expand = c(0,0)) +
  scale_x_continuous(units, lim = c(0, max(rois[[paste0("frame_size.", units)]])), expand = c(0,0)) +
  theme(
    legend.position = "none",
#    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black"))




readMat("/Users/sk/Dropbox/Science/Projects/PhD/manuscripts/ms_D_nanosims_labeling/data/nanosims_data/2014_April/chain3_CH10_3_t1_2/mat/14N12C.mat") -> test
df <- melt(test$IM )
library(scales)
ggplot(df, aes(Var2, -Var1, fill = value)) + geom_raster() + scale_fill_continuous(low="black", high="white", limits=c(0,50000))
